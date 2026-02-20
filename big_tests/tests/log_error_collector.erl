%% @doc Logger handler injected into MIM nodes to collect error logs into ETS.
%% This module is injected by log_error_helper and should not be used directly.
%%
%% Stores structured log data for precise pattern matching:
%% - {Timestamp, Level, Msg, Meta}
%% - Msg is the original logger msg tuple: {report, Map} | {string, S} | {Format, Args}
%% - Meta contains extracted metadata: #{mfa => {M, F, A}, ...}
-module(log_error_collector).

-export([start/1, stop/0, get_errors/0, clear/0, timestamp/0]).
-export([adding_handler/1, removing_handler/1, log/2]).  %% Logger callbacks
-export([table_owner_loop/0]).  %% Internal - for spawned process

-define(TABLE, log_error_collector_table).
-define(OWNER, log_error_collector_owner).

%% Stored entry: {Timestamp, Level, Msg, Meta}
%% - Msg: {report, #{what => atom(), ...}} | {string, binary()} | {Format, Args}
%% - Meta: #{mfa => {Module, Function, Arity}} | #{}
-type log_entry() :: {integer(), atom(), msg(), meta()}.
-type msg() :: {report, map()} | {string, binary() | string()} | {list(), list()} | term().
-type meta() :: #{mfa => mfa(), atom() => term()}.

-export_type([log_entry/0, msg/0, meta/0]).

%% API

%% @doc Start collecting errors. Idempotent - safe to call if already started.
-spec start([atom()]) -> ok | {error, term()}.
start(Levels) ->
    case whereis(?OWNER) of
        undefined ->
            do_start(Levels);
        _Pid ->
            %% Already started, just return ok
            ok
    end.

do_start(Levels) ->
    %% Spawn a dedicated process to own the ETS table
    %% This ensures the table survives across RPC calls
    Owner = spawn(?MODULE, table_owner_loop, []),
    try register(?OWNER, Owner) of
        true ->
            Owner ! {create_table, self()},
            receive
                table_created -> ok
            after 5000 ->
                error(table_creation_timeout)
            end,
            logger:add_handler(?MODULE, ?MODULE, #{config => #{levels => Levels}})
    catch
        error:badarg ->
            %% Race condition: another process registered between whereis and register
            Owner ! stop,
            ok
    end.

%% @doc Stop collecting errors. Idempotent - safe to call if already stopped.
-spec stop() -> ok | {error, term()}.
stop() ->
    case whereis(?OWNER) of
        undefined ->
            %% Already stopped
            ok;
        Pid ->
            _ = logger:remove_handler(?MODULE),
            Pid ! stop,
            ok
    end.

%% Table owner process - keeps the ETS table alive
table_owner_loop() ->
    receive
        {create_table, Caller} ->
            ets:new(?TABLE, [named_table, public, ordered_set]),
            Caller ! table_created,
            table_owner_loop();
        stop ->
            ets:delete(?TABLE),
            ok
    end.

-spec get_errors() -> [log_entry()].
get_errors() ->
    case ets:whereis(?TABLE) of
        undefined -> [];
        _Tid -> ets:tab2list(?TABLE)
    end.

-spec clear() -> true | ok.
clear() ->
    case ets:whereis(?TABLE) of
        undefined -> ok;
        _Tid -> ets:delete_all_objects(?TABLE)
    end.

-spec timestamp() -> integer().
timestamp() ->
    erlang:monotonic_time().

%% Logger callbacks

adding_handler(Config) ->
    {ok, Config}.

removing_handler(_Config) ->
    ok.

log(#{level := Level, msg := Msg, meta := LogMeta} = _Event, #{config := #{levels := Levels}}) ->
    case lists:member(Level, Levels) of
        true ->
            %% Extract relevant metadata
            Meta = extract_meta(LogMeta),
            %% Store the original msg structure for precise pattern matching
            ets:insert(?TABLE, {erlang:monotonic_time(), Level, Msg, Meta});
        false ->
            ok
    end;
log(#{level := Level, msg := Msg} = _Event, #{config := #{levels := Levels}}) ->
    %% Fallback if no meta present
    case lists:member(Level, Levels) of
        true ->
            ets:insert(?TABLE, {erlang:monotonic_time(), Level, Msg, #{}});
        false ->
            ok
    end.

%% Internal functions

-spec extract_meta(map()) -> meta().
extract_meta(LogMeta) ->
    maps:with([mfa, file, line, pid], LogMeta).
