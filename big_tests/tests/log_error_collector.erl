%% @doc Logger handler injected into MIM nodes to collect error logs into ETS.
%% This module is injected into MIM nodes by cth_error_report.
%%
%% Supports multiple named instances running simultaneously.
%% The default instance uses the atom `log_error_collector' as the name.
%% Each instance has its own ETS table, owner process, and logger handler.
%%
%% Stores structured log data for precise pattern matching:
%% - {Timestamp, Level, Msg, Meta}
%% - Msg is the original logger msg tuple: {report, Map} | {string, S} | {Format, Args}
%% - Meta contains extracted metadata: #{mfa => {M, F, A}, ...}
-module(log_error_collector).

-export([start/1, start/2, stop/0, stop/1,
         get_errors/0, get_errors/1,
         get_errors_after/1, get_errors_after/2,
         clear/0, clear/1, timestamp/0]).
-export([adding_handler/1, removing_handler/1, log/2]).  %% Logger callbacks
-export([table_owner_loop/0]).  %% Internal - for spawned process

-define(DEFAULT_NAME, ?MODULE).

%% Stored entry: {Timestamp, Level, Msg, Meta}
%% - Msg: {report, #{what => atom(), ...}} | {string, binary()} | {Format, Args}
%% - Meta: #{mfa => {Module, Function, Arity}} | #{}
-type log_entry() :: {integer(), atom(), msg(), meta()}.
-type msg() :: {report, map()} | {string, binary() | string()} | {list(), list()} | term().
-type meta() :: #{mfa => mfa(), atom() => term()}.
-type instance_name() :: atom().

-export_type([log_entry/0, msg/0, meta/0, instance_name/0]).

%% API - default instance

%% @doc Start collecting errors with the default instance name.
-spec start([atom()]) -> ok | {error, term()}.
start(Levels) ->
    start(?DEFAULT_NAME, Levels).

%% @doc Stop the default instance.
-spec stop() -> ok | {error, term()}.
stop() ->
    stop(?DEFAULT_NAME).

%% @doc Get errors from the default instance.
-spec get_errors() -> [log_entry()].
get_errors() ->
    get_errors(?DEFAULT_NAME).

%% @doc Clear the default instance.
-spec clear() -> true | ok.
clear() ->
    clear(?DEFAULT_NAME).

%% API - named instances

%% @doc Start collecting errors with a given instance name. Idempotent.
-spec start(instance_name(), [atom()]) -> ok | {error, term()}.
start(Name, Levels) ->
    OwnerName = owner_name(Name),
    case whereis(OwnerName) of
        undefined ->
            do_start(Name, Levels);
        _Pid ->
            ok
    end.

%% @doc Stop the named instance. Idempotent.
-spec stop(instance_name()) -> ok | {error, term()}.
stop(Name) ->
    OwnerName = owner_name(Name),
    HandlerId = handler_id(Name),
    case whereis(OwnerName) of
        undefined ->
            ok;
        Pid ->
            _ = logger:remove_handler(HandlerId),
            Pid ! stop,
            ok
    end.

%% @doc Get errors from the named instance.
-spec get_errors(instance_name()) -> [log_entry()].
get_errors(Name) ->
    TableName = table_name(Name),
    case ets:whereis(TableName) of
        undefined -> [];
        _Tid -> ets:tab2list(TableName)
    end.

%% @doc Clear the named instance.
-spec clear(instance_name()) -> true | ok.
clear(Name) ->
    TableName = table_name(Name),
    case ets:whereis(TableName) of
        undefined -> ok;
        _Tid -> ets:delete_all_objects(TableName)
    end.

%% @doc Get errors with timestamp strictly greater than After (default instance).
-spec get_errors_after(integer()) -> [log_entry()].
get_errors_after(After) ->
    get_errors_after(?DEFAULT_NAME, After).

%% @doc Get errors with timestamp strictly greater than After (named instance).
-spec get_errors_after(instance_name(), integer()) -> [log_entry()].
get_errors_after(Name, After) ->
    TableName = table_name(Name),
    case ets:whereis(TableName) of
        undefined ->
            [];
        _Tid ->
            MatchSpec = [{{'$1', '$2', '$3', '$4'},
                          [{'>', '$1', {const, After}}],
                          ['$_']}],
            ets:select(TableName, MatchSpec)
    end.

-spec timestamp() -> integer().
timestamp() ->
    erlang:monotonic_time().

%% Internal - starting

do_start(Name, Levels) ->
    OwnerName = owner_name(Name),
    TableName = table_name(Name),
    HandlerId = handler_id(Name),
    Owner = spawn(?MODULE, table_owner_loop, []),
    try register(OwnerName, Owner) of
        true ->
            Owner ! {create_table, TableName, self()},
            receive
                table_created -> ok
            after 5000 ->
                error(table_creation_timeout)
            end,
            HandlerConfig = #{config => #{levels => Levels, table => TableName}},
            logger:add_handler(HandlerId, ?MODULE, HandlerConfig)
    catch
        error:badarg ->
            Owner ! stop,
            ok
    end.

%% Table owner process - keeps the ETS table alive
table_owner_loop() ->
    receive
        {create_table, TableName, Caller} ->
            ets:new(TableName, [named_table, public, ordered_set]),
            Caller ! table_created,
            table_owner_loop(TableName);
        stop ->
            ok
    end.

table_owner_loop(TableName) ->
    receive
        stop ->
            ets:delete(TableName),
            ok
    end.

%% Logger callbacks

adding_handler(Config) ->
    {ok, Config}.

removing_handler(_Config) ->
    ok.

log(#{level := Level, msg := Msg, meta := LogMeta},
    #{config := #{levels := Levels, table := Table}}) ->
    case lists:member(Level, Levels) of
        true ->
            Meta = extract_meta(LogMeta),
            ets:insert(Table, {erlang:monotonic_time(), Level, Msg, Meta});
        false ->
            ok
    end;
log(#{level := Level, msg := Msg},
    #{config := #{levels := Levels, table := Table}}) ->
    case lists:member(Level, Levels) of
        true ->
            ets:insert(Table, {erlang:monotonic_time(), Level, Msg, #{}});
        false ->
            ok
    end.

%% Name derivation

-spec table_name(instance_name()) -> atom().
table_name(Name) ->
    to_atom(Name, "_table").

-spec owner_name(instance_name()) -> atom().
owner_name(Name) ->
    to_atom(Name, "_owner").

-spec handler_id(instance_name()) -> atom().
handler_id(Name) ->
    to_atom(Name, "_handler").

-spec to_atom(atom(), string()) -> atom().
to_atom(Name, Suffix) ->
    list_to_atom(atom_to_list(Name) ++ Suffix).

%% Internal functions

-spec extract_meta(map()) -> meta().
extract_meta(LogMeta) ->
    Meta = maps:with([mfa, file, line, pid, time], LogMeta),
    Meta#{node => node()}.
