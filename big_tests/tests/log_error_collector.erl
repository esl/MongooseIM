%% @doc Logger handler injected into MIM nodes to collect error logs into ETS.
%% This module is injected by log_error_helper and should not be used directly.
-module(log_error_collector).

-export([start/1, stop/0, get_errors/0, clear/0, timestamp/0]).
-export([adding_handler/1, removing_handler/1, log/2]).  %% Logger callbacks
-export([table_owner_loop/0]).  %% Internal - for spawned process

-define(TABLE, log_error_collector_table).
-define(OWNER, log_error_collector_owner).

%% API

-spec start([atom()]) -> ok | {error, term()}.
start(Levels) ->
    %% Spawn a dedicated process to own the ETS table
    %% This ensures the table survives across RPC calls
    Owner = spawn(?MODULE, table_owner_loop, []),
    register(?OWNER, Owner),
    Owner ! {create_table, self()},
    receive
        table_created -> ok
    after 5000 ->
        error(table_creation_timeout)
    end,
    logger:add_handler(?MODULE, ?MODULE, #{levels => Levels}).

-spec stop() -> ok | {error, term()}.
stop() ->
    Result = logger:remove_handler(?MODULE),
    case whereis(?OWNER) of
        undefined -> ok;
        Pid ->
            Pid ! stop,
            ok
    end,
    Result.

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

-spec get_errors() -> [{integer(), atom(), binary()}].
get_errors() ->
    ets:tab2list(?TABLE).

-spec clear() -> true.
clear() ->
    ets:delete_all_objects(?TABLE).

-spec timestamp() -> integer().
timestamp() ->
    erlang:monotonic_time().

%% Logger callbacks

adding_handler(Config) ->
    {ok, Config}.

removing_handler(_Config) ->
    ok.

log(#{level := Level, msg := Msg} = _Event, #{levels := Levels} = _Config) ->
    case lists:member(Level, Levels) of
        true ->
            FormattedMsg = format_msg(Msg),
            ets:insert(?TABLE, {erlang:monotonic_time(), Level, FormattedMsg});
        false ->
            ok
    end.

%% Internal functions

-spec format_msg(term()) -> binary().
format_msg({string, String}) when is_list(String) ->
    unicode:characters_to_binary(String);
format_msg({string, Binary}) when is_binary(Binary) ->
    Binary;
format_msg({report, Report}) when is_map(Report) ->
    unicode:characters_to_binary(io_lib:format("~0p", [Report]));
format_msg({Format, Args}) when is_list(Format), is_list(Args) ->
    unicode:characters_to_binary(io_lib:format(Format, Args));
format_msg(Other) ->
    unicode:characters_to_binary(io_lib:format("~0p", [Other])).
