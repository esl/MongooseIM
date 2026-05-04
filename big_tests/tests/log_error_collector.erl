%% @doc Logger handler injected into MIM nodes that pushes error log
%% events to the test-runner-side `cth_error_report_sink'.
%%
%% Stateless on the MIM node: no ETS, no owner process. Each error
%% event becomes a message
%%   {log_entry, node(), Level, Msg, Meta}
%% sent to the sink with [noconnect, nosuspend], so a slow or
%% disconnected runner cannot back-pressure logging.
-module(log_error_collector).

-export([start/2, stop/0]).
%% Logger callbacks
-export([adding_handler/1, removing_handler/1, log/2]).

-define(HANDLER_ID, cth_error_report_handler).

-type sink_ref() :: {RegName :: atom(), Node :: node()}.
-type level() :: atom().
-type msg() :: {report, map()} | {string, binary() | string()}
             | {list(), list()} | term().
-type meta() :: #{atom() => term()}.

-export_type([sink_ref/0, level/0, msg/0, meta/0]).

%% API

%% @doc Add the logger handler that forwards `Levels'-level events
%% to `SinkRef' (a `{RegisteredName, Node}' tuple). Idempotent.
-spec start([level()], sink_ref()) -> ok | {error, term()}.
start(Levels, SinkRef) ->
    case logger:get_handler_config(?HANDLER_ID) of
        {ok, _} -> ok;
        {error, _} ->
            logger:add_handler(?HANDLER_ID, ?MODULE,
                               #{config => #{levels => Levels, sink => SinkRef}})
    end.

%% @doc Remove the logger handler. Idempotent.
-spec stop() -> ok.
stop() ->
    _ = logger:remove_handler(?HANDLER_ID),
    ok.

%% Logger callbacks

adding_handler(Config) ->
    {ok, Config}.

removing_handler(_Config) ->
    ok.

log(#{level := Level, msg := Msg, meta := LogMeta},
    #{config := #{levels := Levels, sink := SinkRef}}) ->
    case lists:member(Level, Levels) of
        true ->
            Meta = extract_meta(LogMeta),
            erlang:send(SinkRef, {log_entry, node(), Level, Msg, Meta}, [noconnect, nosuspend]),
            ok;
        false ->
            ok
    end.

%% Internal

-spec extract_meta(map()) -> meta().
extract_meta(LogMeta) ->
    Meta = maps:with([mfa, file, line, pid, time], LogMeta),
    Meta#{node => node()}.
