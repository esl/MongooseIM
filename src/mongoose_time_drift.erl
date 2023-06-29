%% Compares system clocks on all nodes across the cluster.
%% Fires an alarm, if the cluster contains nodes with wrong clocks.
%%
%% It is important for the cluster to be able to use the timestamps
%% which are synced across the cluster. We recommend to use an NTP server for that.
%% Stuff which gets issues with wrong clocks:
%% - MAM message IDs.
%% - CETS node discovery in RDBMS
%%   (the cleaning logic would be disabled if the alarm is fired by this module).
-module(mongoose_time_drift).

-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, has_alarm/0]).

%% Callback
-export([system_time/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ignore_xref([start_link/0, system_time/0]).

-record(state, {}).

-spec system_time() -> integer().
system_time() ->
    os:system_time(seconds).

max_drift_time() ->
    30. %% seconds

check_time_drift() ->
    NodeTimestamps = timestamps(),
    Timestamps = [Timestamp || {_Node, Timestamp} <- NodeTimestamps],
    Max = lists:max(Timestamps),
    Min = lists:min(Timestamps),
    Drift = (Max - Min),
    Fired = Drift > max_drift_time(),
    set_alarm(Fired, #{node_timestamps => NodeTimestamps}).

set_alarm(NewFired, Info) ->
    OldFired = has_alarm(),
    case OldFired =:= NewFired of
        true ->
            ok;
        false ->
            force_set_alarm(NewFired, Info)
    end.

force_set_alarm(true, Info) ->
    ?LOG_WARNING(Info#{what => mongoose_time_drift_fired}),
    alarm_handler:set_alarm({mongoose_time_drift, "Time is not in sync across the cluster"});
force_set_alarm(false, Info) ->
    ?LOG_WARNING(Info#{what => mongoose_time_drift_cleared}),
    alarm_handler:clear_alarm(mongoose_time_drift).

timestamps() ->
    Nodes = [node()|nodes()],
    Results = [{Node, rpc:call(Node, ?MODULE, system_time, [])} || Node <- Nodes],
    [{Node, R} || {Node, R} <- Results, is_integer(R)].

-spec has_alarm() -> boolean().
has_alarm() ->
    lists:keymember(mongoose_time_drift, 1, alarm_handler:get_alarms()).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    check_time_drift(),
    timer:send_interval(timer:seconds(60), check),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check, State) ->
    check_time_drift(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
     ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
