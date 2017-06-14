-module(mod_global_distrib_worker).

-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("global_distrib_metrics.hrl").

-define(TIMEOUT, 60000). % TODO

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(_) ->
    {ok, state, ?TIMEOUT}.

handle_call({data, Stamp, Data}, From, State) ->
    gen_server:reply(From, ok),
    handle_cast({data, Stamp, Data}, State).

handle_cast({data, Stamp, Data}, State) ->
    QueueTimeNative = p1_time_compat:monotonic_time() - Stamp,
    QueueTimeUS = p1_time_compat:convert_time_unit(QueueTimeNative, native, micro_seconds),
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_RECV_QUEUE_TIME, QueueTimeUS),
    {_Stamp2, {From, To, Acc}} = erlang:binary_to_term(Data), %% TODO: Stamp
    ejabberd_router:route(From, To, Acc),
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_MESSAGES_RECEIVED, 1),
    {noreply, State, ?TIMEOUT}.

handle_info(timeout, State) ->
    {stop, {shutdown, spinning_down_idle_worker}, State};
handle_info(_, State) ->
    {noreply, State, ?TIMEOUT}.

terminate(Reason, State) ->
    ok.
