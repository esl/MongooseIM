-module(mod_global_distrib_worker).

-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

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
    {Stamp2, {From, To, Acc}} = erlang:binary_to_term(Data),
    Now = erlang:system_time(milli_seconds),
    %% lager:error("Send to route: ~p ms", [Now - Stamp2]),
    %% lager:error("Recv to route: ~p ms", [erlang:convert_time_unit(erlang:monotonic_time() - Stamp, native, milli_seconds)]),
    %% lager:error("~p", [ erlang:process_info(self(), message_queue_len)]),
    {Duration, _} = timer:tc(ejabberd_router, route, [From, To, Acc]),
    %% lager:error("Routing duration: ~p ms", [erlang:convert_time_unit(Duration, micro_seconds, milli_seconds)]),
    {noreply, State, ?TIMEOUT}.

handle_info(timeout, State) ->
    {stop, {shutdown, spinning_down_idle_worker}, State};
handle_info(_, State) ->
    {noreply, State, ?TIMEOUT}.

terminate(Reason, State) ->
    ok.
