-module(mod_global_distrib_worker).

-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

init(_) ->
    {ok, state}.

handle_call({data, Data}, From, State) ->
    gen_server:reply(From, ok),
    handle_cast({data, Data}, State).

handle_cast({data, Data}, State) ->
    {From, To, Acc} = erlang:binary_to_term(Data),
    ejabberd_router:route(From, To, Acc),
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.
