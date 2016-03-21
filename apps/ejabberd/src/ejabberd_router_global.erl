%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Mar 2016 12:30
%%%-------------------------------------------------------------------
-module(ejabberd_router_global).
-author("bartek").

-behaviour(xmpp_router).

-include("ejabberd.hrl").

%% xmpp_router callback
-export([filter/3, route/3]).

filter(OrigFrom, OrigTo, OrigPacket) ->
    %% Filter globally
    case ejabberd_hooks:run_fold(filter_packet,
        {OrigFrom, OrigTo, OrigPacket}, []) of
        {From, To, Packet} ->
            {From, To, Packet};
        drop ->
            drop
    end.

route(From, To, Packet) ->
    {From, To, Packet}.
