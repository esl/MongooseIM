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

%% API
-export([filter/3, route/3]).
%% xmpp_router callback
-export([do_filter/3, do_route/3]).

filter(From, To, Packet) ->
    xmpp_router:filter(?MODULE, From, To, Packet).
route(From, To, Packet) ->
    xmpp_router:route(?MODULE, From, To, Packet).

do_filter(OrigFrom, OrigTo, OrigPacket) ->
    %% Filter globally
    case ejabberd_hooks:run_fold(filter_packet,
        {OrigFrom, OrigTo, OrigPacket}, []) of
        {From, To, Packet} ->
            {From, To, Packet};
        drop ->
            drop
    end.

do_route(From, To, Packet) ->
    {From, To, Packet}.
