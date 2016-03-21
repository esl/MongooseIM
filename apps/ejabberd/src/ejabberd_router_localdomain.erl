%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Mar 2016 12:30
%%%-------------------------------------------------------------------
-module(ejabberd_router_localdomain).
-author("bartek").

-behaviour(xmpp_router).

-include("ejabberd.hrl").
-include("jlib.hrl").

%% API
%% xmpp_router callback
-export([filter/3, route/3]).

filter(From, To, Packet) ->
    {From, To, Packet}.

route(From, To, Packet) ->
    LDstDomain = To#jid.lserver,
    case mnesia:dirty_read(route, LDstDomain) of
        [] ->
            {From, To, Packet};
        [#route{handler=Handler}] ->
            ejabberd_local_delivery:do_local_route(From, To, Packet,
                LDstDomain, Handler),
            done
    end.
