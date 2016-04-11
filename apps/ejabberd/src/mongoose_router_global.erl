%%%-------------------------------------------------------------------
%%% @doc
%%% Part of a routing chain; does only filtering - uses hooks to check
%%% if the message should be routed at all. This one should be the first
%%% module in chain.
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_router_global).
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
