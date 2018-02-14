%%%-------------------------------------------------------------------
%%% @doc
%%% Part of a routing chain; does only filtering - uses hooks to check
%%% if the message should be routed at all. This one should be the first
%%% module in chain.
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_router_global).
-author('bartlomiej.gorny@erlang-solutions.com').

-behaviour(xmpp_router).

-include("mongoose.hrl").

%% xmpp_router callback
-export([filter/4, route/4]).

filter(OrigFrom, OrigTo, OrigAcc, OrigPacket) ->
    %% Filter globally
    case ejabberd_hooks:run_fold(filter_packet,
        {OrigFrom, OrigTo, OrigAcc, OrigPacket}, []) of
        {From, To, Acc, Packet} ->
            {From, To, Acc, Packet};
        drop ->
            drop
    end.

route(From, To, Acc, Packet) ->
    {From, To, Acc, Packet}.
