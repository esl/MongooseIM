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

%% xmpp_router callback
-export([filter/4, route/4]).

-spec filter(jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()) ->
    drop | xmpp_router:filter().
filter(OrigFrom, OrigTo, OrigAcc, OrigPacket) ->
    %% Filter globally
    case mongoose_hooks:filter_packet({OrigFrom, OrigTo, OrigAcc, OrigPacket}) of
        {From, To, Acc, Packet} ->
            {From, To, Acc, Packet};
        drop ->
            drop
    end.

-spec route(jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()) ->
    {done, mongoose_acc:t()} | xmpp_router:filter().
route(From, To, Acc, Packet) ->
    {From, To, Acc, Packet}.
