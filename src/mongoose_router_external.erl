%%%-------------------------------------------------------------------
%%% @doc
%%% Part of a routing chain; checks if there is an external component
%%% registered for the domain and directs the message there if there is,
%%% otherwise just passes it on.
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_router_external).
-author('bartlomiej.gorny@erlang-solutions.com').

-behaviour(xmpp_router).

-include("jlib.hrl").
-include("external_component.hrl").
%% xmpp_router callback
-export([filter/4, route/4]).

-spec filter(jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()) ->
    drop | xmpp_router:filter().
filter(OrigFrom, OrigTo, OrigAcc, OrigPacket) ->
    {OrigFrom, OrigTo, OrigAcc, OrigPacket}.

-spec route(jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()) ->
    {done, mongoose_acc:t()} | xmpp_router:filter().
route(From, To, Acc0, Packet) ->
    LDstDomain = To#jid.lserver,
    case mongoose_component:lookup_component(LDstDomain) of
        [] ->
            {From, To, Acc0, Packet};
        [#external_component{handler = Handler}|_] -> %% may be multiple on various nodes
            Acc1 = mongoose_local_delivery:do_route(From, To, Acc0, Packet, Handler),
            {done, Acc1}
    end.
