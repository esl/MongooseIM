%%%-------------------------------------------------------------------
%%% @doc
%%% Part of a routing chain: searches for a route registered for the domain,
%%% forwards the message there, or passes on.
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_router_localdomain).
-author('bartlomiej.gorny@erlang-solutions.com').

-behaviour(xmpp_router).

-include("jlib.hrl").

%% xmpp_router callback
-export([filter/4, route/4]).

-spec filter(jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()) ->
    drop | xmpp_router:filter().
filter(From, To, Acc, Packet) ->
    {From, To, Acc, Packet}.

-spec route(jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()) ->
    {done, mongoose_acc:t()} | xmpp_router:filter().
route(From, To, Acc0, Packet) ->
    LDstDomain = To#jid.lserver,
    case mongoose_router:lookup_route(LDstDomain) of
        no_route -> {From, To, Acc0, Packet};
        Handler ->
            Acc1 = mongoose_local_delivery:do_route(From, To, Acc0, Packet, Handler),
            {done, Acc1}
    end.
