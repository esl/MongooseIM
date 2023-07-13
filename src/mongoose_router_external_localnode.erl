%%%-------------------------------------------------------------------
%%% @doc
%%% Part of a routing chain; checks if there is an external component
%%% registered for the domain on this node and directs the message there if there is,
%%% otherwise just passes it on.
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_router_external_localnode).
-author('bartlomiej.gorny@erlang-solutions.com').

-behaviour(xmpp_router).

-include("jlib.hrl").
-include("external_component.hrl").

%% xmpp_router callback
-export([filter/4, route/4]).

filter(OrigFrom, OrigTo, OrigAcc, OrigPacket) ->
    {OrigFrom, OrigTo, OrigAcc, OrigPacket}.

route(From, To, Acc0, Packet) ->
    LDstDomain = To#jid.lserver,
    case mongoose_component:lookup_component(LDstDomain, node()) of
        [] ->
            {From, To, Acc0, Packet};
        [#external_component{handler = Handler}] ->
            Acc1 = mongoose_local_delivery:do_route(From, To, Acc0, Packet, Handler),
            {done, Acc1}
    end.
