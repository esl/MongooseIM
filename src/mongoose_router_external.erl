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

-include("mongoose.hrl").
-include("jlib.hrl").
-include("external_component.hrl").
%% xmpp_router callback
-export([filter/4, route/4]).

filter(OrigFrom, OrigTo, OrigAcc, OrigPacket) ->
    {OrigFrom, OrigTo, OrigAcc, OrigPacket}.

route(From, To, Acc, Packet) ->
    LDstDomain = To#jid.lserver,
    case ejabberd_router:lookup_component(LDstDomain) of
        [] ->
            {From, To, Acc, Packet};
        [#external_component{handler = Handler}|_] -> %% may be multiple on various nodes
            mongoose_local_delivery:do_route(From, To, Acc, Packet,
                LDstDomain, Handler),
            done
    end.
