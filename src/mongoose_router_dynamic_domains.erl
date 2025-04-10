%%%-------------------------------------------------------------------
%%% @doc
%%% this router should be tried in the very end, but before s2s.
%%% it checks if destination domain is configured dynamically,
%%% if it is so - the router adds domain to the routing table,
%%% and retries local routing.
%%%
%%% this ensures lazy dynamic domains registration in the routing
%%% table.
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_router_dynamic_domains).

-behaviour(xmpp_router).

-include("jlib.hrl").

%% API
%% xmpp_router callback
-export([filter/4, route/4]).

-spec filter(jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()) ->
    drop | xmpp_router:filter().
filter(From, To, Acc, Packet) ->
    {From, To, Acc, Packet}.

-spec route(jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()) ->
    {done, mongoose_acc:t()} | xmpp_router:filter().
route(From, To, Acc, Packet) ->
    LDstDomain = To#jid.lserver,
    case mongoose_lazy_routing:maybe_add_domain_or_subdomain(LDstDomain) of
        true ->
            mongoose_router_localdomain:route(From, To, Acc, Packet);
        false -> {From, To, Acc, Packet}
    end.
