%%%-------------------------------------------------------------------
%%% @doc
%%% this router should be tried in the very end, but before s2s.
%%% it checks if destination domain is configured dynamically,
%%% if it is so - the router adds domain fo the routing table,
%%% and retries local routing.
%%%
%%% this ensures lazy dynamic domains registration in the routing
%%% table.
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_router_dynamic_domains).
-author('bartlomiej.gorny@erlang-solutions.com').

-behaviour(xmpp_router).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("route.hrl").

%% API
%% xmpp_router callback
-export([filter/4, route/4]).

filter(From, To, Acc, Packet) ->
    {From, To, Acc, Packet}.

route(From, To, Acc, Packet) ->
    LDstDomain = To#jid.lserver,
    case mongoose_domain_api:get_host_type(LDstDomain) of
        {ok, _} ->
            ejabberd_local:register_host(LDstDomain),
            mongoose_router_localdomain:route(From, To, Acc, Packet);
        {error, not_found} -> {From, To, Acc, Packet}
    end.

