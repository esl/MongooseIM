%%%-------------------------------------------------------------------
%%% @doc
%%% Part of a routing chain: searches for a route registered for the domain,
%%% forwards the messge there, or passes on.
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_router_localdomain).
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
    case route_to_host(From, To, Acc, Packet, LDstDomain) of
        done   -> done;
        Result ->
            case mongoose_subhosts:get_host(LDstDomain) of
                {ok, Host} -> route_to_host(From, To, Acc, Packet, Host);
                undefined  -> Result
            end
    end.

route_to_host(From, To, Acc, Packet, Host) ->
    case mnesia:dirty_read(route, Host) of
        [] -> {From, To, Acc, Packet};
        [#route{handler = Handler}] ->
            mongoose_local_delivery:do_route(From, To, Acc, Packet, Host, Handler),
            done
    end.
