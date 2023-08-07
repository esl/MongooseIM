-module(mongoose_router).

-define(TABLE, ?MODULE).

-export([start/0, routing_modules_list/0, default_routing_modules/0]).

-export([get_all_domains/0, lookup_route/1, is_registered_route/1,
         register_route/2, unregister_route/1]).

-spec get_all_domains() -> [jid:lserver()].
get_all_domains() ->
    ets:select(?TABLE, [{{'$1', '_'}, [], ['$1']}]).

-spec lookup_route(jid:lserver()) -> no_route | mongoose_packet_handler:t().
lookup_route(LDomain) ->
    case ets:lookup(?TABLE, LDomain) of
        [] -> no_route;
        [{_, Handler}] ->
            Handler
    end.

-spec register_route(jid:server(), mongoose_packet_handler:t()) -> any().
register_route(Domain, Handler) ->
    case jid:nameprep(Domain) of
        error ->
            {error, invalid_domain, Domain};
        LDomain ->
            ets:insert(?TABLE, {LDomain, Handler})
    end.

-spec unregister_route(jid:server()) -> any().
unregister_route(Domain) ->
    case jid:nameprep(Domain) of
        error ->
            {error, invalid_domain, Domain};
        LDomain ->
            ets:delete(?TABLE, LDomain)
    end.

-spec is_registered_route(jid:lserver()) -> boolean().
is_registered_route(LDomain) ->
    ets:member(?TABLE, LDomain).

%% start/stop
start() ->
    ets:new(?TABLE, [named_table, public, set, {read_concurrency, true}]),
    mongoose_metrics:ensure_metric(global, routingErrors, spiral).

default_routing_modules() ->
    List = [mongoose_router_global,
            mongoose_router_localdomain,
            mongoose_router_external_localnode,
            mongoose_router_external,
            mongoose_router_dynamic_domains,
            ejabberd_s2s],
    xmpp_router:expand_routing_modules(List).

routing_modules_list() ->
    mongoose_config:get_opt(routing_modules).
