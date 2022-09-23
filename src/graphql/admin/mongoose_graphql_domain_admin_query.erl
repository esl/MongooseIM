-module(mongoose_graphql_domain_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(_Ctx, admin, <<"domainsByHostType">>, #{<<"hostType">> := HostType}) ->
    Domains = mongoose_domain_api:get_domains_by_host_type(HostType),
    Domains2 = lists:map(fun(D) -> {ok, D} end, Domains),
    {ok, Domains2};
execute(_Ctx, admin, <<"domainDetails">>, #{<<"domain">> := Domain}) ->
    case mongoose_domain_sql:select_domain(Domain) of
        {ok, #{host_type := HostType, status := Status}} ->
            {ok, #domain{host_type = HostType, domain = Domain,
                         enabled = Status =:= enabled}};
        {error, not_found} ->
            {error, #{what => domain_not_found, domain => Domain}}
    end.
