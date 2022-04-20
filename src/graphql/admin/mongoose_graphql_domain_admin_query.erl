-module(mongoose_graphql_domain_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(_Ctx, admin, <<"getVcard">>, #{<<"domain">> := Domain}) ->
    case mongoose_domain_sql:select_domain(Domain) of
        {ok, #{host_type := HostType, enabled := Enabled}} ->
            {ok, #domain{host_type = HostType, domain = Domain,
                         enabled = Enabled}};
        {error, not_found} ->
            {error, #{what => domain_not_found, domain => Domain}}
    end.
