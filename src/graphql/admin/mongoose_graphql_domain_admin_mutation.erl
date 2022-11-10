-module(mongoose_graphql_domain_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-import(mongoose_graphql_helper, [format_result/2]).

-include("../mongoose_graphql_types.hrl").

execute(_Ctx, admin, <<"addDomain">>, #{<<"domain">> := Domain, <<"hostType">> := HostType}) ->
    format_result(mongoose_domain_api:insert_domain(Domain, HostType),
                  #{domain => Domain, hostType => HostType});
execute(_Ctx, admin, <<"removeDomain">>, #{<<"domain">> := Domain, <<"hostType">> := HostType}) ->
    format_result(mongoose_domain_api:delete_domain(Domain, HostType),
                  #{domain => Domain, hostType => HostType});
execute(_Ctx, admin, <<"requestRemoveDomain">>, #{<<"domain">> := Domain,
                                                  <<"hostType">> := HostType}) ->
    format_result(mongoose_domain_api:request_delete_domain(Domain, HostType),
                  #{domain => Domain, hostType => HostType});
execute(_Ctx, admin, <<"enableDomain">>, #{<<"domain">> := Domain}) ->
    format_result(mongoose_domain_api:enable_domain(Domain), #{domain => Domain});
execute(_Ctx, admin, <<"disableDomain">>, #{<<"domain">> := Domain}) ->
    format_result(mongoose_domain_api:disable_domain(Domain), #{domain => Domain});
execute(_Ctx, admin, <<"setDomainPassword">>,
        #{<<"domain">> := Domain, <<"password">> := Password}) ->
    format_result(mongoose_domain_api:set_domain_password(Domain, Password), #{domain => Domain});
execute(_Ctx, admin, <<"deleteDomainPassword">>, #{<<"domain">> := Domain}) ->
    format_result(mongoose_domain_api:delete_domain_password(Domain), #{domain => Domain}).
