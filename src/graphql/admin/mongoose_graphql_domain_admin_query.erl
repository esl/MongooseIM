-module(mongoose_graphql_domain_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-import(mongoose_graphql_helper, [format_result/2]).

-include("../mongoose_graphql_types.hrl").

execute(_Ctx, admin, <<"domainsByHostType">>, #{<<"hostType">> := HostType}) ->
    case mongoose_domain_api:check_host_type_and_get_domains(HostType) of
        {ok, Domains} ->
            Domains2 = lists:map(fun(D) -> {ok, D} end, Domains),
            {ok, Domains2};
        Error ->
            format_result(Error, #{hostType => HostType})
    end;
execute(_Ctx, admin, <<"domainDetails">>, #{<<"domain">> := Domain}) ->
    format_result(mongoose_domain_api:get_domain_details(Domain), #{domain => Domain}).
