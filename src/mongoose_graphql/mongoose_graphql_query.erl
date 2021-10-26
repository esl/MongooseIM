-module(mongoose_graphql_query).

-export([execute/4]).

-ignore_xref([execute/4]).

-import(mongoose_graphql_permission, [if_permitted/3]).

-include("mongoose_graphql_types.hrl").

execute(Ctx, _Obj, <<"domainsByHostType">>, #{<<"hostType">> := HostType}) ->
    if_permitted(Ctx, admin,
        fun() ->
            Domains = mongoose_domain_api:get_domains_by_host_type(HostType),
            Domains2 = lists:map(fun(D) -> {ok, D} end, Domains),
            {ok, Domains2}
        end);
execute(Ctx, _Obj, <<"domainDetails">>, #{<<"domain">> := Domain}) ->
    if_permitted(Ctx, admin,
        fun() ->
            case mongoose_domain_sql:select_domain(Domain) of
                {ok, #{host_type := HostType, enabled := Enabled}} ->
                    {ok, #domain{host_type = HostType, domain = Domain,
                                 enabled = Enabled}};
                {error, not_found} = Err ->
                    Err
            end
        end).
