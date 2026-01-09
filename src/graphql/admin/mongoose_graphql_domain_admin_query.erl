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
execute(_Ctx, admin, <<"allDomains">>, _Args) ->
    DynamicDomains = mongoose_domain_api:get_all_domains(),
    StaticDomains = mongoose_domain_api:get_all_static(),

    DynamicList = [D#{type => dynamic} || D <- DynamicDomains],
    StaticList = [#{domain => D, host_type => HT, status => enabled, type => static}
                  || {D, HT} <- StaticDomains],

    AllDomains = merge_domains(StaticList, DynamicList),
    {ok, [ {ok, D} || D <- AllDomains ]};
execute(_Ctx, admin, <<"domainDetails">>, #{<<"domain">> := Domain}) ->
    case mongoose_domain_api:get_domain_details(Domain) of
        {ok, Details} ->
            {ok, Details#{type => dynamic}};
        {static, _} ->
            {ok, HostType} = mongoose_domain_api:get_domain_host_type(Domain),
            {ok, #{domain => Domain, host_type => HostType, status => enabled, type => static}};
        Error ->
            format_result(Error, #{domain => Domain})
    end.

merge_domains(Static, Dynamic) ->
    StaticMap = maps:from_list([{maps:get(domain, D), D} || D <- Static]),
    DynamicMap = maps:from_list([{maps:get(domain, D), D} || D <- Dynamic]),
    %% Merge: static overwrites dynamic if same domain exists
    MergedMap = maps:merge(DynamicMap, StaticMap),
    lists:sort(fun(A, B) -> maps:get(domain, A) < maps:get(domain, B) end, maps:values(MergedMap)).
