-module(mongoose_graphql_admin_mutation).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(_Ctx, _Obj, <<"addDomain">>, #{<<"domain">> := Domain, <<"hostType">> := HostType}) ->
    case mongoose_domain_api:insert_domain(Domain, HostType) of
        ok ->
            {ok, #domain{domain = Domain, host_type = HostType}};
        {error, _} = Err ->
            Err
    end;
execute(_Ctx, _Obj, <<"removeDomain">>, #{<<"domain">> := Domain, <<"hostType">> := HostType}) ->
    case mongoose_domain_api:delete_domain(Domain, HostType) of
        ok ->
            DomainObj = #domain{domain = Domain, host_type = HostType},
            {ok, #{<<"domain">> => DomainObj, <<"msg">> => <<"Domain removed!">>}};
        {error, _} = Err ->
            Err
    end;
execute(_Ctx, _Obj, <<"enableDomain">>, #{<<"domain">> := Domain}) ->
    case mongoose_domain_api:enable_domain(Domain) of
        ok ->
            {ok, #domain{enabled = true, domain = Domain}};
        {error, _} = Err ->
            Err
    end;
execute(_Ctx, _Obj, <<"disableDomain">>, #{<<"domain">> := Domain}) ->
    case mongoose_domain_api:disable_domain(Domain) of
        ok ->
            {ok, #domain{enabled = false, domain = Domain}};
        {error, _} = Err ->
            Err
    end.
