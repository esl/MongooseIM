-module(mongoose_graphql_domain_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(_Ctx, admin, <<"addDomain">>, #{<<"domain">> := Domain, <<"hostType">> := HostType}) ->
    case mongoose_domain_api:insert_domain(Domain, HostType) of
        ok ->
            {ok, #domain{domain = Domain, host_type = HostType}};
        {error, Error} ->
            error_handler(Error, Domain, HostType)
    end;
execute(_Ctx, admin, <<"removeDomain">>, #{<<"domain">> := Domain, <<"hostType">> := HostType}) ->
    case mongoose_domain_api:delete_domain(Domain, HostType) of
        ok ->
            DomainObj = #domain{domain = Domain, host_type = HostType},
            {ok, #{<<"domain">> => DomainObj, <<"msg">> => <<"Domain removed!">>}};
        {error, Error} ->
            error_handler(Error, Domain, HostType)
    end;
execute(_Ctx, admin, <<"enableDomain">>, #{<<"domain">> := Domain}) ->
    case mongoose_domain_api:enable_domain(Domain) of
        ok ->
            {ok, #domain{status = enabled, domain = Domain}};
        {error, Error} ->
            error_handler(Error, Domain, <<>>)
    end;
execute(_Ctx, admin, <<"disableDomain">>, #{<<"domain">> := Domain}) ->
    case mongoose_domain_api:disable_domain(Domain) of
        ok ->
            {ok, #domain{status = disabled, domain = Domain}};
        {error, Error} ->
            error_handler(Error, Domain, <<>>)
    end;
execute(_Ctx, admin, <<"setDomainPassword">>,
        #{<<"domain">> := Domain, <<"password">> := Password}) ->
    case mongoose_domain_api:set_domain_password(Domain, Password) of
        ok ->
            {ok, <<"Domain password set successfully">>};
        {error, Error} ->
            error_handler(Error, Domain, <<>>)
    end;
execute(_Ctx, admin, <<"deleteDomainPassword">>, #{<<"domain">> := Domain}) ->
    ok = mongoose_domain_api:delete_domain_password(Domain),
    {ok, <<"Domain admin deleted successfully">>}.

error_handler(Error, Domain, HostType) ->
    case {error, Error} of
        {error, service_disabled} ->
            {error, service_disabled};
        {error, duplicate} ->
            {error, #{what => domain_duplicate, domain => Domain}};
        {error, not_found} ->
            {error, #{what => domain_not_found, domain => Domain}};
        {error, static} ->
            {error, #{what => domain_static, domain => Domain}};
        {error, wrong_host_type} ->
            {error, #{what => wrong_host_type, host_type => HostType}};
        {error, unknown_host_type} ->
            {error, #{what => unknown_host_type, host_type => HostType}};
        {error, {db_error, Term}} ->
            {error, #{what => db_error, term => Term}}
    end.
