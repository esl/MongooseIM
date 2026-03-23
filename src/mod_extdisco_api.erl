-module (mod_extdisco_api).

-export([get_services/2]).

-spec get_services(Domain :: jid:lserver(), ServiceType :: binary() | null) ->
    {ok, [{ok, #{binary() => term()}}]} | {domain_not_found, string()}.
get_services(Domain, ServiceType) ->
    case mongoose_domain_api:get_domain_host_type(Domain) of
        {ok, HostType} ->
            Service = get_services(HostType, ServiceType, Domain),
            {ok, [prepare_service(S) || S <- Service]};
        _ ->
            {domain_not_found, "domain does not exist"}
    end.

get_services(HostType, null, Domain) ->
    mod_extdisco:get_external_services(HostType, Domain);
get_services(HostType, ServiceType, Domain) when is_binary(ServiceType) ->
    case catch binary_to_existing_atom(ServiceType, utf8) of
        %% if the type is unknown, then we do not have such services
        {'EXIT', _} -> [];
        Type -> mod_extdisco:get_external_services(HostType, Type, Domain)
    end.


prepare_service(Service) ->
    {ok, #{atom_to_binary(Key) => format_value(Key, Value) || Key := Value <- Service}}.

format_value(type, Type) -> atom_to_binary(Type);
format_value(_, Integer) when is_integer(Integer) -> Integer;
format_value(_, Binary) when is_binary(Binary) -> Binary.
