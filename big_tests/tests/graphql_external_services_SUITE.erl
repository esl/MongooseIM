-module(graphql_external_services_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1]).
-import(domain_helper, [host_type/0, domain/0, secondary_domain/0]).
-import(graphql_helper, [execute_command/4, get_ok_value/2,
                         get_bad_request/1, get_unauthorized/1,
                         get_err_msg/1, get_err_code/1]).

-include_lib("eunit/include/eunit.hrl").

-define(TTL, 3600). %% 1h.

suite() ->
    require_rpc_nodes([mim]).

all() ->
    [{group, admin_http_group},
     {group, admin_cli_group},
     {group, domain_admin_group}].

groups() ->
    [{admin_http_group, [], admin_groups()},
     {admin_cli_group, [], admin_groups()},
     {domain_admin_group, [], domain_admin_groups()},
     {admin_external_services_group, [], admin_external_services_tests()},
     {admin_external_services_not_configured_group, [], admin_external_services_not_configured_tests()},
     {domain_admin_external_services_group, [], domain_admin_external_services_tests()}].

admin_groups() ->
    [{group, admin_external_services_group},
     {group, admin_external_services_not_configured_group}].

domain_admin_groups() ->
    [{group, domain_admin_external_services_group},
     {group, admin_external_services_not_configured_group}].

admin_external_services_tests() ->
    [admin_get_services_test,
     admin_get_services_with_type_test,
     admin_get_services_missing_domain_test,
     admin_get_services_no_domain_test].

admin_external_services_not_configured_tests() ->
    [admin_get_services_not_configured_test].

domain_admin_external_services_tests() ->
    [admin_get_services_test,
     admin_get_services_with_type_test,
     admin_get_services_no_domain_test,
     domain_admin_get_services_no_permission_test].

init_per_suite(Config) ->
    Config1 = dynamic_modules:save_modules(host_type(), Config),
    Config2 = ejabberd_node_utils:init(mim(), Config1),
    Config2.

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config).

init_per_group(admin_http_group, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_cli_group, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(domain_admin_group, Config) ->
    graphql_helper:init_domain_admin_handler(Config);
init_per_group(Group, Config) when Group =:= domain_admin_external_services_group;
                                   Group =:= admin_external_services_group ->
    dynamic_modules:ensure_modules(host_type(), [{mod_extdisco, mod_extdisco_opts()}]),
    Config;
init_per_group(admin_external_services_not_configured_group, Config) ->
    dynamic_modules:ensure_modules(host_type(), [{mod_extdisco, stopped}]),
    Config.

end_per_group(GroupName, _Config) when GroupName =:= admin_http_group;
                                       GroupName =:= admin_cli_group;
                                       GroupName =:= domain_admin_group ->
    graphql_helper:clean();
end_per_group(_, _Config) ->
    ok.

% test cases

admin_get_services_test(Config) ->
    Result = admin_get_services(domain(), Config),
    ParsedResult = get_ok_value([data, externalServices, getServices], Result),
    ct:pal("ParsedResult = ~p", [ParsedResult]),
    validate_services(ParsedResult, services()).

admin_get_services_with_type_test(Config) ->
    admin_get_services_with_type_test(Config, stun),
    admin_get_services_with_type_test(Config, turn),
    %% ftp is existing atom on mim1, but there's no such service configured.
    admin_get_services_with_type_test(Config, ftp),
    %% '$$unknown_type$$' atom doesn't exist at mim1.
    admin_get_services_with_type_test(Config, '$$unknown_type$$').

admin_get_services_with_type_test(Config, ServiceType) ->
    Result = admin_get_services(domain(), atom_to_binary(ServiceType), Config),
    ParsedResult = get_ok_value([data, externalServices, getServices], Result),
    ConfiguredServices = [S || #{type := T} = S <- services(), T =:= ServiceType],
    validate_services(ParsedResult, ConfiguredServices).

admin_get_services_no_domain_test(Config) ->
     Result = admin_get_services(#{}, Config),
     get_bad_request(Result).

admin_get_services_missing_domain_test(Config) ->
     Result = admin_get_services(<<"missing.domain.name">>, Config),
     ?assertEqual(<<"domain_not_found">>, get_err_code(Result)),
     ?assertEqual(<<"domain does not exist">>, get_err_msg(Result)).

admin_get_services_not_configured_test(Config) ->
    Result = admin_get_services(domain(), Config),
    ?assertEqual(<<"deps_not_loaded">>, get_err_code(Result)),
    ?assertEqual(<<"Some of the required modules are not loaded">>, get_err_msg(Result)).

domain_admin_get_services_no_permission_test(Config) ->
    Result1 = admin_get_services(<<"missing.domain.name">>, Config),
    get_unauthorized(Result1),
    Result2 = admin_get_services(secondary_domain(), Config),
    get_unauthorized(Result2).

% Helpers functions

mod_extdisco_opts() ->
    ExternalServices = services(),
    config_parser_helper:mod_config(mod_extdisco, #{service => ExternalServices}).

services() ->
    Stun1 = #{type => stun, host => <<"1.1.1.1">>},
    Stun2 = Stun1#{port => 3478, transport => <<"udp">>},
    Stun3 = Stun2#{username => <<"username">>, password => <<"secret">>},
    Turn = Stun2#{type => turn, secret => <<"my_supper_secret">>, ttl => ?TTL},
    [Stun1, Stun2, Stun3, Turn].

validate_services(QueryResult, ConfiguredServices) ->
    [validate_service(QueriedService, ConfiguredService)
        || {QueriedService, ConfiguredService} <- lists:zip(QueryResult, ConfiguredServices)].

validate_service(QueriedService, ConfiguredService) when is_map_key(secret, ConfiguredService) ->
    ExpectedService =
        #{atom_to_binary(K) => maybe_atom_to_binary(V)
            || K := V <- maps:without([secret, ttl], ConfiguredService)},
    Keys = maps:keys(ExpectedService),
    ?assertEqual(ExpectedService, maps:with(Keys, QueriedService)),
    QueriedServiceMissingFields =
        maps:without([~"expires", ~"username", ~"password" | Keys], QueriedService),
    [?assertEqual(null, V) || _K := V <- QueriedServiceMissingFields],
    #{~"expires" := Expires, ~"username" := Username, ~"password" := Password} = QueriedService,
    ?assert(is_binary(Password)),
    ?assertEqual(<<(integer_to_binary(Expires))/binary, ":", (domain())/binary>>, Username),
    ExpectedExpiry = erlang:system_time(second) + maps:get(ttl, ConfiguredService),
    ?assert((ExpectedExpiry >= Expires) andalso (ExpectedExpiry - Expires < 2));
validate_service(QueriedService, ConfiguredService) ->
    ExpectedService =
        #{atom_to_binary(K) => maybe_atom_to_binary(V) || K := V <- ConfiguredService},
    Keys = maps:keys(ExpectedService),
    ?assertEqual(ExpectedService, maps:with(Keys, QueriedService)),
    [?assertEqual(null, V) || _K := V <- maps:without([~"expires" | Keys], QueriedService)].

maybe_atom_to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom);
maybe_atom_to_binary(AnythingElse) -> AnythingElse.

admin_get_services(Domain, Type, Config) ->
    Vars = #{<<"domain">> => Domain, <<"type">> => Type},
    admin_get_services(Vars, Config).

admin_get_services(Domain, Config) when is_binary(Domain) ->
    Vars = #{<<"domain">> => Domain},
    admin_get_services(Vars, Config);
admin_get_services(Vars, Config) when is_map(Vars) ->
    execute_command(<<"externalServices">>, <<"getServices">>, Vars, Config).
