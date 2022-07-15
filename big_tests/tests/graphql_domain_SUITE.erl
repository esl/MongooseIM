-module(graphql_domain_SUITE).

 -include_lib("eunit/include/eunit.hrl").

 -compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [execute_command/4, get_ok_value/2, get_err_msg/1, skip_null_fields/1]).

-define(HOST_TYPE, <<"dummy auth">>).
-define(SECOND_HOST_TYPE, <<"test type">>).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
     [{group, domain_http},
      {group, domain_cli}].

groups() ->
     [{domain_http, [sequence], domain_tests()},
      {domain_cli, [sequence], domain_tests()}].

domain_tests() ->
    [create_domain,
     unknown_host_type_error_formatting,
     static_domain_error_formatting,
     domain_duplicate_error_formatting,
     domain_not_found_error_formatting_after_mutation_disable_domain,
     domain_not_found_error_formatting_after_mutation_enable_domain,
     domain_not_found_error_formatting_after_query,
     wrong_host_type_error_formatting,
     disable_domain,
     enable_domain,
     get_domains_by_host_type,
     get_domain_details,
     delete_domain,
     get_domains_after_deletion,
     set_domain_password,
     set_nonexistent_domain_password,
     delete_domain_password
    ].

init_per_suite(Config) ->
    case mongoose_helper:is_rdbms_enabled(?HOST_TYPE) of
        true ->
            Config1 = ejabberd_node_utils:init(mim(), Config),
            escalus:init_per_suite(Config1);
        false ->
            {skip, require_rdbms}
    end.

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(domain_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(domain_cli, Config) ->
    graphql_helper:init_admin_cli(Config).

end_per_group(_GroupName, _Config) ->
    graphql_helper:clean().

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

create_domain(Config) ->
    create_domain(Config, <<"exampleDomain">>),
    create_domain(Config, <<"exampleDomain2">>).

create_domain(Config, DomainName) ->
    Result = add_domain(DomainName, ?HOST_TYPE, Config),
    ParsedResult = get_ok_value([data, domains, addDomain], Result),
    ?assertEqual(#{<<"domain">> => DomainName,
        <<"hostType">> => ?HOST_TYPE,
        <<"enabled">> => null}, ParsedResult).

unknown_host_type_error_formatting(Config) ->
    DomainName = <<"exampleDomain">>,
    HostType = <<"NonExistingHostType">>,
    Result = add_domain(DomainName, HostType, Config),
    ?assertEqual(<<"Unknown host type">>, get_err_msg(Result)).

static_domain_error_formatting(Config) ->
    DomainName = <<"localhost">>,
    Result = add_domain(DomainName, ?HOST_TYPE, Config),
    ?assertEqual(<<"Domain static">>, get_err_msg(Result)).

domain_duplicate_error_formatting(Config) ->
    DomainName = <<"exampleDomain">>,
    Result = add_domain(DomainName, ?SECOND_HOST_TYPE, Config),
    ?assertEqual(<<"Domain already exists">>, get_err_msg(Result)).

domain_not_found_error_formatting_after_mutation_enable_domain(Config) ->
    DomainName = <<"NonExistingDomain">>,
    Result = enable_domain(DomainName, Config),
    domain_not_found_error_formatting(Result).

domain_not_found_error_formatting_after_mutation_disable_domain(Config) ->
    DomainName = <<"NonExistingDomain">>,
    Result = disable_domain(DomainName, Config),
    domain_not_found_error_formatting(Result).

domain_not_found_error_formatting_after_query(Config) ->
    DomainName = <<"NonExistingDomain">>,
    Result = get_domain_details(DomainName, Config),
    domain_not_found_error_formatting(Result).

wrong_host_type_error_formatting(Config) ->
    Result = remove_domain(<<"exampleDomain">>, ?SECOND_HOST_TYPE, Config),
    ?assertEqual(<<"Wrong host type">>, get_err_msg(Result)).

disable_domain(Config) ->
    Result = disable_domain(<<"exampleDomain">>, Config),
    ParsedResult = get_ok_value([data, domains, disableDomain], Result),
    ?assertMatch(#{<<"domain">> := <<"exampleDomain">>, <<"enabled">> := false}, ParsedResult),
    {ok, Domain} = rpc(mim(), mongoose_domain_sql, select_domain, [<<"exampleDomain">>]),
    ?assertEqual(#{host_type => ?HOST_TYPE, enabled => false}, Domain).

enable_domain(Config) ->
    Result = enable_domain(<<"exampleDomain">>, Config),
    ParsedResult = get_ok_value([data, domains, enableDomain], Result),
    ?assertMatch(#{<<"domain">> := <<"exampleDomain">>, <<"enabled">> := true}, ParsedResult).

get_domains_by_host_type(Config) ->
    Result = get_domains_by_host_type(?HOST_TYPE, Config),
    ParsedResult = get_ok_value([data, domains, domainsByHostType], Result),
    ?assertEqual(lists:sort([<<"exampleDomain">>, <<"exampleDomain2">>]),
                 lists:sort(ParsedResult)).

get_domain_details(Config) ->
    Result = get_domain_details(<<"exampleDomain">>, Config),
    ParsedResult = get_ok_value([data, domains, domainDetails], Result),
    ?assertEqual(#{<<"domain">> => <<"exampleDomain">>,
                   <<"hostType">> => ?HOST_TYPE,
                   <<"enabled">> => true}, ParsedResult).

delete_domain(Config) ->
    Result1 = remove_domain(<<"exampleDomain">>, ?HOST_TYPE, Config),
    ParsedResult1 = get_ok_value([data, domains, removeDomain], Result1),
    ?assertMatch(#{<<"msg">> := <<"Domain removed!">>,
                   <<"domain">> := #{<<"domain">> := <<"exampleDomain">>}},
                 ParsedResult1),
    Result2 = remove_domain(<<"exampleDomain2">>, ?HOST_TYPE, Config),
    ParsedResult2 = get_ok_value([data, domains, removeDomain], Result2),
    ?assertMatch(#{<<"msg">> := <<"Domain removed!">>,
                   <<"domain">> := #{<<"domain">> := <<"exampleDomain2">>}},
                 ParsedResult2).

get_domains_after_deletion(Config) ->
    Result = get_domains_by_host_type(?HOST_TYPE, Config),
    ParsedResult = get_ok_value([data, domains, domainsByHostType], Result),
    ?assertEqual([], ParsedResult).

set_domain_password(Config) ->
    Result = set_domain_password(domain_helper:domain(), <<"secret">>, Config),
    ParsedResult = get_ok_value([data, domains, setDomainPassword], Result),
    ?assertNotEqual(nomatch, binary:match(ParsedResult, <<"successfully">>)).

set_nonexistent_domain_password(Config) ->
    Domain = <<"unknown-domain.com">>,
    Result = set_domain_password(Domain, <<"secret">>, Config),
    domain_not_found_error_formatting(Result).

delete_domain_password(Config) ->
    Result = delete_domain_password(domain_helper:domain(), Config),
    ParsedResult = get_ok_value([data, domains, deleteDomainPassword], Result),
    ?assertNotEqual(nomatch, binary:match(ParsedResult, <<"successfully">>)).

%% Commands

add_domain(Domain, HostType, Config) ->
    Vars = #{domain => Domain, hostType => HostType},
    execute_command(<<"domains">>, <<"addDomain">>, Vars, Config).

enable_domain(Domain, Config) ->
    Vars = #{domain => Domain},
    execute_command(<<"domains">>, <<"enableDomain">>, Vars, Config).

disable_domain(Domain, Config) ->
    Vars = #{domain => Domain},
    execute_command(<<"domains">>, <<"disableDomain">>, Vars, Config).

get_domain_details(Domain, Config) ->
    Vars = #{domain => Domain},
    execute_command(<<"domains">>, <<"domainDetails">>, Vars, Config).

remove_domain(Domain, HostType, Config) ->
    Vars = #{domain => Domain, hostType => HostType},
    execute_command(<<"domains">>, <<"removeDomain">>, Vars, Config).

get_domains_by_host_type(HostType, Config) ->
    Vars = #{hostType => HostType},
    execute_command(<<"domains">>, <<"domainsByHostType">>, Vars, Config).

set_domain_password(Domain, Password, Config) ->
    Vars = #{domain => Domain, password => Password},
    execute_command(<<"domains">>, <<"setDomainPassword">>, Vars, Config).

delete_domain_password(Domain, Config) ->
    Vars = #{domain => Domain},
    execute_command(<<"domains">>, <<"deleteDomainPassword">>, Vars, Config).

%% Helpers

domain_not_found_error_formatting(Result) ->
    ?assertEqual(<<"Given domain does not exist">>, get_err_msg(Result)).
