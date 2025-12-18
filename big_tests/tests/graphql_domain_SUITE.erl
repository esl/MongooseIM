-module(graphql_domain_SUITE).

 -include_lib("eunit/include/eunit.hrl").

 -compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [execute_command/4, get_ok_value/2, get_err_msg/1, skip_null_fields/1,
                         execute_domain_admin_command/4, get_unauthorized/1, get_coercion_err_msg/1]).

-define(HOST_TYPE, <<"dummy auth">>).
-define(SECOND_HOST_TYPE, <<"test type">>).
-define(EXAMPLE_DOMAIN, <<"example.com">>).
-define(SECOND_EXAMPLE_DOMAIN, <<"second.example.com">>).
-define(DOMAIN_ADMIN_EXAMPLE_DOMAIN, <<"domain-admin.example.com">>).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
     [{group, domain_http},
      {group, domain_cli},
      {group, domain_admin_tests}].

groups() ->
     [{domain_http, [sequence], domain_tests()},
      {domain_cli, [sequence], domain_tests()},
      {domain_admin_tests, [sequence], domain_admin_tests()}].

domain_tests() ->
    [create_domain,
     create_domain_fails_if_db_transaction_fails_constantly,
     create_domain_succeeds_if_db_transaction_fails_once,
     unknown_host_type_error_formatting,
     add_static_domain_error_formatting,
     remove_static_domain_error_formatting,
     enable_static_domain_error_formatting,
     disable_static_domain_error_formatting,
     domain_duplicate_error_formatting,
     domain_not_found_error_formatting_after_mutation_disable_domain,
     domain_not_found_error_formatting_after_mutation_enable_domain,
     domain_not_found_error_formatting_after_query,
     wrong_host_type_error_formatting,
     invalid_domain_name_error,
     disable_domain,
     get_all_domains_with_disabled,
     enable_domain,
     get_domains_by_host_type,
     get_all_domains,
     get_domain_details,
     delete_domain,
     request_delete_domain,
     get_domains_after_deletion,
     set_domain_password,
     set_nonexistent_domain_password,
     delete_domain_password,
     delete_nonexistent_domain_password
    ].

domain_admin_tests() ->
    [domain_admin_get_domain_details,
     domain_admin_set_domain_password,
     domain_admin_create_domain_no_permission,
     domain_admin_disable_domain_no_permission,
     domain_admin_enable_domain_no_permission,
     domain_admin_get_domains_by_host_type_no_permission,
     domain_admin_get_all_domains_no_permission,
     domain_admin_get_domain_details_no_permission,
     domain_admin_delete_domain_no_permission,
     domain_admin_set_domain_password_no_permission,
     domain_admin_delete_domain_password_no_permission
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
    graphql_helper:init_admin_cli(Config);
init_per_group(domain_admin_tests, Config) ->
    domain_helper:insert_persistent_domain(mim(), ?DOMAIN_ADMIN_EXAMPLE_DOMAIN, ?HOST_TYPE),
    domain_helper:insert_domain(mim(), ?DOMAIN_ADMIN_EXAMPLE_DOMAIN, ?HOST_TYPE),
    graphql_helper:init_domain_admin_handler(Config, ?DOMAIN_ADMIN_EXAMPLE_DOMAIN).

end_per_group(domain_admin_tests, _Config) ->
    domain_helper:delete_domain(mim(), ?DOMAIN_ADMIN_EXAMPLE_DOMAIN),
    domain_helper:delete_persistent_domain(mim(), ?DOMAIN_ADMIN_EXAMPLE_DOMAIN, ?HOST_TYPE);
end_per_group(_GroupName, _Config) ->
    graphql_helper:clean().

init_per_testcase(get_all_domains_with_disabled, Config) ->
    disable_domain(?EXAMPLE_DOMAIN, Config),
    escalus:init_per_testcase(get_all_domains_with_disabled, Config);
init_per_testcase(CaseName, Config)
  when CaseName =:= create_domain_fails_if_db_transaction_fails_constantly;
       CaseName =:= create_domain_succeeds_if_db_transaction_fails_once ->
    rpc(mim(), meck, new, [mongoose_rdbms, [no_link, passthrough]]),
    escalus:init_per_testcase(CaseName, Config);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(get_all_domains_with_disabled, Config) ->
    enable_domain(?EXAMPLE_DOMAIN, Config),
    escalus:end_per_testcase(get_all_domains_with_disabled, Config);
end_per_testcase(CaseName, Config)
  when CaseName =:= create_domain_fails_if_db_transaction_fails_constantly;
       CaseName =:= create_domain_succeeds_if_db_transaction_fails_once ->
    rpc(mim(), meck, unload, [mongoose_rdbms]),
    escalus:end_per_testcase(CaseName, Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

create_domain(Config) ->
    create_domain(?EXAMPLE_DOMAIN, Config).

create_domain_fails_if_db_transaction_fails_constantly(Config) ->
    ok = rpc(mim(), meck, expect, [mongoose_rdbms, sql_transaction, 2,
                                   {aborted, simulated_db_error}]),
    Result = add_domain(?SECOND_EXAMPLE_DOMAIN, ?HOST_TYPE, Config),
    ?assertEqual(<<"Unexpected DomainAdminMutation resolver crash">>, get_err_msg(Result)).

create_domain_succeeds_if_db_transaction_fails_once(Config) ->
    Seq = meck:seq([{aborted, simulated_db_error}, meck:passthrough()]),
    ok = rpc(mim(), meck, expect, [mongoose_rdbms, sql_transaction, 2, Seq]),
    create_domain(?SECOND_EXAMPLE_DOMAIN, Config).

create_domain(DomainName, Config) ->
    Result = add_domain(DomainName, ?HOST_TYPE, Config),
    ParsedResult = get_ok_value([data, domain, addDomain], Result),
    ?assertEqual(#{<<"domain">> => DomainName,
                   <<"hostType">> => ?HOST_TYPE,
                   <<"status">> => null}, ParsedResult).

unknown_host_type_error_formatting(Config) ->
    DomainName = ?EXAMPLE_DOMAIN,
    HostType = <<"NonExistingHostType">>,
    Result = add_domain(DomainName, HostType, Config),
    ?assertEqual(<<"Unknown host type">>, get_err_msg(Result)),
    Result2 = get_domains_by_host_type(HostType, Config),
    ?assertEqual(<<"Unknown host type">>, get_err_msg(Result2)).

add_static_domain_error_formatting(Config) ->
    DomainName = <<"localhost">>,
    Result = add_domain(DomainName, ?HOST_TYPE, Config),
    ?assertEqual(<<"Domain is static">>, get_err_msg(Result)).

remove_static_domain_error_formatting(Config) ->
    DomainName = <<"localhost">>,
    Result = remove_domain(DomainName, ?HOST_TYPE, Config),
    ?assertEqual(<<"Domain is static">>, get_err_msg(Result)).

enable_static_domain_error_formatting(Config) ->
    DomainName = <<"localhost">>,
    Result = enable_domain(DomainName, Config),
    ?assertEqual(<<"Domain is static">>, get_err_msg(Result)).

disable_static_domain_error_formatting(Config) ->
    DomainName = <<"localhost">>,
    Result = disable_domain(DomainName, Config),
    ?assertEqual(<<"Domain is static">>, get_err_msg(Result)).

domain_duplicate_error_formatting(Config) ->
    DomainName = ?EXAMPLE_DOMAIN,
    Result = add_domain(DomainName, ?SECOND_HOST_TYPE, Config),
    ?assertMatch(<<"Domain already exists">>, get_err_msg(Result)).

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
    Result = remove_domain(?EXAMPLE_DOMAIN, ?SECOND_HOST_TYPE, Config),
    ?assertEqual(<<"Wrong host type was provided">>, get_err_msg(Result)).

invalid_domain_name_error(Config) ->
    %% One operation tested, because they all use the DomainName type
    Result1 = add_domain(<<>>, ?HOST_TYPE, Config),
    get_coercion_err_msg(Result1),
    TooLong = binary:copy(<<$a>>, 1024),
    Result2 = add_domain(TooLong, ?HOST_TYPE, Config),
    get_coercion_err_msg(Result2).

disable_domain(Config) ->
    Result = disable_domain(?EXAMPLE_DOMAIN, Config),
    ParsedResult = get_ok_value([data, domain, disableDomain], Result),
    ?assertMatch(#{<<"domain">> := ?EXAMPLE_DOMAIN, <<"status">> := <<"DISABLED">>}, ParsedResult),
    {ok, Domain} = rpc(mim(), mongoose_domain_sql, select_domain, [?EXAMPLE_DOMAIN]),
    ?assertEqual(#{host_type => ?HOST_TYPE, status => disabled}, Domain).

get_all_domains_with_disabled(Config) ->
    Result = execute_command(<<"domain">>, <<"allDomains">>, #{}, Config),
    ParsedResult = get_ok_value([data, domain, allDomains], Result),
    Expected = [
        #{<<"domain">> => ?EXAMPLE_DOMAIN, <<"hostType">> => ?HOST_TYPE, <<"status">> => <<"DISABLED">>},
        #{<<"domain">> => ?SECOND_EXAMPLE_DOMAIN, <<"hostType">> => ?HOST_TYPE, <<"status">> => <<"ENABLED">>}
    ],
    lists:foreach(fun(E) -> ?assert(lists:member(E, ParsedResult)) end, Expected).

enable_domain(Config) ->
    Result = enable_domain(?EXAMPLE_DOMAIN, Config),
    ParsedResult = get_ok_value([data, domain, enableDomain], Result),
    ?assertMatch(#{<<"domain">> := ?EXAMPLE_DOMAIN, <<"status">> := <<"ENABLED">>}, ParsedResult).

get_domains_by_host_type(Config) ->
    Result = get_domains_by_host_type(?HOST_TYPE, Config),
    ParsedResult = get_ok_value([data, domain, domainsByHostType], Result),
    ?assertEqual(lists:sort([?EXAMPLE_DOMAIN, ?SECOND_EXAMPLE_DOMAIN]),
                 lists:sort(ParsedResult)).

get_all_domains(Config) ->
    Result = execute_command(<<"domain">>, <<"allDomains">>, #{}, Config),
    ParsedResult = get_ok_value([data, domain, allDomains], Result),
    Expected = [
        #{<<"domain">> => ?EXAMPLE_DOMAIN, <<"hostType">> => ?HOST_TYPE, <<"status">> => <<"ENABLED">>},
        #{<<"domain">> => ?SECOND_EXAMPLE_DOMAIN, <<"hostType">> => ?HOST_TYPE, <<"status">> => <<"ENABLED">>}
    ],
    lists:foreach(fun(E) -> ?assert(lists:member(E, ParsedResult)) end, Expected).

get_domain_details(Config) ->
    Result = get_domain_details(?EXAMPLE_DOMAIN, Config),
    ParsedResult = get_ok_value([data, domain, domainDetails], Result),
    ?assertEqual(#{<<"domain">> => ?EXAMPLE_DOMAIN,
                   <<"hostType">> => ?HOST_TYPE,
                   <<"status">> => <<"ENABLED">>}, ParsedResult).

delete_domain(Config) ->
    Result1 = remove_domain(?EXAMPLE_DOMAIN, ?HOST_TYPE, Config),
    ParsedResult1 = get_ok_value([data, domain, removeDomain], Result1),
    ?assertEqual(#{<<"domain">> => ?EXAMPLE_DOMAIN,
                   <<"hostType">> => ?HOST_TYPE,
                   <<"status">> => <<"DELETED">>},
                 ParsedResult1),
    Result2 = remove_domain(?EXAMPLE_DOMAIN, ?HOST_TYPE, Config),
    domain_not_found_error_formatting(Result2).

request_delete_domain(Config) ->
    Result1 = request_remove_domain(?SECOND_EXAMPLE_DOMAIN, ?HOST_TYPE, Config),
    ParsedResult1 = get_ok_value([data, domain, requestRemoveDomain], Result1),
    ?assertEqual(#{<<"domain">> => ?SECOND_EXAMPLE_DOMAIN,
                   <<"hostType">> => ?HOST_TYPE,
                   <<"status">> => <<"DELETING">>},
                 ParsedResult1),
    F = fun() ->
                Result = get_domain_details(?EXAMPLE_DOMAIN, Config),
                domain_not_found_error_formatting(Result)
        end,
    wait_helper:wait_until(F, ok, #{time_left => timer:seconds(5)}),
    Result2 = request_remove_domain(?EXAMPLE_DOMAIN, ?HOST_TYPE, Config),
    domain_not_found_error_formatting(Result2).

get_domains_after_deletion(Config) ->
    Result = get_domains_by_host_type(?HOST_TYPE, Config),
    ParsedResult = get_ok_value([data, domain, domainsByHostType], Result),
    ?assertEqual([], ParsedResult).

set_domain_password(Config) ->
    Result = set_domain_password(domain_helper:domain(), <<"secret">>, Config),
    ParsedResult = get_ok_value([data, domain, setDomainPassword], Result),
    ?assertNotEqual(nomatch, binary:match(ParsedResult, <<"successfully">>)).

set_nonexistent_domain_password(Config) ->
    Domain = <<"unknown-domain.com">>,
    Result = set_domain_password(Domain, <<"secret">>, Config),
    domain_not_found_error_formatting(Result).

delete_domain_password(Config) ->
    Result = delete_domain_password(domain_helper:domain(), Config),
    ParsedResult = get_ok_value([data, domain, deleteDomainPassword], Result),
    ?assertNotEqual(nomatch, binary:match(ParsedResult, <<"successfully">>)),
    Result2 = delete_domain_password(domain_helper:domain(), Config),
    domain_password_not_found_error_formatting(Result2).

delete_nonexistent_domain_password(Config) ->
    Domain = <<"unknown-domain.com">>,
    Result = delete_domain_password(Domain, Config),
    domain_password_not_found_error_formatting(Result).

domain_admin_get_domain_details(Config) ->
    Result = get_domain_details(?DOMAIN_ADMIN_EXAMPLE_DOMAIN, Config),
    ParsedResult = get_ok_value([data, domain, domainDetails], Result),
    ?assertEqual(#{<<"domain">> => ?DOMAIN_ADMIN_EXAMPLE_DOMAIN,
                   <<"hostType">> => ?HOST_TYPE,
                   <<"status">> => <<"ENABLED">>}, ParsedResult).

domain_admin_set_domain_password(Config) ->
    Result = set_domain_password(?DOMAIN_ADMIN_EXAMPLE_DOMAIN, <<"secret">>, Config),
    ParsedResult = get_ok_value([data, domain, setDomainPassword], Result),
    ?assertNotEqual(nomatch, binary:match(ParsedResult, <<"successfully">>)).

domain_admin_create_domain_no_permission(Config) ->
    get_unauthorized(add_domain(?EXAMPLE_DOMAIN, ?HOST_TYPE, Config)),
    get_unauthorized(add_domain(?DOMAIN_ADMIN_EXAMPLE_DOMAIN, ?HOST_TYPE, Config)).

domain_admin_disable_domain_no_permission(Config) ->
    get_unauthorized(disable_domain(?EXAMPLE_DOMAIN, Config)),
    get_unauthorized(disable_domain(?DOMAIN_ADMIN_EXAMPLE_DOMAIN, Config)).

domain_admin_enable_domain_no_permission(Config) ->
    get_unauthorized(enable_domain(?EXAMPLE_DOMAIN, Config)),
    get_unauthorized(enable_domain(?DOMAIN_ADMIN_EXAMPLE_DOMAIN, Config)).

domain_admin_get_domains_by_host_type_no_permission(Config) ->
    get_unauthorized(get_domains_by_host_type(?HOST_TYPE, Config)),
    get_unauthorized(get_domains_by_host_type(domain_helper:host_type(), Config)).

domain_admin_get_all_domains_no_permission(Config) ->
    get_unauthorized(execute_command(<<"domain">>, <<"allDomains">>, #{}, Config)).

domain_admin_get_domain_details_no_permission(Config) ->
    get_unauthorized(get_domain_details(?DOMAIN_ADMIN_EXAMPLE_DOMAIN, Config)),
    get_unauthorized(get_domain_details(?EXAMPLE_DOMAIN, Config)).

domain_admin_set_domain_password_no_permission(Config) ->
    get_unauthorized(set_domain_password(?EXAMPLE_DOMAIN, <<"secret">>, Config)),
    get_unauthorized(set_domain_password(?DOMAIN_ADMIN_EXAMPLE_DOMAIN, <<"secret">>, Config)).

domain_admin_delete_domain_no_permission(Config) ->
    get_unauthorized(remove_domain(?EXAMPLE_DOMAIN, ?HOST_TYPE, Config)),
    get_unauthorized(remove_domain(?DOMAIN_ADMIN_EXAMPLE_DOMAIN, ?HOST_TYPE, Config)).

domain_admin_delete_domain_password_no_permission(Config) ->
    get_unauthorized(delete_domain_password(?EXAMPLE_DOMAIN, Config)),
    get_unauthorized(delete_domain_password(?DOMAIN_ADMIN_EXAMPLE_DOMAIN, Config)).

%% Commands

add_domain(Domain, HostType, Config) ->
    Vars = #{domain => Domain, hostType => HostType},
    execute_command(<<"domain">>, <<"addDomain">>, Vars, Config).

enable_domain(Domain, Config) ->
    Vars = #{domain => Domain},
    execute_command(<<"domain">>, <<"enableDomain">>, Vars, Config).

disable_domain(Domain, Config) ->
    Vars = #{domain => Domain},
    execute_command(<<"domain">>, <<"disableDomain">>, Vars, Config).

get_domain_details(Domain, Config) ->
    Vars = #{domain => Domain},
    execute_command(<<"domain">>, <<"domainDetails">>, Vars, Config).

remove_domain(Domain, HostType, Config) ->
    Vars = #{domain => Domain, hostType => HostType},
    execute_command(<<"domain">>, <<"removeDomain">>, Vars, Config).

request_remove_domain(Domain, HostType, Config) ->
    Vars = #{domain => Domain, hostType => HostType},
    execute_command(<<"domain">>, <<"requestRemoveDomain">>, Vars, Config).

get_domains_by_host_type(HostType, Config) ->
    Vars = #{hostType => HostType},
    execute_command(<<"domain">>, <<"domainsByHostType">>, Vars, Config).

set_domain_password(Domain, Password, Config) ->
    Vars = #{domain => Domain, password => Password},
    execute_command(<<"domain">>, <<"setDomainPassword">>, Vars, Config).

delete_domain_password(Domain, Config) ->
    Vars = #{domain => Domain},
    execute_command(<<"domain">>, <<"deleteDomainPassword">>, Vars, Config).

%% Helpers

domain_not_found_error_formatting(Result) ->
    ?assertMatch(<<"Given domain does not exist", _/binary>>, get_err_msg(Result)).

domain_password_not_found_error_formatting(Result) ->
    ?assertEqual(<<"Domain password does not exist">>, get_err_msg(Result)).
