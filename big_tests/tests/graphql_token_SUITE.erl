-module(graphql_token_SUITE).

-compile([export_all, nowarn_export_all]).

-import(common_helper, [unprep/1]).
-import(distributed_helper, [require_rpc_nodes/1, mim/0]).
-import(graphql_helper, [execute_command/4, execute_user_command/5, user_to_bin/1,
                         get_ok_value/2, get_err_code/1, get_err_msg/1, get_unauthorized/1, get_not_loaded/1]).

-include_lib("eunit/include/eunit.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user},
     {group, domain_admin},
     {group, admin_http},
     {group, admin_cli}].

groups() ->
    [{user, [], user_groups()},
     {domain_admin, domain_admin_tests()},
     {admin_http, [], admin_groups()},
     {admin_cli, [], admin_groups()},
     {user_token_configured, [], user_tests()},
     {user_token_not_configured, [], user_token_not_configured_tests()},
     {admin_token_configured, [], admin_tests()},
     {admin_token_not_configured, [], admin_token_not_configured_tests()}].

user_groups() ->
    [{group, user_token_configured},
     {group, user_token_not_configured}].

admin_groups() ->
    [{group, admin_token_configured},
     {group, admin_token_not_configured}].

user_tests() ->
    [user_request_token_test,
     user_revoke_token_no_token_before_test,
     user_revoke_token_test].

user_token_not_configured_tests() ->
    [user_request_token_test_not_configured,
     user_revoke_token_test_not_configured].

domain_admin_tests() ->
    [admin_request_token_test,
     admin_request_token_test_unprep,
     domain_admin_request_token_no_permission_test,
     domain_admin_revoke_token_no_permission_test,
     admin_revoke_token_no_token_test,
     admin_revoke_token_test,
     admin_revoke_token_test_unprep].

admin_tests() ->
    [admin_request_token_test,
     admin_request_token_test_unprep,
     admin_request_token_no_host_test,
     admin_revoke_token_no_host_test,
     admin_revoke_token_no_token_test,
     admin_revoke_token_test,
     admin_revoke_token_test_unprep].

admin_token_not_configured_tests() ->
    [admin_request_token_test_not_configured,
     admin_revoke_token_test_not_configured].

init_per_suite(Config0) ->
    case mongoose_helper:is_rdbms_enabled(domain_helper:host_type()) of
        true ->
            HostType = domain_helper:host_type(),
            Config = dynamic_modules:save_modules(HostType, Config0),
            Config1 = escalus:init_per_suite(Config),
            ejabberd_node_utils:init(mim(), Config1);
        false ->
            {skip, "RDBMS not available"}
    end.

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

required_modules() ->
    KeyOpts = #{keys => #{token_secret => ram,
                          provision_pre_shared => ram},
                backend => ct_helper:get_internal_database()},
    KeyStoreOpts = config_parser_helper:mod_config(mod_keystore, KeyOpts),
    [{mod_keystore, KeyStoreOpts},
     {mod_auth_token, auth_token_opts()}].

auth_token_opts() ->
    Defaults = config_parser_helper:default_mod_config(mod_auth_token),
    Defaults#{validity_period => #{access => #{value => 60, unit => minutes},
                                   refresh => #{value => 1, unit => days}}}.

init_per_group(admin_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_cli, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(domain_admin, Config) ->
    Config1 = ensure_token_started(Config),
    graphql_helper:init_domain_admin_handler(Config1);
init_per_group(Group, Config) when Group =:= admin_token_configured;
                                   Group =:= user_token_configured ->
    ensure_token_started(Config);
init_per_group(Group, Config) when Group =:= admin_token_not_configured;
                                   Group =:= user_token_not_configured ->
    ensure_token_stopped(Config);
init_per_group(user, Config) ->
    graphql_helper:init_user(Config).

ensure_token_started(Config) ->
    HostType = domain_helper:host_type(),
    dynamic_modules:ensure_modules(HostType, required_modules()),
    Config.

ensure_token_stopped(Config) ->
    HostType = domain_helper:host_type(),
    dynamic_modules:ensure_modules(HostType, [{mod_auth_token, stopped}]),
    Config.

end_per_group(GroupName, _Config) when GroupName =:= admin_http;
                                       GroupName =:= admin_cli;
                                       GroupName =:= user;
                                       GroupName =:= domain_admin ->
    graphql_helper:clean();
end_per_group(_GroupName, _Config) ->
    escalus_fresh:clean().

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

% User tests

user_request_token_test(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_request_token_test/2).

user_request_token_test(Config, Alice) ->
    Res = user_request_token(Alice, Config),
    #{<<"refresh">> := Refresh, <<"access">> := Access} =
        get_ok_value([data, token, requestToken], Res),
    ?assert(is_binary(Refresh)),
    ?assert(is_binary(Access)).

user_revoke_token_no_token_before_test(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_revoke_token_no_token_before_test/2).

user_revoke_token_no_token_before_test(Config, Alice) ->
    Res = user_revoke_token(Alice, Config),
    ?assertEqual(<<"not_found">>, get_err_code(Res)).

user_revoke_token_test(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_revoke_token_test/2).

user_revoke_token_test(Config, Alice) ->
    user_request_token(Alice, Config),
    Res2 = user_revoke_token(Alice, Config),
    ParsedRes = get_ok_value([data, token, revokeToken], Res2),
    ?assertEqual(<<"Revoked">>, ParsedRes).

% User test cases mod_token not configured

user_request_token_test_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_request_token_test_not_configured/2).

user_request_token_test_not_configured(Config, Alice) ->
    Res = user_request_token(Alice, Config),
    get_not_loaded(Res).

user_revoke_token_test_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_revoke_token_test_not_configured/2).

user_revoke_token_test_not_configured(Config, Alice) ->
    Res = user_revoke_token(Alice, Config),
    get_not_loaded(Res).

% Domain admin tests

domain_admin_request_token_no_permission_test(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_request_token_no_permission_test/2).

domain_admin_request_token_no_permission_test(Config, AliceBis) ->
    % External domain user
    Res = admin_request_token(user_to_bin(AliceBis), Config),
    get_unauthorized(Res),
    % Non-existing domain
    Res2 = admin_request_token(<<"eddie@otherhost">>, Config),
    get_unauthorized(Res2).

domain_admin_revoke_token_no_permission_test(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_revoke_token_no_permission_test/2).

domain_admin_revoke_token_no_permission_test(Config, AliceBis) ->
    % External domain user
    Res = admin_revoke_token(user_to_bin(AliceBis), Config),
    get_unauthorized(Res),
    % Non-existing domain
    Res2 = admin_revoke_token(<<"eddie@otherhost">>, Config),
    get_unauthorized(Res2).

% Admin tests

admin_request_token_test(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_request_token_test/2).

admin_request_token_test(Config, Alice) ->
    Res = admin_request_token(user_to_bin(Alice), Config),
    #{<<"refresh">> := Refresh, <<"access">> := Access} =
        get_ok_value([data, token, requestToken], Res),
    ?assert(is_binary(Refresh)),
    ?assert(is_binary(Access)).

admin_request_token_test_unprep(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_request_token_test_unprep/2).

admin_request_token_test_unprep(Config, Alice) ->
    Res = admin_request_token(unprep(user_to_bin(Alice)), Config),
    #{<<"refresh">> := Refresh, <<"access">> := Access} =
        get_ok_value([data, token, requestToken], Res),
    ?assert(is_binary(Refresh)),
    ?assert(is_binary(Access)).

admin_request_token_no_host_test(Config) ->
    Res = admin_request_token(<<"eddie@otherhost">>, Config),
    ?assertEqual(<<"Unknown domain">>, get_err_msg(Res)).

admin_revoke_token_no_host_test(Config) ->
    Res = admin_revoke_token(<<"eddie@otherhost">>, Config),
    ?assertEqual(<<"Unknown domain">>, get_err_msg(Res)).

admin_revoke_token_no_token_test(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_revoke_token_no_token_test/2).

admin_revoke_token_no_token_test(Config, Alice) ->
    Res = admin_revoke_token(user_to_bin(Alice), Config),
    ?assertEqual(<<"not_found">>, get_err_code(Res)).

admin_revoke_token_test(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_revoke_token_test/2).

admin_revoke_token_test(Config, Alice) ->
    admin_request_token(user_to_bin(Alice), Config),
    Res2 = admin_revoke_token(user_to_bin(Alice), Config),
    ParsedRes = get_ok_value([data, token, revokeToken], Res2),
    ?assertEqual(<<"Revoked">>, ParsedRes).

admin_revoke_token_test_unprep(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_revoke_token_test_unprep/2).

admin_revoke_token_test_unprep(Config, Alice) ->
    admin_request_token(unprep(user_to_bin(Alice)), Config),
    Res2 = admin_revoke_token(unprep(user_to_bin(Alice)), Config),
    ParsedRes = get_ok_value([data, token, revokeToken], Res2),
    ?assertEqual(<<"Revoked">>, ParsedRes).

% Admin test cases token not configured

admin_request_token_test_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun admin_request_token_test_not_configured/2).

admin_request_token_test_not_configured(Config, Alice) ->
    Res = admin_request_token(user_to_bin(Alice), Config),
    get_not_loaded(Res).

admin_revoke_token_test_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun admin_revoke_token_test_not_configured/2).

admin_revoke_token_test_not_configured(Config, Alice) ->
    Res = admin_request_token(user_to_bin(Alice), Config),
    get_not_loaded(Res).

% Commands

user_request_token(User, Config) ->
    execute_user_command(<<"token">>, <<"requestToken">>, User, #{}, Config).

user_revoke_token(User, Config) ->
    execute_user_command(<<"token">>, <<"revokeToken">>, User, #{}, Config).

admin_request_token(User, Config) ->
    execute_command(<<"token">>, <<"requestToken">>, #{user => User}, Config).

admin_revoke_token(User, Config) ->
    execute_command(<<"token">>, <<"revokeToken">>, #{user => User}, Config).
