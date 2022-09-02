-module(graphql_token_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [require_rpc_nodes/1, mim/0]).
-import(graphql_helper, [execute_command/4, execute_user_command/5, user_to_bin/1,
                         get_ok_value/2, get_err_code/1, get_unauthorized/1]).

-include_lib("eunit/include/eunit.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user},
     {group, domain_admin},
     {group, admin_http},
     {group, admin_cli}].

groups() ->
    [{user, [], user_tests()},
     {domain_admin, domain_admin_tests()},
     {admin_http, [], admin_tests()},
     {admin_cli, [], admin_tests()}].

user_tests() ->
    [user_request_token_test,
     user_revoke_token_no_token_before_test,
     user_revoke_token_test].

domain_admin_tests() ->
    [admin_request_token_test,
     domain_admin_request_token_no_user_test,
     domain_admin_revoke_token_no_user_test,
     admin_revoke_token_no_token_test,
     admin_revoke_token_test].

admin_tests() ->
    [admin_request_token_test,
     admin_request_token_no_user_test,
     admin_revoke_token_no_user_test,
     admin_revoke_token_no_token_test,
     admin_revoke_token_test].

init_per_suite(Config0) ->
    case mongoose_helper:is_rdbms_enabled(domain_helper:host_type()) of
        true ->
            HostType = domain_helper:host_type(),
            Config = dynamic_modules:save_modules(HostType, Config0),
            dynamic_modules:ensure_modules(HostType, required_modules()),
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
                         provision_pre_shared => ram}},
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
    graphql_helper:init_domain_admin_handler(Config);
init_per_group(user, Config) ->
    graphql_helper:init_user(Config).

end_per_group(_, _Config) ->
    graphql_helper:clean(),
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
    ?assertEqual(<<"Revoked.">>, ParsedRes).

% Domain admin tests

domain_admin_request_token_no_user_test(Config) ->
    get_unauthorized(admin_request_token(<<"AAAAA">>, Config)).

domain_admin_revoke_token_no_user_test(Config) ->
    get_unauthorized(admin_revoke_token(<<"AAAAA">>, Config)).

% Admin tests

admin_request_token_test(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_request_token_test/2).

admin_request_token_test(Config, Alice) ->
    Res = admin_request_token(user_to_bin(Alice), Config),
    #{<<"refresh">> := Refresh, <<"access">> := Access} =
        get_ok_value([data, token, requestToken], Res),
    ?assert(is_binary(Refresh)),
    ?assert(is_binary(Access)).

admin_request_token_no_user_test(Config) ->
    Res = admin_request_token(<<"AAAAA">>, Config),
    ?assertEqual(<<"not_found">>, get_err_code(Res)).

admin_revoke_token_no_user_test(Config) ->
    Res = admin_revoke_token(<<"AAAAA">>, Config),
    ?assertEqual(<<"not_found">>, get_err_code(Res)).

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
    ?assertEqual(<<"Revoked.">>, ParsedRes).

user_request_token(User, Config) ->
    execute_user_command(<<"token">>, <<"requestToken">>, User, #{}, Config).

user_revoke_token(User, Config) ->
    execute_user_command(<<"token">>, <<"revokeToken">>, User, #{}, Config).

admin_request_token(User, Config) ->
    execute_command(<<"token">>, <<"requestToken">>, #{user => User}, Config).

admin_revoke_token(User, Config) ->
    execute_command(<<"token">>, <<"revokeToken">>, #{user => User}, Config).
