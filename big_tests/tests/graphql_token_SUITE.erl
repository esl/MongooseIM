-module(graphql_token_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [require_rpc_nodes/1]).
-import(graphql_helper, [execute_user/3, execute_auth/2, user_to_bin/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user},
     {group, admin}].

groups() ->
    [{user, [], user_cases()},
     {admin, [], admin_cases()}].

user_cases() ->
    [user_request_token,
     user_revoke_token_no_token_before,
     user_revoke_token].

admin_cases() ->
    [admin_request_token,
     admin_request_token_no_user,
     admin_revoke_token_no_user,
     admin_revoke_token_no_token,
     admin_revoke_token].

init_per_suite(Config0) ->
    case mongoose_helper:is_rdbms_enabled(domain_helper:host_type()) of
        true ->
            HostType = domain_helper:host_type(),
            Config = dynamic_modules:save_modules(HostType, Config0),
            dynamic_modules:ensure_modules(HostType, required_modules()),
            escalus:init_per_suite(Config);
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

init_per_group(admin, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(user, Config) ->
    [{schema_endpoint, user} | Config].

end_per_group(_, _Config) ->
    escalus_fresh:clean().

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

% User tests

user_request_token(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_request_token/2).

user_request_token(Config, Alice) ->
    Req = #{query => user_request_token_mutation(), operationName => <<"M1">>,
            variables => #{}},
    Res = execute_user(Req, Alice, Config),
    #{<<"refresh">> := Refresh, <<"access">> := Access} = ok_result(<<"token">>,
                                                                    <<"requestToken">>, Res),
    ?assert(is_binary(Refresh)),
    ?assert(is_binary(Access)).

user_revoke_token_no_token_before(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_revoke_token_no_token_before/2).

user_revoke_token_no_token_before(Config, Alice) ->
    Req = #{query => user_revoke_token_mutation(), operationName => <<"M1">>,
            variables => #{}},
    Res = execute_user(Req, Alice, Config),
    ParsedRes = error_result(<<"extensions">>, <<"code">>, Res),
    ?assertEqual(<<"not_found">>, ParsedRes).

user_revoke_token(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_revoke_token/2).

user_revoke_token(Config, Alice) ->
    Req = #{query => user_request_token_mutation(), operationName => <<"M1">>,
            variables => #{}},
    execute_user(Req, Alice, Config),
    Req2 = #{query => user_revoke_token_mutation(), operationName => <<"M1">>,
            variables => #{}},
    Res2 = execute_user(Req2, Alice, Config),
    ParsedRes = ok_result(<<"token">>, <<"revokeToken">>, Res2),
    ?assertEqual(<<"Revoked.">>, ParsedRes).

% Admin tests

admin_request_token(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_request_token/2).

admin_request_token(Config, Alice) ->
    Req = #{query => admin_request_token_mutation(), operationName => <<"M1">>,
            variables => #{<<"user">> => user_to_bin(Alice)}},
    Res = execute_auth(Req, Config),
    #{<<"refresh">> := Refresh, <<"access">> := Access} = ok_result(<<"token">>,
                                                                    <<"requestToken">>, Res),
    ?assert(is_binary(Refresh)),
    ?assert(is_binary(Access)).

admin_request_token_no_user(Config) ->
    Req = #{query => admin_request_token_mutation(), operationName => <<"M1">>,
            variables => #{<<"user">> => <<"AAAA">>}},
    Res = execute_auth(Req, Config),
    ParsedRes = error_result(<<"extensions">>, <<"code">>, Res),
    ?assertEqual(<<"not_found">>, ParsedRes).

admin_revoke_token_no_user(Config) ->
    Req = #{query => admin_revoke_token_mutation(), operationName => <<"M1">>,
            variables => #{<<"user">> => <<"AAAA">>}},
    Res = execute_auth(Req, Config),
    ParsedRes = error_result(<<"extensions">>, <<"code">>, Res),
    ?assertEqual(<<"not_found">>, ParsedRes).

admin_revoke_token_no_token(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_revoke_token_no_token/2).

admin_revoke_token_no_token(Config, Alice) ->
    Req = #{query => admin_revoke_token_mutation(), operationName => <<"M1">>,
            variables => #{<<"user">> => user_to_bin(Alice)}},
    Res = execute_auth(Req, Config),
    ParsedRes = error_result(<<"extensions">>, <<"code">>, Res),
    ?assertEqual(<<"not_found">>, ParsedRes).

admin_revoke_token(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_revoke_token/2).

admin_revoke_token(Config, Alice) ->
    Req = #{query => admin_request_token_mutation(), operationName => <<"M1">>,
            variables => #{<<"user">> => user_to_bin(Alice)}},
    execute_auth(Req, Config),
    Req2 = #{query => admin_revoke_token_mutation(), operationName => <<"M1">>,
            variables => #{<<"user">> => user_to_bin(Alice)}},
    Res2 = execute_auth(Req2, Config),
    ParsedRes = ok_result(<<"token">>, <<"revokeToken">>, Res2),
    ?assertEqual(<<"Revoked.">>, ParsedRes).

user_request_token_mutation() ->
    <<"mutation M1
       { token {
               requestToken {access refresh}
           } }">>.

user_revoke_token_mutation() ->
    <<"mutation M1
       { token {
               revokeToken
           } }">>.

admin_request_token_mutation() ->
    <<"mutation M1($user: JID!)
       { token {
               requestToken(user: $user) {access refresh}
           } }">>.

admin_revoke_token_mutation() ->
    <<"mutation M1($user: JID!)
       { token {
               revokeToken(user: $user)
           } }">>.

ok_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"data">> := Data}}) ->
    maps:get(What2, maps:get(What1, Data)).

error_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"errors">> := [Data]}}) ->
    maps:get(What2, maps:get(What1, Data)).
