-module(graphql_blocklist_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, rpc/4]).
-import(domain_helper, [domain/0, host_type/0]).
-import(graphql_helper, [execute_command/4, get_bad_request/1, get_err_code/1, get_not_loaded/1, get_ok_value/2,
                         get_unauthorized/1, user_to_bin/1]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("jid/include/jid.hrl").

-define(NONEXISTING_DOMAIN, <<"abc@abc">>).
-define(NONEXISTING_USER, <<"abc@", (domain())/binary>>).

suite() ->
    distributed_helper:require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, admin_http},
     {group, admin_cli},
     {group, domain_admin}].

groups() ->
    [{admin_http, [], admin_groups()},
     {admin_cli, [], admin_groups()},
     {domain_admin, [], domain_admin_groups()},
     {admin_blocklist, [], admin_blocklist_tests()},
     {admin_blocklist_not_configured, [], admin_blocklist_not_configured_tests()},
     {domain_admin_blocklist, [], domain_admin_blocklist_tests()},
     {domain_admin_blocklist_not_configured, [], admin_blocklist_not_configured_tests()}].

admin_groups() ->
    [{group, admin_blocklist},
     {group, admin_blocklist_not_configured}].

domain_admin_groups() ->
    [{group, domain_admin_blocklist},
     {group, domain_admin_blocklist_not_configured}].

admin_blocklist_tests() ->
    [admin_add_user,
     admin_add_user_empty_reason,
     admin_add_user_nonexisting,
     admin_add_user_without_reason,
     admin_remove_user,
     admin_remove_user_nonexisting,
     user_removal].

admin_blocklist_not_configured_tests() ->
    [admin_admin_user_not_configured,
     admin_remove_user_not_configured].

domain_admin_blocklist_tests() ->
    [domain_admin_add_user_no_permission,
     domain_admin_remove_user_no_permission,
     admin_add_user,
     admin_remove_user].

%% Setup & Teardown

init_per_suite(Config) ->
    case mongoose_helper:is_rdbms_enabled(domain_helper:host_type()) of
        true ->
            Config1 = ejabberd_node_utils:init(mim(), Config),
            Config2 = escalus:init_per_suite(Config1),
            dynamic_modules:save_modules(host_type(), Config2);
        false ->
            {skip, "RDBMS not enabled"}
    end.

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(admin_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_cli, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(domain_admin, Config) ->
    graphql_helper:init_domain_admin_handler(Config);
init_per_group(Group, Config) when Group == admin_blocklist;
                                   Group == domain_admin_blocklist ->
    ensure_blocklist_started(),
    Config;
init_per_group(Group, Config) when Group == admin_blocklist_not_configured;
                                   Group == domain_admin_blocklist_not_configured ->
    ensure_blocklist_stopped(),
    Config.

end_per_group(Group, _Config) when Group == admin_http;
                                   Group == admin_cli;
                                   Group == domain_admin ->
    graphql_helper:clean(),
    escalus_fresh:clean();
end_per_group(_Group, _Config) ->
    escalus_fresh:clean().

ensure_blocklist_started() ->
    Opts = config_parser_helper:mod_config_with_auto_backend(mod_blocklist),
    dynamic_modules:ensure_modules(host_type(), [{mod_blocklist, Opts}]).

ensure_blocklist_stopped() ->
    dynamic_modules:ensure_modules(host_type(), [{mod_blocklist, stopped}]).

init_per_testcase(TestCase, Config) ->
    escalus:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    escalus:end_per_testcase(TestCase, Config).

%% Admin

admin_add_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_add_user_story/2).

admin_add_user_story(Config, Alice) ->
    Res = admin_add_user(Alice, <<"Spam">>, Config),
    ?assertEqual(true, get_ok_value([data, blocklist, addUser], Res)),
    % The active session is terminated
    escalus:assert(is_stream_error, [<<"conflict">>, <<"Spam">>],
                   escalus:wait_for_stanza(Alice, 1)),
    % And new sessions are not allowed
    Spec = escalus_users:get_userspec(Config, alice),
    Steps = [start_stream, stream_features, authenticate, bind, {?MODULE, session_not_allowed}],
    escalus_connection:start(Spec, Steps).

admin_add_user_empty_reason(Config) ->
    Res = admin_add_user(?NONEXISTING_USER, <<>>, Config),
    get_bad_request(Res).

admin_add_user_nonexisting(Config) ->
    Res1 = admin_add_user(?NONEXISTING_DOMAIN, <<"Spam">>, Config),
    ?assertEqual(<<"domain_not_found">>, get_err_code(Res1)),
    Res2 = admin_add_user(?NONEXISTING_USER, <<"Spam">>, Config),
    ?assertEqual(<<"user_not_found">>, get_err_code(Res2)).

admin_add_user_without_reason(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_add_user_without_reason_story/2).

admin_add_user_without_reason_story(Config, Alice) ->
    Res4 = admin_add_user(Alice, null, Config),
    ?assertEqual(true, get_ok_value([data, blocklist, addUser], Res4)),
    % The default message is returned
    escalus:assert(is_stream_error, [<<"conflict">>, <<"Kicked by administrator">>],
                   escalus:wait_for_stanza(Alice, 1)).

admin_remove_user(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}]),
    Alice = escalus_users:get_jid(Config1, alice),
    Res1 = admin_remove_user(Alice, Config1),
    ?assertEqual(false, get_ok_value([data, blocklist, removeUser], Res1)),
    admin_add_user(Alice, <<"Spam">>, Config1),
    Res2 = admin_remove_user(Alice, Config1),
    ?assertEqual(true, get_ok_value([data, blocklist, removeUser], Res2)),
    Spec = escalus_users:get_userspec(Config1, alice),
    {ok, _, _} = escalus_connection:start(Spec).

admin_remove_user_nonexisting(Config) ->
    Res1 = admin_remove_user(?NONEXISTING_DOMAIN, Config),
    ?assertEqual(<<"domain_not_found">>, get_err_code(Res1)),
    Res2 = admin_remove_user(?NONEXISTING_USER, Config),
    ?assertEqual(false, get_ok_value([data, blocklist, removeUser], Res2)).

user_removal(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}]),
    Alice = escalus_users:get_jid(Config1, alice),
    admin_add_user(Alice, <<"Spam">>, Config1),
    Spec = escalus_users:get_userspec(Config1, alice),
    escalus_users:delete_users(Config1, [{alice, Spec}]),
    Res = admin_remove_user(Alice, Config1),
    ?assertEqual(false, get_ok_value([data, blocklist, removeUser], Res)).

admin_admin_user_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_admin_user_not_configured_story/2).

admin_admin_user_not_configured_story(Config, Alice) ->
    Res = admin_add_user(Alice, <<"Spam">>, Config),
    get_not_loaded(Res).

admin_remove_user_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_remove_user_not_configured_story/2).

admin_remove_user_not_configured_story(Config, Alice) ->
    Res = admin_remove_user(Alice, Config),
    get_not_loaded(Res).

%% Domain admin

domain_admin_add_user_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_add_user_no_permission_story/2).

domain_admin_add_user_no_permission_story(Config, Alice) ->
    Res = admin_add_user(Alice, <<"Spam">>, Config),
    get_unauthorized(Res).

domain_admin_remove_user_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_remove_user_no_permission_story/2).

domain_admin_remove_user_no_permission_story(Config, Alice) ->
    Res = admin_remove_user(Alice, Config),
    get_unauthorized(Res).

%% Steps

session_not_allowed(Client, Features) ->
    escalus_connection:send(Client, escalus_stanza:session()),
    escalus:assert(is_iq_error, escalus_connection:get_stanza(Client, session_reply)),
    {Client, Features}.

%% Helpers

admin_add_user(User, Reason, Config) ->
    Vars = #{user => user_to_bin(User), reason => Reason},
    execute_command(<<"blocklist">>, <<"addUser">>, Vars, Config).

admin_remove_user(User, Config) ->
    Vars = #{user => user_to_bin(User)},
    execute_command(<<"blocklist">>, <<"removeUser">>, Vars, Config).
