-module(graphql_roster_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [execute_user_command/5, execute_command/4, get_listener_port/1,
                         get_listener_config/1, get_ok_value/2, get_err_value/2, get_err_msg/1,
                         get_err_msg/2, get_bad_request/1, user_to_jid/1, user_to_bin/1,
                         get_unauthorized/1, get_err_code/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/mod_roster.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user_roster},
     {group, admin_roster_http},
     {group, admin_roster_cli},
     {group, domain_admin_roster}].

groups() ->
    [{user_roster, [], user_roster_tests()},
     {admin_roster_http, [], admin_roster_tests()},
     {admin_roster_cli, [], admin_roster_tests()},
     {domain_admin_roster, [], domain_admin_tests()}].

user_roster_tests() ->
    [user_add_and_delete_contact,
     user_try_add_nonexistent_contact,
     user_add_contacts,
     user_try_delete_nonexistent_contact,
     user_delete_contacts,
     user_invite_accept_and_cancel_subscription,
     user_decline_subscription_ask,
     user_list_contacts,
     user_get_contact,
     user_get_nonexistent_contact
    ].

admin_roster_tests() ->
    [admin_add_and_delete_contact,
     admin_try_add_nonexistent_contact,
     admin_try_add_contact_to_nonexistent_user,
     admin_try_add_contact_with_unknown_domain,
     admin_add_contacts,
     admin_try_delete_nonexistent_contact,
     admin_try_delete_contact_with_unknown_domain,
     admin_delete_contacts,
     admin_invite_accept_and_cancel_subscription,
     admin_decline_subscription_ask,
     admin_try_subscribe_with_unknown_domain,
     admin_set_mutual_subscription,
     admin_set_mutual_subscription_try_connect_nonexistent_users,
     admin_set_mutual_subscription_try_disconnect_nonexistent_users,
     admin_subscribe_to_all,
     admin_subscribe_to_all_with_wrong_user,
     admin_subscribe_to_all_no_groups,
     admin_subscribe_to_all_without_arguments,
     admin_subscribe_all_to_all,
     admin_subscribe_all_to_all_with_wrong_user,
     admin_subscribe_all_to_all_no_groups,
     admin_subscribe_all_to_all_without_arguments,
     admin_list_contacts,
     admin_list_contacts_wrong_user,
     admin_get_contact,
     admin_get_contact_wrong_user,
     admin_subscribe_all_to_all_empty_list
    ].

domain_admin_tests() ->
    [admin_add_and_delete_contact,
     admin_try_add_nonexistent_contact,
     admin_try_add_contact_to_nonexistent_user,
     domain_admin_try_add_contact_with_unknown_domain,
     domain_admin_try_add_contact_no_permission,
     admin_add_contacts,
     admin_try_delete_nonexistent_contact,
     domain_admin_try_delete_contact_with_unknown_domain,
     domain_admin_try_delete_contact_no_permission,
     admin_set_mutual_subscription,
     domain_admin_set_mutual_subscription_try_connect_nonexistent_users,
     domain_admin_set_mutual_subscription_try_connect_users_no_permission,
     domain_admin_set_mutual_subscription_try_disconnect_nonexistent_users,
     domain_admin_set_mutual_subscription_try_disconnect_users_no_permission,
     domain_admin_subscribe_to_all_no_permission,
     admin_subscribe_to_all,
     domain_admin_subscribe_to_all_with_wrong_user,
     admin_subscribe_to_all_no_groups,
     admin_subscribe_to_all_without_arguments,
     domain_admin_subscribe_all_to_all_no_permission,
     admin_subscribe_all_to_all,
     domain_admin_subscribe_all_to_all_with_wrong_user,
     admin_subscribe_all_to_all_no_groups,
     admin_subscribe_all_to_all_without_arguments,
     admin_list_contacts,
     domain_admin_list_contacts_wrong_user,
     domain_admin_list_contacts_no_permission,
     admin_get_contact,
     domain_admin_get_contact_wrong_user,
     domain_admin_get_contacts_no_permission
    ].

init_per_suite(Config) ->
    Config1 = ejabberd_node_utils:init(mim(), Config),
    Config2 = escalus:init_per_suite(Config1),
    dynamic_modules:save_modules(domain_helper:host_type(), Config2).

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(admin_roster_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_roster_cli, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(domain_admin_roster, Config) ->
    graphql_helper:init_domain_admin_handler(Config);
init_per_group(user_roster, Config) ->
    graphql_helper:init_user(Config).

end_per_group(_GroupName, _Config) ->
    graphql_helper:clean(),
    escalus_fresh:clean().

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

-define(ADD_CONTACT_PATH, [data, roster, addContact]).
-define(ADD_CONTACTS_PATH, [data, roster, addContacts]).
-define(DELETE_CONTACT_PATH, [data, roster, deleteContact]).
-define(DELETE_CONTACTS_PATH, [data, roster, deleteContacts]).
-define(LIST_CONTACTS_PATH, [data, roster, listContacts]).
-define(GET_CONTACT_PATH, [data, roster, getContact]).
-define(SUBSCRIBE_ALL_TO_ALL_PATH, [data, roster, subscribeAllToAll]).
-define(SUBSCRIBE_TO_ALL_PATH, [data, roster, subscribeToAll]).
-define(MUTUAL_SUBSCRIPTION_PATH, [data, roster, setMutualSubscription]).

-define(NONEXISTENT_DOMAIN_USER, <<"abc@abc">>).
-define(NONEXISTENT_USER, <<"abc@", (domain_helper:domain())/binary>>).
-define(NONEXISTENT_USER2, <<"abc2@", (domain_helper:domain())/binary>>).
-define(DEFAULT_GROUPS, [<<"Family">>]).

%% Admin test cases

admin_add_and_delete_contact(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_add_and_delete_contact_story/3).

admin_add_and_delete_contact_story(Config, Alice, Bob) ->
    Res = admin_add_contact(Alice, Bob, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?ADD_CONTACT_PATH, Res),
                                          <<"successfully">>)),
    check_contacts([Bob], Alice),

    Res2 = admin_delete_contact(Alice, Bob, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?DELETE_CONTACT_PATH, Res2),
                                          <<"successfully">>)),
    check_contacts([], Alice).

admin_try_add_nonexistent_contact(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_try_add_nonexistent_contact/2).

admin_try_add_nonexistent_contact(Config, Alice) ->
    Contact = ?NONEXISTENT_DOMAIN_USER,
    Res = admin_add_contact(Alice, Contact, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), Contact)),
    check_contacts([], Alice).

admin_try_add_contact_to_nonexistent_user(Config) ->
    User = ?NONEXISTENT_USER,
    Contact = ?NONEXISTENT_USER2,
    Res = admin_add_contact(User, Contact, Config),
    ?assertEqual(<<"user_not_exist">>, get_err_code(Res)).

admin_try_add_contact_with_unknown_domain(Config) ->
    User = ?NONEXISTENT_DOMAIN_USER,
    Contact = ?NONEXISTENT_USER2,
    Res = admin_add_contact(User, Contact, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_try_delete_nonexistent_contact(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_try_delete_nonexistent_contact_story/3).

admin_try_delete_nonexistent_contact_story(Config, Alice, Bob) ->
    Res = admin_delete_contact(Alice, Bob, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not exist">>)).

admin_try_delete_contact_with_unknown_domain(Config) ->
    User = ?NONEXISTENT_DOMAIN_USER,
    Res = admin_delete_contact(User, User, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_add_contacts(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_add_contacts_story/3).

admin_add_contacts_story(Config, Alice, Bob) ->
    Res = admin_add_contacts(Alice, [Bob, ?NONEXISTENT_DOMAIN_USER], Config),
    [R1, null] = get_err_value(?ADD_CONTACTS_PATH, Res),
    ?assertNotEqual(nomatch, binary:match(R1, <<"successfully">>)),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not exist">>)).

admin_delete_contacts(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_delete_contacts_story/3).

admin_delete_contacts_story(Config, Alice, Bob) ->
    admin_add_contacts(Alice, [Bob], Config),
    Res = admin_delete_contacts(Alice, [Bob, ?NONEXISTENT_DOMAIN_USER], Config),
    [R1, null] = get_err_value(?DELETE_CONTACTS_PATH, Res),
    ?assertNotEqual(nomatch, binary:match(R1, <<"successfully">>)),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not exist">>)).

admin_invite_accept_and_cancel_subscription(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_invite_accept_and_cancel_subscription_story/3).

admin_invite_accept_and_cancel_subscription_story(Config, Alice, Bob) ->
    % Add contacts
    admin_add_contact(Alice, Bob, Config),
    admin_add_contact(Bob, Alice, Config),
    escalus:assert(is_roster_set, escalus:wait_for_stanza(Bob, 1)),
    escalus:assert(is_roster_set, escalus:wait_for_stanza(Alice, 1)),
    % Send invitation to subscribe
    admin_subscription(Alice, Bob, <<"INVITE">>, Config),
    escalus:assert(is_roster_set, escalus:wait_for_stanza(Alice)),
    escalus:assert(is_presence_with_type, [<<"subscribe">>], escalus:wait_for_stanza(Bob)),
    ?assertMatch(#roster{ask = out, subscription = none}, get_roster(Alice, Bob)),
    % Accept invitation
    admin_subscription(Bob, Alice, <<"ACCEPT">>, Config),
    escalus:assert(is_roster_set, escalus:wait_for_stanza(Bob)),
    IsSub = fun(S) -> escalus_pred:is_presence_with_type(<<"subscribed">>, S) end,
    escalus:assert_many([is_roster_set, IsSub, is_presence],
                        escalus:wait_for_stanzas(Alice, 3)),
    ?assertMatch(#roster{ask = none, subscription = from}, get_roster(Bob, Alice)),
    % Cancel subscription
    admin_subscription(Alice, Bob, <<"CANCEL">>, Config),
    escalus:assert(is_roster_set, escalus:wait_for_stanza(Alice)),
    ?assertMatch(#roster{ask = none, subscription = none}, get_roster(Alice, Bob)).

admin_decline_subscription_ask(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_decline_subscription_ask_story/3).

admin_decline_subscription_ask_story(Config, Alice, Bob) ->
    % Add contacts
    admin_add_contact(Alice, Bob, null, null, Config),
    admin_add_contact(Bob, Alice, null, null, Config),
    escalus:assert(is_roster_set, escalus:wait_for_stanza(Bob, 1)),
    escalus:assert(is_roster_set, escalus:wait_for_stanza(Alice, 1)),
    % Send invitation to subscribe
    admin_subscription(Bob, Alice, <<"INVITE">>, Config),
    ?assertMatch(#roster{ask = in, subscription = none}, get_roster(Alice, Bob)),
    ?assertMatch(#roster{ask = out, subscription = none}, get_roster(Bob, Alice)),
    % Decline the invitation
    admin_subscription(Alice, Bob, <<"DECLINE">>, Config),
    ?assertMatch(does_not_exist, get_roster(Alice, Bob)),
    ?assertMatch(#roster{ask = none, subscription = none}, get_roster(Bob, Alice)).

admin_try_subscribe_with_unknown_domain(Config) ->
    Bob = ?NONEXISTENT_DOMAIN_USER,
    Alice = ?NONEXISTENT_USER,
    Res = admin_subscription(Bob, Alice, <<"INVITE">>, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_set_mutual_subscription(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_set_mutual_subscription_story/3).

admin_set_mutual_subscription_story(Config, Alice, Bob) ->
    Res = admin_mutual_subscription(Alice, Bob, <<"CONNECT">>, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?MUTUAL_SUBSCRIPTION_PATH, Res),
                                          <<"successfully">>)),
    ?assertMatch(#roster{ask = none, subscription = both}, get_roster(Alice, Bob)),
    ?assertMatch(#roster{ask = none, subscription = both}, get_roster(Bob, Alice)),

    Res2 = admin_mutual_subscription(Alice, Bob, <<"DISCONNECT">>, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?MUTUAL_SUBSCRIPTION_PATH, Res2),
                                          <<"successfully">>)),
    ?assertMatch(does_not_exist, get_roster(Alice, Bob)),
    ?assertMatch(does_not_exist, get_roster(Bob, Alice)).

admin_set_mutual_subscription_try_connect_nonexistent_users(Config) ->
    Alice = ?NONEXISTENT_DOMAIN_USER,
    Bob = ?NONEXISTENT_USER,
    Res = admin_mutual_subscription(Alice, Bob, <<"CONNECT">>, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_set_mutual_subscription_try_disconnect_nonexistent_users(Config) ->
    Alice = ?NONEXISTENT_DOMAIN_USER,
    Bob = ?NONEXISTENT_USER,
    Res = admin_mutual_subscription(Alice, Bob, <<"DISCONNECT">>, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_subscribe_to_all(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}, {kate, 1}],
                                    fun admin_subscribe_to_all_story/4).

admin_subscribe_to_all_story(Config, Alice, Bob, Kate) ->
    Res = admin_subscribe_to_all(Alice, [Bob, Kate], Config),
    check_if_created_succ(?SUBSCRIBE_TO_ALL_PATH, Res),

    check_contacts([Bob, Kate], Alice),
    check_contacts([Alice], Bob),
    check_contacts([Alice], Kate).

admin_subscribe_to_all_with_wrong_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_subscribe_to_all_with_wrong_user_story/3).

admin_subscribe_to_all_with_wrong_user_story(Config, Alice, Bob) ->
    Kate = ?NONEXISTENT_DOMAIN_USER,
    Res = admin_subscribe_to_all(Alice, [Bob, Kate], Config),
    check_if_created_succ(?SUBSCRIBE_TO_ALL_PATH, Res, [true, false]),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not exist">>)),

    check_contacts([Bob], Alice),
    check_contacts([Alice], Bob).

admin_subscribe_to_all_no_groups(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}, {kate, 1}],
                                    fun admin_subscribe_to_all_no_groups_story/4).

admin_subscribe_to_all_no_groups_story(Config, Alice, Bob, Kate) ->
    EmptyGroups = [],
    Res = admin_subscribe_to_all(Alice, [Bob, Kate], null, Config),
    check_if_created_succ(?SUBSCRIBE_TO_ALL_PATH, Res),

    check_contacts([Bob, Kate], Alice, EmptyGroups),
    check_contacts([Alice], Bob, EmptyGroups),
    check_contacts([Alice], Kate, EmptyGroups).

admin_subscribe_to_all_without_arguments(Config) ->
    Res = admin_subscribe_to_all_no_args(Config),
    get_bad_request(Res).

admin_subscribe_all_to_all(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}, {kate, 1}],
                                    fun admin_subscribe_all_to_all_story/4).

admin_subscribe_all_to_all_story(Config, Alice, Bob, Kate) ->
    Res = admin_subscribe_all_to_all([Alice, Bob, Kate], Config),
    check_if_created_succ(?SUBSCRIBE_ALL_TO_ALL_PATH, Res),

    check_contacts([Bob, Kate], Alice),
    check_contacts([Alice, Kate], Bob),
    check_contacts([Alice, Bob], Kate).

admin_subscribe_all_to_all_with_wrong_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_subscribe_all_to_all_with_wrong_user_story/3).

admin_subscribe_all_to_all_with_wrong_user_story(Config, Alice, Bob) ->
    Kate = ?NONEXISTENT_DOMAIN_USER,
    Res = admin_subscribe_all_to_all([Alice, Bob, Kate], Config),
    check_if_created_succ(?SUBSCRIBE_ALL_TO_ALL_PATH, Res, [true, false, false]),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(1, Res), <<"does not exist">>)),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(2, Res), <<"does not exist">>)),

    check_contacts([Bob], Alice),
    check_contacts([Alice], Bob).

admin_subscribe_all_to_all_no_groups(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}, {kate, 1}],
                                    fun admin_subscribe_all_to_all_no_groups_story/4).

admin_subscribe_all_to_all_no_groups_story(Config, Alice, Bob, Kate) ->
    EmptyGroups = [],
    Res = admin_subscribe_all_to_all([Alice, Bob, Kate], null, Config),
    check_if_created_succ(?SUBSCRIBE_ALL_TO_ALL_PATH, Res),

    check_contacts([Bob, Kate], Alice, EmptyGroups),
    check_contacts([Alice, Kate], Bob, EmptyGroups),
    check_contacts([Alice, Bob], Kate, EmptyGroups).

admin_subscribe_all_to_all_without_arguments(Config) ->
    Res = admin_subscribe_all_to_all_no_args(Config),
    get_bad_request(Res).

admin_list_contacts(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_list_contacts_story/3).

admin_list_contacts_story(Config, Alice, Bob) ->
    BobBin = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
    BobName = escalus_client:username(Bob),
    admin_add_contact(Alice, Bob, Config),
    Res = admin_list_contacts(Alice, Config),
    [#{<<"subscription">> := <<"NONE">>, <<"ask">> := <<"NONE">>, <<"jid">> := BobBin,
       <<"name">> := BobName, <<"groups">> := ?DEFAULT_GROUPS}] =
        get_ok_value([data, roster, listContacts], Res).

admin_list_contacts_wrong_user(Config) ->
    % User with a non-existent domain
    Res = admin_list_contacts(?NONEXISTENT_DOMAIN_USER, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)),
    % Non-existent user with existent domain
    Res2 = admin_list_contacts(?NONEXISTENT_USER, Config),
    ?assertEqual(<<"user_not_exist">>, get_err_code(Res2)).

admin_get_contact(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_get_contact_story/3).

admin_get_contact_story(Config, Alice, Bob) ->
    BobBin = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
    BobName = escalus_client:username(Bob),
    admin_add_contact(Alice, Bob, Config),
    Res = admin_get_contact(Alice, Bob, Config),
    #{<<"subscription">> := <<"NONE">>, <<"ask">> := <<"NONE">>, <<"jid">> := BobBin,
      <<"name">> := BobName, <<"groups">> := ?DEFAULT_GROUPS} =
        get_ok_value([data, roster, getContact], Res).

admin_get_contact_wrong_user(Config) ->
    % User with a non-existent domain
    Res = admin_get_contact(?NONEXISTENT_DOMAIN_USER, ?NONEXISTENT_USER, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)),
    % Non-existent user with existent domain
    Res2 = admin_get_contact(?NONEXISTENT_USER, ?NONEXISTENT_USER, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not exist">>)).

admin_subscribe_all_to_all_empty_list(Config) ->
    Res = admin_subscribe_all_to_all([], Config),
    ?assertEqual([], get_ok_value(?SUBSCRIBE_ALL_TO_ALL_PATH, Res)).

%% User test cases

user_add_and_delete_contact(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_add_and_delete_contact_story/3).

user_add_and_delete_contact_story(Config, Alice, Bob) ->
    % Add a new contact
    Res = user_add_contact(Alice, Bob, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?ADD_CONTACT_PATH, Res),
                                          <<"successfully">>)),
    check_contacts([Bob], Alice),
    % Delete a contact
    Res2 = user_delete_contact(Alice, Bob, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?DELETE_CONTACT_PATH, Res2),
                                          <<"successfully">>)),
    check_contacts([], Alice).

user_try_add_nonexistent_contact(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_try_add_nonexistent_contact/2).

user_try_add_nonexistent_contact(Config, Alice) ->
    Contact = ?NONEXISTENT_DOMAIN_USER,
    Res = user_add_contact(Alice, Contact, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), Contact)),
    check_contacts([], Alice).

user_add_contacts(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_add_contacts_story/3).

user_add_contacts_story(Config, Alice, Bob) ->
    Res = user_add_contacts(Alice, [Bob, ?NONEXISTENT_DOMAIN_USER], Config),
    [R1, null] = get_ok_value(?ADD_CONTACTS_PATH, Res),
    ?assertNotEqual(nomatch, binary:match(R1, <<"successfully">>)),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not exist">>)).

user_try_delete_nonexistent_contact(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_try_delete_nonexistent_contact_story/3).

user_try_delete_nonexistent_contact_story(Config, Alice, Bob) ->
    Res = user_delete_contact(Alice, Bob, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not exist">>)).

user_delete_contacts(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_delete_contacts_story/3).

user_delete_contacts_story(Config, Alice, Bob) ->
    user_add_contact(Alice, Bob, Config),

    Res = user_delete_contacts(Alice, [Bob, ?NONEXISTENT_DOMAIN_USER], Config),
    [R1, null] = get_ok_value(?DELETE_CONTACTS_PATH, Res),
    ?assertNotEqual(nomatch, binary:match(R1, <<"successfully">>)),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not exist">>)).

user_invite_accept_and_cancel_subscription(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_invite_accept_and_cancel_subscription_story/3).

user_invite_accept_and_cancel_subscription_story(Config, Alice, Bob) ->
    % Add contacts
    user_add_contact(Alice, Bob, Config),
    user_add_contact(Bob, Alice, Config),
    escalus:assert(is_roster_set, escalus:wait_for_stanza(Bob, 1)),
    escalus:assert(is_roster_set, escalus:wait_for_stanza(Alice, 1)),
    % Send invitation to subscribe
    user_subscription(Alice, Bob, <<"INVITE">>, Config),
    escalus:assert(is_roster_set, escalus:wait_for_stanza(Alice)),
    escalus:assert(is_presence_with_type, [<<"subscribe">>], escalus:wait_for_stanza(Bob)),
    ?assertMatch(#roster{ask = out, subscription = none}, get_roster(Alice, Bob)),
    % Accept invitation
    user_subscription(Bob, Alice, <<"ACCEPT">>, Config),
    escalus:assert(is_roster_set, escalus:wait_for_stanza(Bob)),
    IsSub = fun(S) -> escalus_pred:is_presence_with_type(<<"subscribed">>, S) end,
    escalus:assert_many([is_roster_set, IsSub, is_presence],
                        escalus:wait_for_stanzas(Alice, 3)),
    ?assertMatch(#roster{ask = none, subscription = from}, get_roster(Bob, Alice)),
    % Cancel subscription
    user_subscription(Alice, Bob, <<"CANCEL">>, Config),
    escalus:assert(is_roster_set, escalus:wait_for_stanza(Alice)),
    ?assertMatch(#roster{ask = none, subscription = none}, get_roster(Alice, Bob)).

user_decline_subscription_ask(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_decline_subscription_ask_story/3).

user_decline_subscription_ask_story(Config, Alice, Bob) ->
    % Add contacts
    user_add_contact(Alice, Bob, Config),
    user_add_contact(Bob, Alice, Config),
    escalus:assert(is_roster_set, escalus:wait_for_stanza(Bob, 1)),
    escalus:assert(is_roster_set, escalus:wait_for_stanza(Alice, 1)),
    % Send invitation to subscribe
    user_subscription(Bob, Alice, <<"INVITE">>, Config),
    ?assertMatch(#roster{ask = in, subscription = none}, get_roster(Alice, Bob)),
    ?assertMatch(#roster{ask = out, subscription = none}, get_roster(Bob, Alice)),
    % Decline the invitation
    user_subscription(Alice, Bob, <<"DECLINE">>, Config),
    ?assertMatch(does_not_exist, get_roster(Alice, Bob)),
    ?assertMatch(#roster{ask = none, subscription = none}, get_roster(Bob, Alice)).

user_list_contacts(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_list_contacts_story/3).

user_list_contacts_story(Config, Alice, Bob) ->
    BobBin = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
    Name = <<"Bobek">>,
    user_add_contact(Alice, Bob, Name, ?DEFAULT_GROUPS, Config),
    Res = user_list_contacts(Alice, Config),
    [#{<<"subscription">> := <<"NONE">>, <<"ask">> := <<"NONE">>, <<"jid">> := BobBin,
       <<"name">> := Name, <<"groups">> := ?DEFAULT_GROUPS}] =
        get_ok_value(?LIST_CONTACTS_PATH, Res).

user_get_contact(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_get_contact_story/3).

user_get_contact_story(Config, Alice, Bob) ->
    BobBin = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
    Name = <<"Bobek">>,
    user_add_contact(Alice, Bob, Name, ?DEFAULT_GROUPS, Config),
    Res = user_get_contact(Alice, Bob, Config),
    #{<<"subscription">> := <<"NONE">>, <<"ask">> := <<"NONE">>, <<"jid">> := BobBin,
      <<"name">> := Name, <<"groups">> := ?DEFAULT_GROUPS} =
        get_ok_value(?GET_CONTACT_PATH, Res).

user_get_nonexistent_contact(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_get_nonexistent_contact_story/2).

user_get_nonexistent_contact_story(Config, Alice) ->
    Res = user_get_contact(Alice, ?NONEXISTENT_DOMAIN_USER, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not exist">>)).

% Domain admin test cases

domain_admin_try_add_contact_with_unknown_domain(Config) ->
    User = ?NONEXISTENT_DOMAIN_USER,
    Contact = ?NONEXISTENT_USER2,
    Res = admin_add_contact(User, Contact, Config),
    get_unauthorized(Res).

domain_admin_try_add_contact_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}, {bob, 1}],
                                    fun domain_admin_try_add_contact_no_permission_story/3).

domain_admin_try_add_contact_no_permission_story(Config, Alice, Bob) ->
    Res = admin_add_contact(Alice, Bob, Config),
    get_unauthorized(Res).

domain_admin_try_delete_contact_with_unknown_domain(Config) ->
    User = ?NONEXISTENT_DOMAIN_USER,
    Res = admin_delete_contact(User, User, Config),
    get_unauthorized(Res).

domain_admin_try_delete_contact_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}, {bob, 1}],
                                    fun domain_admin_try_delete_contact_no_permission_story/3).

domain_admin_try_delete_contact_no_permission_story(Config, Alice, Bob) ->
    Res = admin_delete_contact(Alice, Bob, Config),
    get_unauthorized(Res).

domain_admin_set_mutual_subscription_try_connect_nonexistent_users(Config) ->
    Alice = ?NONEXISTENT_DOMAIN_USER,
    Bob = ?NONEXISTENT_USER,
    Res = admin_mutual_subscription(Alice, Bob, <<"CONNECT">>, Config),
    get_unauthorized(Res).

domain_admin_set_mutual_subscription_try_connect_users_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}, {bob, 1}],
        fun domain_admin_set_mutual_subscription_try_connect_users_no_permission_story/3).

domain_admin_set_mutual_subscription_try_connect_users_no_permission_story(Config, Alice, Bob) ->
    Res = admin_mutual_subscription(Alice, Bob, <<"CONNECT">>, Config),
    get_unauthorized(Res).

domain_admin_set_mutual_subscription_try_disconnect_nonexistent_users(Config) ->
    Alice = ?NONEXISTENT_DOMAIN_USER,
    Bob = ?NONEXISTENT_USER,
    Res = admin_mutual_subscription(Alice, Bob, <<"DISCONNECT">>, Config),
    get_unauthorized(Res).

domain_admin_set_mutual_subscription_try_disconnect_users_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}, {bob, 1}],
        fun domain_admin_set_mutual_subscription_try_disconnect_users_no_permission_story/3).

domain_admin_set_mutual_subscription_try_disconnect_users_no_permission_story(Config, Alice, Bob) ->
    Res = admin_mutual_subscription(Alice, Bob, <<"DISCONNECT">>, Config),
    get_unauthorized(Res).

domain_admin_subscribe_to_all_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_subscribe_to_all_no_permission/2).

domain_admin_subscribe_to_all_no_permission(Config, Alice) ->
    get_unauthorized(admin_subscribe_to_all(Alice, [], Config)).

domain_admin_subscribe_all_to_all_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}, {bob, 1}, {kate, 1}],
        fun domain_admin_subscribe_all_to_all_no_permission/4).

domain_admin_subscribe_all_to_all_no_permission(Config, Alice, Bob, Kate) ->
    Res = admin_subscribe_all_to_all([Alice, Bob, Kate], Config),
    get_unauthorized(Res).

domain_admin_subscribe_all_to_all_with_wrong_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
        fun domain_admin_subscribe_all_to_all_with_wrong_user_story/3).

domain_admin_subscribe_all_to_all_with_wrong_user_story(Config, Alice, Bob) ->
    Kate = ?NONEXISTENT_DOMAIN_USER,
    Res = admin_subscribe_all_to_all([Alice, Bob, Kate], Config),
    get_unauthorized(Res).

domain_admin_subscribe_to_all_with_wrong_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun domain_admin_subscribe_to_all_with_wrong_user_story/3).

domain_admin_subscribe_to_all_with_wrong_user_story(Config, Alice, Bob) ->
    Kate = ?NONEXISTENT_DOMAIN_USER,
    Res = admin_subscribe_to_all(Alice, [Bob, Kate], Config),
    check_if_created_succ(?SUBSCRIBE_TO_ALL_PATH, Res, [true, false]),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not exist">>)),
    check_contacts([Bob], Alice),
    check_contacts([Alice], Bob).

domain_admin_list_contacts_wrong_user(Config) ->
    % User with a non-existent domain
    Res = admin_list_contacts(?NONEXISTENT_DOMAIN_USER, Config),
    get_unauthorized(Res),
    % Non-existent user with existent domain
    Res2 = admin_list_contacts(?NONEXISTENT_USER, Config),
    ?assertEqual(<<"user_not_exist">>, get_err_code(Res2)).

domain_admin_list_contacts_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_list_contacts_no_permission_story/2).

domain_admin_list_contacts_no_permission_story(Config, Alice) ->
    Res = admin_list_contacts(Alice, Config),
    get_unauthorized(Res).

domain_admin_get_contact_wrong_user(Config) ->
    % User with a non-existent domain
    Res = admin_get_contact(?NONEXISTENT_DOMAIN_USER, ?NONEXISTENT_USER, Config),
    get_unauthorized(Res),
    % Non-existent user with existent domain
    Res2 = admin_get_contact(?NONEXISTENT_USER, ?NONEXISTENT_USER, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not exist">>)).

domain_admin_get_contacts_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}, {bob, 1}],
                                    fun domain_admin_get_contacts_no_permission_story/3).

domain_admin_get_contacts_no_permission_story(Config, Alice, Bob) ->
    Res = admin_get_contact(Alice, Bob, Config),
    get_unauthorized(Res).

% Helpers

admin_add_contact(User, Contact, Config) ->
    Name = escalus_utils:get_username(Contact),
    admin_add_contact(User, Contact, Name, ?DEFAULT_GROUPS, Config).

user_add_contact(User, Contact, Config) ->
    Name = escalus_utils:get_username(Contact),
    user_add_contact(User, Contact, Name, ?DEFAULT_GROUPS, Config).

check_contacts(ContactClients, User) ->
    check_contacts(ContactClients, User, ?DEFAULT_GROUPS).

check_contacts(ContactClients, User, ContactGroups) ->
    Expected = [escalus_utils:jid_to_lower(escalus_client:short_jid(Client))
                || Client <- ContactClients],
    ExpectedNames = [escalus_client:username(Client) || Client <- ContactClients],
    ActualContacts = get_roster(User),
    Actual = [ jid:to_binary(JID) || #roster{jid = JID} <- ActualContacts],
    ActualNames = [ Name || #roster{name = Name} <- ActualContacts],
    ?assertEqual(lists:sort(Expected), lists:sort(Actual)),
    ?assertEqual(lists:sort(ExpectedNames), lists:sort(ActualNames)),
    [?assertEqual(ContactGroups, Groups) || #roster{groups = Groups} <- ActualContacts].

check_if_created_succ(Path, Res) ->
    OkList = get_ok_value(Path, Res),
    lists:foreach(fun check_created_msg/1, OkList).

check_if_created_succ(Path, Res, ExpectedOkValue) ->
    OkList = case lists:member(false, ExpectedOkValue) of
                 true -> get_err_value(Path, Res);
                 false -> get_ok_value(Path, Res)
             end,
    OkList2 = lists:zip(OkList, ExpectedOkValue),
    [check_created_msg(Msg) || {Msg, true} <- OkList2].

check_created_msg(Msg) ->
    ?assertNotEqual(nomatch, binary:match(Msg, <<"created successfully">>)).

get_roster(User) ->
    {ok, Roster} = rpc(mim(), mod_roster_api, list_contacts, [user_to_jid(User)]),
    Roster.

get_roster(User, Contact) ->
    rpc(mim(), mod_roster, get_roster_entry,
        [domain_helper:host_type(),
         user_to_jid(User),
         jid:to_lower(user_to_jid(Contact)),
         full]).

make_contacts(Users) ->
    make_contacts(Users, ?DEFAULT_GROUPS).

make_contacts(Users, Groups) ->
    [make_contact(U, Groups) || U <- Users].

make_contact(U) ->
    make_contact(U, ?DEFAULT_GROUPS).

make_contact(U, Groups) ->
    #{jid => user_to_bin(U), name => escalus_utils:get_username(U), groups => Groups}.

%% Commands

admin_add_contact(User, Contact, Name, Groups, Config) ->
    Vars = #{user => user_to_bin(User), contact => user_to_bin(Contact),
             name => Name, groups => Groups},
    execute_command(<<"roster">>, <<"addContact">>, Vars, Config).

admin_add_contacts(User, Contacts, Config) ->
    Vars = #{user => user_to_bin(User), contacts => make_contacts(Contacts)},
    execute_command(<<"roster">>, <<"addContacts">>, Vars, Config).

admin_delete_contact(User, Contact, Config) ->
    Vars = #{user => user_to_bin(User), contact => user_to_bin(Contact)},
    execute_command(<<"roster">>, <<"deleteContact">>, Vars, Config).

admin_delete_contacts(User, Contacts, Config) ->
    Vars = #{user => user_to_bin(User), contacts => [user_to_bin(C) || C <- Contacts]},
    execute_command(<<"roster">>, <<"deleteContacts">>, Vars, Config).

admin_subscription(User, Contact, Action, Config) ->
    Vars = #{user => user_to_bin(User), contact => user_to_bin(Contact), action => Action},
    execute_command(<<"roster">>, <<"subscription">>, Vars, Config).

admin_mutual_subscription(User, Contact, Action, Config) ->
    Vars = #{userA => user_to_bin(User), userB => user_to_bin(Contact), action => Action},
    execute_command(<<"roster">>, <<"setMutualSubscription">>, Vars, Config).

admin_subscribe_to_all(User, Contacts, Config) ->
    admin_subscribe_to_all(User, Contacts, ?DEFAULT_GROUPS, Config).

admin_subscribe_to_all(User, Contacts, Groups, Config) ->
    Vars = #{user => make_contact(User, Groups), contacts => make_contacts(Contacts, Groups)},
    execute_command(<<"roster">>, <<"subscribeToAll">>, Vars, Config).

admin_subscribe_to_all_no_args(Config) ->
    execute_command(<<"roster">>, <<"subscribeToAll">>, #{}, Config).

admin_subscribe_all_to_all(Users, Config) ->
    admin_subscribe_all_to_all(Users, ?DEFAULT_GROUPS, Config).

admin_subscribe_all_to_all(Users, Groups, Config) ->
    Vars = #{contacts => make_contacts(Users, Groups)},
    execute_command(<<"roster">>, <<"subscribeAllToAll">>, Vars, Config).

admin_subscribe_all_to_all_no_args(Config) ->
    execute_command(<<"roster">>, <<"subscribeAllToAll">>, #{}, Config).

admin_list_contacts(User, Config) ->
    Vars = #{user => user_to_bin(User)},
    execute_command(<<"roster">>, <<"listContacts">>, Vars, Config).

admin_get_contact(User, Contact, Config) ->
    Vars = #{user => user_to_bin(User), contact => user_to_bin(Contact)},
    execute_command(<<"roster">>, <<"getContact">>, Vars, Config).

user_add_contact(User, Contact, Name, Groups, Config) ->
    Vars = #{contact => user_to_bin(Contact), name => Name, groups => Groups},
    execute_user_command(<<"roster">>, <<"addContact">>, User, Vars, Config).

user_add_contacts(User, Contacts, Config) ->
    Vars = #{contacts => make_contacts(Contacts)},
    execute_user_command(<<"roster">>, <<"addContacts">>, User, Vars, Config).

user_delete_contact(User, Contact, Config) ->
    Vars = #{contact => user_to_bin(Contact)},
    execute_user_command(<<"roster">>, <<"deleteContact">>, User, Vars, Config).

user_delete_contacts(User, Contacts, Config) ->
    Vars = #{contacts => [user_to_bin(C) || C <- Contacts]},
    execute_user_command(<<"roster">>, <<"deleteContacts">>, User, Vars, Config).

user_subscription(User, Contact, Action, Config) ->
    Vars = #{contact => user_to_bin(Contact), action => Action},
    execute_user_command(<<"roster">>, <<"subscription">>, User, Vars, Config).

user_list_contacts(User, Config) ->
    execute_user_command(<<"roster">>, <<"listContacts">>, User, #{}, Config).

user_get_contact(User, Contact, Config) ->
    Vars = #{contact => user_to_bin(Contact)},
    execute_user_command(<<"roster">>, <<"getContact">>, User, Vars, Config).
