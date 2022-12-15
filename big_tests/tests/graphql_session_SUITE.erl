-module(graphql_session_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-import(common_helper, [unprep/1]).
-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(domain_helper, [domain/0]).
-import(graphql_helper, [execute_user_command/5, execute_command/4,  get_listener_port/1,
                         get_listener_config/1, get_ok_value/2, get_err_msg/1, get_unauthorized/1,
                         get_coercion_err_msg/1]).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user_session},
     {group, admin_session},
     {group, domain_admin_session}].

groups() ->
    [{user_session, [parallel], user_session_tests()},
     {admin_session, [], [{group, admin_session_http}, {group, admin_session_cli}]},
     {admin_session_http, [], admin_session_tests()},
     {admin_session_cli, [], admin_session_tests()},
     {domain_admin_session, [], domain_admin_session_tests()}].

user_session_tests() ->
    [user_list_resources,
     user_count_resources,
     user_sessions_info].

admin_session_tests() ->
    [admin_list_sessions,
     admin_count_sessions,
     admin_list_user_sessions,
     admin_count_user_resources,
     admin_get_user_resource,
     admin_list_users_with_status,
     admin_count_users_with_status,
     admin_kick_user_session,
     admin_kick_user,
     admin_set_presence,
     admin_set_presence_away,
     admin_set_presence_unavailable].

domain_admin_session_tests() ->
    [domain_admin_list_sessions,
     domain_admin_count_sessions,
     admin_list_user_sessions,
     domain_admin_list_user_sessions_no_permission,
     admin_count_user_resources,
     domain_admin_count_user_resources_no_permission,
     admin_get_user_resource,
     domain_admin_get_user_resource_no_permission,
     domain_admin_list_users_with_status,
     domain_admin_count_users_with_status,
     admin_kick_user_session,
     domain_admin_kick_user_session_no_permission,
     domain_admin_kick_user_no_permission,
     admin_set_presence,
     admin_set_presence_away,
     admin_set_presence_unavailable,
     domain_admin_set_presence_no_permission].

init_per_suite(Config) ->
    Config1 = ejabberd_node_utils:init(mim(), Config),
    Config2 = escalus:init_per_suite(Config1),
    dynamic_modules:save_modules(domain_helper:host_type(), Config2).

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(admin_session, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, alice_bis, bob]));
init_per_group(admin_session_cli, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_session_http, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(domain_admin_session, Config) ->
    Config1 = graphql_helper:init_domain_admin_handler(Config),
    case Config1 of
        {skip, require_rdbms} ->
            Config1;
        _ ->
            escalus:create_users(Config1, escalus:get_users([alice, alice_bis, bob]))
    end;
init_per_group(user_session, Config) ->
    graphql_helper:init_user(Config).

end_per_group(admin_session, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, alice_bis, bob]));
end_per_group(domain_admin_session, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, alice_bis, bob])),
    escalus_fresh:clean(),
    graphql_helper:clean();
end_per_group(_GroupName, _Config) ->
    escalus_fresh:clean(),
    graphql_helper:clean().

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%% Test cases

user_list_resources(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 2}], fun user_list_resources_story/3).

user_list_resources_story(Config, Alice, Alice2) ->
    Result = user_list_resources(Alice, Config),
    Path = [data, session, listResources],
    ExpectedRes = [escalus_client:resource(Alice), escalus_client:resource(Alice2)],
    ?assertMatch(ExpectedRes, lists:sort(get_ok_value(Path, Result))).

user_count_resources(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 3}], fun user_count_resources_story/4).

user_count_resources_story(Config, Alice, _Alice2, _Alice3) ->
    Result = user_count_resources(Alice, Config),
    Path = [data, session, countResources],
    ?assertEqual(3, get_ok_value(Path, Result)).

user_sessions_info(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_sessions_info_story/2).

user_sessions_info_story(Config, Alice) ->
    Result = user_list_sessions(Alice, Config),
    ExpectedUser = escalus_utils:jid_to_lower(escalus_client:full_jid(Alice)),
    Path = [data, session, listSessions],
    ?assertMatch([#{<<"user">> := ExpectedUser}], get_ok_value(Path, Result)).


domain_admin_list_sessions(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {alice_bis, 1}, {bob, 1}],
                                    fun domain_admin_list_sessions_story/4).

domain_admin_list_sessions_story(Config, Alice, AliceB, _Bob) ->
    Domain = escalus_client:server(Alice),
    BisDomain = escalus_client:server(AliceB),
    Path = [data, session, listSessions],
    % List all sessions
    Res = list_sessions(null, Config),
    get_unauthorized(Res),
    % List sessions for an external domain
    Res2 = list_sessions(BisDomain, Config),
    get_unauthorized(Res2),
    % List sessions for local domain
    Res3 = list_sessions(Domain, Config),
    Sessions = get_ok_value(Path, Res3),
    ?assertEqual(2, length(Sessions)).

domain_admin_count_sessions(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {alice_bis, 1}, {bob, 1}],
                                    fun domain_admin_count_sessions_story/4).

domain_admin_count_sessions_story(Config, Alice, AliceB, _Bob) ->
    Domain = escalus_client:server(Alice),
    BisDomain = escalus_client:server(AliceB),
    Path = [data, session, countSessions],
    % Count all sessions
    Res = count_sessions(null, Config),
    get_unauthorized(Res),
    % Count sessions for an external domain
    Res2 = count_sessions(BisDomain, Config),
    get_unauthorized(Res2),
    % Count sessions for local domain
    Res3 = count_sessions(Domain, Config),
    Number = get_ok_value(Path, Res3),
    ?assertEqual(2, Number).

domain_admin_list_user_sessions_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_list_user_sessions_no_permission_story/2).

domain_admin_list_user_sessions_no_permission_story(Config, AliceBis) ->
    AliceBisJID = escalus_client:full_jid(AliceBis),
    Res = list_user_sessions(AliceBisJID, Config),
    get_unauthorized(Res).

domain_admin_count_user_resources_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_count_user_resources_story_no_permission/2).

domain_admin_count_user_resources_story_no_permission(Config, AliceBis) ->
    AliceBisJID = escalus_client:full_jid(AliceBis),
    Res = count_user_resources(AliceBisJID, Config),
    get_unauthorized(Res).

domain_admin_get_user_resource_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_get_user_resource_story_no_permission_story/2).

domain_admin_get_user_resource_story_no_permission_story(Config, AliceBis) ->
    AliceBisJID = escalus_client:short_jid(AliceBis),
    Res = get_user_resource(AliceBisJID, 2, Config),
    get_unauthorized(Res).

domain_admin_kick_user_session_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_kick_user_session_no_permission_story/2).

domain_admin_kick_user_session_no_permission_story(Config, AliceBis) ->
    AliceBisJID = escalus_client:full_jid(AliceBis),
    Reason = <<"Test kick">>,
    Res = kick_user_session(AliceBisJID, Reason, Config),
    get_unauthorized(Res).

domain_admin_kick_user_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_kick_user_no_permission_story/2).

domain_admin_kick_user_no_permission_story(Config, AliceBis) ->
    AliceBisJID = escalus_client:short_jid(AliceBis),
    Reason = <<"Test kick">>,
    Res = kick_user(AliceBisJID, Reason, Config),
    get_unauthorized(Res).

domain_admin_set_presence_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_set_presence_no_permission_story/2).

domain_admin_set_presence_no_permission_story(Config, AliceBis) ->
    AliceBisJID = escalus_client:full_jid(AliceBis),
    Type = <<"AVAILABLE">>,
    Show = <<"ONLINE">>,
    Status = <<"Be right back">>,
    Priority = 1,
    Res = set_presence(AliceBisJID, Type, Show, Status, Priority, Config),
    get_unauthorized(Res).

domain_admin_list_users_with_status(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {alice_bis, 1}],
                                    fun domain_admin_list_users_with_status_story/3).

domain_admin_list_users_with_status_story(Config, Alice, _AliceB) ->
    AliceJID = escalus_client:full_jid(Alice),
    AwayStatus = <<"away">>,
    AwayPresence = escalus_stanza:presence_show(AwayStatus),
    DndStatus = <<"dnd">>,
    DndPresence = escalus_stanza:presence_show(DndStatus),
    % List users with away status globally
    escalus_client:send(Alice, AwayPresence),
    Res = list_users_with_status(null, AwayStatus, Config),
    get_unauthorized(Res),
    % List users with away status for a domain
    assert_list_users_with_status([AliceJID], domain(), AwayStatus, Config),
    assert_list_users_with_status([AliceJID], unprep(domain()), AwayStatus, Config),
    % List users with away status for an external domain
    Res3 = list_users_with_status(domain_helper:secondary_domain(), AwayStatus, Config),
    get_unauthorized(Res3),
    % List users with dnd status globally
    escalus_client:send(Alice, DndPresence),
    Res4 = list_users_with_status(null, DndStatus, Config),
    get_unauthorized(Res4),
    % List users with dnd status for a domain
    assert_list_users_with_status([AliceJID], domain(), DndStatus, Config),
    % List users with dnd status for an external domain
    Res6 = list_users_with_status(domain_helper:secondary_domain(), AwayStatus, Config),
    get_unauthorized(Res6).

domain_admin_count_users_with_status(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {alice_bis, 1}],
                                    fun domain_admin_count_users_with_status_story/3).

domain_admin_count_users_with_status_story(Config, Alice, _AliceB) ->
    AwayStatus = <<"away">>,
    AwayPresence = escalus_stanza:presence_show(AwayStatus),
    DndStatus = <<"dnd">>,
    DndPresence = escalus_stanza:presence_show(DndStatus),
    % Count users with away status globally
    escalus_client:send(Alice, AwayPresence),
    Res = count_users_with_status(null, AwayStatus, Config),
    get_unauthorized(Res),
    % Count users with away status for a domain
    assert_count_users_with_status(1, domain_helper:domain(), AwayStatus, Config),
    assert_count_users_with_status(1, unprep(domain_helper:domain()), AwayStatus, Config),
    % Count users with dnd status globally
    escalus_client:send(Alice, DndPresence),
    Res3 = count_users_with_status(null, DndStatus, Config),
    get_unauthorized(Res3),
    % Count users with dnd status for a domain
    assert_count_users_with_status(1, domain_helper:domain(), DndStatus, Config).

admin_list_sessions(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {alice_bis, 1}, {bob, 1}],
                                    fun admin_list_sessions_story/4).

admin_list_sessions_story(Config, _Alice, AliceB, _Bob) ->
    BisDomain = escalus_client:server(AliceB),
    Path = [data, session, listSessions],
    % List all sessions
    Res = list_sessions(null, Config),
    Sessions = get_ok_value(Path, Res),
    ?assertEqual(3, length(Sessions)),
    % List sessions for a domain
    Res2 = list_sessions(BisDomain, Config),
    Sessions2 = get_ok_value(Path, Res2),
    ?assertEqual(1, length(Sessions2)),
    Res3 = list_sessions(unprep(BisDomain), Config),
    Sessions3 = get_ok_value(Path, Res3),
    ?assertEqual(1, length(Sessions3)),
    % List sessions for a non-existing domain
    Res4 = list_sessions(<<"nonexisting">>, Config),
    ?assertEqual(<<"Domain not found">>, get_err_msg(Res4)).

admin_count_sessions(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {alice_bis, 1}, {bob, 1}],
                                    fun admin_count_sessions_story/4).

admin_count_sessions_story(Config, _Alice, AliceB, _Bob) ->
    BisDomain = escalus_client:server(AliceB),
    Path = [data, session, countSessions],
    % Count all sessions
    Res = count_sessions(null, Config),
    Number = get_ok_value(Path, Res),
    ?assertEqual(3, Number),
    % Count sessions for a domain
    Res2 = count_sessions(BisDomain, Config),
    ?assertEqual(1,  get_ok_value(Path, Res2)),
    Res3 = count_sessions(unprep(BisDomain), Config),
    ?assertEqual(1,  get_ok_value(Path, Res3)),
    % Count sessions for a non-existing domain
    Res4 = count_sessions(<<"nonexisting">>, Config),
    ?assertEqual(<<"Domain not found">>, get_err_msg(Res4)).

admin_list_user_sessions(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 2}, {bob, 1}],
                                    fun admin_list_user_sessions_story/4).

admin_list_user_sessions_story(Config, Alice, Alice2, _Bob) ->
    S1JID = escalus_client:full_jid(Alice),
    S2JID = escalus_client:full_jid(Alice2),
    Path = [data, session, listUserSessions],
    Res = list_user_sessions(S1JID, Config),
    ExpectedRes = lists:map(fun escalus_utils:jid_to_lower/1, [S1JID, S2JID]),
    Sessions = get_ok_value(Path, Res),
    ?assertEqual(2, length(Sessions)),
    check_users(ExpectedRes, Sessions),
    % Check for a non-existing user
    Domain = domain(),
    Res2 = list_user_sessions(<<"alien@", Domain/binary>>, Config),
    ?assertEqual(<<"Given user does not exist">>, get_err_msg(Res2)).

admin_count_user_resources(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 3}], fun admin_count_user_resources_story/4).

admin_count_user_resources_story(Config, Alice, _Alice2, _Alice3) ->
    Path = [data, session, countUserResources],
    JID = escalus_client:full_jid(Alice),
    Res = count_user_resources(JID, Config),
    ?assertEqual(3, get_ok_value(Path, Res)),
    % Check for a non-existing user
    Domain = domain(),
    Res2 = count_user_resources(<<"alien@", Domain/binary>>, Config),
    ?assertEqual(<<"Given user does not exist">>, get_err_msg(Res2)).

admin_get_user_resource(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 3}], fun admin_get_user_resource_story/4).

admin_get_user_resource_story(Config, Alice, Alice2, _Alice3) ->
    Path = [data, session, getUserResource],
    JID = escalus_client:short_jid(Alice),
    % Provide a correct resource number
    Res = get_user_resource(JID, 2, Config),
    ?assertEqual(escalus_client:resource(Alice2), get_ok_value(Path, Res)),
    % Provide a wrong resource number
    Res2 = get_user_resource(JID, 4, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"Wrong resource number">>)),
    % Check for a non-existing user
    Domain = domain(),
    Res3 = get_user_resource(<<"alien@", Domain/binary>>, 1, Config),
    ?assertEqual(<<"Given user does not exist">>, get_err_msg(Res3)).

admin_count_users_with_status(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {alice_bis, 1}],
                                    fun admin_count_users_with_status_story/3).

admin_count_users_with_status_story(Config, Alice, AliceB) ->
    AwayStatus = <<"away">>,
    AwayPresence = escalus_stanza:presence_show(AwayStatus),
    DndStatus = <<"dnd">>,
    DndPresence = escalus_stanza:presence_show(DndStatus),
    % Count users with away status globally
    escalus_client:send(Alice, AwayPresence),
    escalus_client:send(AliceB, AwayPresence),
    assert_count_users_with_status(2, null, AwayStatus, Config),
    % Count users with away status for a domain
    assert_count_users_with_status(1, domain_helper:domain(), AwayStatus, Config),
    assert_count_users_with_status(1, unprep(domain_helper:domain()), AwayStatus, Config),
    % Count users with dnd status globally
    escalus_client:send(AliceB, DndPresence),
    assert_count_users_with_status(1, null, DndStatus, Config),
    % Count users with dnd status for a non-existing domain
    Res = count_users_with_status(<<"nonexisting">>, DndStatus, Config),
    ?assertEqual(<<"Domain not found">>, get_err_msg(Res)).

admin_list_users_with_status(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {alice_bis, 1}],
                                    fun admin_list_users_with_status_story/3).

admin_list_users_with_status_story(Config, Alice, AliceB) ->
    AliceJID = escalus_client:full_jid(Alice),
    AliceBJID = escalus_client:full_jid(AliceB),
    AwayStatus = <<"away">>,
    AwayPresence = escalus_stanza:presence_show(AwayStatus),
    DndStatus = <<"dnd">>,
    DndPresence = escalus_stanza:presence_show(DndStatus),
    % List users with away status globally
    escalus_client:send(Alice, AwayPresence),
    escalus_client:send(AliceB, AwayPresence),
    assert_list_users_with_status([AliceJID, AliceBJID], null, AwayStatus, Config),
    % List users with away status for a domain
    assert_list_users_with_status([AliceJID], domain(), AwayStatus, Config),
    assert_list_users_with_status([AliceJID], unprep(domain()), AwayStatus, Config),
    % List users with dnd status globally
    escalus_client:send(AliceB, DndPresence),
    assert_list_users_with_status([AliceBJID], null, DndStatus, Config),
    % List users with dnd status for a non-existing domain
    Res = count_users_with_status(<<"nonexisting">>, DndStatus, Config),
    ?assertEqual(<<"Domain not found">>, get_err_msg(Res)).

admin_kick_user_session(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 2}], fun admin_kick_user_session_story/3).

admin_kick_user_session_story(Config, Alice1, Alice2) ->
    JIDA1 = escalus_client:full_jid(Alice1),
    JIDA2 = escalus_client:full_jid(Alice2),
    Reason = <<"Test kick">>,
    Path = [data, session, kickUserSession, message],
    Res = kick_user_session(JIDA1, Reason, Config),
    % Kick an active session
    ?assertNotEqual(nomatch, binary:match(get_ok_value(Path, Res), <<"kicked">>)),
    ?assertEqual(JIDA1, get_ok_value([data, session, kickUserSession, jid], Res)),
    % Try to kick an offline session
    Res2 = kick_user_session(JIDA1, Reason, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"No active session">>)),
    % Try to kick a session with JID without a resource
    Res3 = kick_user_session(escalus_client:short_jid(Alice2), Reason, Config),
    ?assertNotEqual(nomatch, binary:match(get_coercion_err_msg(Res3), <<"jid_without_resource">>)),
    % Kick active session without reason text
    Res4 = kick_user_session(JIDA2, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(Path, Res4), <<"kicked">>)),
    % Kick a non-existing user
    Domain = domain(),
    Res5 = kick_user_session(<<"alien@", Domain/binary, "/mobile">>, null, Config),
    ?assertEqual(<<"Given user does not exist">>, get_err_msg(Res5)).

admin_kick_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 2}], fun admin_kick_user_story/3).

admin_kick_user_story(Config, Alice1, _Alice2) ->
    AliceJID = escalus_client:short_jid(Alice1),
    Reason = <<"Test kick">>,
    Res1 = kick_user(AliceJID, Reason, Config),
    Res2 = get_ok_value([data, session, kickUser], Res1),
    ?assertEqual(2, length(Res2)),
    ?assertEqual([true, true], [Kicked || #{<<"kicked">> := Kicked} <- Res2]),
    % Kick a non-existing user
    Domain = domain(),
    Res5 = kick_user(<<"alien@", Domain/binary>>, null, Config),
    ?assertEqual(<<"Given user does not exist">>, get_err_msg(Res5)).

admin_set_presence(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_set_presence_story/2).

admin_set_presence_story(Config, Alice) ->
    JID = escalus_client:full_jid(Alice),
    ShortJID = escalus_client:short_jid(Alice),
    Type = <<"AVAILABLE">>,
    Show = <<"ONLINE">>,
    Status = <<"Be right back">>,
    Priority = 1,
    % Send short JID
    Res = set_presence(ShortJID, Type, Show, Status, Priority, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"resource is empty">>)),
    % Non-existing user
    Domain = domain(),
    Res2 = set_presence(<<"alien@", Domain/binary, "/mobile">>, Type, Show, Status, Priority, Config),
    ?assertEqual(<<"Given user does not exist">>, get_err_msg(Res2)),
    % Send full JID
    Path = [data, session, setPresence, message],
    Res3 = set_presence(JID, Type, Show, Status, Priority, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(Path, Res3), <<"set successfully">>)),
    Presence = escalus:wait_for_stanza(Alice),
    ?assertNot(escalus_pred:is_presence_with_show(<<"online">>, Presence)),
    escalus:assert(is_presence_with_type, [<<"available">>], Presence),
    escalus:assert(is_presence_with_status, [Status], Presence),
    escalus:assert(is_presence_with_priority, [<<"1">>], Presence).

admin_set_presence_away(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_set_presence_away_story/2).

admin_set_presence_away_story(Config, Alice) ->
    JID = escalus_client:full_jid(Alice),
    Type = <<"AVAILABLE">>,
    Show = <<"AWAY">>,
    Path = [data, session, setPresence, message],
    Res2 = set_presence(JID, Type, Show, null, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(Path, Res2), <<"set successfully">>)),
    Presence = escalus:wait_for_stanza(Alice),
    escalus:assert(is_presence_with_type, [<<"available">>], Presence),
    escalus:assert(is_presence_with_show, [<<"away">>], Presence).

admin_set_presence_unavailable(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 2}], fun admin_set_presence_unavailable_story/3).

admin_set_presence_unavailable_story(Config, Alice, Alice2) ->
    JID = escalus_client:full_jid(Alice),
    Type = <<"UNAVAILABLE">>,
    Status = <<"I'm sleeping">>,
    Path = [data, session, setPresence, message],
    Res2 = set_presence(JID, Type, null, Status, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(Path, Res2), <<"set successfully">>)),
    Presence = escalus:wait_for_stanza(Alice2),
    escalus:assert(is_presence_with_type, [<<"unavailable">>], Presence),
    escalus:assert(is_presence_with_status, [Status], Presence).


%% Commands

user_list_resources(User, Config) ->
    execute_user_command(<<"session">>, <<"listResources">>, User, #{}, Config).

user_count_resources(User, Config) ->
    execute_user_command(<<"session">>, <<"countResources">>, User, #{}, Config).

user_list_sessions(User, Config) ->
    execute_user_command(<<"session">>, <<"listSessions">>, User, #{}, Config).

list_sessions(Domain, Config) ->
    Vars = #{<<"domain">> => Domain},
    execute_command(<<"session">>, <<"listSessions">>, Vars, Config).

count_sessions(Domain, Config) ->
    Vars = #{<<"domain">> => Domain},
    execute_command(<<"session">>, <<"countSessions">>, Vars, Config).

list_user_sessions(User, Config) ->
    Vars = #{<<"user">> => User},
    execute_command(<<"session">>, <<"listUserSessions">>, Vars, Config).

count_user_resources(User, Config) ->
    Vars = #{<<"user">> => User},
    execute_command(<<"session">>, <<"countUserResources">>, Vars, Config).

get_user_resource(User, Number, Config) ->
    Vars = #{<<"user">> => User, <<"number">> => Number},
    execute_command(<<"session">>, <<"getUserResource">>, Vars, Config).

list_users_with_status(Domain, Status, Config) ->
    Vars = #{<<"domain">> => Domain, <<"status">> => Status},
    execute_command(<<"session">>, <<"listUsersWithStatus">>, Vars, Config).

count_users_with_status(Domain, Status, Config) ->
    Vars = #{<<"domain">> => Domain, <<"status">> => Status},
    execute_command(<<"session">>, <<"countUsersWithStatus">>, Vars, Config).

kick_user_session(JID, Reason, Config) ->
    Vars = #{<<"user">> => JID, <<"reason">> => Reason},
    execute_command(<<"session">>, <<"kickUserSession">>, Vars, Config).

kick_user(JID, Reason, Config) ->
    Vars = #{<<"user">> => JID, <<"reason">> => Reason},
    execute_command(<<"session">>, <<"kickUser">>, Vars, Config).

set_presence(JID, Type, Show, Status, Priority, Config) ->
    Vars = #{<<"user">> => JID, <<"type">> => Type, <<"show">> => Show,
             <<"status">> => Status, <<"priority">> => Priority},
    execute_command(<<"session">>, <<"setPresence">>, Vars, Config).

%% Helpers

assert_list_users_with_status(ExpectedUsers, Domain, Status, Config) ->
    Res = list_users_with_status(Domain, Status, Config),
    Users = get_ok_value([data, session, listUsersWithStatus], Res),
    check_users(ExpectedUsers, Users).

assert_count_users_with_status(ExpectedCount, Domain, Status, Config) ->
    Res = count_users_with_status(Domain, Status, Config),
    Count = get_ok_value([data, session, countUsersWithStatus], Res),
    ?assertEqual(ExpectedCount, Count).

-spec check_users([jid:literal_jid()], [#{user := jid:literal_jid()}]) -> boolean().
check_users(Expected, ActualUsers) ->
    ActualJIDs = [JID || #{<<"user">> := JID} <- ActualUsers],
    ?assertEqual(lists:sort(Expected), lists:sort(ActualJIDs)).
