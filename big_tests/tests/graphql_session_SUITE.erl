-module(graphql_session_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [execute_user_command/5, execute_command/4,  get_listener_port/1,
                         get_listener_config/1, get_ok_value/2, get_err_msg/1, get_unauthorized/1]).

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
     admin_kick_session,
     admin_set_presence,
     admin_set_presence_away,
     admin_set_presence_unavailable].

domain_admin_session_tests() ->
    [domain_admin_list_sessions,
     domain_admin_count_sessions,
     admin_list_user_sessions,
     admin_count_user_resources,
     admin_get_user_resource,
     domain_admin_list_users_with_status,
     domain_admin_count_users_with_status,
     admin_kick_session,
     admin_set_presence,
     admin_set_presence_away,
     admin_set_presence_unavailable].

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

domain_admin_list_users_with_status(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {alice_bis, 1}],
                                    fun domain_admin_list_users_with_status_story/3).

domain_admin_list_users_with_status_story(Config, Alice, _AliceB) ->
    AliceJID = escalus_client:full_jid(Alice),
    Path = [data, session, listUsersWithStatus],
    AwayStatus = <<"away">>,
    AwayPresence = escalus_stanza:presence_show(AwayStatus),
    DndStatus = <<"dnd">>,
    DndPresence = escalus_stanza:presence_show(DndStatus),
    % List users with away status globally
    escalus_client:send(Alice, AwayPresence),
    Res = list_users_with_status(null, AwayStatus, Config),
    get_unauthorized(Res),
    % List users with away status for a domain
    Res2 = list_users_with_status(domain_helper:domain(), AwayStatus, Config),
    StatusUsers = get_ok_value(Path, Res2),
    ?assertEqual(1, length(StatusUsers)),
    check_users([AliceJID], StatusUsers),
    % List users with dnd status globally
    escalus_client:send(Alice, DndPresence),
    Res3 = list_users_with_status(null, DndStatus, Config),
    get_unauthorized(Res3),
    % List users with dnd status for a domain
    Res4 = list_users_with_status(domain_helper:domain(), DndStatus, Config),
    StatusUsers2 = get_ok_value(Path, Res4),
    ?assertEqual(1, length(StatusUsers2)),
    check_users([AliceJID], StatusUsers2).

domain_admin_count_users_with_status(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {alice_bis, 1}],
                                    fun domain_admin_count_users_with_status_story/3).

domain_admin_count_users_with_status_story(Config, Alice, _AliceB) ->
    Path = [data, session, countUsersWithStatus],
    AwayStatus = <<"away">>,
    AwayPresence = escalus_stanza:presence_show(AwayStatus),
    DndStatus = <<"dnd">>,
    DndPresence = escalus_stanza:presence_show(DndStatus),
    % Count users with away status globally
    escalus_client:send(Alice, AwayPresence),
    Res = count_users_with_status(null, AwayStatus, Config),
    get_unauthorized(Res),
    % Count users with away status for a domain
    Res2 = count_users_with_status(domain_helper:domain(), AwayStatus, Config),
    ?assertEqual(1, get_ok_value(Path, Res2)),
    % Count users with dnd status globally
    escalus_client:send(Alice, DndPresence),
    Res3 = count_users_with_status(null, DndStatus, Config),
    get_unauthorized(Res3),
    % Count users with dnd status for a domain
    Res4 = count_users_with_status(domain_helper:domain(), DndStatus, Config),
    ?assertEqual(1, get_ok_value(Path, Res4)).

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
    ?assertEqual(1, length(Sessions2)).

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
    Number2 = get_ok_value(Path, Res2),
    ?assertEqual(1, Number2).

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
    check_users(ExpectedRes, Sessions).

admin_count_user_resources(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 3}], fun admin_count_user_resources_story/4).

admin_count_user_resources_story(Config, Alice, _Alice2, _Alice3) ->
    Path = [data, session, countUserResources],
    JID = escalus_client:full_jid(Alice),
    Res = count_user_resources(JID, Config),
    ?assertEqual(3, get_ok_value(Path, Res)).

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
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"Wrong resource number">>)).

admin_count_users_with_status(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {alice_bis, 1}],
                                    fun admin_count_users_with_status_story/3).

admin_count_users_with_status_story(Config, Alice, AliceB) ->
    Path = [data, session, countUsersWithStatus],
    AwayStatus = <<"away">>,
    AwayPresence = escalus_stanza:presence_show(AwayStatus),
    DndStatus = <<"dnd">>,
    DndPresence = escalus_stanza:presence_show(DndStatus),
    % Count users with away status globally
    escalus_client:send(Alice, AwayPresence),
    escalus_client:send(AliceB, AwayPresence),
    Res = count_users_with_status(null, AwayStatus, Config),
    ?assertEqual(2, get_ok_value(Path, Res)),
    % Count users with away status for a domain
    Res2 = count_users_with_status(domain_helper:domain(), AwayStatus, Config),
    ?assertEqual(1, get_ok_value(Path, Res2)),
    % Count users with dnd status globally
    escalus_client:send(AliceB, DndPresence),
    Res3 = count_users_with_status(null, DndStatus, Config),
    ?assertEqual(1, get_ok_value(Path, Res3)).

admin_list_users_with_status(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {alice_bis, 1}],
                                    fun admin_list_users_with_status_story/3).

admin_list_users_with_status_story(Config, Alice, AliceB) ->
    AliceJID = escalus_client:full_jid(Alice),
    AliceBJID = escalus_client:full_jid(AliceB),
    Path = [data, session, listUsersWithStatus],
    AwayStatus = <<"away">>,
    AwayPresence = escalus_stanza:presence_show(AwayStatus),
    DndStatus = <<"dnd">>,
    DndPresence = escalus_stanza:presence_show(DndStatus),
    % List users with away status globally
    escalus_client:send(Alice, AwayPresence),
    escalus_client:send(AliceB, AwayPresence),
    Res = list_users_with_status(null, AwayStatus, Config),
    StatusUsers = get_ok_value(Path, Res),
    ?assertEqual(2, length(StatusUsers)),
    check_users([AliceJID, AliceBJID], StatusUsers),
    % List users with away status for a domain
    Res2 = list_users_with_status(domain_helper:domain(), AwayStatus, Config),
    StatusUsers2 = get_ok_value(Path, Res2),
    ?assertEqual(1, length(StatusUsers2)),
    check_users([AliceJID], StatusUsers2),
    % List users with dnd status globally
    escalus_client:send(AliceB, DndPresence),
    Res3 = list_users_with_status(null, DndStatus, Config),
    StatusUsers3 = get_ok_value(Path, Res3),
    ?assertEqual(1, length(StatusUsers3)),
    check_users([AliceBJID], StatusUsers3).

admin_kick_session(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 2}], fun admin_kick_session_story/3).

admin_kick_session_story(Config, Alice1, Alice2) ->
    JIDA1 = escalus_client:full_jid(Alice1),
    JIDA2 = escalus_client:full_jid(Alice2),
    Reason = <<"Test kick">>,
    Path = [data, session, kickUser, message],
    Res = kick_user(JIDA1, Reason, Config),
    % Kick an active session
    ?assertNotEqual(nomatch, binary:match(get_ok_value(Path, Res), <<"kicked">>)),
    ?assertEqual(JIDA1, get_ok_value([data, session, kickUser, jid], Res)),
    % Try to kick an offline session
    Res2 = kick_user(JIDA1, Reason, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"No active session">>)),
    % Try to kick a session with JID without a resource
    Res3 = kick_user(escalus_client:short_jid(Alice2), Reason, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"No active session">>)),
    % Kick another active session
    Res4 = kick_user(JIDA2, Reason, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(Path, Res4), <<"kicked">>)).

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
    % Send full JID
    Path = [data, session, setPresence, message],
    Res2 = set_presence(JID, Type, Show, Status, Priority, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(Path, Res2), <<"set successfully">>)),
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

kick_user(JID, Reason, Config) ->
    Vars = #{<<"user">> => JID, <<"reason">> => Reason},
    execute_command(<<"session">>, <<"kickUser">>, Vars, Config).

set_presence(JID, Type, Show, Status, Priority, Config) ->
    Vars = #{<<"user">> => JID, <<"type">> => Type, <<"show">> => Show,
             <<"status">> => Status, <<"priority">> => Priority},
    execute_command(<<"session">>, <<"setPresence">>, Vars, Config).

%% Helpers

-spec check_users([jid:literal_jid()], [#{user := jid:literal_jid()}]) -> boolean().
check_users(Expected, ActualUsers) ->
    ActualJIDs = [JID || #{<<"user">> := JID} <- ActualUsers],
    ?assertEqual(lists:sort(Expected), lists:sort(ActualJIDs)).
