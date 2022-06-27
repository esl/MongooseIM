-module(graphql_session_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [execute/3, execute_auth/2, get_listener_port/1,
                         get_listener_config/1, get_ok_value/2, get_err_msg/1]).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user_session},
     {group, admin_session}].

groups() ->
    [{user_session, [parallel], user_session_handler()},
     {admin_session, [], admin_session_handler()}].

user_session_handler() ->
    [user_list_resources,
     user_count_resources,
     user_sessions_info].

admin_session_handler() ->
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

init_per_suite(Config) ->
    Config2 = escalus:init_per_suite(Config),
    dynamic_modules:save_modules(domain_helper:host_type(), Config2).

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(admin_session, Config) ->
    Config1 = escalus:create_users(Config, escalus:get_users([alice, alice_bis, bob])),
    graphql_helper:init_admin_handler(Config1);
init_per_group(user_session, Config) ->
    [{schema_endpoint, user} | Config].

end_per_group(admin_session, Config) ->
    escalus_fresh:clean(),
    escalus:delete_users(Config, escalus:get_users([alice, alice_bis, bob]));
end_per_group(user_session, _Config) ->
    escalus_fresh:clean().

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%% Test cases

user_list_resources(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 2}], fun user_list_resources_story/3).

user_list_resources_story(Config, Alice, Alice2) ->
    Ep = ?config(schema_endpoint, Config),
    Creds = graphql_helper:make_creds(Alice),

    Query = <<"query{ session { listResources } }">>,
    Result = execute(Ep, #{query => Query}, Creds),

    Path = [data, session, listResources],
    ExpectedRes = [escalus_client:resource(Alice), escalus_client:resource(Alice2)],
    ?assertMatch(ExpectedRes, lists:sort(get_ok_value(Path, Result))).

user_count_resources(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 3}], fun user_count_resources_story/4).

user_count_resources_story(Config, Alice, _Alice2, _Alice3) ->
    Ep = ?config(schema_endpoint, Config),
    Creds = graphql_helper:make_creds(Alice),

    Query = <<"query{ session { countResources } }">>,
    Result = execute(Ep, #{query => Query}, Creds),

    Path = [data, session, countResources],
    ?assertEqual(3, get_ok_value(Path, Result)).

user_sessions_info(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_sessions_info_story/2).

user_sessions_info_story(Config, Alice) ->
    Ep = ?config(schema_endpoint, Config),
    Creds = graphql_helper:make_creds(Alice),

    Query = <<"query{ session { listSessions { user } } }">>,
    Result = execute(Ep, #{query => Query}, Creds),
    ExpectedUser = escalus_utils:jid_to_lower(escalus_client:full_jid(Alice)),

    Path = [data, session, listSessions],
    ?assertMatch([#{<<"user">> := ExpectedUser}], get_ok_value(Path, Result)).

admin_list_sessions(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {alice_bis, 1}, {bob, 1}],
                                    fun admin_list_sessions_story/4).

admin_list_sessions_story(Config, _Alice, AliceB, _Bob) ->
    BisDomain = escalus_client:server(AliceB),
    Path = [data, session, listSessions],
    % List all sessions
    Res = execute_auth(list_sessions_body(null), Config),
    Sessions = get_ok_value(Path, Res),
    ?assertEqual(3, length(Sessions)),
    % List sessions for a domain
    Res2 = execute_auth(list_sessions_body(BisDomain), Config),
    Sessions2 = get_ok_value(Path, Res2),
    ?assertEqual(1, length(Sessions2)).

admin_count_sessions(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {alice_bis, 1}, {bob, 1}],
                                    fun admin_count_sessions_story/4).

admin_count_sessions_story(Config, _Alice, AliceB, _Bob) ->
    BisDomain = escalus_client:server(AliceB),
    Path = [data, session, countSessions],
    % Count all sessions
    Res = execute_auth(count_sessions_body(null), Config),
    Number = get_ok_value(Path, Res),
    ?assertEqual(3, Number),
    % Count sessions for a domain
    Res2 = execute_auth(count_sessions_body(BisDomain), Config),
    Number2 = get_ok_value(Path, Res2),
    ?assertEqual(1, Number2).

admin_list_user_sessions(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 2}, {bob, 1}],
                                    fun admin_list_user_sessions_story/4).

admin_list_user_sessions_story(Config, Alice, Alice2, _Bob) ->
    S1JID = escalus_client:full_jid(Alice),
    S2JID = escalus_client:full_jid(Alice2),
    Path = [data, session, listUserSessions],
    Res = execute_auth(list_user_sessions_body(S1JID), Config),
    ExpectedRes = lists:map(fun escalus_utils:jid_to_lower/1, [S1JID, S2JID]),
    Sessions = get_ok_value(Path, Res),
    ?assertEqual(2, length(Sessions)),
    ?assert(users_match(ExpectedRes, Sessions)).

admin_count_user_resources(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 3}], fun admin_count_user_resources_story/4).

admin_count_user_resources_story(Config, Alice, _Alice2, _Alice3) ->
    Path = [data, session, countUserResources],
    JID = escalus_client:full_jid(Alice),
    Res = execute_auth(count_user_resources_body(JID), Config),
    ?assertEqual(3, get_ok_value(Path, Res)).

admin_get_user_resource(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 3}], fun admin_get_user_resource_story/4).

admin_get_user_resource_story(Config, Alice, Alice2, _Alice3) ->
    Path = [data, session, getUserResource],
    JID = escalus_client:short_jid(Alice),
    % Provide a correct resource number
    Res = execute_auth(get_user_resource_body(JID, 2), Config),
    ?assertEqual(escalus_client:resource(Alice2), get_ok_value(Path, Res)),
    % Provide a wrong resource number
    Res2 = execute_auth(get_user_resource_body(JID, 4), Config),
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
    Res = execute_auth(count_users_with_status_body(null, AwayStatus), Config),
    ?assertEqual(2, get_ok_value(Path, Res)),
    % Count users with away status for a domain
    Res2 = execute_auth(count_users_with_status_body(domain_helper:domain(), AwayStatus), Config),
    ?assertEqual(1, get_ok_value(Path, Res2)),
    % Count users with dnd status globally
    escalus_client:send(AliceB, DndPresence),
    Res3 = execute_auth(count_users_with_status_body(null, DndStatus), Config),
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
    Res = execute_auth(list_users_with_status_body(null, AwayStatus), Config),
    StatusUsers = get_ok_value(Path, Res),
    ?assertEqual(2, length(StatusUsers)),
    ?assert(users_match([AliceJID, AliceBJID], StatusUsers)),
    % List users with away status for a domain
    Res2 = execute_auth(list_users_with_status_body(domain_helper:domain(), AwayStatus), Config),
    StatusUsers2 = get_ok_value(Path, Res2),
    ?assertEqual(1, length(StatusUsers2)),
    ?assert(users_match([AliceJID], StatusUsers2)),
    % List users with dnd status globally
    escalus_client:send(AliceB, DndPresence),
    Res3 = execute_auth(list_users_with_status_body(null, DndStatus), Config),
    StatusUsers3 = get_ok_value(Path, Res3),
    ?assertEqual(1, length(StatusUsers3)),
    ?assert(users_match([AliceBJID], StatusUsers3)).

admin_kick_session(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 2}], fun admin_kick_session_story/3).

admin_kick_session_story(Config, Alice1, Alice2) ->
    JIDA1 = escalus_client:full_jid(Alice1),
    JIDA2 = escalus_client:full_jid(Alice2),
    Reason = <<"Test kick">>,
    Path = [data, session, kickUser, message],
    Res = execute_auth(kick_session_body(JIDA1, Reason), Config),
    % Kick an active session
    ?assertNotEqual(nomatch, binary:match(get_ok_value(Path, Res), <<"kicked">>)),
    ?assertEqual(JIDA1, get_ok_value([data, session, kickUser, jid], Res)),
    % Try to kick an offline session
    Res2 = execute_auth(kick_session_body(JIDA1, Reason), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"No active session">>)),
    % Try to kick a session with JID without a resource
    Res3 = execute_auth(kick_session_body(escalus_client:short_jid(Alice2), Reason), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"No active session">>)),
    % Kick another active session
    Res4 = execute_auth(kick_session_body(JIDA2, Reason), Config),
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
    Res = execute_auth(set_presence_body(ShortJID, Type, Show, Status, Priority), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"resource is empty">>)),
    % Send full JID
    Path = [data, session, setPresence, message],
    Res2 = execute_auth(set_presence_body(JID, Type, Show, Status, Priority), Config),
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
    Res2 = execute_auth(set_presence_body(JID, Type, Show, null, null), Config),
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
    Res2 = execute_auth(set_presence_body(JID, Type, null, Status, null), Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(Path, Res2), <<"set successfully">>)),
    Presence = escalus:wait_for_stanza(Alice2),
    escalus:assert(is_presence_with_type, [<<"unavailable">>], Presence),
    escalus:assert(is_presence_with_status, [Status], Presence).

%% Helpers

-spec users_match([jid:literal_jid()], [#{user := jid:literal_jid()}]) -> boolean().
users_match(Expected, Actual) ->
    lists:all(fun(#{<<"user">> := JID}) -> lists:member(JID, Expected) end, Actual).

%% Request bodies

list_sessions_body(Domain) ->
    Query = <<"query Q1($domain: String)
              { session { listSessions(domain: $domain) { user } } }">>,
    OpName = <<"Q1">>,
    Vars = #{<<"domain">> => Domain},
    #{query => Query, operationName => OpName, variables => Vars}.

count_sessions_body(Domain) ->
    Query = <<"query Q1($domain: String)
              { session { countSessions(domain: $domain) } }">>,
    OpName = <<"Q1">>,
    Vars = #{<<"domain">> => Domain},
    #{query => Query, operationName => OpName, variables => Vars}.

list_user_sessions_body(JID) ->
    Query = <<"query Q1($user: JID!)
              { session { listUserSessions(user: $user) { user } } }">>,
    OpName = <<"Q1">>,
    Vars = #{<<"user">> => JID},
    #{query => Query, operationName => OpName, variables => Vars}.

count_user_resources_body(JID) ->
    Query = <<"query Q1($user: JID!)
              { session { countUserResources(user: $user) } }">>,
    OpName = <<"Q1">>,
    Vars = #{<<"user">> => JID},
    #{query => Query, operationName => OpName, variables => Vars}.

get_user_resource_body(JID, Number) ->
    Query = <<"query Q1($user: JID!, $number: Int!)
              { session { getUserResource(user: $user, number: $number) } }">>,
    OpName = <<"Q1">>,
    Vars = #{<<"user">> => JID, <<"number">> => Number},
    #{query => Query, operationName => OpName, variables => Vars}.

list_users_with_status_body(Domain, Status) ->
    Query = <<"query Q1($domain: String, $status: String!)
              { session { listUsersWithStatus(domain: $domain, status: $status) { user } } }">>,
    OpName = <<"Q1">>,
    Vars = #{<<"domain">> => Domain, <<"status">> => Status},
    #{query => Query, operationName => OpName, variables => Vars}.

count_users_with_status_body(Domain, Status) ->
    Query = <<"query Q1($domain: String, $status: String!)
              { session { countUsersWithStatus(domain: $domain, status: $status) } }">>,
    OpName = <<"Q1">>,
    Vars = #{<<"domain">> => Domain, <<"status">> => Status},
    #{query => Query, operationName => OpName, variables => Vars}.

kick_session_body(JID, Reason) ->
    Query = <<"mutation M1($user: JID!, $reason: String!)
              { session { kickUser(user: $user, reason: $reason) { jid message }} }">>,
    OpName = <<"M1">>,
    Vars = #{<<"user">> => JID, <<"reason">> => Reason},
    #{query => Query, operationName => OpName, variables => Vars}.

set_presence_body(JID, Type, Show, Status, Priority) ->
    Query = <<"mutation M1($user: JID!, $type: PresenceType!, $show: PresenceShow, $status: String, $priority: Int)
              { session { setPresence(user: $user, type: $type, show: $show, status: $status, priority: $priority)
              { message jid } } }">>,
    OpName = <<"M1">>,
    Vars = #{<<"user">> => JID, <<"type">> => Type, <<"show">> => Show, <<"status">> => Status, <<"priority">> => Priority},
    #{query => Query, operationName => OpName, variables => Vars}.
