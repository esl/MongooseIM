-module(graphql_muc_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [execute_user/3, execute_auth/2, get_ok_value/2, get_err_msg/1,
                         user_to_bin/1, user_to_full_bin/1, user_to_jid/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user_muc},
     {group, admin_muc}].

groups() ->
    [{user_muc, [parallel], user_muc_handler()},
     {admin_muc, [parallel], admin_muc_handler()}].

user_muc_handler() ->
    [user_create_and_delete_room,
     user_try_delete_nonexistent_room,
     user_try_delete_room_by_not_owner,
     user_try_create_instant_room_with_nonexistent_domain,
     user_list_rooms,
     user_list_room_users,
     user_list_room_users_without_anonymous_mode,
     user_try_list_room_users_without_permission,
     user_try_list_nonexistent_room_users,
     user_change_room_config,
     user_try_change_nonexistent_room_config,
     user_get_room_config,
     user_try_get_nonexistent_room_config,
     user_invite_user,
     user_kick_user,
     user_send_message_to_room,
     user_send_message_to_room_with_specified_res,
     user_send_private_message,
     user_without_session_send_message_to_room,
     user_get_room_messages,
     user_try_get_nonexistent_room_messages,
     user_try_get_room_messages_without_permission,
     user_owner_set_user_affiliation,
     user_admin_set_user_affiliation,
     user_member_set_user_affiliation,
     user_try_set_nonexistent_room_affiliation,
     user_moderator_set_user_role,
     user_participant_set_user_role,
     user_try_set_nonexistent_room_role,
     user_can_enter_room,
     user_can_enter_room_with_password,
     user_can_exit_room,
     user_list_room_affiliation,
     user_try_list_room_affiliation_without_permission,
     user_try_list_nonexistent_room_affiliations
    ].

admin_muc_handler() ->
    [admin_create_and_delete_room,
     admin_try_create_instant_room_with_nonexistent_domain,
     admin_try_create_instant_room_with_nonexistent_user,
     admin_try_delete_nonexistent_room,
     admin_try_delete_room_with_nonexistent_domain,
     admin_list_rooms,
     admin_list_room_users,
     admin_try_list_users_from_nonexistent_room,
     admin_change_room_config,
     admin_try_change_nonexistent_room_config,
     admin_get_room_config,
     admin_try_get_nonexistent_room_config,
     admin_invite_user,
     admin_invite_user_with_password,
     admin_try_invite_user_to_nonexistent_room,
     admin_kick_user,
     admin_send_message_to_room,
     admin_send_private_message,
     admin_get_room_messages,
     admin_try_get_nonexistent_room_messages,
     admin_set_user_affiliation,
     admin_try_set_nonexistent_room_user_affiliation,
     admin_set_user_role,
     admin_try_set_nonexistent_room_user_role,
     admin_make_user_enter_room,
     admin_make_user_enter_room_with_password,
     admin_make_user_exit_room,
     admin_list_room_affiliation,
     admin_try_list_nonexistent_room_affiliation
    ].

init_per_suite(Config) ->
    HostType = domain_helper:host_type(),
    Config2 = escalus:init_per_suite(Config),
    Config3 = dynamic_modules:save_modules(HostType, Config2),
    Config4 = rest_helper:maybe_enable_mam(mam_helper:backend(), HostType, Config3),
    dynamic_modules:restart(HostType, mod_disco,
                            config_parser_helper:default_mod_config(mod_disco)),
    muc_helper:load_muc(),
    mongoose_helper:ensure_muc_clean(),
    Config4.

end_per_suite(Config) ->
    escalus_fresh:clean(),
    mongoose_helper:ensure_muc_clean(),
    muc_helper:unload_muc(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(admin_muc, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(user_muc, Config) ->
    [{schema_endpoint, user} | Config].

end_per_group(_GN, Config) ->
    Config.

init_per_testcase(TC, Config) ->
    rest_helper:maybe_skip_mam_test_cases(TC, [user_get_room_messages,
                                               admin_get_room_messages], Config).

end_per_testcase(TC, Config) ->
    escalus:end_per_testcase(TC, Config).

-define(CREATE_INSTANT_ROOM_PATH, [data, muc, createInstantRoom]).
-define(LIST_ROOMS_PATH, [data, muc, listRooms]).
-define(INVITE_USER_PATH, [data, muc, inviteUser]).
-define(KICK_USER_PATH, [data, muc, kickUser]).
-define(DELETE_ROOM_PATH, [data, muc, deleteRoom]).
-define(SEND_MESSAGE_PATH, [data, muc, sendMessageToRoom]).
-define(SEND_PRIV_MESG_PATH, [data, muc, sendPrivateMessage]).
-define(GET_MESSAGES_PATH, [data, muc, getRoomMessages]).
-define(LIST_ROOM_USERS_PATH, [data, muc, listRoomUsers]).
-define(LIST_ROOM_AFFILIATIONS_PATH, [data, muc, listRoomAffiliations]).
-define(CHANGE_ROOM_CONFIG_PATH, [data, muc, changeRoomConfiguration]).
-define(GET_ROOM_CONFIG_PATH, [data, muc, getRoomConfig]).
-define(SET_AFFILIATION_PATH, [data, muc, setUserAffiliation]).
-define(SET_ROLE_PATH, [data, muc, setUserRole]).
-define(ENTER_ROOM_PATH, [data, muc, enterRoom]).
-define(EXIT_ROOM_PATH, [data, muc, exitRoom]).

-define(NONEXISTENT_ROOM, <<"room@room">>).
-define(NONEXISTENT_ROOM2, <<"room@", (muc_helper:muc_host())/binary>>).
-define(PASSWORD, <<"pa5sw0rd">>).

admin_list_rooms(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun admin_list_rooms_story/3).

admin_list_rooms_story(Config, Alice, Bob) ->
    AliceJID = jid:from_binary(escalus_client:short_jid(Alice)),
    BobJID = jid:from_binary(escalus_client:short_jid(Bob)),
    AliceRoom = rand_name(),
    BobRoom = rand_name(),
    muc_helper:create_instant_room(AliceRoom, AliceJID, <<"Ali">>, []),
    muc_helper:create_instant_room(BobRoom, BobJID, <<"Bob">>, [{public_list, false}]),
    Res = execute_auth(admin_list_rooms_body(muc_helper:muc_host(), Alice, null, null), Config),
    #{<<"rooms">> := Rooms } = get_ok_value(?LIST_ROOMS_PATH, Res),
    ?assert(contain_room(AliceRoom, Rooms)),
    ?assert(contain_room(BobRoom, Rooms)).

admin_create_and_delete_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_create_and_delete_room_story/2).

admin_create_and_delete_room_story(Config, Alice) ->
    Name = <<"first-alice-room">>,
    MUCServer = muc_helper:muc_host(),
    RoomJID = jid:make_bare(Name, MUCServer),
    % Create instant room
    Res = execute_auth(admin_create_instant_room_body(MUCServer, Name, Alice, <<"Ali">>), Config),
    ?assertMatch(#{<<"title">> := Name, <<"private">> := false, <<"usersNumber">> := 0},
                 get_ok_value(?CREATE_INSTANT_ROOM_PATH, Res)),
    Res2 = execute_auth(admin_list_rooms_body(MUCServer, Alice, null, null), Config),
    ?assert(contain_room(Name, get_ok_value(?LIST_ROOMS_PATH, Res2))),
    % Delete room
    Res3 = execute_auth(delete_room_body(RoomJID, null), Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?DELETE_ROOM_PATH, Res3),
                                          <<"successfully">>)),
    Res4 = execute_auth(admin_list_rooms_body(MUCServer, Alice, null, null), Config),
    ?assertNot(contain_room(Name, get_ok_value(?LIST_ROOMS_PATH, Res4))).

admin_try_create_instant_room_with_nonexistent_domain(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_try_create_instant_room_with_nonexistent_domain_story/2).

admin_try_create_instant_room_with_nonexistent_domain_story(Config, Alice) ->
    Res = execute_auth(admin_create_instant_room_body(<<"unknown">>, rand_name(), Alice, <<"Ali">>),
                       Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_try_create_instant_room_with_nonexistent_user(Config) ->
    Name = rand_name(),
    MUCServer = muc_helper:muc_host(),
    JID = <<(rand_name())/binary, "@localhost">>,
    Res = execute_auth(admin_create_instant_room_body(MUCServer, Name, JID, <<"Ali">>), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_try_delete_nonexistent_room(Config) ->
    RoomJID = jid:make_bare(<<"unknown">>, muc_helper:muc_host()),
    Res = execute_auth(delete_room_body(RoomJID, null), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"non-existent">>)).

admin_try_delete_room_with_nonexistent_domain(Config) ->
    RoomJID = jid:make_bare(<<"unknown">>, <<"unknown">>),
    Res = execute_auth(delete_room_body(RoomJID, null), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"non-existent">>)).

admin_invite_user(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}], fun admin_invite_user_story/3).

admin_invite_user_story(Config, Alice, Bob) ->
    RoomJIDBin = ?config(room_jid, Config),
    RoomJID = jid:from_binary(RoomJIDBin),
    Res = execute_auth(admin_invite_user_body(RoomJID, Alice, Bob, null), Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?INVITE_USER_PATH, Res),
                                          <<"successfully">>)),
    Stanza = escalus:wait_for_stanza(Bob),
    escalus:assert(is_message, Stanza),
    ?assertEqual(RoomJIDBin,
                 exml_query:path(Stanza, [{element, <<"x">>}, {attr, <<"jid">>}])),
    ?assertEqual(undefined, exml_query:path(Stanza, [{element, <<"x">>}, {attr, <<"password">>}])).

admin_invite_user_with_password(Config) ->
    muc_helper:story_with_room(Config, [{password_protected, true}, {password, ?PASSWORD}],
                               [{alice, 1}, {bob, 1}], fun admin_invite_user_with_password/3).

admin_invite_user_with_password(Config, Alice, Bob) ->
    RoomJIDBin = ?config(room_jid, Config),
    RoomJID = jid:from_binary(RoomJIDBin),
    Res = execute_auth(admin_invite_user_body(RoomJID, Alice, Bob, null), Config),
    assert_success(?INVITE_USER_PATH, Res),
    Stanza = escalus:wait_for_stanza(Bob),
    escalus:assert(is_message, Stanza),
    ?assertEqual(RoomJIDBin,
                 exml_query:path(Stanza, [{element, <<"x">>}, {attr, <<"jid">>}])),
    ?assertEqual(?PASSWORD, exml_query:path(Stanza, [{element, <<"x">>}, {attr, <<"password">>}])).

admin_try_invite_user_to_nonexistent_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_try_invite_user_to_nonexistent_room_story/3).

admin_try_invite_user_to_nonexistent_room_story(Config, Alice, Bob) ->
    Res = execute_auth(admin_invite_user_body(?NONEXISTENT_ROOM, Alice, Bob, null), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_kick_user(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}], fun admin_kick_user_story/3).

admin_kick_user_story(Config, Alice, Bob) ->
    RoomJIDBin = ?config(room_jid, Config),
    RoomJID = jid:from_binary(RoomJIDBin),
    BobNick = <<"Bobek">>,
    Reason = <<"You are too laud">>,
    enter_room(RoomJID, Alice, <<"ali">>),
    enter_room(RoomJID, Bob, BobNick),
    Res = execute_auth(kick_user_body(RoomJID, BobNick, Reason), Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?KICK_USER_PATH, Res),
                                          <<"successfully">>)),
    escalus:wait_for_stanzas(Bob, 2),
    KickStanza = escalus:wait_for_stanza(Bob),
    escalus:assert(is_presence_with_type, [<<"unavailable">>], KickStanza),
    ?assertEqual(Reason,
                 exml_query:path(KickStanza, [{element, <<"x">>}, {element, <<"item">>},
                                              {element, <<"reason">>}, cdata])).

admin_send_message_to_room(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun admin_send_message_to_room_story/3).

admin_send_message_to_room_story(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Message = <<"Hello All!">>,
    BobNick = <<"Bobek">>,
    enter_room(RoomJID, Bob, BobNick),
    escalus:wait_for_stanza(Bob),
    % Send message
    Res = execute_auth(admin_send_message_to_room_body(RoomJID, Bob, Message), Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SEND_MESSAGE_PATH, Res),
                                          <<"successfully">>)),
    assert_is_message_correct(RoomJID, BobNick, <<"groupchat">>, Message,
                              escalus:wait_for_stanza(Bob)).

admin_send_private_message(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun admin_send_private_message/3).

admin_send_private_message(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Message = <<"Hello Bob!">>,
    BobNick = <<"Bobek">>,
    AliceNick = <<"Ali">>,
    enter_room(RoomJID, Alice, AliceNick),
    enter_room(RoomJID, Bob, BobNick),
    escalus:wait_for_stanzas(Bob, 2),
    % Send message
    Res = execute_auth(admin_send_private_message_body(RoomJID, Alice, BobNick, Message), Config),
    assert_success(?SEND_PRIV_MESG_PATH, Res),
    assert_is_message_correct(RoomJID, AliceNick, <<"chat">>, Message,
                              escalus:wait_for_stanza(Bob)).

admin_get_room_config(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}], fun admin_get_room_config_story/2).

admin_get_room_config_story(Config, _Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Res = execute_auth(get_room_config_body(RoomJID), Config),
    assert_default_room_config(Res).

admin_try_get_nonexistent_room_config(Config) ->
    Res = execute_auth(get_room_config_body(?NONEXISTENT_ROOM), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_change_room_config(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}], fun admin_change_room_config_story/2).

admin_change_room_config_story(Config, _Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Title = <<"aloes">>,
    Description = <<"The chat about aloes">>,
    Public = false,
    RoomConfig = #{title => Title, description => Description, public => Public},
    Res = execute_auth(change_room_config_body(RoomJID, RoomConfig), Config),
    ?assertMatch(#{<<"title">> := Title,
                   <<"description">> := Description,
                   <<"public">> := Public}, get_ok_value(?CHANGE_ROOM_CONFIG_PATH, Res)).

admin_try_change_nonexistent_room_config(Config) ->
    RoomConfig = #{title => <<"NewTitle">>},
    Res = execute_auth(change_room_config_body(?NONEXISTENT_ROOM, RoomConfig), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_list_room_users(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun admin_list_room_users_story/3).

admin_list_room_users_story(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Bobek">>,
    AliceNick = <<"Ali">>,
    enter_room(RoomJID, Bob, BobNick),
    enter_room(RoomJID, Alice, AliceNick),
    Res = execute_auth(list_room_users_body(RoomJID), Config),
    ExpectedUsers = [{escalus_client:full_jid(Bob), BobNick, <<"PARTICIPANT">>},
                     {escalus_client:full_jid(Alice), AliceNick, <<"MODERATOR">>}],
    assert_room_users(ExpectedUsers, get_ok_value(?LIST_ROOM_USERS_PATH, Res)).

admin_try_list_users_from_nonexistent_room(Config) ->
    Res = execute_auth(list_room_users_body(?NONEXISTENT_ROOM), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_get_room_messages(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun admin_get_room_messages_story/3).

admin_get_room_messages_story(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    enter_room(RoomJID, Bob, <<"Bobek">>),
    enter_room(RoomJID, Alice, <<"Ali">>),
    escalus:wait_for_stanzas(Bob, 2),
    execute_auth(admin_send_message_to_room_body(RoomJID, Bob, <<"Hi!">>), Config),
    escalus:wait_for_stanzas(Bob, 1),
    mam_helper:maybe_wait_for_archive(Config),
    Res = execute_auth(get_room_messages_body(RoomJID, 50, null), Config),
    #{<<"stanzas">> := [#{<<"stanza">> := StanzaXML}], <<"limit">> := 50} =
        get_ok_value(?GET_MESSAGES_PATH, Res),
    ?assertMatch({ok, #xmlel{name = <<"message">>}}, exml:parse(StanzaXML)).

admin_try_get_nonexistent_room_messages(Config) ->
    Res = execute_auth(get_room_messages_body(?NONEXISTENT_ROOM, null, null), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).


admin_set_user_affiliation(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun admin_set_user_affiliation/3).

admin_set_user_affiliation(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    % Grant member affiliation
    Res = execute_auth(set_user_affiliation_body(RoomJID, Bob, member), Config),
    assert_success(?SET_AFFILIATION_PATH, Res),
    assert_user_affiliation(RoomJID, Bob, member),
    % Grant admin affiliation
    Res1 = execute_auth(set_user_affiliation_body(RoomJID, Bob, admin), Config),
    assert_success(?SET_AFFILIATION_PATH, Res1),
    assert_user_affiliation(RoomJID, Bob, admin),
    % Grant owner affiliation
    Res2 = execute_auth(set_user_affiliation_body(RoomJID, Bob, owner), Config),
    assert_success(?SET_AFFILIATION_PATH, Res2),
    assert_user_affiliation(RoomJID, Bob, owner),
    % Revoke affiliation
    Res3 = execute_auth(set_user_affiliation_body(RoomJID, Bob, none), Config),
    assert_success(?SET_AFFILIATION_PATH, Res3),
    assert_user_affiliation(RoomJID, Bob, none),
    % Ban user
    Res4 = execute_auth(set_user_affiliation_body(RoomJID, Bob, outcast), Config),
    assert_success(?SET_AFFILIATION_PATH, Res4),
    assert_user_affiliation(RoomJID, Bob, outcast).


admin_try_set_nonexistent_room_user_affiliation(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_try_set_nonexistent_room_user_affiliation/2).

admin_try_set_nonexistent_room_user_affiliation(Config, Alice) ->
    Res = execute_auth(set_user_affiliation_body(?NONEXISTENT_ROOM, Alice, admin), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_set_user_role(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}], fun admin_set_user_role/3).

admin_set_user_role(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Boobek">>,
    enter_room(RoomJID, Alice, escalus_client:username(Alice)),
    enter_room(RoomJID, Bob, BobNick),
    % Change from participant to visitor
    Res = execute_auth(set_user_role_body(RoomJID, BobNick, visitor), Config),
    assert_success(?SET_ROLE_PATH, Res),
    assert_user_role(RoomJID, Bob, visitor),
    % Change from visitor to participant
    Res1 = execute_auth(set_user_role_body(RoomJID, BobNick, participant), Config),
    assert_success(?SET_ROLE_PATH, Res1),
    assert_user_role(RoomJID, Bob, participant),
    % Change from participant to moderator
    Res2 = execute_auth(set_user_role_body(RoomJID, BobNick, moderator), Config),
    assert_success(?SET_ROLE_PATH, Res2),
    assert_user_role(RoomJID, Bob, moderator).

admin_try_set_nonexistent_room_user_role(Config) ->
    Res = execute_auth(set_user_role_body(?NONEXISTENT_ROOM, <<"Alice">>, moderator), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_make_user_enter_room(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}], fun admin_make_user_enter_room/2).

admin_make_user_enter_room(Config, Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Nick = <<"ali">>,
    JID = jid:from_binary(escalus_client:full_jid(Alice)),
    % Alice enter room with password
    Res = execute_auth(admin_enter_room_body(RoomJID, Alice, Nick, null), Config),
    assert_success(?ENTER_ROOM_PATH, Res),
    ?assertMatch([#{nick := Nick, jid := JID}], get_room_users(RoomJID)).

admin_make_user_enter_room_with_password(Config) ->
    muc_helper:story_with_room(Config, [{password_protected, true}, {password, ?PASSWORD}],
                               [{alice, 1}, {bob, 1}],
                               fun admin_make_user_enter_room_with_password/3).

admin_make_user_enter_room_with_password(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Nick = <<"ali">>,
    JID = jid:from_binary(escalus_client:full_jid(Alice)),
    % Alice enter room with password
    Res = execute_auth(admin_enter_room_body(RoomJID, Alice, Nick, ?PASSWORD), Config),
    assert_success(?ENTER_ROOM_PATH, Res),
    ?assertMatch([#{nick := Nick, jid := JID}], get_room_users(RoomJID)),
    % Bob try enter room without password 
    Res1 = execute_auth(admin_enter_room_body(RoomJID, Bob, <<"Bobek">>, null), Config),
    assert_success(?ENTER_ROOM_PATH, Res1),
    ?assertMatch([_], get_room_users(RoomJID)),
    % Bob enter room with password
    Res2 = execute_auth(admin_enter_room_body(RoomJID, Bob, <<"Bobek">>, ?PASSWORD), Config),
    assert_success(?ENTER_ROOM_PATH, Res2),
    ?assertMatch([_, _], get_room_users(RoomJID)).

admin_make_user_exit_room(Config) ->
    muc_helper:story_with_room(Config, [{persistent, true}], [{alice, 1}],
                               fun admin_make_user_exit_room/2).

admin_make_user_exit_room(Config, Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Nick = <<"ali">>,
    enter_room(RoomJID, Alice, Nick),
    ?assertMatch([_], get_room_users(RoomJID)),
    Res = execute_auth(admin_exit_room_body(RoomJID, Alice, Nick), Config),
    assert_success(?EXIT_ROOM_PATH, Res),
    ?assertMatch([], get_room_users(RoomJID)).

admin_list_room_affiliation(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun admin_list_room_affiliation/3).

admin_list_room_affiliation(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Bobek">>,
    AliceNick = <<"Ali">>,
    enter_room(RoomJID, Bob, BobNick),
    enter_room(RoomJID, Alice, AliceNick),
    AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
    BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
    % List all owners
    Res = execute_auth(list_room_affiliations_body(RoomJID, owner), Config),
    ?assertMatch([#{<<"jid">> := AliceJID, <<"affiliation">> := <<"OWNER">>}],
                 get_ok_value(?LIST_ROOM_AFFILIATIONS_PATH, Res)),
    % List all members
    execute_auth(set_user_affiliation_body(RoomJID, Bob, member), Config),
    Res1 = execute_auth(list_room_affiliations_body(RoomJID, member), Config),
    ?assertMatch([#{<<"jid">> := BobJID, <<"affiliation">> := <<"MEMBER">>}],
                 get_ok_value(?LIST_ROOM_AFFILIATIONS_PATH, Res1)),
    % List all
    Res2 = execute_auth(list_room_affiliations_body(RoomJID, null), Config),
    ?assertMatch([_, _], get_ok_value(?LIST_ROOM_AFFILIATIONS_PATH, Res2)).

admin_try_list_nonexistent_room_affiliation(Config) ->
    Res = execute_auth(list_room_affiliations_body(?NONEXISTENT_ROOM, null), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

%% User test cases

user_list_rooms(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun user_list_rooms_story/3).

user_list_rooms_story(Config, Alice, Bob) ->
    AliceJID = jid:from_binary(escalus_client:short_jid(Alice)),
    BobJID = jid:from_binary(escalus_client:short_jid(Bob)),
    AliceRoom = rand_name(),
    BobRoom = rand_name(),
    muc_helper:create_instant_room(AliceRoom, AliceJID, <<"Ali">>, []),
    muc_helper:create_instant_room(BobRoom, BobJID, <<"Bob">>, []),

    Res = execute_user(user_list_rooms_body(muc_helper:muc_host(), null, null), Alice, Config),
    #{<<"rooms">> := Rooms } = get_ok_value(?LIST_ROOMS_PATH, Res),
    ?assert(contain_room(AliceRoom, Rooms)),
    ?assert(contain_room(BobRoom, Rooms)).

user_create_and_delete_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_create_and_delete_room_story/2).

user_create_and_delete_room_story(Config, Alice) ->
    Name = rand_name(),
    MUCServer = muc_helper:muc_host(),
    RoomJID = jid:make_bare(Name, MUCServer),
    % Create instant room
    Res = execute_user(user_create_instant_room_body(MUCServer, Name, <<"Ali">>), Alice, Config),
    ?assertMatch(#{<<"title">> := Name, <<"private">> := false, <<"usersNumber">> := 0},
                 get_ok_value(?CREATE_INSTANT_ROOM_PATH, Res)),
    Res2 = execute_user(user_list_rooms_body(MUCServer, null, null), Alice, Config),
    ?assert(contain_room(Name, get_ok_value(?LIST_ROOMS_PATH, Res2))),
    % Delete room
    Res3 = execute_user(delete_room_body(RoomJID, null), Alice, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?DELETE_ROOM_PATH, Res3),
                                          <<"successfully">>)),
    Res4 = execute_user(user_list_rooms_body(MUCServer, null, null), Alice, Config),
    ?assertNot(contain_room(Name, get_ok_value(?LIST_ROOMS_PATH, Res4))).

user_try_create_instant_room_with_nonexistent_domain(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_create_instant_room_with_nonexistent_domain_story/2).

user_try_create_instant_room_with_nonexistent_domain_story(Config, Alice) ->
    Res = execute_user(user_create_instant_room_body(<<"unknown">>, rand_name(), <<"Ali">>),
                       Alice, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

user_try_delete_nonexistent_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_delete_nonexistent_room_story/2).

user_try_delete_nonexistent_room_story(Config, Alice) ->
    RoomJID = jid:make_bare(<<"unknown">>, muc_helper:muc_host()),
    Res = execute_user(delete_room_body(RoomJID, null), Alice, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"non-existent">>)).

user_try_delete_room_by_not_owner(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_try_delete_room_by_not_owner_story/3).

user_try_delete_room_by_not_owner_story(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Res = execute_user(delete_room_body(RoomJID, null), Bob, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not have permission">>)).

user_invite_user(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}], fun user_invite_user_story/3).

user_invite_user_story(Config, Alice, Bob) ->
    RoomJIDBin = ?config(room_jid, Config),
    RoomJID = jid:from_binary(RoomJIDBin),
    Res = execute_user(user_invite_user_body(RoomJID, Bob, null), Alice, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?INVITE_USER_PATH, Res),
                                          <<"successfully">>)),
    Stanza = escalus:wait_for_stanza(Bob),
    escalus:assert(is_message, Stanza),
    ?assertEqual(RoomJIDBin,
                 exml_query:path(Stanza, [{element, <<"x">>}, {attr, <<"jid">>}])).

user_kick_user(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}], fun user_kick_user_story/3).

user_kick_user_story(Config, Alice, Bob) ->
    RoomJIDBin = ?config(room_jid, Config),
    RoomJID = jid:from_binary(RoomJIDBin),
    BobNick = <<"Bobek">>,
    Reason = <<"You are too loud">>,
    enter_room(RoomJID, Alice, <<"ali">>),
    enter_room(RoomJID, Bob, BobNick),
    Res = execute_user(kick_user_body(RoomJID, BobNick, Reason), Alice, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?KICK_USER_PATH, Res),
                                          <<"successfully">>)),
    escalus:wait_for_stanzas(Bob, 2),
    KickStanza = escalus:wait_for_stanza(Bob),
    escalus:assert(is_presence_with_type, [<<"unavailable">>], KickStanza),
    ?assertEqual(Reason,
                 exml_query:path(KickStanza, [{element, <<"x">>}, {element, <<"item">>},
                                              {element, <<"reason">>}, cdata])).

user_send_message_to_room(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_send_message_to_room_story/3).

user_send_message_to_room_story(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Message = <<"Hello All!">>,
    BobNick = <<"Bobek">>,
    enter_room(RoomJID, Bob, BobNick),
    escalus:wait_for_stanza(Bob),
    % Send message
    Res = execute_user(user_send_message_to_room_body(RoomJID, Message, null), Bob, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SEND_MESSAGE_PATH, Res),
                                          <<"successfully">>)),
    assert_is_message_correct(RoomJID, BobNick, <<"groupchat">>, Message,
                              escalus:wait_for_stanza(Bob)).

user_send_message_to_room_with_specified_res(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 2}],
                               fun user_send_message_to_room_with_specified_res_story/4).

user_send_message_to_room_with_specified_res_story(Config, _Alice, Bob, Bob2) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Message = <<"Hello All!">>,
    BobNick = <<"Bobek">>,
    enter_room(RoomJID, Bob2, BobNick),
    escalus:wait_for_stanza(Bob2),
    % Send message
    Res = execute_user(user_send_message_to_room_body(RoomJID, Message, <<"res2">>), Bob, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SEND_MESSAGE_PATH, Res),
                                          <<"successfully">>)),
    assert_is_message_correct(RoomJID, BobNick, <<"groupchat">>, Message,
                              escalus:wait_for_stanza(Bob2)).

user_send_private_message(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_send_private_message/3).

user_send_private_message(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Message = <<"Hello Bob!">>,
    BobNick = <<"Bobek">>,
    AliceNick = <<"Ali">>,
    enter_room(RoomJID, Bob, BobNick),
    enter_room(RoomJID, Alice, AliceNick),
    escalus:wait_for_stanzas(Bob, 2),
    % Send message
    Res = execute_user(user_send_private_message_body(RoomJID, Message, BobNick, null),
                       Alice, Config),
    assert_success(?SEND_PRIV_MESG_PATH, Res),
    assert_is_message_correct(RoomJID, AliceNick, <<"chat">>, Message,
                              escalus:wait_for_stanza(Bob)).

user_send_private_message_with_specified_res(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 2}, {bob, 1}],
                               fun user_send_private_message/3).

user_send_private_message_with_specified_res(Config, Alice, Alice2, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Message = <<"Hello Bob!">>,
    BobNick = <<"Bobek">>,
    AliceNick = <<"Ali">>,
    enter_room(RoomJID, Bob, BobNick),
    enter_room(RoomJID, Alice2, AliceNick),
    escalus:wait_for_stanzas(Bob, 2),
    % Send message
    Res = execute_user(user_send_private_message_body(RoomJID, Message, BobNick, <<"res2">>),
                       Alice, Config),
    assert_success(?SEND_PRIV_MESG_PATH, Res),
    assert_is_message_correct(RoomJID, AliceNick, <<"chat">>, Message,
                              escalus:wait_for_stanza(Bob)).

user_without_session_send_message_to_room(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}],
                               fun user_without_session_send_message_to_room_story/2).

user_without_session_send_message_to_room_story(Config, Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    JID = jid:from_binary(escalus_client:full_jid(Alice)),
    {exit, _} = rpc(mim(), ejabberd_c2s, terminate_session, [JID, <<"Kicked">>]),
    % Send message
    Res = execute_user(user_send_message_to_room_body(RoomJID, <<"Hello!">>, null), Alice, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not have any session">>)).

user_get_room_config(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_get_room_config_story/3).

user_get_room_config_story(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Res = execute_user(get_room_config_body(RoomJID), Alice, Config),
    assert_default_room_config(Res),
    % Not an owner tries to get room config
    Res2 = execute_user(get_room_config_body(RoomJID), Bob, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not have permission">>)).

user_try_get_nonexistent_room_config(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_get_nonexistent_room_config_story/2).

user_try_get_nonexistent_room_config_story(Config, Alice) ->
    Res = execute_user(get_room_config_body(?NONEXISTENT_ROOM), Alice, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

user_change_room_config(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_change_room_config_story/3).

user_change_room_config_story(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Title = <<"aloes">>,
    Description = <<"The chat about aloes">>,
    Public = false,
    RoomConfig = #{title => Title, description => Description, public => Public},
    Res = execute_user(change_room_config_body(RoomJID, RoomConfig), Alice, Config),
    ?assertMatch(#{<<"title">> := Title,
                   <<"description">> := Description,
                   <<"public">> := Public}, get_ok_value(?CHANGE_ROOM_CONFIG_PATH, Res)),
    % Not an owner tries to change the room config
    Res2 = execute_user(change_room_config_body(RoomJID, RoomConfig), Bob, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not have permission">>)).

user_try_change_nonexistent_room_config(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_change_nonexistent_room_config_story/2).

user_try_change_nonexistent_room_config_story(Config, Alice) ->
    RoomConfig = #{title => <<"NewTitle">>},
    Res = execute_user(change_room_config_body(?NONEXISTENT_ROOM, RoomConfig), Alice, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

user_list_room_users(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_list_room_users_story/3).

user_list_room_users_story(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Bobek">>,
    AliceNick = <<"Ali">>,
    enter_room(RoomJID, Bob, BobNick),
    enter_room(RoomJID, Alice, AliceNick),
    Res = execute_user(list_room_users_body(RoomJID), Alice, Config),
    ExpectedUsers = [{null, BobNick, <<"PARTICIPANT">>},
                     {null, AliceNick, <<"MODERATOR">>}],
    assert_room_users(ExpectedUsers, get_ok_value(?LIST_ROOM_USERS_PATH, Res)).

user_list_room_users_without_anonymous_mode(Config) ->
    muc_helper:story_with_room(Config, [{anonymous, false}], [{alice, 1}, {bob, 1}],
                               fun user_list_room_users_without_anonymous_mode_story/3).

user_list_room_users_without_anonymous_mode_story(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Bobek">>,
    AliceNick = <<"Ali">>,
    enter_room(RoomJID, Bob, BobNick),
    enter_room(RoomJID, Alice, AliceNick),
    Res = execute_user(list_room_users_body(RoomJID), Alice, Config),
    ExpectedUsers = [{escalus_client:full_jid(Bob), BobNick, <<"PARTICIPANT">>},
                     {escalus_client:full_jid(Alice), AliceNick, <<"MODERATOR">>}],
    assert_room_users(ExpectedUsers, get_ok_value(?LIST_ROOM_USERS_PATH, Res)).

user_try_list_nonexistent_room_users(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_list_nonexistent_room_users_story/2).

user_try_list_nonexistent_room_users_story(Config, Alice) ->
    Res = execute_user(list_room_users_body(?NONEXISTENT_ROOM), Alice, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

user_try_list_room_users_without_permission(Config) ->
    muc_helper:story_with_room(Config, [{members_only, true}], [{alice, 1}, {bob, 1}],
                               fun user_try_list_room_users_without_permission_story/3).

user_try_list_room_users_without_permission_story(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Res = execute_user(list_room_users_body(RoomJID), Bob, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not have permission">>)).

user_get_room_messages(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_get_room_messages_story/3).

user_get_room_messages_story(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    enter_room(RoomJID, Bob, <<"Bobek">>),
    enter_room(RoomJID, Alice, <<"Ali">>),
    escalus:wait_for_stanzas(Bob, 2),
    execute_user(user_send_message_to_room_body(RoomJID, <<"Hi!">>, null), Bob, Config),
    escalus:wait_for_stanzas(Bob, 1),
    mam_helper:maybe_wait_for_archive(Config),
    Res = execute_user(get_room_messages_body(RoomJID, 50, null), Alice, Config),
    #{<<"stanzas">> := [#{<<"stanza">> := StanzaXML}], <<"limit">> := 50} =
        get_ok_value(?GET_MESSAGES_PATH, Res),
    ?assertMatch({ok, #xmlel{name = <<"message">>}}, exml:parse(StanzaXML)).

user_try_get_nonexistent_room_messages(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_get_nonexistent_room_messages_story/2).

user_try_get_nonexistent_room_messages_story(Config, Alice) ->
    % Non-existent room with non-existent domain
    Res = execute_user(get_room_messages_body(?NONEXISTENT_ROOM, null, null), Alice, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)),
    % Non-existent room with existent domain
    Res2 = execute_user(get_room_messages_body(?NONEXISTENT_ROOM2, null, null), Alice, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)).

user_try_get_room_messages_without_permission(Config) ->
    muc_helper:story_with_room(Config, [{members_only, true}], [{alice, 1}, {bob, 1}],
                               fun user_try_get_room_messages_without_permission/3).

user_try_get_room_messages_without_permission(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Res = execute_user(get_room_messages_body(RoomJID, null, null), Bob, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not have permission">>)).

user_owner_set_user_affiliation(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_owner_set_user_affiliation/3).

user_owner_set_user_affiliation(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    % Grant a member affiliation 
    Res = execute_user(set_user_affiliation_body(RoomJID, Bob, member), Alice, Config),
    assert_success(?SET_AFFILIATION_PATH, Res),
    assert_user_affiliation(RoomJID, Bob, member),
    % Grant a member affiliation 
    Res1 = execute_user(set_user_affiliation_body(RoomJID, Bob, admin), Alice, Config),
    assert_success(?SET_AFFILIATION_PATH, Res1),
    assert_user_affiliation(RoomJID, Bob, admin),
    % Grant a owner affiliation
    Res2 = execute_user(set_user_affiliation_body(RoomJID, Bob, owner), Alice, Config),
    assert_success(?SET_AFFILIATION_PATH, Res2),
    assert_user_affiliation(RoomJID, Bob, owner),
    % Revoke affiliation 
    Res3 = execute_user(set_user_affiliation_body(RoomJID, Bob, none), Alice, Config),
    assert_success(?SET_AFFILIATION_PATH, Res3),
    assert_user_affiliation(RoomJID, Bob, none),
    % Ban user
    Res4 = execute_user(set_user_affiliation_body(RoomJID, Bob, outcast), Alice, Config),
    assert_success(?SET_AFFILIATION_PATH, Res4),
    assert_user_affiliation(RoomJID, Bob, outcast).


user_admin_set_user_affiliation(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}, {kate, 1}],
                               fun user_admin_set_user_affiliation/4).

user_admin_set_user_affiliation(Config, Alice, Bob, Kate) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    execute_user(set_user_affiliation_body(RoomJID, Bob, admin), Alice, Config),
    % Grant member affiliation
    Res = execute_user(set_user_affiliation_body(RoomJID, Kate, member), Bob, Config),
    assert_success(?SET_AFFILIATION_PATH, Res),
    assert_user_affiliation(RoomJID, Kate, member),
    % Revoke affiliation
    Res1 = execute_user(set_user_affiliation_body(RoomJID, Kate, none), Bob, Config),
    assert_success(?SET_AFFILIATION_PATH, Res1),
    assert_user_affiliation(RoomJID, Kate, none),
    % Admin cannot grant admin affiliation
    Res2 = execute_user(set_user_affiliation_body(RoomJID, Kate, admin), Bob, Config),
    assert_no_permission(Res2),
    % Admin cannot grant owner affiliation
    Res3 = execute_user(set_user_affiliation_body(RoomJID, Kate, owner), Bob, Config),
    assert_no_permission(Res3),
    % Admin can ban member
    Res4 = execute_user(set_user_affiliation_body(RoomJID, Kate, outcast), Bob, Config),
    assert_success(?SET_AFFILIATION_PATH, Res4),
    assert_user_affiliation(RoomJID, Kate, outcast),
    % Admin cannot ban owner
    Res5 = execute_user(set_user_affiliation_body(RoomJID, Alice, outcast), Bob, Config),
    assert_no_permission(Res5).

user_member_set_user_affiliation(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}, {kate, 1}],
                               fun user_member_set_user_affiliation/4).

user_member_set_user_affiliation(Config, Alice, Bob, Kate) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    execute_user(set_user_affiliation_body(RoomJID, Bob, member), Alice, Config),
    Res = execute_user(set_user_affiliation_body(RoomJID, Kate, member), Bob, Config),
    assert_no_permission(Res),
    Res1 = execute_user(set_user_affiliation_body(RoomJID, Kate, admin), Bob, Config),
    assert_no_permission(Res1),
    Res2 = execute_user(set_user_affiliation_body(RoomJID, Kate, owner), Bob, Config),
    assert_no_permission(Res2),
    execute_user(set_user_affiliation_body(RoomJID, Kate, member), Alice, Config),
    Res3 = execute_user(set_user_affiliation_body(RoomJID, Kate, none), Bob, Config),
    assert_no_permission(Res3).

user_try_set_nonexistent_room_affiliation(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_set_nonexistent_room_affiliation/2).

user_try_set_nonexistent_room_affiliation(Config, Alice) ->
    Res = execute_user(set_user_affiliation_body(?NONEXISTENT_ROOM, Alice, none), Alice, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

user_moderator_set_user_role(Config) ->
    muc_helper:story_with_room(Config, [{anonymous, false}, {persistent, true}],
                               [{alice, 1}, {bob, 1}],
                               fun user_moderator_set_user_role/3).

user_moderator_set_user_role(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Boobek">>,
    enter_room(RoomJID, Alice, escalus_client:username(Alice)),
    enter_room(RoomJID, Bob, BobNick),
    % Change from participant to visitor 
    Res = execute_user(set_user_role_body(RoomJID, BobNick, visitor), Alice, Config),
    assert_success(?SET_ROLE_PATH, Res),
    assert_user_role(RoomJID, Bob, visitor),
    % Change from visitor to moderator 
    Res1 = execute_user(set_user_role_body(RoomJID, BobNick, participant), Alice, Config),
    assert_success(?SET_ROLE_PATH, Res1),
    assert_user_role(RoomJID, Bob, participant),
    % Change from participant to moderator 
    Res2 = execute_user(set_user_role_body(RoomJID, BobNick, moderator), Alice, Config),
    assert_success(?SET_ROLE_PATH, Res2),
    assert_user_role(RoomJID, Bob, moderator).

user_participant_set_user_role(Config) ->
    muc_helper:story_with_room(Config, [{anonymous, false}, {persistent, true}],
                               [{alice, 1}, {bob, 1}, {kate, 1}],
                               fun user_participant_set_user_role/4).

user_participant_set_user_role(Config, _Alice, Bob, Kate) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Boobek">>,
    KateNick = <<"Katek">>,
    enter_room(RoomJID, Bob, BobNick),
    enter_room(RoomJID, Kate, KateNick),
    % Try change from participant to visitor
    Res = execute_user(set_user_role_body(RoomJID, KateNick, visitor), Bob, Config),
    assert_no_permission(Res),
    % Change from participant to participant with success response
    Res1 = execute_user(set_user_role_body(RoomJID, KateNick, participant), Bob, Config),
    assert_success(?SET_ROLE_PATH, Res1),
    assert_user_role(RoomJID, Bob, participant),
    % Try change from participant to moderator 
    Res2 = execute_user(set_user_role_body(RoomJID, KateNick, moderator), Bob, Config),
    assert_no_permission(Res2).

user_try_set_nonexistent_room_role(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_set_nonexistent_room_role/2).

user_try_set_nonexistent_room_role(Config, Alice) ->
    Res = execute_user(set_user_role_body(?NONEXISTENT_ROOM, <<"Ali">>, participant), Alice, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

user_can_enter_room(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}], fun user_can_enter_room/2).

user_can_enter_room(Config, Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Nick = <<"ali">>,
    JID = jid:from_binary(escalus_utils:jid_to_lower(escalus_client:full_jid(Alice))),
    Resource = escalus_client:resource(Alice),
    Res = execute_user(user_enter_room_body(RoomJID, Nick, Resource, null), Alice, Config),
    assert_success(?ENTER_ROOM_PATH, Res),
    ?assertMatch([#{nick := Nick, jid := JID}], get_room_users(RoomJID)).
    
user_can_enter_room_with_password(Config) ->
    muc_helper:story_with_room(Config, [{password_protected, true}, {password, ?PASSWORD}],
                               [{alice, 1}, {bob, 1}],
                               fun user_can_enter_room_with_password/3).

user_can_enter_room_with_password(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Nick = <<"ali">>,
    JID = jid:from_binary(escalus_utils:jid_to_lower(escalus_client:full_jid(Alice))),
    Resource = escalus_client:resource(Alice),
    % Alice enter room with password
    Res = execute_user(user_enter_room_body(RoomJID, Nick, Resource, ?PASSWORD), Alice, Config),
    assert_success(?ENTER_ROOM_PATH, Res),
    ?assertMatch([#{nick := Nick, jid := JID}], get_room_users(RoomJID)),
    % Bob try enter room without password
    Res1 = execute_user(user_enter_room_body(RoomJID, <<"Bobek">>, Resource, null), Bob, Config),
    assert_success(?ENTER_ROOM_PATH, Res1),
    ?assertMatch([_], get_room_users(RoomJID)),
    % Bob enter room with password
    Res2 = execute_user(user_enter_room_body(RoomJID, <<"Bobek">>, Resource, ?PASSWORD), Bob, Config),
    assert_success(?ENTER_ROOM_PATH, Res2),
    ?assertMatch([_, _], get_room_users(RoomJID)).

user_can_exit_room(Config) ->
    muc_helper:story_with_room(Config, [{persistent, true}], [{alice, 1}],
                               fun user_can_exit_room/2).

user_can_exit_room(Config, Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Nick = <<"ali">>,
    Resource = escalus_client:resource(Alice),
    enter_room(RoomJID, Alice, Nick),
    ?assertMatch([_], get_room_users(RoomJID)),
    Res = execute_user(user_exit_room_body(RoomJID, Nick, Resource), Alice, Config),
    assert_success(?EXIT_ROOM_PATH, Res),
    ?assertMatch([], get_room_users(RoomJID)).

user_list_room_affiliation(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_list_room_affiliation/3).

user_list_room_affiliation(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Bobek">>,
    AliceNick = <<"Ali">>,
    enter_room(RoomJID, Bob, BobNick),
    enter_room(RoomJID, Alice, AliceNick),
    AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
    BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
    % List all owners
    Res = execute_user(list_room_affiliations_body(RoomJID, owner), Alice, Config),
    ?assertMatch([#{<<"jid">> := AliceJID, <<"affiliation">> := <<"OWNER">>}],
                 get_ok_value(?LIST_ROOM_AFFILIATIONS_PATH, Res)),
    % List all members
    execute_user(set_user_affiliation_body(RoomJID, Bob, member), Alice, Config),
    Res1 = execute_user(list_room_affiliations_body(RoomJID, member), Alice, Config),
    ?assertMatch([#{<<"jid">> := BobJID, <<"affiliation">> := <<"MEMBER">>}],
                 get_ok_value(?LIST_ROOM_AFFILIATIONS_PATH, Res1)),
    % List all
    Res2 = execute_user(list_room_affiliations_body(RoomJID, null), Alice, Config),
    ?assertMatch([_, _], get_ok_value(?LIST_ROOM_AFFILIATIONS_PATH, Res2)).

user_try_list_room_affiliation_without_permission(Config) ->
    muc_helper:story_with_room(Config, [{members_only, true}], [{alice, 1}, {bob, 1}],
                               fun user_try_list_room_users_without_permission/3).

user_try_list_room_users_without_permission(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Res = execute_user(list_room_affiliations_body(RoomJID, null), Bob, Config),
    assert_no_permission(Res).

user_try_list_nonexistent_room_affiliations(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_list_nonexistent_room_affiliations/2).

user_try_list_nonexistent_room_affiliations(Config, Alice) ->
    Res = execute_user(list_room_affiliations_body(?NONEXISTENT_ROOM, null), Alice, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

%% Helpers

assert_no_permission(Res) ->
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not have permission">>)).

assert_success(Path, Res) ->
    ?assertNotEqual(nomatch, binary:match(get_ok_value(Path, Res), <<"successfully">>)).

get_room_affiliation(RoomJID, Aff) ->
    {ok, Affs} = rpc(mim(), mod_muc_api, get_room_affiliation, [RoomJID, Aff]),
    Affs.

get_room_users(RoomJID) ->
    {ok, Users} = rpc(mim(), mod_muc_api, get_room_users, [RoomJID]),
    Users.

assert_user_role(RoomJID, User, none) ->
    UserJID = jid:from_binary(escalus_client:full_jid(User)),
    ?assertNot(lists:any(fun(#{jid := JID}) -> jid:are_bare_equal(JID, UserJID) end,
                         get_room_users(RoomJID)));
assert_user_role(RoomJID, User, Role) ->
    UserJID = jid:from_binary(escalus_client:full_jid(User)),
    ?assert(lists:any(fun(#{jid := JID, role := Role2}) ->
                              Role =:= Role2 andalso jid:are_bare_equal(JID, UserJID) end,
                      get_room_users(RoomJID))).

assert_user_affiliation(RoomJID, User, none) ->
    Affs = get_room_affiliation(RoomJID, undefined),
    UserSimpleJID = jid:to_lower(user_to_jid(User)),
    ?assertNot(lists:any(fun({U, _}) -> U == UserSimpleJID end, Affs));
assert_user_affiliation(RoomJID, User, Aff) ->
    Affs = get_room_affiliation(RoomJID, Aff),
    Elem = {jid:to_lower(user_to_jid(User)), Aff},
    ?assert(lists:member(Elem, Affs)).

rand_name() ->
    rpc(mim(), mongoose_bin, gen_from_crypto, []).

-spec assert_room_users([{jid:jid(), binary(), binary()}], [map()]) -> ok.
assert_room_users(Expected, Actual) ->
    ActualTuples = [{JID, Nick, Role} || #{<<"jid">> := JID, <<"role">> := Role, <<"nick">> := Nick} <- Actual],
    ?assertEqual(lists:sort(Expected), lists:sort(ActualTuples)).

assert_is_message_correct(RoomJID, SenderNick, Type, Text, ReceivedMessage) ->
    escalus_pred:is_message(ReceivedMessage),
    From = jid:to_binary(jid:replace_resource(RoomJID, SenderNick)),
    From = exml_query:attr(ReceivedMessage, <<"from">>),
    Type = exml_query:attr(ReceivedMessage, <<"type">>),
    Body = #xmlel{name = <<"body">>, children = [#xmlcdata{content=Text}]},
    Body = exml_query:subelement(ReceivedMessage, <<"body">>).

enter_room(RoomJID, User, Nick) ->
    JID = jid:to_binary(jid:replace_resource(RoomJID, Nick)),
    Pres = escalus_stanza:to(escalus_stanza:presence(<<"available">>, []), JID),
    escalus:send(User, Pres),
    escalus:wait_for_stanza(User).

contain_room(Name, #{<<"rooms">> := Rooms}) ->
    contain_room(Name, Rooms);
contain_room(Name, Rooms) when is_list(Rooms) -> 
    lists:any(fun(#{<<"title">> := T}) -> T =:= Name end, Rooms).

assert_default_room_config(Response) ->
    ?assertMatch(#{<<"title">> := <<>>,
                   <<"description">> := <<>>,
                   <<"allowChangeSubject">> := true,
                   <<"allowQueryUsers">> := true,
                   <<"allowPrivateMessages">> := true,
                   <<"allowVisitorStatus">> := true,
                   <<"allowVisitorNickchange">> := true,
                   <<"public">> := true,
                   <<"publicList">> := true,
                   <<"persistent">> := false,
                   <<"moderated">> := true,
                   <<"membersByDefault">> := true,
                   <<"membersOnly">> := false,
                   <<"allowUserInvites">> := false,
                   <<"allowMultipleSession">> := false,
                   <<"passwordProtected">> := false,
                   <<"password">> := <<>>,
                   <<"anonymous">> := true,
                   <<"mayGetMemberList">> := [],
                   <<"maxUsers">> := 200,
                   <<"logging">> := false}, get_ok_value(?GET_ROOM_CONFIG_PATH, Response)).

atom_to_enum_item(null) -> null;
atom_to_enum_item(Atom) -> list_to_binary(string:to_upper(atom_to_list(Atom))).

%% Request bodies

admin_create_instant_room_body(MUCDomain, Name, Owner, Nick) ->
    Query = <<"mutation M1($mucDomain: String!, $name: String!, $owner: JID!, $nick: String!)
              { muc { createInstantRoom(mucDomain: $mucDomain, name: $name, owner: $owner, nick: $nick)
              { jid title private usersNumber } } }">>,
    OpName = <<"M1">>,
    Vars = #{mucDomain => MUCDomain, name => Name, owner => user_to_bin(Owner), nick => Nick},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_invite_user_body(Room, Sender, Recipient, Reason) ->
    Query = <<"mutation M1($room: JID!, $sender: JID!, $recipient: JID!, $reason: String)
              { muc { inviteUser(room: $room, sender: $sender, recipient: $recipient, reason: $reason) } }">>,
    OpName = <<"M1">>,
    Vars = #{room => jid:to_binary(Room), sender => user_to_bin(Sender),
             recipient => user_to_bin(Recipient), reason => Reason},
    #{query => Query, operationName => OpName, variables => Vars}.

kick_user_body(Room, Nick, Reason) ->
    Query = <<"mutation M1($room: JID!, $nick: String!, $reason: String)
              { muc { kickUser(room: $room, nick: $nick, reason: $reason) } }">>,
    OpName = <<"M1">>,
    Vars = #{room => jid:to_binary(Room), nick => Nick, reason => Reason},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_send_message_to_room_body(Room, From, Body) ->
    Query = <<"mutation M1($room: JID!, $from: JID!, $body: String!)
              { muc { sendMessageToRoom(room: $room, from: $from, body: $body) } }">>,
    OpName = <<"M1">>,
    Vars = #{room => jid:to_binary(Room), from => user_to_full_bin(From), body => Body},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_send_private_message_body(Room, From, ToNick, Body) ->
    Query = <<"mutation M1($room: JID!, $from: JID!, $toNick: String!, $body: String!)
              { muc { sendPrivateMessage(room: $room, from: $from, toNick: $toNick, body: $body) } }">>,
    OpName = <<"M1">>,
    Vars = #{room => jid:to_binary(Room), from => user_to_full_bin(From),
             toNick => ToNick, body => Body},
    #{query => Query, operationName => OpName, variables => Vars}.


admin_enter_room_body(Room, User, Nick, Password) ->
    Query = <<"mutation M1($room: JID!, $user: JID!, $nick: String! $password: String)
               { muc { enterRoom(room: $room, user: $user, nick: $nick, password: $password) } }">>,
    OpName = <<"M1">>,
    Vars = #{room => jid:to_binary(Room), user => user_to_full_bin(User), nick => Nick, password => Password},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_exit_room_body(Room, User, Nick) ->
    Query = <<"mutation M1($room: JID!, $user: JID!, $nick: String!)
               { muc { exitRoom(room: $room, user: $user, nick: $nick) } }">>,
    OpName = <<"M1">>,
    Vars = #{room => jid:to_binary(Room), user => user_to_full_bin(User), nick => Nick},
    #{query => Query, operationName => OpName, variables => Vars}.

delete_room_body(Room, Reason) ->
    Query = <<"mutation M1($room: JID!, $reason: String)
              { muc { deleteRoom(room: $room, reason: $reason) } }">>,
    OpName = <<"M1">>,
    Vars = #{room => jid:to_binary(Room), reason => Reason},
    #{query => Query, operationName => OpName, variables => Vars}.

change_room_config_body(Room, Config) ->
    Query = <<"mutation M1($room: JID!, $config: MUCRoomConfigInput!)
              { muc { changeRoomConfiguration(room: $room, config: $config)
              { title description allowChangeSubject allowQueryUsers allowPrivateMessages
                allowVisitorStatus allowVisitorNickchange public publicList persistent
                moderated membersByDefault membersOnly allowUserInvites allowMultipleSession
                passwordProtected password anonymous mayGetMemberList maxUsers logging } } }">>,
    OpName = <<"M1">>,
    Vars = #{room => jid:to_binary(Room), config => Config},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_list_rooms_body(MUCDomain, From, Limit, Index) ->
    Query = <<"query Q1($mucDomain: String!, $from: JID, $limit: Int, $index: Int)
              { muc { listRooms(mucDomain: $mucDomain, from: $from, limit: $limit, index: $index)
              { rooms { jid title private usersNumber } count index first last} } }">>,
    OpName = <<"Q1">>,
    Vars = #{mucDomain => MUCDomain, from => user_to_bin(From), limit => Limit, index => Index},
    #{query => Query, operationName => OpName, variables => Vars}.

get_room_config_body(Room) ->
    Query = <<"query Q1($room: JID!)
              { muc { getRoomConfig(room: $room)
              { title description allowChangeSubject allowQueryUsers allowPrivateMessages
                allowVisitorStatus allowVisitorNickchange public publicList persistent
                moderated membersByDefault membersOnly allowUserInvites allowMultipleSession
                passwordProtected password anonymous mayGetMemberList maxUsers logging } } }">>,
    OpName = <<"Q1">>,
    Vars = #{room => jid:to_binary(Room)},
    #{query => Query, operationName => OpName, variables => Vars}.

list_room_users_body(RoomJID) ->
    Query = <<"query Q1($room: JID!)
              { muc { listRoomUsers(room: $room)
              { jid nick role } } }">>,
    OpName = <<"Q1">>,
    Vars = #{room => jid:to_binary(RoomJID)},
    #{query => Query, operationName => OpName, variables => Vars}.

get_room_messages_body(RoomJID, PageSize, Before) ->
    Query = <<"query Q1($room: JID!, $pageSize: Int, $before: DateTime)
              { muc { getRoomMessages(room: $room, pageSize: $pageSize, before: $before)
              { stanzas { stanza } limit } } }">>,
    OpName = <<"Q1">>,
    Vars = #{<<"room">> => jid:to_binary(RoomJID), <<"pageSize">> => PageSize,
             <<"before">> => Before},
    #{query => Query, operationName => OpName, variables => Vars}.

user_list_rooms_body(MUCDomain, Limit, Index) ->
    Query = <<"query Q1($mucDomain: String!, $limit: Int, $index: Int)
              { muc { listRooms(mucDomain: $mucDomain, limit: $limit, index: $index)
              { rooms { jid title private usersNumber } count index first last} } }">>,
    OpName = <<"Q1">>,
    Vars = #{mucDomain => MUCDomain, limit => Limit, index => Index},
    #{query => Query, operationName => OpName, variables => Vars}.

user_send_message_to_room_body(Room, Body, Resource) ->
    Query = <<"mutation M1($room: JID!, $body: String!, $resource: String)
              { muc { sendMessageToRoom(room: $room, body: $body, resource: $resource) } }">>,
    OpName = <<"M1">>,
    Vars = #{room => jid:to_binary(Room), body => Body, resource => Resource},
    #{query => Query, operationName => OpName, variables => Vars}.

user_send_private_message_body(Room, Body, ToNick, Resource) ->
    Query = <<"mutation M1($room: JID!, $body: String!, $toNick: String!, $resource: String)
              { muc { sendPrivateMessage(room: $room, body: $body, toNick: $toNick, resource: $resource) } }">>,
    OpName = <<"M1">>,
    Vars = #{room => jid:to_binary(Room), body => Body, toNick => ToNick, resource => Resource},
    #{query => Query, operationName => OpName, variables => Vars}.


user_create_instant_room_body(MUCDomain, Name, Nick) ->
    Query = <<"mutation M1($mucDomain: String!, $name: String!, $nick: String!)
              { muc { createInstantRoom(mucDomain: $mucDomain, name: $name, nick: $nick)
              { jid title private usersNumber } } }">>,
    OpName = <<"M1">>,
    Vars = #{mucDomain => MUCDomain, name => Name, nick => Nick},
    #{query => Query, operationName => OpName, variables => Vars}.

user_invite_user_body(Room, Recipient, Reason) ->
    Query = <<"mutation M1($room: JID!, $recipient: JID!, $reason: String)
              { muc { inviteUser(room: $room, recipient: $recipient, reason: $reason) } }">>,
    OpName = <<"M1">>,
    Vars = #{room => jid:to_binary(Room), recipient => user_to_bin(Recipient), reason => Reason},
    #{query => Query, operationName => OpName, variables => Vars}.

set_user_affiliation_body(Room, User, Aff) ->
    Query = <<"mutation M1($room: JID!, $user: JID!, $affiliation: MUCAffiliation!)
               { muc { setUserAffiliation(room: $room, user: $user, affiliation: $affiliation) } }">>,
    OpName = <<"M1">>,
    Vars = #{room => jid:to_binary(Room), user => user_to_bin(User),
             affiliation => atom_to_enum_item(Aff)},
    #{query => Query, operationName => OpName, variables => Vars}.

set_user_role_body(Room, User, Role) ->
    Query = <<"mutation M1($room: JID!, $nick: String!, $role: MUCRole!)
               { muc { setUserRole(room: $room, nick: $nick, role: $role) } }">>,
    OpName = <<"M1">>,
    Vars = #{room => jid:to_binary(Room), nick => user_to_bin(User),
             role => atom_to_enum_item(Role)},
    #{query => Query, operationName => OpName, variables => Vars}.

user_enter_room_body(Room, Nick, Resource, Password) ->
    Query = <<"mutation M1($room: JID!, $nick: String!, $resource: String!, $password: String)
               { muc { enterRoom(room: $room, nick: $nick, resource: $resource, password: $password) } }">>,
    OpName = <<"M1">>,
    Vars = #{room => jid:to_binary(Room), resource => Resource, password => Password, nick => Nick},
    #{query => Query, operationName => OpName, variables => Vars}.

user_exit_room_body(Room, Nick, Resource) ->
    Query = <<"mutation M1($room: JID!, $nick: String!, $resource: String!)
               { muc { exitRoom(room: $room, nick: $nick, resource: $resource) } }">>,
    OpName = <<"M1">>,
    Vars = #{room => jid:to_binary(Room), resource => Resource, nick => Nick},
    #{query => Query, operationName => OpName, variables => Vars}.

list_room_affiliations_body(Room, Aff) ->
    Query = <<"query Q1($room: JID!, $affiliation: MUCAffiliation)
               { muc { listRoomAffiliations(room: $room, affiliation: $affiliation)
               { jid affiliation } } }">>,
    OpName = <<"Q1">>,
    Vars = #{room => jid:to_binary(Room), affiliation => atom_to_enum_item(Aff)},
    #{query => Query, operationName => OpName, variables => Vars}.
