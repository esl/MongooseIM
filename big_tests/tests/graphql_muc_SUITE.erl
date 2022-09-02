-module(graphql_muc_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [execute_command/4, execute_user_command/5, get_ok_value/2, get_err_msg/1,
                         get_coercion_err_msg/1, user_to_bin/1, user_to_full_bin/1, user_to_jid/1,
                         get_unauthorized/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user_muc},
     {group, admin_muc_http},
     {group, admin_muc_cli},
     {group, domain_admin_muc}].

groups() ->
    [{user_muc, [parallel], user_muc_tests()},
     {admin_muc_http, [parallel], admin_muc_tests()},
     {admin_muc_cli, [], admin_muc_tests()},
     {domain_admin_muc, [], domain_admin_muc_tests()}].

user_muc_tests() ->
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
     user_try_kick_user_from_nonexistent_room,
     user_try_kick_user_without_moderator_resource,
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
     user_list_room_affiliations,
     user_try_list_room_affiliations_without_permission,
     user_try_list_nonexistent_room_affiliations
    ].

admin_muc_tests() ->
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
     admin_try_kick_user_from_nonexistent_room,
     admin_try_kick_user_from_room_without_moderators,
     admin_send_message_to_room,
     admin_send_private_message,
     admin_get_room_messages,
     admin_try_get_nonexistent_room_messages,
     admin_set_user_affiliation,
     admin_try_set_nonexistent_room_user_affiliation,
     admin_set_user_role,
     admin_try_set_nonexistent_room_user_role,
     admin_try_set_nonexistent_nick_role,
     admin_try_set_user_role_in_room_without_moderators,
     admin_make_user_enter_room,
     admin_make_user_enter_room_with_password,
     admin_make_user_enter_room_bare_jid,
     admin_make_user_exit_room,
     admin_make_user_exit_room_bare_jid,
     admin_list_room_affiliations,
     admin_try_list_nonexistent_room_affiliations
    ].

domain_admin_muc_tests() ->
    [admin_create_and_delete_room,
     admin_try_create_instant_room_with_nonexistent_domain,
     domain_admin_try_create_instant_room_with_nonexistent_user,
     admin_try_delete_nonexistent_room,
     domain_admin_try_delete_room_with_nonexistent_domain,
     admin_list_rooms,
     admin_list_room_users,
     domain_admin_try_list_users_from_nonexistent_room,
     admin_change_room_config,
     domain_admin_try_change_nonexistent_room_config,
     admin_get_room_config,
     domain_admin_try_get_nonexistent_room_config,
     admin_invite_user,
     admin_invite_user_with_password,
     admin_try_invite_user_to_nonexistent_room,
     admin_kick_user,
     domain_admin_try_kick_user_from_nonexistent_room,
     admin_try_kick_user_from_room_without_moderators,
     admin_send_message_to_room,
     admin_send_private_message,
     admin_get_room_messages,
     domain_admin_try_get_nonexistent_room_messages,
     admin_set_user_affiliation,
     domain_admin_try_set_nonexistent_room_user_affiliation,
     admin_set_user_role,
     domain_admin_try_set_nonexistent_room_user_role,
     admin_try_set_nonexistent_nick_role,
     admin_try_set_user_role_in_room_without_moderators,
     admin_make_user_enter_room,
     admin_make_user_enter_room_with_password,
     admin_make_user_enter_room_bare_jid,
     admin_make_user_exit_room,
     admin_make_user_exit_room_bare_jid,
     admin_list_room_affiliations,
     domain_admin_try_list_nonexistent_room_affiliations
    ].

init_per_suite(Config) ->
    HostType = domain_helper:host_type(),
    Config2 = escalus:init_per_suite(Config),
    Config3 = dynamic_modules:save_modules(HostType, Config2),
    Config4 = rest_helper:maybe_enable_mam(mam_helper:backend(), HostType, Config3),
    Config5 = ejabberd_node_utils:init(mim(), Config4),
    dynamic_modules:restart(HostType, mod_disco,
                            config_parser_helper:default_mod_config(mod_disco)),
    muc_helper:load_muc(),
    mongoose_helper:ensure_muc_clean(),
    Config5.

end_per_suite(Config) ->
    escalus_fresh:clean(),
    mongoose_helper:ensure_muc_clean(),
    muc_helper:unload_muc(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(admin_muc_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_muc_cli, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(domain_admin_muc, Config) ->
    graphql_helper:init_domain_admin_handler(Config);
init_per_group(user_muc, Config) ->
    graphql_helper:init_user(Config).

end_per_group(_GN, _Config) ->
    graphql_helper:clean().

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
    Res = list_rooms(muc_helper:muc_host(), Alice, null, null, Config),
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
    Res = create_instant_room(MUCServer, Name, Alice, <<"Ali">>, Config),
    ?assertMatch(#{<<"title">> := Name, <<"private">> := false, <<"usersNumber">> := 0},
                 get_ok_value(?CREATE_INSTANT_ROOM_PATH, Res)),
    Res2 = list_rooms(MUCServer, Alice, null, null, Config),
    ?assert(contain_room(Name, get_ok_value(?LIST_ROOMS_PATH, Res2))),
    % Delete room
    Res3 = delete_room(RoomJID, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?DELETE_ROOM_PATH, Res3),
                                          <<"successfully">>)),
    Res4 = list_rooms(MUCServer, Alice, null, null, Config),
    ?assertNot(contain_room(Name, get_ok_value(?LIST_ROOMS_PATH, Res4))).

admin_try_create_instant_room_with_nonexistent_domain(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_try_create_instant_room_with_nonexistent_domain_story/2).

admin_try_create_instant_room_with_nonexistent_domain_story(Config, Alice) ->
    Res = create_instant_room(<<"unknown">>, rand_name(), Alice, <<"Ali">>, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_try_create_instant_room_with_nonexistent_user(Config) ->
    Name = rand_name(),
    MUCServer = muc_helper:muc_host(),
    JID = <<(rand_name())/binary, "@localhost">>,
    Res = create_instant_room(MUCServer, Name, JID, <<"Ali">>, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_try_delete_nonexistent_room(Config) ->
    RoomJID = jid:make_bare(<<"unknown">>, muc_helper:muc_host()),
    Res = delete_room(RoomJID, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"non-existent">>)).

admin_try_delete_room_with_nonexistent_domain(Config) ->
    RoomJID = jid:make_bare(<<"unknown">>, <<"unknown">>),
    Res = delete_room(RoomJID, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"non-existent">>)).

admin_invite_user(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}], fun admin_invite_user_story/3).

admin_invite_user_story(Config, Alice, Bob) ->
    RoomJIDBin = ?config(room_jid, Config),
    RoomJID = jid:from_binary(RoomJIDBin),
    Res = invite_user(RoomJID, Alice, Bob, null, Config),
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
    Res = invite_user(RoomJID, Alice, Bob, null, Config),
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
    Res = invite_user(?NONEXISTENT_ROOM, Alice, Bob, null, Config),
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
    Res = kick_user(RoomJID, BobNick, Reason, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?KICK_USER_PATH, Res),
                                          <<"successfully">>)),
    escalus:wait_for_stanzas(Bob, 2),
    KickStanza = escalus:wait_for_stanza(Bob),
    escalus:assert(is_presence_with_type, [<<"unavailable">>], KickStanza),
    ?assertEqual(Reason,
                 exml_query:path(KickStanza, [{element, <<"x">>}, {element, <<"item">>},
                                              {element, <<"reason">>}, cdata])).

admin_try_kick_user_from_room_without_moderators(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun admin_try_kick_user_from_room_without_moderators/3).

admin_try_kick_user_from_room_without_moderators(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Bobek">>,
    enter_room(RoomJID, Bob, BobNick),
    Res = kick_user(RoomJID, BobNick, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_try_kick_user_from_nonexistent_room(Config) ->
    Res = kick_user(?NONEXISTENT_ROOM, <<"ali">>, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_send_message_to_room(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun admin_send_message_to_room_story/3).

admin_send_message_to_room_story(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Message = <<"Hello All!">>,
    BobNick = <<"Bobek">>,
    enter_room(RoomJID, Bob, BobNick),
    escalus:wait_for_stanza(Bob),
    % Try send message from bare JID,
    BareBob = escalus_client:short_jid(Bob),
    Res = send_message_to_room(RoomJID, BareBob, Message, Config),
    assert_no_full_jid(Res),
    % Send message
    Res1 = send_message_to_room(RoomJID, Bob, Message, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SEND_MESSAGE_PATH, Res1),
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
    % Try send private message from bare JID,
    BareAlice = escalus_client:short_jid(Alice),
    Res = send_private_message(RoomJID, BareAlice, BobNick, Message, Config),
    assert_no_full_jid(Res),
    % Send message
    Res1 = send_private_message(RoomJID, Alice, BobNick, Message, Config),
    assert_success(?SEND_PRIV_MESG_PATH, Res1),
    assert_is_message_correct(RoomJID, AliceNick, <<"chat">>, Message,
                              escalus:wait_for_stanza(Bob)).

admin_get_room_config(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}], fun admin_get_room_config_story/2).

admin_get_room_config_story(Config, _Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Res = get_room_config(RoomJID, Config),
    assert_default_room_config(Res).

admin_try_get_nonexistent_room_config(Config) ->
    Res = get_room_config(?NONEXISTENT_ROOM, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_change_room_config(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}], fun admin_change_room_config_story/2).

admin_change_room_config_story(Config, _Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Title = <<"aloes">>,
    Description = <<"The chat about aloes">>,
    Public = false,
    RoomConfig = #{title => Title, description => Description, public => Public},
    Res = change_room_config(RoomJID, RoomConfig, Config),
    ?assertMatch(#{<<"title">> := Title,
                   <<"description">> := Description,
                   <<"public">> := Public}, get_ok_value(?CHANGE_ROOM_CONFIG_PATH, Res)).

admin_try_change_nonexistent_room_config(Config) ->
    RoomConfig = #{title => <<"NewTitle">>},
    Res = change_room_config(?NONEXISTENT_ROOM, RoomConfig, Config),
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
    Res = list_room_users(RoomJID, Config),
    ExpectedUsers = [{escalus_client:full_jid(Bob), BobNick, <<"PARTICIPANT">>},
                     {escalus_client:full_jid(Alice), AliceNick, <<"MODERATOR">>}],
    assert_room_users(ExpectedUsers, get_ok_value(?LIST_ROOM_USERS_PATH, Res)).

admin_try_list_users_from_nonexistent_room(Config) ->
    Res = list_room_users(?NONEXISTENT_ROOM, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_get_room_messages(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun admin_get_room_messages_story/3).

admin_get_room_messages_story(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    enter_room(RoomJID, Bob, <<"Bobek">>),
    enter_room(RoomJID, Alice, <<"Ali">>),
    escalus:wait_for_stanzas(Bob, 2),
    send_message_to_room(RoomJID, Bob, <<"Hi!">>, Config),
    escalus:wait_for_stanzas(Bob, 1),
    mam_helper:maybe_wait_for_archive(Config),
    Res = get_room_messages(RoomJID, 50, null, Config),
    #{<<"stanzas">> := [#{<<"stanza">> := StanzaXML}], <<"limit">> := 50} =
        get_ok_value(?GET_MESSAGES_PATH, Res),
    ?assertMatch({ok, #xmlel{name = <<"message">>}}, exml:parse(StanzaXML)).

admin_try_get_nonexistent_room_messages(Config) ->
    Res = get_room_messages(?NONEXISTENT_ROOM, null, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).


admin_set_user_affiliation(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun admin_set_user_affiliation/3).

admin_set_user_affiliation(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    % Grant member affiliation
    Res = set_user_affiliation(RoomJID, Bob, member, Config),
    assert_success(?SET_AFFILIATION_PATH, Res),
    assert_user_affiliation(RoomJID, Bob, member),
    % Grant admin affiliation
    Res1 = set_user_affiliation(RoomJID, Bob, admin, Config),
    assert_success(?SET_AFFILIATION_PATH, Res1),
    assert_user_affiliation(RoomJID, Bob, admin),
    % Grant owner affiliation
    Res2 = set_user_affiliation(RoomJID, Bob, owner, Config),
    assert_success(?SET_AFFILIATION_PATH, Res2),
    assert_user_affiliation(RoomJID, Bob, owner),
    % Revoke affiliation
    Res3 = set_user_affiliation(RoomJID, Bob, none, Config),
    assert_success(?SET_AFFILIATION_PATH, Res3),
    assert_user_affiliation(RoomJID, Bob, none),
    % Ban user
    Res4 = set_user_affiliation(RoomJID, Bob, outcast, Config),
    assert_success(?SET_AFFILIATION_PATH, Res4),
    assert_user_affiliation(RoomJID, Bob, outcast).

admin_try_set_nonexistent_room_user_affiliation(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_try_set_nonexistent_room_user_affiliation/2).

admin_try_set_nonexistent_room_user_affiliation(Config, Alice) ->
    Res = set_user_affiliation(?NONEXISTENT_ROOM, Alice, admin, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_set_user_role(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}], fun admin_set_user_role/3).

admin_set_user_role(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Boobek">>,
    enter_room(RoomJID, Alice, escalus_client:username(Alice)),
    enter_room(RoomJID, Bob, BobNick),
    % Change from participant to visitor
    Res = set_user_role(RoomJID, BobNick, visitor, Config),
    assert_success(?SET_ROLE_PATH, Res),
    assert_user_role(RoomJID, Bob, visitor),
    % Change from visitor to participant
    Res1 = set_user_role(RoomJID, BobNick, participant, Config),
    assert_success(?SET_ROLE_PATH, Res1),
    assert_user_role(RoomJID, Bob, participant),
    % Change from participant to moderator
    Res2 = set_user_role(RoomJID, BobNick, moderator, Config),
    assert_success(?SET_ROLE_PATH, Res2),
    assert_user_role(RoomJID, Bob, moderator).

admin_try_set_nonexistent_nick_role(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}], fun admin_try_set_nonexistent_nick_role/2).

admin_try_set_nonexistent_nick_role(Config, Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    enter_room(RoomJID, Alice, escalus_client:username(Alice)),
    Res = set_user_role(RoomJID, <<"kik">>, visitor, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not exist">>)).

admin_try_set_user_role_in_room_without_moderators(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun admin_try_set_user_role_in_room_without_moderators/3).

admin_try_set_user_role_in_room_without_moderators(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Boobek">>,
    enter_room(RoomJID, Bob, BobNick),
    Res = set_user_role(RoomJID, BobNick, visitor, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_try_set_nonexistent_room_user_role(Config) ->
    Res = set_user_role(?NONEXISTENT_ROOM, <<"Alice">>, moderator, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_make_user_enter_room(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}], fun admin_make_user_enter_room/2).

admin_make_user_enter_room(Config, Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Nick = <<"ali">>,
    JID = jid:from_binary(escalus_client:full_jid(Alice)),
    % Alice enter room with password
    Res = enter_room(RoomJID, Alice, Nick, null, Config),
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
    Res = enter_room(RoomJID, Alice, Nick, ?PASSWORD, Config),
    assert_success(?ENTER_ROOM_PATH, Res),
    ?assertMatch([#{nick := Nick, jid := JID}], get_room_users(RoomJID)),
    % Bob try enter room without password
    Res1 = enter_room(RoomJID, Bob, <<"Bobek">>, null, Config),
    assert_success(?ENTER_ROOM_PATH, Res1),
    ?assertMatch([_], get_room_users(RoomJID)),
    % Bob enter room with password
    Res2 = enter_room(RoomJID, Bob, <<"Bobek">>, ?PASSWORD, Config),
    assert_success(?ENTER_ROOM_PATH, Res2),
    ?assertMatch([_, _], get_room_users(RoomJID)).

admin_make_user_enter_room_bare_jid(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}], fun admin_make_user_enter_room_bare_jid/2).

admin_make_user_enter_room_bare_jid(Config, Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BareAlice = escalus_client:short_jid(Alice),
    Res = enter_room(RoomJID, BareAlice, <<"Ali">>, null, Config),
    assert_no_full_jid(Res).

admin_make_user_exit_room(Config) ->
    muc_helper:story_with_room(Config, [{persistent, true}], [{alice, 1}],
                               fun admin_make_user_exit_room/2).

admin_make_user_exit_room(Config, Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Nick = <<"ali">>,
    enter_room(RoomJID, Alice, Nick),
    ?assertMatch([_], get_room_users(RoomJID)),
    Res = exit_room(RoomJID, Alice, Nick, Config),
    assert_success(?EXIT_ROOM_PATH, Res),
    ?assertMatch([], get_room_users(RoomJID)).

admin_make_user_exit_room_bare_jid(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}], fun admin_make_user_exit_room_bare_jid/2).

admin_make_user_exit_room_bare_jid(Config, Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BareAlice = escalus_client:short_jid(Alice),
    Nick = <<"ali">>,
    enter_room(RoomJID, Alice, Nick),
    Res = exit_room(RoomJID, BareAlice, Nick, Config),
    assert_no_full_jid(Res).

admin_list_room_affiliations(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun admin_list_room_affiliations/3).

admin_list_room_affiliations(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Bobek">>,
    AliceNick = <<"Ali">>,
    enter_room(RoomJID, Bob, BobNick),
    enter_room(RoomJID, Alice, AliceNick),
    AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
    BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
    % List all owners
    Res = list_room_affiliations(RoomJID, owner, Config),
    ?assertMatch([#{<<"jid">> := AliceJID, <<"affiliation">> := <<"OWNER">>}],
                 get_ok_value(?LIST_ROOM_AFFILIATIONS_PATH, Res)),
    % List all members
    set_user_affiliation(RoomJID, Bob, member, Config),
    Res1 = list_room_affiliations(RoomJID, member, Config),
    ?assertMatch([#{<<"jid">> := BobJID, <<"affiliation">> := <<"MEMBER">>}],
                 get_ok_value(?LIST_ROOM_AFFILIATIONS_PATH, Res1)),
    % List all
    Res2 = list_room_affiliations(RoomJID, null, Config),
    ?assertMatch([_, _], get_ok_value(?LIST_ROOM_AFFILIATIONS_PATH, Res2)).

admin_try_list_nonexistent_room_affiliations(Config) ->
    Res = list_room_affiliations(?NONEXISTENT_ROOM, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

%% Domain admin test cases

domain_admin_try_delete_room_with_nonexistent_domain(Config) ->
    RoomJID = jid:make_bare(<<"unknown">>, <<"unknown">>),
    get_unauthorized(delete_room(RoomJID, null, Config)).

domain_admin_try_create_instant_room_with_nonexistent_user(Config) ->
    Name = rand_name(),
    LocalDomain = domain_helper:domain(),
    ExternalDomain = <<"external">>,
    MUCServer = muc_helper:muc_host(),
    
    LocalJID = <<(rand_name())/binary, "@", LocalDomain/binary>>,
    Res1 = create_instant_room(MUCServer, Name, LocalJID, <<"Ali">>, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res1), <<"not found">>)),

    ExternalJID = <<(rand_name())/binary, "@", ExternalDomain/binary>>,
    Res2 = create_instant_room(MUCServer, Name, ExternalJID, <<"Ali">>, Config),
    get_unauthorized(Res2).

domain_admin_try_list_users_from_nonexistent_room(Config) ->
    get_unauthorized(list_room_users(?NONEXISTENT_ROOM, Config)).

domain_admin_try_change_nonexistent_room_config(Config) ->
    RoomConfig = #{title => <<"NewTitle">>},
    get_unauthorized(change_room_config(?NONEXISTENT_ROOM, RoomConfig, Config)).

domain_admin_try_get_nonexistent_room_config(Config) ->
    get_unauthorized(get_room_config(?NONEXISTENT_ROOM, Config)).

domain_admin_try_kick_user_from_nonexistent_room(Config) ->
    get_unauthorized(kick_user(?NONEXISTENT_ROOM, <<"ali">>, null, Config)).

domain_admin_try_get_nonexistent_room_messages(Config) ->
    get_unauthorized(get_room_messages(?NONEXISTENT_ROOM, null, null, Config)).

domain_admin_try_set_nonexistent_room_user_affiliation(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun domain_admin_try_set_nonexistent_room_user_affiliation/2).

domain_admin_try_set_nonexistent_room_user_affiliation(Config, Alice) ->
    get_unauthorized(set_user_affiliation(?NONEXISTENT_ROOM, Alice, admin, Config)).

domain_admin_try_set_nonexistent_room_user_role(Config) ->
    get_unauthorized(set_user_role(?NONEXISTENT_ROOM, <<"Alice">>, moderator, Config)).

domain_admin_try_list_nonexistent_room_affiliations(Config) ->
    get_unauthorized(list_room_affiliations(?NONEXISTENT_ROOM, null, Config)).

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

    Res = user_list_rooms(Alice, muc_helper:muc_host(), null, null, Config),
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
    Res = user_create_instant_room(Alice, MUCServer, Name, <<"Ali">>, Config),
    ?assertMatch(#{<<"title">> := Name, <<"private">> := false, <<"usersNumber">> := 0},
                 get_ok_value(?CREATE_INSTANT_ROOM_PATH, Res)),
    Res2 = user_list_rooms(Alice, MUCServer, null, null, Config),
    ?assert(contain_room(Name, get_ok_value(?LIST_ROOMS_PATH, Res2))),
    % Delete room
    Res3 = user_delete_room(Alice, RoomJID, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?DELETE_ROOM_PATH, Res3),
                                          <<"successfully">>)),
    Res4 = user_list_rooms(Alice, MUCServer, null, null, Config),
    ?assertNot(contain_room(Name, get_ok_value(?LIST_ROOMS_PATH, Res4))).

user_try_create_instant_room_with_nonexistent_domain(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_create_instant_room_with_nonexistent_domain_story/2).

user_try_create_instant_room_with_nonexistent_domain_story(Config, Alice) ->
    Res = user_create_instant_room(Alice, <<"unknown">>, rand_name(), <<"Ali">>, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

user_try_delete_nonexistent_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_delete_nonexistent_room_story/2).

user_try_delete_nonexistent_room_story(Config, Alice) ->
    RoomJID = jid:make_bare(<<"unknown">>, muc_helper:muc_host()),
    Res = user_delete_room(Alice, RoomJID, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"non-existent">>)).

user_try_delete_room_by_not_owner(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_try_delete_room_by_not_owner_story/3).

user_try_delete_room_by_not_owner_story(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Res = user_delete_room(Bob, RoomJID, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not have permission">>)).

user_invite_user(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}], fun user_invite_user_story/3).

user_invite_user_story(Config, Alice, Bob) ->
    RoomJIDBin = ?config(room_jid, Config),
    RoomJID = jid:from_binary(RoomJIDBin),
    Res = user_invite_user(Alice, RoomJID, Bob, null, Config),
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
    Res = user_kick_user(Alice, RoomJID, BobNick, Reason, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?KICK_USER_PATH, Res),
                                          <<"successfully">>)),
    escalus:wait_for_stanzas(Bob, 2),
    KickStanza = escalus:wait_for_stanza(Bob),
    escalus:assert(is_presence_with_type, [<<"unavailable">>], KickStanza),
    ?assertEqual(Reason,
                 exml_query:path(KickStanza, [{element, <<"x">>}, {element, <<"item">>},
                                              {element, <<"reason">>}, cdata])).

user_try_kick_user_without_moderator_resource(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_try_kick_user_without_moderator_resource/3).

user_try_kick_user_without_moderator_resource(Config, Alice, Bob) ->
    RoomJIDBin = ?config(room_jid, Config),
    RoomJID = jid:from_binary(RoomJIDBin),
    BobNick = <<"Bobek">>,
    enter_room(RoomJID, Bob, BobNick),
    Res = user_kick_user(Alice, RoomJID, BobNick, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

user_try_kick_user_from_nonexistent_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_kick_user_from_nonexistent_room/2).

user_try_kick_user_from_nonexistent_room(Config, Alice) ->
    Res = user_kick_user(Alice, ?NONEXISTENT_ROOM, <<"bobi">>, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

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
    Res = user_send_message_to_room(Bob, RoomJID, Message, null, Config),
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
    Res = user_send_message_to_room(Bob, RoomJID, Message, <<"res2">>, Config),
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
    Res = user_send_private_message(Alice, RoomJID, Message, BobNick, null, Config),
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
    Res = user_send_private_message(Alice, RoomJID, Message, BobNick, <<"res2">>, Config),
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
    Res = user_send_message_to_room(Alice, RoomJID, <<"Hello!">>, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not have any session">>)).

user_get_room_config(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_get_room_config_story/3).

user_get_room_config_story(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Res = user_get_room_config(Alice, RoomJID, Config),
    assert_default_room_config(Res),
    % Not an owner tries to get room config
    Res2 = user_get_room_config(Bob, RoomJID, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not have permission">>)).

user_try_get_nonexistent_room_config(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_get_nonexistent_room_config_story/2).

user_try_get_nonexistent_room_config_story(Config, Alice) ->
    Res = user_get_room_config(Alice, ?NONEXISTENT_ROOM, Config),
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
    Res = user_change_room_config(Alice, RoomJID, RoomConfig, Config),
    ?assertMatch(#{<<"title">> := Title,
                   <<"description">> := Description,
                   <<"public">> := Public}, get_ok_value(?CHANGE_ROOM_CONFIG_PATH, Res)),
    % Not an owner tries to change the room config
    Res2 = user_change_room_config(Bob, RoomJID, RoomConfig, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not have permission">>)).

user_try_change_nonexistent_room_config(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_change_nonexistent_room_config_story/2).

user_try_change_nonexistent_room_config_story(Config, Alice) ->
    RoomConfig = #{title => <<"NewTitle">>},
    Res = user_change_room_config(Alice, ?NONEXISTENT_ROOM, RoomConfig, Config),
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
    Res = user_list_room_users(Alice, RoomJID, Config),
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
    Res = user_list_room_users(Alice, RoomJID, Config),
    ExpectedUsers = [{escalus_client:full_jid(Bob), BobNick, <<"PARTICIPANT">>},
                     {escalus_client:full_jid(Alice), AliceNick, <<"MODERATOR">>}],
    assert_room_users(ExpectedUsers, get_ok_value(?LIST_ROOM_USERS_PATH, Res)).

user_try_list_nonexistent_room_users(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_list_nonexistent_room_users_story/2).

user_try_list_nonexistent_room_users_story(Config, Alice) ->
    Res = user_list_room_users(Alice, ?NONEXISTENT_ROOM, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

user_try_list_room_users_without_permission(Config) ->
    muc_helper:story_with_room(Config, [{members_only, true}], [{alice, 1}, {bob, 1}],
                               fun user_try_list_room_users_without_permission_story/3).

user_try_list_room_users_without_permission_story(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Res = user_list_room_users(Bob, RoomJID, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not have permission">>)).

user_get_room_messages(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_get_room_messages_story/3).

user_get_room_messages_story(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    enter_room(RoomJID, Bob, <<"Bobek">>),
    enter_room(RoomJID, Alice, <<"Ali">>),
    escalus:wait_for_stanzas(Bob, 2),
    user_send_message_to_room(Bob, RoomJID, <<"Hi!">>, null, Config),
    escalus:wait_for_stanzas(Bob, 1),
    mam_helper:maybe_wait_for_archive(Config),
    Res = user_get_room_messages(Alice, RoomJID, 50, null, Config),
    #{<<"stanzas">> := [#{<<"stanza">> := StanzaXML}], <<"limit">> := 50} =
        get_ok_value(?GET_MESSAGES_PATH, Res),
    ?assertMatch({ok, #xmlel{name = <<"message">>}}, exml:parse(StanzaXML)).

user_try_get_nonexistent_room_messages(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_get_nonexistent_room_messages_story/2).

user_try_get_nonexistent_room_messages_story(Config, Alice) ->
    % Non-existent room with non-existent domain
    Res = user_get_room_messages(Alice, ?NONEXISTENT_ROOM, null, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)),
    % Non-existent room with existent domain
    Res2 = user_get_room_messages(Alice, ?NONEXISTENT_ROOM2, null, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)).

user_try_get_room_messages_without_permission(Config) ->
    muc_helper:story_with_room(Config, [{members_only, true}], [{alice, 1}, {bob, 1}],
                               fun user_try_get_room_messages_without_permission/3).

user_try_get_room_messages_without_permission(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Res = user_get_room_messages(Bob, RoomJID, null, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not have permission">>)).

user_owner_set_user_affiliation(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_owner_set_user_affiliation/3).

user_owner_set_user_affiliation(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    % Grant a member affiliation
    Res = user_set_user_affiliation(Alice, RoomJID, Bob, member, Config),
    assert_success(?SET_AFFILIATION_PATH, Res),
    assert_user_affiliation(RoomJID, Bob, member),
    % Grant a member affiliation
    Res1 = user_set_user_affiliation(Alice, RoomJID, Bob, admin, Config),
    assert_success(?SET_AFFILIATION_PATH, Res1),
    assert_user_affiliation(RoomJID, Bob, admin),
    % Grant a owner affiliation
    Res2 = user_set_user_affiliation(Alice, RoomJID, Bob, owner, Config),
    assert_success(?SET_AFFILIATION_PATH, Res2),
    assert_user_affiliation(RoomJID, Bob, owner),
    % Revoke affiliation
    Res3 = user_set_user_affiliation(Alice, RoomJID, Bob, none, Config),
    assert_success(?SET_AFFILIATION_PATH, Res3),
    assert_user_affiliation(RoomJID, Bob, none),
    % Ban user
    Res4 = user_set_user_affiliation(Alice, RoomJID, Bob, outcast, Config),
    assert_success(?SET_AFFILIATION_PATH, Res4),
    assert_user_affiliation(RoomJID, Bob, outcast).


user_admin_set_user_affiliation(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}, {kate, 1}],
                               fun user_admin_set_user_affiliation/4).

user_admin_set_user_affiliation(Config, Alice, Bob, Kate) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    user_set_user_affiliation(Alice, RoomJID, Bob, admin, Config),
    % Grant member affiliation
    Res = user_set_user_affiliation(Bob, RoomJID, Kate, member, Config),
    assert_success(?SET_AFFILIATION_PATH, Res),
    assert_user_affiliation(RoomJID, Kate, member),
    % Revoke affiliation
    Res1 = user_set_user_affiliation(Bob, RoomJID, Kate, none, Config),
    assert_success(?SET_AFFILIATION_PATH, Res1),
    assert_user_affiliation(RoomJID, Kate, none),
    % Admin cannot grant admin affiliation
    Res2 = user_set_user_affiliation(Bob, RoomJID, Kate, admin, Config),
    assert_no_permission(Res2),
    % Admin cannot grant owner affiliation
    Res3 = user_set_user_affiliation(Bob, RoomJID, Kate, owner, Config),
    assert_no_permission(Res3),
    % Admin can ban member
    Res4 = user_set_user_affiliation(Bob, RoomJID, Kate, outcast, Config),
    assert_success(?SET_AFFILIATION_PATH, Res4),
    assert_user_affiliation(RoomJID, Kate, outcast),
    % Admin cannot ban owner
    Res5 = user_set_user_affiliation(Bob, RoomJID, Alice, outcast, Config),
    assert_no_permission(Res5).

user_member_set_user_affiliation(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}, {kate, 1}],
                               fun user_member_set_user_affiliation/4).

user_member_set_user_affiliation(Config, Alice, Bob, Kate) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    user_set_user_affiliation(Alice, RoomJID, Bob, member, Config),
    % Member cannot grant member affiliation
    Res = user_set_user_affiliation(Bob, RoomJID, Kate, member, Config),
    assert_no_permission(Res),
    % Member cannot grant member admin affiliation
    Res1 = user_set_user_affiliation(Bob, RoomJID, Kate, admin, Config),
    assert_no_permission(Res1),
    % Member cannot grant member owner affiliation
    Res2 = user_set_user_affiliation(Bob, RoomJID, Kate, owner, Config),
    assert_no_permission(Res2),
    % Member cannot revoke member affiliation
    user_set_user_affiliation(Alice, RoomJID, Kate, member, Config),
    Res3 = user_set_user_affiliation(Bob, RoomJID, Kate, none, Config),
    assert_no_permission(Res3).

user_try_set_nonexistent_room_affiliation(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_set_nonexistent_room_affiliation/2).

user_try_set_nonexistent_room_affiliation(Config, Alice) ->
    Res = user_set_user_affiliation(Alice, ?NONEXISTENT_ROOM, Alice, none, Config),
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
    Res = user_set_user_role(Alice, RoomJID, BobNick, visitor, Config),
    assert_success(?SET_ROLE_PATH, Res),
    assert_user_role(RoomJID, Bob, visitor),
    % Change from visitor to participant
    Res1 = user_set_user_role(Alice, RoomJID, BobNick, participant, Config),
    assert_success(?SET_ROLE_PATH, Res1),
    assert_user_role(RoomJID, Bob, participant),
    % Change from participant to moderator
    Res2 = user_set_user_role(Alice, RoomJID, BobNick, moderator, Config),
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
    Res = user_set_user_role(Bob, RoomJID, KateNick, visitor, Config),
    assert_no_permission(Res),
    % Change from participant to participant with success response
    Res1 = user_set_user_role(Bob, RoomJID, KateNick, participant, Config),
    assert_success(?SET_ROLE_PATH, Res1),
    assert_user_role(RoomJID, Bob, participant),
    % Try change from participant to moderator
    Res2 = user_set_user_role(Bob, RoomJID, KateNick, moderator, Config),
    assert_no_permission(Res2).

user_try_set_nonexistent_room_role(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_set_nonexistent_room_role/2).

user_try_set_nonexistent_room_role(Config, Alice) ->
    Res = user_set_user_role(Alice, ?NONEXISTENT_ROOM, <<"Ali">>, participant, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

user_can_enter_room(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}], fun user_can_enter_room/2).

user_can_enter_room(Config, Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Nick = <<"ali">>,
    JID = jid:from_binary(escalus_utils:jid_to_lower(escalus_client:full_jid(Alice))),
    Resource = escalus_client:resource(Alice),
    Res = user_enter_room(Alice, RoomJID, Nick, Resource, null, Config),
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
    Res = user_enter_room(Alice, RoomJID, Nick, Resource, ?PASSWORD, Config),
    assert_success(?ENTER_ROOM_PATH, Res),
    ?assertMatch([#{nick := Nick, jid := JID}], get_room_users(RoomJID)),
    % Bob try enter room without password
    Res1 = user_enter_room(Bob, RoomJID, <<"Bobek">>, Resource, null, Config),
    assert_success(?ENTER_ROOM_PATH, Res1),
    ?assertMatch([_], get_room_users(RoomJID)),
    % Bob enter room with password
    Res2 = user_enter_room(Bob, RoomJID, <<"Bobek">>, Resource, ?PASSWORD, Config),
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
    Res = user_exit_room(Alice, RoomJID, Nick, Resource, Config),
    assert_success(?EXIT_ROOM_PATH, Res),
    ?assertMatch([], get_room_users(RoomJID)).

user_list_room_affiliations(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_list_room_affiliations/3).

user_list_room_affiliations(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Bobek">>,
    AliceNick = <<"Ali">>,
    enter_room(RoomJID, Bob, BobNick),
    enter_room(RoomJID, Alice, AliceNick),
    AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
    BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
    % List all owners
    Res = user_list_room_affiliations(Alice, RoomJID, owner, Config),
    ?assertMatch([#{<<"jid">> := AliceJID, <<"affiliation">> := <<"OWNER">>}],
                 get_ok_value(?LIST_ROOM_AFFILIATIONS_PATH, Res)),
    % List all members
    user_set_user_affiliation(Alice, RoomJID, Bob, member, Config),
    Res1 = user_list_room_affiliations(Alice, RoomJID, member, Config),
    ?assertMatch([#{<<"jid">> := BobJID, <<"affiliation">> := <<"MEMBER">>}],
                 get_ok_value(?LIST_ROOM_AFFILIATIONS_PATH, Res1)),
    % List all
    Res2 = user_list_room_affiliations(Alice, RoomJID, null, Config),
    ?assertMatch([_, _], get_ok_value(?LIST_ROOM_AFFILIATIONS_PATH, Res2)).

user_try_list_room_affiliations_without_permission(Config) ->
    muc_helper:story_with_room(Config, [{members_only, true}], [{alice, 1}, {bob, 1}],
                               fun user_try_list_room_affiliations_without_permission/3).

user_try_list_room_affiliations_without_permission(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Res = user_list_room_affiliations(Bob, RoomJID, null, Config),
    assert_no_permission(Res).

user_try_list_nonexistent_room_affiliations(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_list_nonexistent_room_affiliations/2).

user_try_list_nonexistent_room_affiliations(Config, Alice) ->
    Res = user_list_room_affiliations(Alice, ?NONEXISTENT_ROOM, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

%% Helpers

assert_no_full_jid(Res) ->
    ?assertNotEqual(nomatch, binary:match(get_coercion_err_msg(Res), <<"jid_without_resource">>)).

assert_no_permission(Res) ->
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not have permission">>)).

assert_success(Path, Res) ->
    ?assertNotEqual(nomatch, binary:match(get_ok_value(Path, Res), <<"successfully">>)).

get_room_affiliation(RoomJID, Aff) ->
    {ok, Affs} = rpc(mim(), mod_muc_api, get_room_affiliations, [RoomJID, Aff]),
    Affs.

get_room_users(RoomJID) ->
    {ok, Users} = rpc(mim(), mod_muc_api, get_room_users, [RoomJID]),
    Users.

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

%% Commands

create_instant_room(MUCDomain, Name, Owner, Nick, Config) ->
    Vars = #{mucDomain => MUCDomain, name => Name, owner => user_to_bin(Owner), nick => Nick},
    execute_command(<<"muc">>, <<"createInstantRoom">>, Vars, Config).

invite_user(Room, Sender, Recipient, Reason, Config) ->
    Vars = #{room => jid:to_binary(Room), sender => user_to_bin(Sender),
             recipient => user_to_bin(Recipient), reason => Reason},
    execute_command(<<"muc">>, <<"inviteUser">>, Vars, Config).

kick_user(Room, Nick, Reason, Config) ->
    Vars = #{room => jid:to_binary(Room), nick => Nick, reason => Reason},
    execute_command(<<"muc">>, <<"kickUser">>, Vars, Config).

send_message_to_room(Room, From, Body, Config) ->
    Vars = #{room => jid:to_binary(Room), from => user_to_full_bin(From), body => Body},
    execute_command(<<"muc">>, <<"sendMessageToRoom">>, Vars, Config).

send_private_message(Room, From, ToNick, Body, Config) ->
    Vars = #{room => jid:to_binary(Room), from => user_to_full_bin(From),
             toNick => ToNick, body => Body},
    execute_command(<<"muc">>, <<"sendPrivateMessage">>, Vars, Config).

enter_room(Room, User, Nick, Password, Config) ->
    Vars = #{room => jid:to_binary(Room), user => user_to_full_bin(User),
             nick => Nick, password => Password},
    execute_command(<<"muc">>, <<"enterRoom">>, Vars, Config).

delete_room(Room, Reason, Config) ->
    Vars = #{room => jid:to_binary(Room), reason => Reason},
    execute_command(<<"muc">>, <<"deleteRoom">>, Vars, Config).

change_room_config(Room, RoomConfig, Config) ->
    Vars = #{room => jid:to_binary(Room), config => RoomConfig},
    execute_command(<<"muc">>, <<"changeRoomConfiguration">>, Vars, Config).

list_rooms(MUCDomain, From, Limit, Index, Config) ->
    Vars = #{mucDomain => MUCDomain, from => user_to_bin(From), limit => Limit, index => Index},
    execute_command(<<"muc">>, <<"listRooms">>, Vars, Config).

get_room_config(Room, Config) ->
    Vars = #{room => jid:to_binary(Room)},
    execute_command(<<"muc">>, <<"getRoomConfig">>, Vars, Config).

list_room_users(RoomJID, Config) ->
    Vars = #{room => jid:to_binary(RoomJID)},
    execute_command(<<"muc">>, <<"listRoomUsers">>, Vars, Config).

get_room_messages(RoomJID, PageSize, Before, Config) ->
    Vars = #{<<"room">> => jid:to_binary(RoomJID), <<"pageSize">> => PageSize,
             <<"before">> => Before},
    execute_command(<<"muc">>, <<"getRoomMessages">>, Vars, Config).

set_user_affiliation(Room, User, Aff, Config) ->
    Vars = #{room => jid:to_binary(Room), user => user_to_bin(User),
             affiliation => atom_to_enum_item(Aff)},
    execute_command(<<"muc">>, <<"setUserAffiliation">>, Vars, Config).

set_user_role(Room, User, Role, Config) ->
    Vars = #{room => jid:to_binary(Room), nick => user_to_bin(User),
             role => atom_to_enum_item(Role)},
    execute_command(<<"muc">>, <<"setUserRole">>, Vars, Config).

exit_room(Room, User, Nick, Config) ->
    Vars = #{room => jid:to_binary(Room), user => user_to_full_bin(User), nick => Nick},
    execute_command(<<"muc">>, <<"exitRoom">>, Vars, Config).

list_room_affiliations(Room, Aff, Config) ->
    Vars = #{room => jid:to_binary(Room), affiliation => atom_to_enum_item(Aff)},
    execute_command(<<"muc">>, <<"listRoomAffiliations">>, Vars, Config).

user_kick_user(User, Room, Nick, Reason, Config) ->
    Vars = #{room => jid:to_binary(Room), nick => Nick, reason => Reason},
    execute_user_command(<<"muc">>, <<"kickUser">>, User, Vars, Config).

user_enter_room(User, Room, Nick, Resource, Password, Config) ->
    Vars = #{room => jid:to_binary(Room), nick => Nick, resource => Resource, password => Password},
    execute_user_command(<<"muc">>, <<"enterRoom">>, User, Vars, Config).

user_get_room_messages(User, RoomJID, PageSize, Before, Config) ->
    Vars = #{<<"room">> => jid:to_binary(RoomJID), <<"pageSize">> => PageSize,
             <<"before">> => Before},
    execute_user_command(<<"muc">>, <<"getRoomMessages">>, User, Vars, Config).

user_delete_room(User, Room, Reason, Config) ->
    Vars = #{room => jid:to_binary(Room), reason => Reason},
    execute_user_command(<<"muc">>, <<"deleteRoom">>, User, Vars, Config).

user_change_room_config(User, Room, RoomConfig, Config) ->
    Vars = #{room => jid:to_binary(Room), config => RoomConfig},
    execute_user_command(<<"muc">>, <<"changeRoomConfiguration">>, User, Vars, Config).

user_list_rooms(User, MUCDomain, Limit, Index, Config) ->
    Vars = #{mucDomain => MUCDomain, limit => Limit, index => Index},
    execute_user_command(<<"muc">>, <<"listRooms">>, User, Vars, Config).

user_get_room_config(User, Room, Config) ->
    Vars = #{room => jid:to_binary(Room)},
    execute_user_command(<<"muc">>, <<"getRoomConfig">>, User, Vars, Config).

user_list_room_users(User, RoomJID, Config) ->
    Vars = #{room => jid:to_binary(RoomJID)},
    execute_user_command(<<"muc">>, <<"listRoomUsers">>, User, Vars, Config).

user_send_message_to_room(User, Room, Body, Resource, Config) ->
    Vars = #{room => jid:to_binary(Room), body => Body, resource => Resource},
    execute_user_command(<<"muc">>, <<"sendMessageToRoom">>, User, Vars, Config).

user_send_private_message(User, Room, Body, ToNick, Resource, Config) ->
    Vars = #{room => jid:to_binary(Room), body => Body, toNick => ToNick, resource => Resource},
    execute_user_command(<<"muc">>, <<"sendPrivateMessage">>, User, Vars, Config).

user_create_instant_room(User, MUCDomain, Name, Nick, Config) ->
    Vars = #{mucDomain => MUCDomain, name => Name, nick => Nick},
    execute_user_command(<<"muc">>, <<"createInstantRoom">>, User, Vars, Config).

user_invite_user(User, Room, Recipient, Reason, Config) ->
    Vars = #{room => jid:to_binary(Room), recipient => user_to_bin(Recipient), reason => Reason},
    execute_user_command(<<"muc">>, <<"inviteUser">>, User, Vars, Config).

user_set_user_affiliation(User, Room, QueriedUser, Aff, Config) ->
    Vars = #{room => jid:to_binary(Room), user => user_to_bin(QueriedUser),
             affiliation => atom_to_enum_item(Aff)},
    execute_user_command(<<"muc">>, <<"setUserAffiliation">>, User, Vars, Config).

user_set_user_role(User, Room, QueriedUser, Role, Config) ->
    Vars = #{room => jid:to_binary(Room), nick => user_to_bin(QueriedUser),
             role => atom_to_enum_item(Role)},
    execute_user_command(<<"muc">>, <<"setUserRole">>, User, Vars, Config).

user_exit_room(User, Room, Nick, Resource, Config) ->
    Vars = #{room => jid:to_binary(Room), resource => Resource, nick => Nick},
    execute_user_command(<<"muc">>, <<"exitRoom">>, User, Vars, Config).

user_list_room_affiliations(User, Room, Aff, Config) ->
    Vars = #{room => jid:to_binary(Room), affiliation => atom_to_enum_item(Aff)},
    execute_user_command(<<"muc">>, <<"listRoomAffiliations">>, User, Vars, Config).
