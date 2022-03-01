-module(graphql_muc_light_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [execute/3, execute_auth/2, get_listener_port/1,
                         get_listener_config/1, get_ok_value/2, get_err_msg/1,
                         make_creds/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("jid/include/jid.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-define(UNKNOWN_DOMAIN, <<"not-existing-domain">>).
-define(UNKNOWN, <<"not-existing">>).

%% GraphQL Paths
-define(CREATE_ROOM_PATH, [data, muc_light, createRoom]).
-define(CHANGE_CONFIG_PATH, [data, muc_light, changeRoomConfiguration]).
-define(INVITE_USER_PATH, [data, muc_light, inviteUser]).
-define(KICK_USER_PATH, [data, muc_light, kickUser]).
-define(DELETE_ROOM_PATH, [data, muc_light, deleteRoom]).
-define(SEND_MESSAGE_PATH, [data, muc_light, sendMessageToRoom]).
-define(GET_MESSAGES_PATH, [data, muc_light, getRoomMessages]).
-define(LIST_USER_ROOMS_PATH, [data, muc_light, listUserRooms]).
-define(USER_LIST_ROOMS_PATH, [data, muc_light, listRooms]).
-define(LIST_ROOM_USERS_PATH, [data, muc_light, listRoomUsers]).
-define(GET_ROOM_CONFIG_PATH, [data, muc_light, getRoomConfig]).
-define(GET_BLOCKING_LIST_PATH, [data, muc_light, getBlockingList]).
-define(SET_BLOCKING_LIST_PATH, [data, muc_light, setBlockingList]).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user_muc_light},
     {group, admin_muc_light}].

groups() ->
    [{user_muc_light, [parallel], user_muc_light_handler()},
     {admin_muc_light, [parallel], admin_muc_light_handler()}].

user_muc_light_handler() ->
    [user_create_room,
     user_create_identified_room,
     user_change_room_config,
     user_change_room_config_errors,
     user_invite_user,
     user_invite_user_errors,
     user_delete_room,
     user_kick_user,
     user_send_message_to_room,
     user_send_message_to_room_errors,
     user_get_room_messages,
     user_list_rooms,
     user_list_room_users,
     user_get_room_config,
     user_blocking_list
    ].

admin_muc_light_handler() ->
    [admin_create_room,
     admin_create_identified_room,
     admin_change_room_config,
     admin_change_room_config_errors,
     admin_invite_user,
     admin_invite_user_errors,
     admin_delete_room,
     admin_kick_user,
     admin_send_message_to_room,
     admin_send_message_to_room_errors,
     admin_get_room_messages,
     admin_list_user_rooms,
     admin_list_room_users,
     admin_get_room_config,
     admin_blocking_list
    ].

init_per_suite(Config) ->
    Config1 = init_modules(Config),
    [{muc_light_host, muc_light_helper:muc_host()}
     | escalus:init_per_suite(Config1)].

end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_modules(Config) ->
    HostType = domain_helper:host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config),
    Config2 = rest_helper:maybe_enable_mam(mam_helper:backend(), HostType, Config1),
    dynamic_modules:ensure_modules(HostType, required_modules(suite)),
    Config2.

required_modules(_) ->
    [{mod_muc_light, common_muc_light_opts()}].

common_muc_light_opts() ->
    MucPattern = distributed_helper:subhost_pattern(muc_light_helper:muc_host_pattern()),
    [{host, MucPattern},
     {rooms_in_rosters, true}].

init_per_group(admin_muc_light, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(user_muc_light, Config) ->
    [{schema_endpoint, user} | Config].

end_per_group(_GN, Config) ->
    Config.

init_per_testcase(TC, Config) ->
    rest_helper:maybe_skip_mam_test_cases(TC, [user_get_room_messages,
                                               admin_get_room_messages], Config).

end_per_testcase(TC, Config) ->
    escalus:end_per_testcase(TC, Config).

%% User test cases

user_create_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_create_room_story/2).

user_create_room_story(Config, Alice) ->
    Ep = ?config(schema_endpoint, Config),
    MucServer = ?config(muc_light_host, Config),
    AliceBinLower = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
    Creds = make_creds(Alice),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    Res = execute(Ep, user_create_room_body(MucServer, Name, Subject, null), Creds),
    #{<<"jid">> := JID, <<"name">> := Name, <<"subject">> := Subject,
      <<"participants">> := Participants} = get_ok_value(?CREATE_ROOM_PATH, Res),
    ?assertMatch(#jid{server = MucServer}, jid:from_binary(JID)),
    ?assertEqual([#{<<"jid">> => AliceBinLower, <<"affiliation">> => <<"OWNER">>}], Participants),
    % Try with a non-existing domain
    Res2 = execute(Ep, user_create_room_body(?UNKNOWN_DOMAIN, Name, Subject, null), Creds),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)).

user_create_identified_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_create_identified_room_story/2).

user_create_identified_room_story(Config, Alice) ->
    Ep = ?config(schema_endpoint, Config),
    MucServer = ?config(muc_light_host, Config),
    Creds = make_creds(Alice),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    Id = <<"my_user_room">>,
    Res = execute(Ep, user_create_room_body(MucServer, Name, Subject, Id), Creds),
    #{<<"jid">> := JID, <<"name">> := Name, <<"subject">> := Subject} = get_ok_value(?CREATE_ROOM_PATH, Res),
    ?assertMatch(#jid{user = Id, server = MucServer}, jid:from_binary(JID)),
    % Create a room with an existing ID
    Res2 = execute(Ep, user_create_room_body(MucServer, <<"snd room">>, Subject, Id), Creds),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"already exists">>)),
    % Try with a non-existing domain
    Res3 = execute(Ep, user_create_room_body(?UNKNOWN_DOMAIN, <<"name">>, Subject, Id), Creds),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not found">>)).

user_change_room_config(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_change_room_config_story/2).

user_change_room_config_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    Ep = ?config(schema_endpoint, Config),
    MUCServer = ?config(muc_light_host, Config),
    Creds = make_creds(Alice),
    % Create a new room
    {ok, #{jid := RoomJID}} = create_room(<<>>, MUCServer, <<"ornithology">>, <<"birds">>, AliceBin),
    % Try to change the room configuration
    Name2 = <<"changed room">>,
    Subject2 = <<"not testing">>,
    Res = execute(Ep, user_change_room_configuration_body(jid:to_binary(RoomJID), Name2, Subject2), Creds),
    ?assertMatch(#{<<"name">> := Name2, <<"subject">> := Subject2}, get_ok_value(?CHANGE_CONFIG_PATH, Res)).

user_change_room_config_errors(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_change_room_config_errors_story/3).

user_change_room_config_errors_story(Config, Alice, Bob) ->
    Ep = ?config(schema_endpoint, Config),
    MUCServer = ?config(muc_light_host, Config),
    CredsAlice = make_creds(Alice),
    CredsBob = make_creds(Bob),
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    RoomName = <<"first room">>,
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(<<>>, MUCServer, RoomName, <<"subject">>, AliceBin),
    % Try to change the config with a non-existing domain
    Res = execute(Ep, user_change_room_configuration_body(
                        make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), RoomName, <<"subject2">>), CredsAlice),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)),
    % Try to change the config of the non-existing room
    Res2 = execute(Ep, user_change_room_configuration_body(
                         make_bare_jid(?UNKNOWN, MUCServer), RoomName, <<"subject2">>), CredsAlice),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)),
    % Try to change the config by the user that not occupy this room
    Res3 = execute(Ep, user_change_room_configuration_body(
                         jid:to_binary(RoomJID), RoomName, <<"subject2">>), CredsBob),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not occupy this room">>)),
    % Try to change a config by the user without permission
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    Res4 = execute(Ep, user_change_room_configuration_body(
                         jid:to_binary(RoomJID), RoomName, <<"subject2">>), CredsBob),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res4),
                                          <<"does not have permission to change">>)).

user_invite_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun user_invite_user_story/3).

user_invite_user_story(Config, Alice, Bob) ->
    Ep = ?config(schema_endpoint, Config),
    MUCServer = ?config(muc_light_host, Config),
    CredsAlice = make_creds(Alice),
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    Domain = escalus_client:server(Alice),
    Name = <<"first room">>,
    {ok, #{jid := RoomJID}} = create_room(<<>>, MUCServer, Name, <<"subject2">>, AliceBin),
    % Room owner can invite a user
    Res = execute(Ep, user_invite_user_body(jid:to_binary(RoomJID), BobBin), CredsAlice),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?INVITE_USER_PATH, Res),
                                          <<"successfully">>)),
    escalus:wait_for_stanza(Bob),
    BobName = escalus_utils:jid_to_lower(escalus_client:username(Bob)),
    AliceName = escalus_utils:jid_to_lower(escalus_client:username(Alice)),
    ExpectedAff = lists:sort([{{AliceName, Domain}, owner},
                              {{BobName, Domain}, member}]),
    ?assertMatch(ExpectedAff, lists:sort(get_room_aff(RoomJID))).

user_invite_user_errors(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_invite_user_errors_story/3).

user_invite_user_errors_story(Config, Alice, Bob) ->
    Ep = ?config(schema_endpoint, Config),
    MUCServer = ?config(muc_light_host, Config),
    CredsAlice = make_creds(Alice),
    CredsBob = make_creds(Bob),
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(<<>>, MUCServer, <<"first room">>, <<"subject">>, AliceBin),
    % Try to invite a user to not existing room
    Res = execute(Ep, user_invite_user_body(
                        make_bare_jid(?UNKNOWN, MUCServer), BobBin), CredsAlice),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not occupy this room">>)),
    % User without rooms tries to invite a user
    Res2 = execute(Ep, user_invite_user_body(
                         jid:to_binary(RoomJID), AliceBin), CredsBob),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not occupy this room">>)),
    % Try with a non-existing domain
    Res3 = execute(Ep, user_invite_user_body(
                         make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), BobBin), CredsAlice),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not found">>)).

user_delete_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun user_delete_room_story/3).

user_delete_room_story(Config, Alice, Bob) ->
    Ep = ?config(schema_endpoint, Config),
    MUCServer = ?config(muc_light_host, Config),
    CredsAlice = make_creds(Alice),
    CredsBob = make_creds(Bob),
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    Name = <<"first room">>,
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(<<>>, MUCServer, Name, <<"subject">>, AliceBin),
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    % Member cannot delete room
    Res = execute(Ep, delete_room_body(jid:to_binary(RoomJID)), CredsBob),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res),
                                          <<"cannot delete this room">>)),
    % Owner can delete own room
    Res2 = execute(Ep, delete_room_body(jid:to_binary(RoomJID)), CredsAlice),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?DELETE_ROOM_PATH, Res2),
                                          <<"successfully">>)),
    ?assertEqual({error, not_exists}, get_room_info(jid:from_binary(RoomJID))),
    % Try with a non-existing domain
    Res3 = execute(Ep, delete_room_body(make_bare_jid(RoomID, ?UNKNOWN_DOMAIN)), CredsAlice),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not found">>)),
    % Try with a non-existing room
    Res4 = execute(Ep, delete_room_body(make_bare_jid(?UNKNOWN, MUCServer)), CredsAlice),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res4), <<"not existing room">>)).

user_kick_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun user_kick_user_story/3).

user_kick_user_story(Config, Alice, Bob) ->
    Ep = ?config(schema_endpoint, Config),
    MUCServer = ?config(muc_light_host, Config),
    CredsAlice = make_creds(Alice),
    CredsBob = make_creds(Bob),
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    RoomName = <<"first room">>,
    {ok, #{jid := RoomJID}} = create_room(<<>>, MUCServer, RoomName, <<"subject">>, AliceBin),
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    % Member kicks himself from a room
    ?assertEqual(2, length(get_room_aff(RoomJID))),
    Res = execute(Ep, user_kick_user_body(jid:to_binary(RoomJID), null), CredsBob),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?KICK_USER_PATH, Res),
                                          <<"successfully">>)),
    ?assertEqual(1, length(get_room_aff(RoomJID))),
    % Owner kicks the member from a room
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    Res2 = execute(Ep, user_kick_user_body(jid:to_binary(RoomJID), BobBin), CredsAlice),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?KICK_USER_PATH, Res2),
                                          <<"successfully">>)),
    ?assertEqual(1, length(get_room_aff(RoomJID))).

user_send_message_to_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_send_message_to_room_story/3).

user_send_message_to_room_story(Config, Alice, Bob) ->
    Ep = ?config(schema_endpoint, Config),
    MUCServer = ?config(muc_light_host, Config),
    CredsAlice = make_creds(Alice),
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    RoomName = <<"first room">>,
    MsgBody = <<"Hello there!">>,
    {ok, #{jid := RoomJID}} = create_room(<<>>, MUCServer, RoomName, <<"subject">>, AliceBin),
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    Res = execute(Ep, user_send_message_to_room_body(jid:to_binary(RoomJID), MsgBody), CredsAlice),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SEND_MESSAGE_PATH, Res),
                                          <<"successfully">>)),
    [_, Msg] = escalus:wait_for_stanzas(Bob, 2),
    escalus:assert(is_message, Msg).

user_send_message_to_room_errors(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_send_message_to_room_errors_story/3).

user_send_message_to_room_errors_story(Config, Alice, Bob) ->
    Ep = ?config(schema_endpoint, Config),
    MUCServer = ?config(muc_light_host, Config),
    CredsAlice = make_creds(Alice),
    CredsBob = make_creds(Bob),
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    MsgBody = <<"Hello there!">>,
    {ok, #{jid := #jid{luser = ARoomID} = ARoomJID}} =
        create_room(<<>>, MUCServer, <<"alice room">>, <<"subject">>, AliceBin),
    % Try with a non-existing domain
    Res = execute(Ep, user_send_message_to_room_body(
                        make_bare_jid(ARoomID, ?UNKNOWN_DOMAIN), MsgBody), CredsAlice),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)),
    % Try with a user without rooms
    Res2 = execute(Ep, user_send_message_to_room_body(
                         jid:to_binary(ARoomJID), MsgBody), CredsBob),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not occupy this room">>)),
    % Try with a room not occupied by this user
    {ok, #{jid := _RoomJID2}} = create_room(<<>>, MUCServer, <<"bob room">>, <<"subject">>, BobBin),
    Res3 = execute(Ep, user_send_message_to_room_body(
                         jid:to_binary(ARoomJID), MsgBody), CredsBob),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not occupy this room">>)).

user_get_room_messages(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun user_get_room_messages_story/3).

user_get_room_messages_story(Config, Alice, Bob) ->
    Ep = ?config(schema_endpoint, Config),
    MUCServer = ?config(muc_light_host, Config),
    CredsAlice = make_creds(Alice),
    CredsBob = make_creds(Bob),
    AliceBin = escalus_client:short_jid(Alice),
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(<<>>, MUCServer, <<"first room">>, <<"subject">>, AliceBin),
    Message = <<"Hello friends">>,
    send_message_to_room(RoomJID, jid:from_binary(AliceBin), Message),
    mam_helper:maybe_wait_for_archive(Config),
    % Get messages so far
    Limit = 40,
    Res = execute(Ep, get_room_messages_body(jid:to_binary(RoomJID), Limit, null), CredsAlice),
    #{<<"stanzas">> :=[#{<<"stanza">> := StanzaXML}], <<"limit">> := Limit} =
        get_ok_value(?GET_MESSAGES_PATH, Res),
    ?assertMatch({ok, #xmlel{name = <<"message">>}}, exml:parse(StanzaXML)),
    % Get messages before the given date and time
    Before = <<"2022-02-17T04:54:13+00:00">>,
    Res2 = execute(Ep, get_room_messages_body(jid:to_binary(RoomJID), null, Before), CredsAlice),
    ?assertMatch(#{<<"stanzas">> := [], <<"limit">> := 50}, get_ok_value(?GET_MESSAGES_PATH, Res2)),
    % Try to pass too big page size value
    Res3 = execute(Ep, get_room_messages_body(jid:to_binary(RoomJID), 51, Before), CredsAlice),
    ?assertMatch(#{<<"limit">> := 50}, get_ok_value(?GET_MESSAGES_PATH, Res3)),
    % Try with a non-existing domain
    Res4 = execute(Ep, get_room_messages_body(
                          make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), Limit, null), CredsAlice),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res4), <<"not found">>)),
    % Try with a user that is not a room member
    Res5 = execute(Ep, get_room_messages_body(
                          jid:to_binary(RoomJID), Limit, null), CredsBob),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res5), <<"not occupy this room">>)).

user_list_rooms(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_list_rooms_story/2).

user_list_rooms_story(Config, Alice) ->
    Ep = ?config(schema_endpoint, Config),
    MUCServer = ?config(muc_light_host, Config),
    CredsAlice = make_creds(Alice),
    AliceBin = escalus_client:short_jid(Alice),
    {ok, #{jid := RoomJID}} = create_room(<<>>, MUCServer, <<"room a">>, <<"subject">>, AliceBin),
    {ok, #{jid := RoomJID2}} = create_room(<<>>, MUCServer, <<"room b">>, <<"subject">>, AliceBin),
    Res = execute(Ep, user_list_rooms_body(), CredsAlice),
    ?assertEqual(lists:sort([jid:to_binary(RoomJID), jid:to_binary(RoomJID2)]),
                 lists:sort(get_ok_value(?USER_LIST_ROOMS_PATH, Res))).

user_list_room_users(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun user_list_room_users_story/3).

user_list_room_users_story(Config, Alice, Bob) ->
    Ep = ?config(schema_endpoint, Config),
    MUCServer = ?config(muc_light_host, Config),
    CredsAlice = make_creds(Alice),
    CredsBob = make_creds(Bob),
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    AliceLower = escalus_utils:jid_to_lower(AliceBin),
    {ok, #{jid := RoomJID}} = create_room(<<>>, MUCServer, <<"room a">>, <<"subject">>, AliceBin),
    % Owner can list rooms
    Res = execute(Ep, list_room_users_body(jid:to_binary(RoomJID)), CredsAlice),
    ?assertEqual([#{<<"jid">> => AliceLower, <<"affiliation">> => <<"OWNER">>}],
                 get_ok_value(?LIST_ROOM_USERS_PATH, Res)),
    % Try with a non-existing domain
    Res2 = execute(Ep, list_room_users_body(
                         make_bare_jid(RoomJID#jid.luser, ?UNKNOWN_DOMAIN)), CredsAlice),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)),
    % Try with a non-existing room
    Res3 = execute(Ep, list_room_users_body(
                         make_bare_jid(?UNKNOWN, MUCServer)), CredsAlice),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not found">>)),
    % User that is not a member cannot get aff
    Res4 = execute(Ep, list_room_users_body(jid:to_binary(RoomJID)), CredsBob),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res4), <<"not occupy this room">>)),
    % Member can get aff
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    escalus:wait_for_stanza(Bob),
    Res5 = execute(Ep, list_room_users_body(jid:to_binary(RoomJID)), CredsBob),
    ?assertMatch([_, _], get_ok_value(?LIST_ROOM_USERS_PATH, Res5)).

user_get_room_config(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun user_get_room_config_story/3).

user_get_room_config_story(Config, Alice, Bob) ->
    Ep = ?config(schema_endpoint, Config),
    MUCServer = ?config(muc_light_host, Config),
    CredsAlice = make_creds(Alice),
    CredsBob = make_creds(Bob),
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    AliceLower = escalus_utils:jid_to_lower(AliceBin),
    BobLower = escalus_utils:jid_to_lower(BobBin),
    RoomName = <<"first room">>,
    RoomSubject = <<"Room about nothing">>,
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(<<>>, MUCServer, RoomName, RoomSubject, AliceBin),
    RoomJIDBin = jid:to_binary(RoomJID),
    Res = execute(Ep, get_room_config_body(jid:to_binary(RoomJID)), CredsAlice),
    % Owner can get a config
    ?assertEqual(#{<<"jid">> => RoomJIDBin, <<"subject">> => RoomSubject, <<"name">> => RoomName,
                    <<"participants">> => [#{<<"jid">> => AliceLower,
                                             <<"affiliation">> => <<"OWNER">>}]},
                 get_ok_value(?GET_ROOM_CONFIG_PATH, Res)),
    % Try with a non-existing domain
    Res2 = execute(Ep, get_room_config_body(make_bare_jid(RoomID, ?UNKNOWN_DOMAIN)), CredsAlice),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)),
    % Try with a non-existing room
    Res3 = execute(Ep, get_room_config_body(make_bare_jid(?UNKNOWN, MUCServer)), CredsAlice),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not found">>)),
    % User that is not a member cannot get a room config
    Res4 = execute(Ep, get_room_config_body(jid:to_binary(RoomJID)), CredsBob),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res4), <<"not occupy this room">>)),
    % Member can get a config
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    Res5 = execute(Ep, get_room_config_body(jid:to_binary(RoomJID)), CredsAlice),
    ?assertEqual(#{<<"jid">> => RoomJIDBin, <<"subject">> => RoomSubject, <<"name">> => RoomName,
                    <<"participants">> => [#{<<"jid">> => AliceLower,
                                             <<"affiliation">> => <<"OWNER">>},
                                           #{<<"jid">> => BobLower,
                                             <<"affiliation">> => <<"MEMBER">>}]},
                 get_ok_value(?GET_ROOM_CONFIG_PATH, Res5)).

user_blocking_list(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun user_blocking_list_story/3).

user_blocking_list_story(Config, Alice, Bob) ->
    Ep = ?config(schema_endpoint, Config),
    CredsAlice = make_creds(Alice),
    BobBin = escalus_client:full_jid(Bob),
    BobShortBin = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
    {ok, #{jid := RoomJID}} = create_room(<<>>, ?config(muc_light_host, Config),
                                          <<"room">>, <<"subject">>, BobBin),
    RoomBin = jid:to_binary(RoomJID),
    Res = execute(Ep, user_get_blocking_body(), CredsAlice),
    ?assertMatch([], get_ok_value(?GET_BLOCKING_LIST_PATH, Res)),
    Res2 = execute(Ep, user_set_blocking_body([{<<"USER">>, <<"DENY">>, BobBin}]), CredsAlice),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SET_BLOCKING_LIST_PATH, Res2),
                                          <<"successfully">>)),
    Res3 = execute(Ep, user_get_blocking_body(), CredsAlice),
    ?assertEqual([#{<<"what">> => <<"USER">>,
                    <<"action">> => <<"DENY">>,
                    <<"who">> => BobShortBin}],
                 get_ok_value(?GET_BLOCKING_LIST_PATH, Res3)),
    Res4 = execute(Ep, user_set_blocking_body([{<<"USER">>, <<"ALLOW">>, BobBin},
                                               {<<"ROOM">>, <<"DENY">>, jid:to_binary(RoomJID)}]),
                   CredsAlice),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SET_BLOCKING_LIST_PATH, Res4),
                                          <<"successfully">>)),
    Res5 = execute(Ep, user_get_blocking_body(), CredsAlice),
    ?assertEqual([#{<<"what">> => <<"ROOM">>,
                    <<"action">> => <<"DENY">>,
                    <<"who">> => RoomBin}],
                 get_ok_value(?GET_BLOCKING_LIST_PATH, Res5)).

%% Admin test cases

admin_blocking_list(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun admin_blocking_list_story/3).

admin_blocking_list_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:full_jid(Alice),
    BobBin = escalus_client:full_jid(Bob),
    BobShortBin = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
    Res = execute_auth(admin_get_user_blocking_body(AliceBin), Config),
    ?assertMatch([], get_ok_value(?GET_BLOCKING_LIST_PATH, Res)),
    Res2 = execute_auth(admin_set_blocking_body(
                          AliceBin, [{<<"USER">>, <<"DENY">>, BobBin}]), Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SET_BLOCKING_LIST_PATH, Res2),
                                          <<"successfully">>)),
    Res3 = execute_auth(admin_get_user_blocking_body(AliceBin), Config),
    ?assertEqual([#{<<"what">> => <<"USER">>,
                    <<"action">> => <<"DENY">>,
                    <<"who">> => BobShortBin}],
                 get_ok_value(?GET_BLOCKING_LIST_PATH, Res3)),
    Res4 = execute_auth(admin_set_blocking_body(
                          AliceBin, [{<<"USER">>, <<"ALLOW">>, BobBin}]), Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SET_BLOCKING_LIST_PATH, Res4),
                                          <<"successfully">>)),
    Res5 = execute_auth(admin_get_user_blocking_body(AliceBin), Config),
    ?assertMatch([], get_ok_value(?GET_BLOCKING_LIST_PATH, Res5)),
    % Check whether errors are handled correctly
    InvalidUser = make_bare_jid(?UNKNOWN, ?UNKNOWN_DOMAIN),
    Res6 = execute_auth(admin_get_user_blocking_body(InvalidUser), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res6), <<"not found">>)),
    Res7 = execute_auth(admin_set_blocking_body(InvalidUser, []), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res7), <<"not found">>)).

admin_create_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_create_room_story/2).

admin_create_room_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    AliceBinLower = escalus_utils:jid_to_lower(AliceBin),
    MucServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    Res = execute_auth(admin_create_room_body(MucServer, Name, AliceBin, Subject, null), Config),
    Path = [data, muc_light, createRoom],
    #{<<"jid">> := JID, <<"name">> := Name, <<"subject">> := Subject,
      <<"participants">> := Participants} = get_ok_value(Path, Res),
    ?assertMatch(#jid{server = MucServer}, jid:from_binary(JID)),
    ?assertEqual([#{<<"jid">> => AliceBinLower, <<"affiliation">> => <<"OWNER">>}], Participants),
    % Try with a non-existing domain
    Res2 = execute_auth(admin_create_room_body(?UNKNOWN_DOMAIN, Name, AliceBin, Subject, null),
                        Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)).

admin_create_identified_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_create_identified_room_story/2).

admin_create_identified_room_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    MucServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    Id = <<"my_room">>,
    Res = execute_auth(admin_create_room_body(MucServer, Name, AliceBin, Subject, Id), Config),
    Path = [data, muc_light, createRoom],
    #{<<"jid">> := JID, <<"name">> := Name, <<"subject">> := Subject} = get_ok_value(Path, Res),
    ?assertMatch(#jid{user = Id, server = MucServer}, jid:from_binary(JID)),
    % Create a room with an existing ID
    Res2 = execute_auth(admin_create_room_body(MucServer, <<"snd room">>, AliceBin, Subject, Id),
                        Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"already exists">>)),
    % Try with a non-existing domain
    Res3 = execute_auth(admin_create_room_body(?UNKNOWN_DOMAIN, <<"name">>, AliceBin, Subject, Id),
                        Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not found">>)).

admin_change_room_config(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_change_room_config_story/2).

admin_change_room_config_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    MUCServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    % Create a new room
    {ok, #{jid := RoomJID}} = create_room(<<>>, MUCServer, Name, Subject, AliceBin),
    % Try to change the room configuration
    Name2 = <<"changed room">>,
    Subject2 = <<"not testing">>,
    Res = execute_auth(admin_change_room_configuration_body(jid:to_binary(RoomJID),
                                                            AliceBin, Name2, Subject2), Config),
    Path = [data, muc_light, changeRoomConfiguration],
    ?assertMatch(#{<<"name">> := Name2, <<"subject">> := Subject2}, get_ok_value(Path, Res)).

admin_change_room_config_errors(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_change_room_config_errors_story/3).

admin_change_room_config_errors_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    MUCServer = ?config(muc_light_host, Config),
    RoomName = <<"first room">>,
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(<<>>, MUCServer, RoomName, <<"subject">>, AliceBin),
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    % Try to change the config with a non-existing domain
    Res = execute_auth(admin_change_room_configuration_body(
                         make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), AliceBin, RoomName, <<"subject2">>), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)),
    % Try to change the config of the non-existing room
    Res2 = execute_auth(admin_change_room_configuration_body(
                          make_bare_jid(<<"unknown">>, MUCServer), AliceBin,
                          RoomName, <<"subject2">>), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)),
    % Try to change the config by the non-existing user
    Res3 = execute_auth(admin_change_room_configuration_body(
                          jid:to_binary(RoomJID), <<"wrong-user@wrong-domain">>,
                          RoomName, <<"subject2">>), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not occupy this room">>)),
    % Try to change a config by the user without permission
    Res4 = execute_auth(admin_change_room_configuration_body(
                          jid:to_binary(RoomJID), BobBin, RoomName, <<"subject2">>), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res4),
                                          <<"does not have permission to change">>)).

admin_invite_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun admin_invite_user_story/3).

admin_invite_user_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    Domain = escalus_client:server(Alice),
    MUCServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    {ok, #{jid := RoomJID}} = create_room(<<>>, MUCServer, Name, <<"subject2">>, AliceBin),

    Res = execute_auth(admin_invite_user_body(jid:to_binary(RoomJID), AliceBin, BobBin), Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value([data, muc_light, inviteUser], Res),
                                          <<"successfully">>)),
    BobName = escalus_utils:jid_to_lower(escalus_client:username(Bob)),
    AliceName = escalus_utils:jid_to_lower(escalus_client:username(Alice)),
    ExpectedAff = lists:sort([{{AliceName, Domain}, owner},
                              {{BobName, Domain}, member}]),
    ?assertMatch(ExpectedAff, lists:sort(get_room_aff(RoomJID))).

admin_invite_user_errors(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_invite_user_errors_story/3).

admin_invite_user_errors_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    MUCServer = ?config(muc_light_host, Config),
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(<<>>, MUCServer, <<"first room">>, <<"subject">>, AliceBin),
    % Try to invite a user to not existing room
    Res = execute_auth(admin_invite_user_body(
                         make_bare_jid(?UNKNOWN, MUCServer), AliceBin, BobBin), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not occupy this room">>)),
    % User without rooms tries to invite a user
    Res2 = execute_auth(admin_invite_user_body(
                          jid:to_binary(RoomJID), BobBin, AliceBin), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not occupy this room">>)),
    % Try with a non-existing domain
    Res3 = execute_auth(admin_invite_user_body(
                          make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), AliceBin, BobBin), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not found">>)).

admin_delete_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_delete_room_story/2).

admin_delete_room_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    MUCServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(<<>>, MUCServer, Name, <<"subject">>, AliceBin),
    Res = execute_auth(delete_room_body(jid:to_binary(RoomJID)), Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?DELETE_ROOM_PATH, Res),
                                          <<"successfully">>)),
    ?assertEqual({error, not_exists}, get_room_info(jid:from_binary(RoomJID))),
    % Try with a non-existing domain
    Res2 = execute_auth(delete_room_body(make_bare_jid(RoomID, ?UNKNOWN_DOMAIN)), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)),
    % Try with a non-existing room
    Res3 = execute_auth(delete_room_body(make_bare_jid(?UNKNOWN, MUCServer)), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not found">>)).

admin_kick_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun admin_kick_user_story/3).

admin_kick_user_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    MUCServer = ?config(muc_light_host, Config),
    RoomName = <<"first room">>,
    RoomID = <<"kick_user_test_room">>,
    {ok, #{jid := RoomJID}} = create_room(RoomID, MUCServer, RoomName, <<"subject">>, AliceBin),
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    ?assertEqual(2, length(get_room_aff(RoomJID))),
    Res = execute_auth(admin_kick_user_body(jid:to_binary(RoomJID), BobBin), Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value([data, muc_light, kickUser], Res),
                                          <<"successfully">>)),
    ?assertEqual(1, length(get_room_aff(RoomJID))).

admin_send_message_to_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_send_message_to_room_story/3).

admin_send_message_to_room_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    MUCServer = ?config(muc_light_host, Config),
    RoomName = <<"first room">>,
    MsgBody = <<"Hello there!">>,
    {ok, #{jid := RoomJID}} = create_room(<<>>, MUCServer, RoomName, <<"subject">>, AliceBin),
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    Res = execute_auth(admin_send_message_to_room_body(
                         jid:to_binary(RoomJID), AliceBin, MsgBody), Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SEND_MESSAGE_PATH, Res),
                                          <<"successfully">>)),
    [_, Msg] = escalus:wait_for_stanzas(Bob, 2),
    escalus:assert(is_message, Msg).

admin_send_message_to_room_errors(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_send_message_to_room_errors_story/3).

admin_send_message_to_room_errors_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    MUCServer = ?config(muc_light_host, Config),
    MsgBody = <<"Hello there!">>,
    {ok, #{jid := #jid{luser = ARoomID} = ARoomJID}} =
        create_room(<<>>, MUCServer, <<"alice room">>, <<"subject">>, AliceBin),
    % Try with a non-existing domain
    Res2 = execute_auth(admin_send_message_to_room_body(
                          make_bare_jid(ARoomID, ?UNKNOWN_DOMAIN), AliceBin, MsgBody), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)),
    % Try with a user without rooms
    Res3 = execute_auth(admin_send_message_to_room_body(
                          jid:to_binary(ARoomJID), BobBin, MsgBody), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"does not occupy this room">>)),
    % Try with a room not occupied by this user
    {ok, #{jid := _RoomJID2}} = create_room(<<>>, MUCServer, <<"bob room">>, <<"subject">>, BobBin),
    Res4 = execute_auth(admin_send_message_to_room_body(
                          jid:to_binary(ARoomJID), BobBin, MsgBody), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res4), <<"does not occupy this room">>)).

admin_get_room_messages(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_get_room_messages_story/2).

admin_get_room_messages_story(Config, Alice) ->
    Path = [data, muc_light, getRoomMessages, stanzas],
    AliceBin = escalus_client:short_jid(Alice),
    %Domain = escalus_client:server(Alice),
    MUCServer = ?config(muc_light_host, Config),
    RoomName = <<"first room">>,
    RoomName2 = <<"second room">>,
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(<<>>, MUCServer, RoomName, <<"subject">>, AliceBin),
    {ok, _} = create_room(<<>>, MUCServer, RoomName2, <<"subject">>, AliceBin),
    Message = <<"Hello friends">>,
    send_message_to_room(RoomJID, jid:from_binary(AliceBin), Message),
    mam_helper:maybe_wait_for_archive(Config),
    % Get messages so far
    Res = execute_auth(get_room_messages_body(jid:to_binary(RoomJID), 50, null), Config),
    [#{<<"stanza">> := StanzaXML}] = get_ok_value(Path, Res),
    ?assertMatch({ok, #xmlel{name = <<"message">>}}, exml:parse(StanzaXML)),
    % Get messages before the given date and time
    Before = <<"2022-02-17T04:54:13+00:00">>,
    Res2 = execute_auth(get_room_messages_body(jid:to_binary(RoomJID), 50, Before), Config),
    ?assertMatch([], get_ok_value(Path, Res2)),
    % Try with a non-existing domain
    Res3 = execute_auth(get_room_messages_body(
                          make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), 50, null), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not found">>)).

admin_list_user_rooms(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_list_user_rooms_story/2).

admin_list_user_rooms_story(Config, Alice) ->
    Path = [data, muc_light, listUserRooms],
    AliceBin = escalus_client:short_jid(Alice),
    Domain = escalus_client:server(Alice),
    MUCServer = ?config(muc_light_host, Config),
    RoomName = <<"first room">>,
    RoomName2 = <<"second room">>,
    {ok, #{jid := RoomJID}} = create_room(<<>>, MUCServer, RoomName, <<"subject">>, AliceBin),
    {ok, #{jid := RoomJID2}} = create_room(<<>>, MUCServer, RoomName2, <<"subject">>, AliceBin),
    Res = execute_auth(admin_list_user_rooms_body(AliceBin), Config),
    ?assertEqual(lists:sort([jid:to_binary(RoomJID), jid:to_binary(RoomJID2)]),
                 lists:sort(get_ok_value(Path, Res))),
    % Try with a non-existing user
    Res2 = execute_auth(admin_list_user_rooms_body(<<"not-exist@", Domain/binary>>), Config),
    ?assertEqual([], lists:sort(get_ok_value(Path, Res2))),
    % Try with a non-existing domain
    Res3 = execute_auth(admin_list_user_rooms_body(<<"not-exist@not-exist">>), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not found">>)).

admin_list_room_users(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_list_room_users_story/2).

admin_list_room_users_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    AliceLower = escalus_utils:jid_to_lower(AliceBin),
    MUCServer = ?config(muc_light_host, Config),
    RoomName = <<"first room">>,
    {ok, #{jid := RoomJID}} = create_room(<<>>, MUCServer, RoomName, <<"subject">>, AliceBin),
    Res = execute_auth(list_room_users_body(jid:to_binary(RoomJID)), Config),
    ?assertEqual([#{<<"jid">> => AliceLower, <<"affiliation">> => <<"OWNER">>}],
                 get_ok_value([data, muc_light, listRoomUsers], Res)),
    % Try with a non-existing domain
    Res2 = execute_auth(list_room_users_body(
                          make_bare_jid(RoomJID#jid.luser, ?UNKNOWN_DOMAIN)), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)),
    % Try with a non-existing room
    Res3 = execute_auth(list_room_users_body(
                          make_bare_jid(?UNKNOWN, MUCServer)), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not found">>)).

admin_get_room_config(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_get_room_config_story/2).

admin_get_room_config_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    AliceLower = escalus_utils:jid_to_lower(AliceBin),
    MUCServer = ?config(muc_light_host, Config),
    RoomName = <<"first room">>,
    RoomSubject = <<"Room about nothing">>,
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(<<>>, MUCServer, RoomName, RoomSubject, AliceBin),
    RoomJIDBin = jid:to_binary(RoomJID),
    Res = execute_auth(get_room_config_body(jid:to_binary(RoomJID)), Config),
    ?assertEqual(#{<<"jid">> => RoomJIDBin, <<"subject">> => RoomSubject, <<"name">> => RoomName,
                    <<"participants">> => [#{<<"jid">> => AliceLower,
                                             <<"affiliation">> => <<"OWNER">>}]},
                 get_ok_value([data, muc_light, getRoomConfig], Res)),
    % Try with a non-existing domain
    Res2 = execute_auth(get_room_config_body(make_bare_jid(RoomID, ?UNKNOWN_DOMAIN)), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)),
    % Try with a non-existing room
    Res3 = execute_auth(get_room_config_body(make_bare_jid(?UNKNOWN, MUCServer)), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not found">>)).

%% Helpers

make_bare_jid(User, Server) ->
    JID = jid:make_bare(User, Server),
    jid:to_binary(JID).

send_message_to_room(RoomJID, SenderJID, Message) ->
    rpc(mim(), mod_muc_light_api, send_message, [RoomJID, SenderJID, Message]).

get_room_messages(ID, Domain) ->
    {ok, Messages} = rpc(mim(), mod_muc_light_api, get_room_messages, [Domain, ID]),
    Messages.

create_room(Id, Domain, Name, Subject, CreatorBin) ->
    CreatorJID = jid:from_binary(CreatorBin),
    rpc(mim(), mod_muc_light_api, create_room, [Domain, Id, Name, CreatorJID, Subject]).

invite_user(RoomJID, SenderBin, RecipientBin) ->
    SenderJID = jid:from_binary(SenderBin),
    RecipientJID = jid:from_binary(RecipientBin),
    rpc(mim(), mod_muc_light_api, invite_to_room, [RoomJID, SenderJID, RecipientJID]).

get_room_info(JID) ->
    HostType = domain_helper:host_type(),
    RoomUS = jid:to_lus(JID),
    rpc(mim(), mod_muc_light_db_backend, get_info, [HostType, RoomUS]).

get_room_aff(JID) ->
    {ok, _, Aff, _} = get_room_info(JID),
    Aff. 

prepare_blocking_items_for_query(Items) ->
    [#{<<"who">> => Who, <<"what">> => What,
       <<"action">> => Action} || {What, Action, Who} <- Items].

%% Request bodies

admin_create_room_body(MUCDomain, Name, Owner, Subject, Id) ->
    Query = <<"mutation M1($mucDomain: String!, $name: String!, $owner: JID!, $subject: String!, $id: String)
              { muc_light { createRoom(mucDomain: $mucDomain, name: $name, owner: $owner, subject: $subject, id: $id)
              { jid name subject participants {jid affiliation} } } }">>,
    OpName = <<"M1">>,
    Vars = #{<<"mucDomain">> => MUCDomain, <<"name">> => Name, <<"owner">> => Owner,
             <<"subject">> => Subject, <<"id">> => Id},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_change_room_configuration_body(RoomJID, OwnerJID, Name, Subject) ->
    Query = <<"mutation M1($room: JID!, $name: String!, $owner: JID!, $subject: String!)
              { muc_light { changeRoomConfiguration(room: $room, name: $name, owner: $owner, subject: $subject)
              { jid name subject participants {jid affiliation} } } }">>,
    OpName = <<"M1">>,
    Vars = #{<<"room">> => RoomJID, <<"name">> => Name, <<"owner">> => OwnerJID,
             <<"subject">> => Subject},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_invite_user_body(RoomJID, Sender, Recipient) ->
    Query = <<"mutation M1($room: JID!, $sender: JID!, $recipient: JID!)
              { muc_light { inviteUser(room: $room, sender: $sender, recipient: $recipient) } }">>,
    OpName = <<"M1">>,
    Vars = #{<<"room">> => RoomJID, <<"sender">> => Sender, <<"recipient">> => Recipient},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_kick_user_body(RoomJID, User) ->
    Query = <<"mutation M1($room: JID!, $user: JID!)
              { muc_light { kickUser(room: $room, user: $user)} }">>,
    OpName = <<"M1">>,
    Vars = #{<<"room">> => RoomJID, <<"user">> => User},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_send_message_to_room_body(RoomJID, From, Body) ->
    Query = <<"mutation M1($room: JID!, $from: JID!, $body: String!)
              { muc_light { sendMessageToRoom(room: $room, from: $from, body: $body)} }">>,
    OpName = <<"M1">>,
    Vars = #{<<"room">> => RoomJID, <<"from">> => From, <<"body">> => Body},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_list_user_rooms_body(User) ->
    Query = <<"query Q1($user: JID!)
              { muc_light { listUserRooms(user: $user) } }">>,
    OpName = <<"Q1">>,
    Vars = #{<<"user">> => User},
    #{query => Query, operationName => OpName, variables => Vars}.

user_create_room_body(MUCDomain, Name, Subject, Id) ->
    Query = <<"mutation M1($mucDomain: String!, $name: String!, $subject: String!, $id: String)
              { muc_light { createRoom(mucDomain: $mucDomain, name: $name, subject: $subject, id: $id)
              { jid name subject participants {jid affiliation} } } }">>,
    OpName = <<"M1">>,
    Vars = #{<<"mucDomain">> => MUCDomain, <<"name">> => Name, <<"subject">> => Subject,
             <<"id">> => Id},
    #{query => Query, operationName => OpName, variables => Vars}.

user_change_room_configuration_body(RoomJID, Name, Subject) ->
    Query = <<"mutation M1($room: JID!, $name: String!, $subject: String!)
              { muc_light { changeRoomConfiguration(room: $room, name: $name, subject: $subject)
              { jid name subject participants {jid affiliation} } } }">>,
    OpName = <<"M1">>,
    Vars = #{<<"room">> => RoomJID, <<"name">> => Name, <<"subject">> => Subject},
    #{query => Query, operationName => OpName, variables => Vars}.

user_invite_user_body(RoomJID, Recipient) ->
    Query = <<"mutation M1($room: JID!, $recipient: JID!)
              { muc_light { inviteUser(room: $room, recipient: $recipient) } }">>,
    OpName = <<"M1">>,
    Vars = #{<<"room">> => RoomJID, <<"recipient">> => Recipient},
    #{query => Query, operationName => OpName, variables => Vars}.

delete_room_body(RoomJID) ->
    Query = <<"mutation M1($room: JID!)
              { muc_light { deleteRoom(room: $room) } }">>,
    OpName = <<"M1">>,
    Vars = #{<<"room">> => RoomJID},
    #{query => Query, operationName => OpName, variables => Vars}.

user_kick_user_body(RoomJID, User) ->
    Query = <<"mutation M1($room: JID!, $user: JID)
              { muc_light { kickUser(room: $room, user: $user)} }">>,
    OpName = <<"M1">>,
    Vars = #{<<"room">> => RoomJID, <<"user">> => User},
    #{query => Query, operationName => OpName, variables => Vars}.

user_send_message_to_room_body(RoomJID, Body) ->
    Query = <<"mutation M1($room: JID!, $body: String!)
              { muc_light { sendMessageToRoom(room: $room, body: $body)} }">>,
    OpName = <<"M1">>,
    Vars = #{<<"room">> => RoomJID, <<"body">> => Body},
    #{query => Query, operationName => OpName, variables => Vars}.

get_room_messages_body(RoomJID, PageSize, Before) ->
    Query = <<"query Q1($room: JID!, $pageSize: Int, $before: DateTime)
              { muc_light { getRoomMessages(room: $room, pageSize: $pageSize, before: $before)
              { stanzas { stanza } limit } } }">>,
    OpName = <<"Q1">>,
    Vars = #{<<"room">> => RoomJID, <<"pageSize">> => PageSize, <<"before">> => Before},
    #{query => Query, operationName => OpName, variables => Vars}.

user_list_rooms_body() ->
    Query = <<"query Q1 { muc_light { listRooms } }">>,
    #{query => Query, operationName => <<"Q1">>, variables => #{}}.

list_room_users_body(RoomJID) ->
    Query = <<"query Q1($room: JID!)
              { muc_light { listRoomUsers(room: $room)
              { jid affiliation} } }">>,
    OpName = <<"Q1">>,
    Vars = #{<<"room">> => RoomJID},
    #{query => Query, operationName => OpName, variables => Vars}.

get_room_config_body(RoomJID) ->
    Query = <<"query Q1($room: JID!)
              { muc_light { getRoomConfig(room: $room)
              { jid name subject participants {jid affiliation} } } }">>,
    OpName = <<"Q1">>,
    Vars = #{<<"room">> => RoomJID},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_get_user_blocking_body(UserJID) ->
    Query = <<"query Q1($user: JID!)
              { muc_light { getBlockingList(user: $user)
              { who what action } } }">>,
    OpName = <<"Q1">>,
    Vars = #{<<"user">> => UserJID},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_set_blocking_body(UserJID, Items) ->
    Query = <<"mutation M1($user: JID!, $items: [BlockingInput!]!)
              { muc_light { setBlockingList(user: $user, items: $items) } }">>,
    OpName = <<"M1">>,
    Vars = #{<<"user">> => UserJID, <<"items">> => prepare_blocking_items_for_query(Items)},
    #{query => Query, operationName => OpName, variables => Vars}.

user_get_blocking_body() ->
    Query = <<"query Q1 { muc_light { getBlockingList { who what action } } }">>,
    OpName = <<"Q1">>,
    #{query => Query, operationName => OpName}.

user_set_blocking_body(Items) ->
    Query = <<"mutation M1($items: [BlockingInput!]!)
              { muc_light { setBlockingList(items: $items) } }">>,
    OpName = <<"M1">>,
    Vars = #{<<"items">> => prepare_blocking_items_for_query(Items)},
    #{query => Query, operationName => OpName, variables => Vars}.
