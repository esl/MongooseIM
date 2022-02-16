-module(graphql_muc_light_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [execute/3, execute_auth/2, get_listener_port/1,
                         get_listener_config/1, get_ok_value/2, get_err_msg/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("jid/include/jid.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-define(UNKNOWN_DOMAIN, <<"not-existing-domain">>).
-define(UNKNOWN, <<"not-existing">>).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user_muc_light},
     {group, admin_muc_light}].

groups() ->
    [{user_muc_light, [], user_muc_light_handler()},
     {admin_muc_light, [], admin_muc_light_handler()}].

user_muc_light_handler() ->
    [mock].

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
     admin_get_room_config
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

required_modules(SuiteOrTC) ->
    [{mod_muc_light, common_muc_light_opts() ++ muc_light_opts(SuiteOrTC)}].

muc_light_opts(config_can_be_changed_by_all) ->
    [{all_can_configure, true}];
muc_light_opts(suite) ->
    [].

common_muc_light_opts() ->
    MucPattern = distributed_helper:subhost_pattern(muc_light_helper:muc_host_pattern()),
    [{host, MucPattern},
     {rooms_in_rosters, true}].

init_per_group(admin_muc_light, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(_GN, Config) ->
    Config.

end_per_group(_GN, Config) ->
    Config.

init_per_testcase(TC, Config) ->
    rest_helper:maybe_skip_mam_test_cases(TC, [admin_get_room_messages], Config).

end_per_testcase(TC, Config) ->
    escalus:end_per_testcase(TC, Config).

mock(_Config) ->
    ok.

admin_create_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_create_room_story/2).

admin_create_room_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    AliceBinLower = escalus_utils:jid_to_lower(AliceBin),
    Domain = escalus_client:server(Alice),
    MucServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    Res = execute_auth(admin_create_room_body(Domain, Name, AliceBin, Subject, null), Config),
    Path = [data, muc_light, createRoom],
    #{<<"jid">> := JID, <<"name">> := Name, <<"subject">> := Subject,
      <<"participants">> := Participants} = get_ok_value(Path, Res),
    ?assertMatch(#jid{server = MucServer}, jid:from_binary(JID)),
    ?assertEqual([#{<<"jid">> => AliceBinLower, <<"affiliance">> => <<"owner">>}], Participants),
    % Try with a non-existing domain
    Res2 = execute_auth(admin_create_room_body(?UNKNOWN_DOMAIN, Name, AliceBin, Subject, null), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not exist">>)).

admin_create_identified_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_create_identified_room_story/2).

admin_create_identified_room_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    Domain = escalus_client:server(Alice),
    MucServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    Id = <<"my_room">>,
    Res = execute_auth(admin_create_room_body(Domain, Name, AliceBin, Subject, Id), Config),
    Path = [data, muc_light, createRoom],
    #{<<"jid">> := JID, <<"name">> := Name, <<"subject">> := Subject} = get_ok_value(Path, Res),
    ?assertMatch(#jid{user = Id, server = MucServer}, jid:from_binary(JID)),
    % Create a room with an existing ID
    Res2 = execute_auth(admin_create_room_body(Domain, <<"snd room">>, AliceBin, Subject, Id), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"already exists">>)),
    % Try with a non-existing domain
    Res3 = execute_auth(admin_create_room_body(?UNKNOWN_DOMAIN, <<>>, AliceBin, Subject, Id), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"does not exist">>)).

admin_change_room_config(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_change_room_config_story/2).

admin_change_room_config_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    Domain = escalus_client:server(Alice),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    Id = atom_to_binary(?FUNCTION_NAME),
    % Create a new room
    execute_auth(admin_create_room_body(Domain, Name, AliceBin, Subject, Id), Config),
    % Try to change the room configuration
    Name2 = <<"changed room">>,
    Subject2 = <<"not testing">>,
    Res = execute_auth(admin_change_room_configuration_body(Id, Domain, AliceBin, Name2, Subject2), Config),
    Path = [data, muc_light, changeRoomConfiguration],
    ?assertMatch(#{<<"name">> := Name2, <<"subject">> := Subject2}, get_ok_value(Path, Res)).

admin_change_room_config_errors(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun admin_change_room_config_errors_story/3).

admin_change_room_config_errors_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    Domain = escalus_client:server(Alice),
    RoomName = <<"first room">>,
    {ok, #{jid := #jid{luser = RoomID}}} = create_room(<<>>, Domain, RoomName, <<>>, AliceBin),
    {ok, _} = invite_user(Domain, RoomName, AliceBin, BobBin),
    % Try to change the config with a non-existing domain
    Res = execute_auth(admin_change_room_configuration_body(RoomID, ?UNKNOWN_DOMAIN, AliceBin, RoomName, <<>>), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not exist">>)),
    % Try to change the config of the non-existing room
    Res2 = execute_auth(admin_change_room_configuration_body(<<"unknown">>, Domain, AliceBin, RoomName, <<>>), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not exist">>)),
    % Try to change the config by the non-existing user
    Res3 = execute_auth(admin_change_room_configuration_body(RoomID, Domain, <<"wrong-user@wrong-domain">>, RoomName, <<>>), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not room participant">>)),
    % Try to change a config by the user without permission
    Res4 = execute_auth(admin_change_room_configuration_body(RoomID, Domain, BobBin, RoomName, <<>>), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res4), <<"not permission to change">>)).

admin_invite_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun admin_invite_user_story/3).

admin_invite_user_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    Domain = escalus_client:server(Alice),
    Name = <<"first room">>,
    Name2 = <<"second room">>,
    {ok, #{jid := RoomJID}} = create_room(<<>>, Domain, Name, <<>>, AliceBin),
    {ok, _} = create_room(<<>>, Domain, Name2, <<>>, AliceBin),

    Res = execute_auth(admin_invite_user_body(Domain, Name, AliceBin, BobBin), Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value([data, muc_light, inviteUser], Res),
                                          <<"successfully">>)),
    BobName = escalus_utils:jid_to_lower(escalus_client:username(Bob)),
    AliceName = escalus_utils:jid_to_lower(escalus_client:username(Alice)),
    ExpectedAff = lists:sort([{{AliceName, Domain}, owner},
                              {{BobName, Domain}, member}]),
    ?assertMatch(ExpectedAff, lists:sort(get_room_aff(RoomJID))).

admin_invite_user_errors(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun admin_invite_user_errors_story/3).

admin_invite_user_errors_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    Domain = escalus_client:server(Alice),
    Name = <<"first room">>,
    {ok, #{jid := _RoomJID}} = create_room(<<>>, Domain, Name, <<>>, AliceBin),
    % Try to invite a user to not existing room
    Res = execute_auth(admin_invite_user_body(Domain, <<>>, AliceBin, BobBin), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not exist">>)),
    % User without rooms tries to invite a user
    Res2 = execute_auth(admin_invite_user_body(Domain, <<>>, BobBin, AliceBin), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not occupy any room">>)),
    % Try with a non-existing domain
    Res3 = execute_auth(admin_invite_user_body(?UNKNOWN_DOMAIN, Name, AliceBin, BobBin), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"does not exist">>)).

admin_delete_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_delete_room_story/2).

admin_delete_room_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    Domain = escalus_client:server(Alice),
    Name = <<"first room">>,
    RoomID = <<"delete_room_id">>,
    {ok, #{jid := RoomJID}} = create_room(RoomID, Domain, Name, <<>>, AliceBin),
    Res = execute_auth(admin_delete_room_body(Domain, RoomID), Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value([data, muc_light, deleteRoom], Res),
                                          <<"successfully">>)),
    ?assertEqual({error, not_exists}, get_room_info(jid:from_binary(RoomJID))),
    % Try with a non-existing domain
    Res2 = execute_auth(admin_delete_room_body(?UNKNOWN_DOMAIN, RoomID), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not exist">>)).

admin_kick_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun admin_kick_user_story/3).

admin_kick_user_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    Domain = escalus_client:server(Alice),
    RoomName = <<"first room">>,
    RoomID = <<"kick_user_test_room">>,
    {ok, #{jid := RoomJID}} = create_room(RoomID, Domain, RoomName, <<>>, AliceBin),
    {ok, _} = invite_user(Domain, RoomName, AliceBin, BobBin),
    ?assertEqual(2, length(get_room_aff(RoomJID))),
    Res = execute_auth(admin_kick_user_body(Domain, RoomID, BobBin), Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value([data, muc_light, kickUser], Res),
                                          <<"successfully">>)),
    ?assertEqual(1, length(get_room_aff(RoomJID))),
    % Try with a non-existing domain
    Res2 = execute_auth(admin_kick_user_body(?UNKNOWN_DOMAIN, RoomID, BobBin), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not exist">>)).

admin_send_message_to_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_send_message_to_room_story/3).

admin_send_message_to_room_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    Domain = escalus_client:server(Alice),
    RoomName = <<"first room">>,
    RoomID = <<"send_message_test_room">>,
    MsgBody = <<"Hello there!">>,
    {ok, #{jid := _RoomJID}} = create_room(RoomID, Domain, RoomName, <<>>, AliceBin),
    {ok, _} = invite_user(Domain, RoomName, AliceBin, BobBin),
    Res = execute_auth(admin_send_message_to_room_body(Domain, RoomName, AliceBin, MsgBody), Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value([data, muc_light, sendMessageToRoom], Res),
                                          <<"successfully">>)).

admin_send_message_to_room_errors(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_send_message_to_room_errors_story/3).

admin_send_message_to_room_errors_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    Domain = escalus_client:server(Alice),
    ARoomName = <<"alice room">>,
    BRoomName = <<"bob room">>,
    MsgBody = <<"Hello there!">>,
    {ok, #{jid := _RoomJID}} = create_room(<<>>, Domain, ARoomName, <<>>, AliceBin),
    % Try with a non-existing domain
    Res2 = execute_auth(admin_send_message_to_room_body(?UNKNOWN_DOMAIN, ARoomName, AliceBin, MsgBody), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not exist">>)),
    % Try with a user without rooms
    Res3 = execute_auth(admin_send_message_to_room_body(Domain, ARoomName, BobBin, MsgBody), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"does not occupy any room">>)),
    % Try with a room not occupied by this user
    {ok, #{jid := _RoomJID2}} = create_room(<<>>, Domain, BRoomName, <<>>, BobBin),
    Res4 = execute_auth(admin_send_message_to_room_body(Domain, ARoomName, BobBin, MsgBody), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res4), <<"does not found">>)).

admin_get_room_messages(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_get_room_messages_story/2).

admin_get_room_messages_story(Config, Alice) ->
    Path = [data, muc_light, getRoomMessages, stanzas],
    AliceBin = escalus_client:short_jid(Alice),
    Domain = escalus_client:server(Alice),
    RoomName = <<"first room">>,
    RoomName2 = <<"second room">>,
    RoomID = <<"get_messages_test_room">>,
    {ok, #{jid := _RoomJID}} = create_room(RoomID, Domain, RoomName, <<>>, AliceBin),
    {ok, _} = create_room(<<>>, Domain, RoomName2, <<>>, AliceBin),
    Message = <<"Hello friends">>,
    send_message_to_room(Domain, RoomName, jid:from_binary(AliceBin), Message),
    mam_helper:maybe_wait_for_archive(Config),
    % Get messages so far
    Res = execute_auth(admin_get_room_messages_body(Domain, RoomID, 50, null), Config),
    [#{<<"stanza">> := StanzaXML}] = get_ok_value(Path, Res),
    ?assertMatch({ok, #xmlel{name = <<"message">>}}, exml:parse(StanzaXML)),
    % Get messages before the given date and time
    Before = <<"2022-02-17T04:54:13+00:00">>,
    Res2 = execute_auth(admin_get_room_messages_body(Domain, RoomID, 50, Before), Config),
    ?assertMatch([], get_ok_value(Path, Res2)),
    % Try with a non-existing domain
    Res3 = execute_auth(admin_get_room_messages_body(?UNKNOWN_DOMAIN, RoomID, 50, null), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"does not exist">>)).

admin_list_user_rooms(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_list_user_rooms_story/2).

admin_list_user_rooms_story(Config, Alice) ->
    Path = [data, muc_light, listUserRooms],
    AliceBin = escalus_client:short_jid(Alice),
    Domain = escalus_client:server(Alice),
    RoomName = <<"first room">>,
    RoomName2 = <<"second room">>,
    {ok, #{jid := RoomJID}} = create_room(<<>>, Domain, RoomName, <<>>, AliceBin),
    {ok, #{jid := RoomJID2}} = create_room(<<>>, Domain, RoomName2, <<>>, AliceBin),
    Res = execute_auth(admin_list_user_rooms_body(AliceBin), Config),
    ?assertEqual(lists:sort([jid:to_binary(RoomJID), jid:to_binary(RoomJID2)]),
                 lists:sort(get_ok_value(Path, Res))),
    % Try with a non-existing user
    Res2 = execute_auth(admin_list_user_rooms_body(<<"not-exist@", Domain/binary>>), Config),
    ?assertEqual([], lists:sort(get_ok_value(Path, Res2))),
    % Try with a non-existing domain
    Res3 = execute_auth(admin_list_user_rooms_body(<<"not-exist@not-exist">>), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"does not exist">>)).

admin_list_room_users(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_list_room_users_story/2).

admin_list_room_users_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    AliceLower = escalus_utils:jid_to_lower(AliceBin),
    Domain = escalus_client:server(Alice),
    RoomName = <<"first room">>,
    {ok, #{jid := #jid{luser = RoomID}}} = create_room(<<>>, Domain, RoomName, <<>>, AliceBin),
    Res = execute_auth(admin_list_room_users_body(Domain, RoomID), Config),
    ?assertEqual([#{<<"jid">> => AliceLower, <<"affiliance">> => <<"owner">>}],
                 get_ok_value([data, muc_light, listRoomUsers], Res)),
    % Try with a non-existing domain
    Res2 = execute_auth(admin_list_room_users_body(?UNKNOWN_DOMAIN, RoomID), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not exist">>)).

admin_get_room_config(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_get_room_config_story/2).

admin_get_room_config_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    AliceLower = escalus_utils:jid_to_lower(AliceBin),
    Domain = escalus_client:server(Alice),
    RoomName = <<"first room">>,
    RoomSubject = <<"Room about nothing">>,
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} = create_room(<<>>, Domain, RoomName,
                                                                 RoomSubject, AliceBin),
    RoomJIDBin = jid:to_binary(RoomJID),
    Res = execute_auth(admin_get_room_config_body(Domain, RoomID), Config),
    ?assertEqual(#{<<"jid">> => RoomJIDBin, <<"subject">> => RoomSubject, <<"name">> => RoomName,
                    <<"participants">> => [#{<<"jid">> => AliceLower, <<"affiliance">> => <<"owner">>}]},
                 get_ok_value([data, muc_light, getRoomConfig], Res)),
    % Try with a non-existing domain
    Res2 = execute_auth(admin_get_room_config_body(?UNKNOWN_DOMAIN, RoomID), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not exist">>)),
    % Try with a non-existing room
    Res3 = execute_auth(admin_get_room_config_body(Domain, ?UNKNOWN), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"does not exist">>)).

%% Helpers

send_message_to_room(Domain, RoomName, SenderJID, Message) ->
    rpc(mim(), mod_muc_light_api, send_message, [Domain, RoomName, SenderJID, Message]).

get_room_messages(ID, Domain) ->
    {ok, Messages} = rpc(mim(), mod_muc_light_api, get_room_messages, [Domain, ID]),
    Messages.

create_room(Id, Domain, Name, Subject, CreatorBin) ->
    CreatorJID = jid:from_binary(CreatorBin),
    rpc(mim(), mod_muc_light_api, create_room, [Domain, Id, Name, CreatorJID, Subject]).

invite_user(Domain, RoomName, SenderBin, RecipientBin) ->
    SenderJID = jid:from_binary(SenderBin),
    RecipientJID = jid:from_binary(RecipientBin),
    rpc(mim(), mod_muc_light_api, invite_to_room, [Domain, RoomName, SenderJID, RecipientJID]).

get_room_info(JID) ->
    HostType = domain_helper:host_type(),
    RoomUS = jid:to_lus(JID),
    rpc(mim(), mod_muc_light_db_backend, get_info, [HostType, RoomUS]).

get_room_aff(JID) ->
    {ok, _, Aff, _} = get_room_info(JID),
    Aff. 

%% Request bodies

admin_create_room_body(Domain, Name, Owner, Subject, Id) ->
    Query = <<"mutation M1($domain: String!, $name: String!, $owner: JID!, $subject: String!, $id: String)
              { muc_light { createRoom(domain: $domain, name: $name, owner: $owner, subject: $subject, id: $id)
              { jid name subject participants {jid affiliance} } } }">>,
    OpName = <<"M1">>,
    Vars = #{<<"domain">> => Domain, <<"name">> => Name, <<"owner">> => Owner, <<"subject">> => Subject, <<"id">> => Id},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_change_room_configuration_body(Id, Domain, Owner, Name, Subject) ->
    Query = <<"mutation M1($id: String!, $domain: String!, $name: String!, $owner: JID!, $subject: String!)
              { muc_light { changeRoomConfiguration(id: $id, domain: $domain, name: $name, owner: $owner, subject: $subject)
              { jid name subject participants {jid affiliance} } } }">>,
    OpName = <<"M1">>,
    Vars = #{<<"id">> => Id, <<"domain">> => Domain, <<"name">> => Name, <<"owner">> => Owner, <<"subject">> => Subject},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_invite_user_body(Domain, Name, Sender, Recipient) ->
    Query = <<"mutation M1($domain: String!, $name: String!, $sender: JID!, $recipient: JID!)
              { muc_light { inviteUser(domain: $domain, name: $name, sender: $sender, recipient: $recipient) } }">>,
    OpName = <<"M1">>,
    Vars = #{<<"domain">> => Domain, <<"name">> => Name, <<"sender">> => Sender, <<"recipient">> => Recipient},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_delete_room_body(Domain, RoomID) ->
    Query = <<"mutation M1($domain: String!, $id: String!)
              { muc_light { deleteRoom(domain: $domain, id: $id)} }">>,
    OpName = <<"M1">>,
    Vars = #{<<"domain">> => Domain, <<"id">> => RoomID},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_kick_user_body(Domain, RoomID, User) ->
    Query = <<"mutation M1($domain: String!, $id: String!, $user: JID!)
              { muc_light { kickUser(domain: $domain, id: $id, user: $user)} }">>,
    OpName = <<"M1">>,
    Vars = #{<<"domain">> => Domain, <<"id">> => RoomID, <<"user">> => User},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_send_message_to_room_body(Domain, RoomName, From, Body) ->
    Query = <<"mutation M1($domain: String!, $name: String!, $from: JID!, $body: String!)
              { muc_light { sendMessageToRoom(domain: $domain, name: $name, from: $from, body: $body)} }">>,
    OpName = <<"M1">>,
    Vars = #{<<"domain">> => Domain, <<"name">> => RoomName, <<"from">> => From, <<"body">> => Body},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_get_room_messages_body(Domain, RoomID, PageSize, Before) ->
    Query = <<"query Q1($domain: String!, $id: String!, $pageSize: Int!, $before: DateTime)
              { muc_light { getRoomMessages(domain: $domain, id: $id, pageSize: $pageSize, before: $before)
              { stanzas { stanza } } } }">>,
    OpName = <<"Q1">>,
    Vars = #{<<"domain">> => Domain, <<"id">> => RoomID,
             <<"pageSize">> => PageSize, <<"before">> => Before},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_list_user_rooms_body(User) ->
    Query = <<"query Q1($user: JID!)
              { muc_light { listUserRooms(user: $user) } }">>,
    OpName = <<"Q1">>,
    Vars = #{<<"user">> => User},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_list_room_users_body(Domain, RoomID) ->
    Query = <<"query Q1($domain: String!, $id: String!)
              { muc_light { listRoomUsers(domain: $domain, id: $id)
              { jid affiliance } } }">>,
    OpName = <<"Q1">>,
    Vars = #{<<"domain">> => Domain, <<"id">> => RoomID},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_get_room_config_body(Domain, RoomID) ->
    Query = <<"query Q1($domain: String!, $id: String!)
              { muc_light { getRoomConfig(domain: $domain, id: $id)
              { jid name subject participants {jid affiliance} } } }">>,
    OpName = <<"Q1">>,
    Vars = #{<<"domain">> => Domain, <<"id">> => RoomID},
    #{query => Query, operationName => OpName, variables => Vars}.
