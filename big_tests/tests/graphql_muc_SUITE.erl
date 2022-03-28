-module(graphql_muc_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [execute_user/3, execute_auth/2, get_ok_value/2, get_err_msg/1,
                         user_to_bin/1, user_to_full_bin/1]).

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
    [mock].

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
     admin_try_invite_user_to_nonexistent_room,
     admin_kick_user,
     admin_send_message_to_room,
     admin_get_room_messages,
     admin_try_get_nonexistent_room_messages].

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

mock(_Config) ->
    ok.

-define(CREATE_INSTANT_ROOM_PATH, [data, muc, createInstantRoom]).
-define(LIST_ROOMS_PATH, [data, muc, listRooms]).
-define(INVITE_USER_PATH, [data, muc, inviteUser]).
-define(KICK_USER_PATH, [data, muc, kickUser]).
-define(DELETE_ROOM_PATH, [data, muc, deleteRoom]).
-define(SEND_MESSAGE_PATH, [data, muc, sendMessageToRoom]).
-define(GET_MESSAGES_PATH, [data, muc, getRoomMessages]).
-define(LIST_ROOM_USERS_PATH, [data, muc, listRoomUsers]).
-define(CHANGE_ROOM_CONFIG_PATH, [data, muc, changeRoomConfiguration]).
-define(GET_ROOM_CONFIG_PATH, [data, muc, getRoomConfig]).

-define(NONEXISTENT_ROOM, <<"room@room">>).
-define(NONEXISTENT_ROOM2, <<"room@", (muc_helper:muc_host())/binary>>).

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
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res),
                                          <<"not existing">>)).

admin_try_delete_room_with_nonexistent_domain(Config) ->
    RoomJID = jid:make_bare(<<"unknown">>, <<"unknown">>),
    Res = execute_auth(delete_room_body(RoomJID, null), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res),
                                          <<"not existing">>)).

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
                 exml_query:path(Stanza, [{element, <<"x">>}, {attr, <<"jid">>}])).

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
    Messsage = <<"Hello All!">>,
    BobNick = <<"Bobek">>,
    enter_room(RoomJID, Bob, BobNick),
    escalus:wait_for_stanza(Bob),
    % Send message
    Res = execute_auth(admin_send_message_to_room_body(RoomJID, Bob, Messsage), Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SEND_MESSAGE_PATH, Res),
                                          <<"successfully">>)),
    assert_is_message_correct(RoomJID, BobNick, <<"groupchat">>, Messsage,
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

%% Helpers

rand_name() ->
    rpc(mim(), mongoose_bin, gen_from_crypto, []).

-spec assert_room_users([{jid:jid(), binary(), binary()}], [map()]) -> ok.
assert_room_users(Expected, Actual) ->
    lists:all(fun(#{<<"jid">> := JID, <<"role">> := Role, <<"nick">> := Nick}) ->
                      lists:any(fun({JID2, Nick2, Role2}) ->
                                        JID =:= JID2 andalso Nick =:= Nick2 andalso Role =:= Role2
                                end, Expected)
              end, Actual).

assert_is_message_correct(RoomJID, SenderNick, Type, Text, ReceivedMessage) ->
    escalus_pred:is_message(ReceivedMessage),
    From = jid:to_binary(jid:replace_resource(RoomJID, SenderNick)),
    From  = exml_query:attr(ReceivedMessage, <<"from">>),
    Type  = exml_query:attr(ReceivedMessage, <<"type">>),
    Body = #xmlel{name = <<"body">>, children = [#xmlcdata{content=Text}]},
    Body = exml_query:subelement(ReceivedMessage, <<"body">>).

enter_room(RoomJID, User, Nick) ->
    JID = jid:to_binary(jid:replace_resource(RoomJID, Nick)),
    %X = #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_MUC}]},
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
                   <<"moderated">> := false,
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
