-module(inbox_SUITE).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-define(NS_DISPLAY, <<"forward:display-names">>).

-import(escalus_ejabberd, [rpc/3]).

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).
%% tests
-export([simple_msg/1, empty_inbox/1, two_unread/1,
         mark_read/1, mark_unread_bad_id/1, two_conversations/1,
         simple_muclight/1, advanced_muclight/1, groupchat_markers/1,
         create_groupchat/1]).

-import(muc_helper, [foreach_occupant/3, foreach_recipient/2]).
-import(muc_light_helper, [bin_aff_users/1, aff_msg_verify_fun/1, gc_message_verify_fun/3,
        lbin/1, room_bin_jid/1, verify_aff_bcast/2, verify_aff_bcast/3, verify_aff_users/2,
        kv_el/2, to_lus/2, stanza_create_room/3, create_room/6, stanza_aff_set/2, default_config/0]).

-define(NS_ESL_INBOX, <<"erlang-solutions.com:xmpp:inbox:0">>).
-define(MUCLIGHT_HOST, <<"muclight.localhost">>).
-define(ROOM, <<"testroom">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
  [
      {group, one_to_one},
      {group, muclight}
  ].

groups() ->
  [
      {one_to_one, [sequence], [
        simple_msg, two_conversations, empty_inbox, two_unread,
        mark_read, mark_unread_bad_id
      ]},
      {muclight, [sequence], [
       simple_muclight,
       advanced_muclight,
       groupchat_markers,
       create_groupchat]}
  ].

suite() ->
  escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

%%
%% We assume that MAM is enabled
%%
init_per_suite(Config) ->
  Host = ct:get_config({hosts, mim, domain}),
  dynamic_modules:start(Host, mod_inbox,
    [{backend, odbc}]),
  dynamic_modules:start(Host, mod_muc_light,
    [{host, binary_to_list(?MUCLIGHT_HOST)},{backend, odbc}]),
  Config1 = escalus:init_per_suite(Config),
  escalus:create_users(Config1, escalus:get_users([alice, bob, kate, mike, carol])).

end_per_suite(Config) ->
  Host = ct:get_config({hosts, mim, domain}),
  Config1 = escalus:delete_users(Config, escalus:get_users([alice, bob, kate, mike])),
  dynamic_modules:stop(Host, mod_inbox),
  dynamic_modules:stop(Host, mod_muc_light),
  escalus:end_per_suite(Config1).

init_per_group(muclight, Config) ->
  create_room(?ROOM, ?MUCLIGHT_HOST, alice, [bob, kate], Config, ver(1));
init_per_group(_GroupName, Config) ->
  Config.

end_per_group(Group, Config) when Group == muclight orelse Group == corner ->
  muc_light_helper:clear_db(),
  Config;
end_per_group(_GroupName, Config) ->
  Config.



init_per_testcase(CaseName, Config) ->
  Host = ct:get_config({hosts, mim, domain}),
  clear_inbox_all([alice, bob, kate, mike], Host),
  escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
  escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Inbox tests one-to-one
%%--------------------------------------------------------------------

simple_msg(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
    Msg1 = escalus_stanza:chat_to(Bob, <<"Hello">>),
    BobJid = bin(escalus_client:full_jid(Bob)),
    AliceJid = bin(escalus_client:full_jid(Alice)),
    escalus:send(Alice, Msg1),
    M = escalus:wait_for_stanza(Bob),
    escalus:assert(is_chat_message, M),
    check_inbox(Alice, <<"1">>, [{<<"0">>, AliceJid, BobJid,<<"Hello">>}])
                                                end).

two_conversations(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    Msg1 = escalus_stanza:chat_to(Bob, <<"Hello Bob">>),
    Msg2 = escalus_stanza:chat_to(Kate, <<"Hello Kate">>),
    BobJid = bin(escalus_client:full_jid(Bob)),
    AliceJid = bin(escalus_client:full_jid(Alice)),
    KateJid = bin(escalus_client:full_jid(Kate)),
    escalus:send(Alice, Msg1),
    escalus:send(Alice, Msg2),
    M1 = escalus:wait_for_stanza(Bob),
    M2 = escalus:wait_for_stanza(Kate),
    escalus:assert(is_chat_message, M1),
    escalus:assert(is_chat_message, M2),
    check_inbox(Alice, <<"2">>, [{<<"0">>,  AliceJid, BobJid,  <<"Hello Bob">>},
                                 {<<"0">>, AliceJid, KateJid, <<"Hello Kate">>}]),
    check_inbox(Kate, <<"1">>, [{<<"1">>,AliceJid, KateJid,  <<"Hello Kate">>}]),
    check_inbox(Bob, <<"1">>, [{<<"1">>,AliceJid, BobJid,  <<"Hello Bob">>}])

                                                           end).

empty_inbox(Config) ->
  escalus:story(Config, [{kate, 1}], fun(Kate) ->
    Stanza = get_inbox_stanza(),
    escalus:send(Kate, Stanza),
    [ResIQ] = escalus:wait_for_stanzas(Kate, 1),
    true = escalus_pred:is_iq_result(ResIQ),
    TotalCount = get_inbox_count(ResIQ),
    <<"0">> = TotalCount
                                     end).

two_unread(Config) ->
  escalus:story(Config, [{kate, 1}, {mike, 1}], fun(Kate, Mike) ->
    Msg1 = escalus_stanza:chat_to(Mike, <<"Hello">>),
    Msg2 = escalus_stanza:chat_to(Mike, <<"How are you">>),
    %% sender is bare JID
    KateJid = bin(escalus_client:full_jid(Kate)),
    %% receiver is full JID
    MikeJid = bin(escalus_client:full_jid(Mike)),
    escalus:send(Kate, Msg1),
    escalus:send(Kate, Msg2),
    [M1, M2] = escalus:wait_for_stanzas(Mike, 2),
    escalus:assert(is_chat_message, M1),
    escalus:assert(is_chat_message, M2),
    check_inbox(Mike, <<"1">>, [{<<"2">>, KateJid, MikeJid, <<"How are you">>}]),
    check_inbox(Kate, <<"1">>, [{<<"0">>, KateJid, MikeJid, <<"How are you">>}])
                                                end).

mark_read(Config) ->
  escalus:story(Config, [{kate, 1}, {mike, 1}], fun(Kate, Mike) ->
    Msg1 = escalus_stanza:set_id(escalus_stanza:chat_to(Mike, <<"Hi mike">>), <<"123123">>),
    %% sender is bare JID
    KateJid = bin(escalus_client:full_jid(Kate)),
    MikeJid = bin(escalus_client:full_jid(Mike)),
    escalus:send(Kate, Msg1),
    M1 = escalus:wait_for_stanza(Mike),
    escalus:assert(is_chat_message, M1),
    check_inbox(Mike, <<"1">>, [{<<"1">>, KateJid, MikeJid, <<"Hi mike">>}]),
    MsgId = get_msg_id(M1),
    ChatMarker = escalus_stanza:chat_marker(KateJid, <<"displayed">>, MsgId),
    escalus:send(Mike, ChatMarker),
    %% Now Mike asks for inbox second time
    check_inbox(Mike, <<"1">>, [{<<"0">>, KateJid, MikeJid, <<"Hi mike">>}])
                                                end).

mark_unread_bad_id(Config) ->
  escalus:story(Config, [{kate, 1}, {mike, 1}], fun(Kate, Mike) ->
    Msg1 = escalus_stanza:set_id(escalus_stanza:chat_to(Mike, <<"okey dockey">>), <<"111">>),
    %% sender is bare JID
    KateJid = bin(escalus_client:full_jid(Kate)),
    MikeJid = bin(escalus_client:full_jid(Mike)),
    escalus:send(Kate, Msg1),
    M1 = escalus:wait_for_stanza(Mike),
    escalus:assert(is_chat_message, M1),
    check_inbox(Mike, <<"1">>, [{<<"1">>, KateJid, MikeJid, <<"okey dockey">>}]),
    MsgId = <<"badId">>,
    ChatMarker = escalus_stanza:chat_marker(KateJid, <<"displayed">>, MsgId),
    escalus:send(Mike, ChatMarker),
    %% Now Mike asks for inbox second time. Unread count should remain the same
    check_inbox(Mike, <<"1">>, [{<<"1">>, KateJid, MikeJid, <<"okey dockey">>}])
                                                end).

%%--------------------------------------------------------------------
%% Inbox tests muclight
%%--------------------------------------------------------------------

simple_muclight(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    Msg = <<"Hi Room!">>,
    Id = <<"MyID">>,
    AliceJid = lbin(escalus_client:short_jid(Alice)),
    RoomJid = room_bin_jid(?ROOM),
    AliceRoomJid = <<RoomJid/binary,"/", AliceJid/binary>>,
    Stanza = escalus_stanza:set_id(
      escalus_stanza:groupchat_to(RoomJid, Msg), Id),
    escalus:send(Alice, Stanza),
    R0 = escalus:wait_for_stanza(Alice),
    R1 = escalus:wait_for_stanza(Bob),
    R2 = escalus:wait_for_stanza(Kate),
    escalus:assert(is_groupchat_message, R0),
    escalus:assert(is_groupchat_message, R1),
    escalus:assert(is_groupchat_message, R2),
    GetInbox = get_inbox_stanza(),
    escalus:send(Alice, GetInbox),
    [Message, ResIQ] = escalus:wait_for_stanzas(Alice, 2),
    Unread = get_unread_count(Message),
    true = escalus_pred:is_message(Message),
    true = escalus_pred:is_iq_result(ResIQ),
    [Msg2] = get_inner_msg(Message),
    AliceRoomJid = exml_query:path(Msg2, [{attr, <<"from">>}]),
    AliceJid = exml_query:path(Msg2, [{attr, <<"to">>}]),
    [Content2] = exml_query:paths(Msg2, [{element, <<"body">>}, cdata]),
    Unread = get_unread_count(Message),
    TotalCount = get_inbox_count(ResIQ),
    Content2 = <<"Hi Room!">>,
    <<"1">> = TotalCount,
    <<"0">> = Unread


                                                           end).

advanced_muclight(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    Msg1 = <<"Hi Room!">>,
    Msg2 = <<"How are you?">>,
    Id = <<"MyID">>,
    BobJid = lbin(escalus_client:short_jid(Bob)),
    KateJid = lbin(escalus_client:short_jid(Kate)),
    RoomJid = room_bin_jid(?ROOM),
    BobRoomJid = <<RoomJid/binary,"/", BobJid/binary>>,
    Stanza1 = escalus_stanza:set_id(
      escalus_stanza:groupchat_to(RoomJid, Msg1), Id),
    escalus:send(Alice, Stanza1),
    R0 = escalus:wait_for_stanza(Alice),
    R1 = escalus:wait_for_stanza(Bob),
    R2 = escalus:wait_for_stanza(Kate),
    escalus:assert(is_groupchat_message, R0),
    escalus:assert(is_groupchat_message, R1),
    escalus:assert(is_groupchat_message, R2),
    %% Bob sends second message
    Stanza2 = escalus_stanza:set_id(
      escalus_stanza:groupchat_to(RoomJid, Msg2), Id),
    escalus:send(Bob, Stanza2),
    R3 = escalus:wait_for_stanza(Alice),
    R4 = escalus:wait_for_stanza(Bob),
    R5 = escalus:wait_for_stanza(Kate),
    escalus:assert(is_groupchat_message, R3),
    escalus:assert(is_groupchat_message, R4),
    escalus:assert(is_groupchat_message, R5),
    GetInbox = get_inbox_stanza(),
    escalus:send(Kate, GetInbox),
    [Message, ResIQ] = escalus:wait_for_stanzas(Kate, 2),
    Unread = get_unread_count(Message),
    true = escalus_pred:is_message(Message),
    true = escalus_pred:is_iq_result(ResIQ),
    [Msg3] = get_inner_msg(Message),
    BobRoomJid = exml_query:path(Msg3, [{attr, <<"from">>}]),
    KateJid = exml_query:path(Msg3, [{attr, <<"to">>}]),
    [Content2] = exml_query:paths(Msg3, [{element, <<"body">>}, cdata]),
    Unread = get_unread_count(Message),
    TotalCount = get_inbox_count(ResIQ),
    Content2 = <<"How are you?">>,
    <<"1">> = TotalCount,
    <<"2">> = Unread
                                                           end).

groupchat_markers(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    Msg = <<"Welcome guys">>,
    Id = <<"special-id-123">>,
    RoomJid = room_bin_jid(?ROOM),
    AliceJid = lbin(escalus_client:short_jid(Alice)),
    BobJid = lbin(escalus_client:short_jid(Bob)),
    KateJid = lbin(escalus_client:short_jid(Kate)),
    Stanza = escalus_stanza:set_id(
      escalus_stanza:groupchat_to(RoomJid, Msg), Id),
    escalus:send(Alice, Stanza),
    R0 = escalus:wait_for_stanza(Alice),
    R1 = escalus:wait_for_stanza(Bob),
    R2 = escalus:wait_for_stanza(Kate),
    escalus:assert(is_groupchat_message, R0),
    escalus:assert(is_groupchat_message, R1),
    escalus:assert(is_groupchat_message, R2),
    check_inbox(Alice, <<"1">>, [{<<"0">>, (RoomJid),  (RoomJid), AliceJid, <<>>}]),
    check_inbox(Kate, <<"1">>, [{<<"1">>, (RoomJid),  (RoomJid), KateJid, <<>>}]),
    check_inbox(Bob, <<"1">>, [{<<"1">>, (RoomJid),  (RoomJid), BobJid, <<>>}]),
    %% Now Bob sends marker
    ChatMarker = set_type(escalus_stanza:chat_marker(RoomJid, <<"displayed">>, Id), <<"groupchat">>),
    escalus:send(Bob, ChatMarker),
    R3 = escalus:wait_for_stanza(Alice),
    R4 = escalus:wait_for_stanza(Bob),
    R5 = escalus:wait_for_stanza(Kate),
    escalus_pred:is_chat_marker(<<"displayed">>, Id, R3),
    escalus_pred:is_chat_marker(<<"displayed">>, Id, R4),
    escalus_pred:is_chat_marker(<<"displayed">>, Id, R5),
    %% Bob asks for inbox second time
    check_inbox(Alice, <<"1">>, [{<<"0">>, (RoomJid),  (RoomJid), AliceJid, <<>>}]),
    check_inbox(Kate, <<"1">>, [{<<"1">>, (RoomJid),  (RoomJid), KateJid, <<>>}]),
    check_inbox(Bob, <<"1">>, [{<<"0">>, (RoomJid),  RoomJid, BobJid, <<>>}])

                                                           end).

create_groupchat(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    InitOccupants = [{Alice, member}, {Kate, member}],
    FinalOccupants = [{Bob, owner} | InitOccupants],
    InitConfig = [{<<"roomname">>, <<"Bob's room">>}],
    AliceJid = lbin(escalus_client:short_jid(Alice)),
    BobJid = lbin(escalus_client:short_jid(Bob)),
    KateJid = lbin(escalus_client:short_jid(Kate)),
    RoomNode = <<"bobroom">>,
    escalus:send(Bob, stanza_create_room(RoomNode, InitConfig, InitOccupants)),
    verify_aff_bcast(FinalOccupants, FinalOccupants),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),
    %% affiliation change messages in inbox
    check_inbox(Alice, <<"1">>, [{<<"1">>, room_bin_jid(RoomNode), AliceJid, <<>>}]),
    check_inbox(Bob, <<"1">>, [{<<"1">>, room_bin_jid(RoomNode), BobJid, <<>>}]),
    check_inbox(Kate, <<"1">>, [{<<"1">>, room_bin_jid(RoomNode), KateJid, <<>>}])
                                                           end).


check_inbox(Client, ExpectedCount, MsgCheckList) when is_integer(ExpectedCount) ->
  check_inbox(Client, integer_to_binary(ExpectedCount), MsgCheckList);
check_inbox(Client, ExpectedCount, MsgCheckList) ->
  check_inbox(Client, ExpectedCount, MsgCheckList, fun(X) -> X end).

check_inbox(Client, ExpectedCount, MsgCheckList, AdditionalCheck) ->
  GetInbox = get_inbox_stanza(),
  escalus:send(Client, GetInbox),
  Stanzas = escalus:wait_for_stanzas(Client, binary_to_integer(ExpectedCount)),
  ResIQ = escalus:wait_for_stanza(Client),
  ExpectedCount = get_inbox_count(ResIQ),
  Merged = lists:zip(Stanzas, MsgCheckList),
  io:format("Merged is ~p~n", [Merged]),
  io:format("and stanzas is ~p~n", [Stanzas]),
  Res = [foreach_inbox_message(M, Unread, FromJid, ToJid, Content)
    || {M, {Unread, FromJid, ToJid, Content}} <- Merged],
  io:format("Res is ~p~n",[Res]),
  %% Apply additional asserts
  [AdditionalCheck(M) || M <- Stanzas].



foreach_inbox_message(Message, Unread, FromJid, ToJid, Content) ->
  Unread = get_unread_count(Message),
  true = escalus_pred:is_message(Message),
  Unread = get_unread_count(Message),
  [InnerMsg] = get_inner_msg(Message),
  FromJid = exml_query:path(InnerMsg, [{attr, <<"from">>}]),
  ToJid = exml_query:path(InnerMsg, [{attr, <<"to">>}]),
  InnerContent = exml_query:path(InnerMsg, [{element, <<"body">>}, cdata], []),
  InnerContent = Content.



%% Helpers
%

set_type(Msg, Type) ->
  Attrs = Msg#xmlel.attrs,
  Msg#xmlel{attrs = Attrs ++ [{<<"type">>, Type}]}.

-spec bin(Bin :: binary()) -> binary().
bin(Bin) -> list_to_binary(binary_to_list(Bin)).


get_inner_msg(Msg) ->
  exml_query:paths(Msg, [{element, <<"result">>}, {element, <<"forwarded">>},
    {element, <<"message">>}]).

get_inbox_stanza() ->
  GetIQ = escalus_stanza:iq_get(?NS_ESL_INBOX, []),
  Id = escalus_stanza:id(),
  QueryTag = #xmlel{name = <<"inbox">>,
    attrs = [{<<"queryid">>, Id}, {<<"xmlns">>, ?NS_ESL_INBOX}]},
  GetIQ#xmlel{children = [QueryTag]}.

get_unread_count(Msg) ->
  [Val] = exml_query:paths(Msg, [{element, <<"result">>}, {attr, <<"unread">>}]),
  Val.

get_inbox_count(Packet) ->
  [Val] = exml_query:paths(Packet, [{element_with_ns, ?NS_ESL_INBOX}, cdata]),
  Val.

get_msg_id(Msg) ->
  exml_query:path(Msg, [{attr, <<"id">>}], undefined).

clear_inbox_all(UserList, Host) ->
  JIDs = [escalus_users:get_jid(escalus_users:get_users(UserList),U) || U <- UserList],
  [escalus_ejabberd:rpc(mod_inbox, clear_inbox, [JID,Host]) || JID <- JIDs].


-spec ver(Int :: integer()) -> binary().
ver(Int) ->
  <<"ver-", (list_to_binary(integer_to_list(Int)))/binary>>.

assert_aff_change_stanza(Stanza, Target, Change) ->
  TargetJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Target)),
  ID = exml_query:attr(Stanza, <<"id">>),
  DisplayName = exml_query:path(Stanza,
    [{element_with_ns, <<"forward:groupchat-display-name">>}, {attr, <<"name">>}]),
  true = (DisplayName /= undefined),
  true = is_binary(ID) andalso ID /= <<>>,
  Users = exml_query:paths(Stanza, [{element, <<"x">>}, {element, <<"user">>}]),
  [User] = [User || User <- Users, TargetJID == exml_query:cdata(User)],
  Change = exml_query:attr(User, <<"affiliation">>),
  TargetJID = exml_query:cdata(User).

special_chat_to(Client, Content, SenderDisplay, RecipientDisplay) ->
  Msg1 = escalus_stanza:chat_to(Client, Content),
  DisplayNameTag = #xmlel{name = <<"data">>,
    attrs = [{<<"xmlns">>, ?NS_DISPLAY},
      {<<"sender">>,SenderDisplay}, {<<"recipient">>, RecipientDisplay}]},
  Msg1#xmlel{children = Msg1#xmlel.children ++ [DisplayNameTag]}.


