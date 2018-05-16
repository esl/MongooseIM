-module(inbox_SUITE).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

-import(escalus_ejabberd, [rpc/3]).

-export([all/0,
         groups/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).
%% tests
-export([simple_msg/1,
         empty_inbox/1,
         two_unread/1,
         mark_read/1,
         mark_unread_bad_id/1,
         two_conversations/1,
         to_offline/1,
         to_not_existing/1,
         simple_muclight/1,
         advanced_muclight/1,
         groupchat_markers_simple/1,
         groupchat_markers_advanced/1,
         create_groupchat/1,
         create_groupchat_no_aff/1,
         leave_and_remove_conversation/1,
         leave_and_remain_conversation/1]).

-import(muc_helper, [foreach_occupant/3, foreach_recipient/2]).
-import(muc_light_helper, [bin_aff_users/1, aff_msg_verify_fun/1, gc_message_verify_fun/3, ver/1,
        lbin/1, room_bin_jid/1, verify_aff_bcast/2, verify_aff_bcast/3, verify_aff_users/2,
        kv_el/2, to_lus/2, stanza_create_room/3, create_room/6, stanza_aff_set/2, default_config/0]).

-define(NS_ESL_INBOX, <<"erlang-solutions.com:xmpp:inbox:0">>).
-define(ROOM, <<"testroom1">>).
-define(ROOM2, <<"testroom2">>).

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
        simple_msg,
        two_conversations,
        to_offline,
        to_not_existing,
        empty_inbox,
        two_unread,
        mark_read,
        mark_unread_bad_id
      ]},
      {muclight, [sequence], [
        simple_muclight,
        advanced_muclight,
        groupchat_markers_simple,
        groupchat_markers_advanced,
        create_groupchat,
        create_groupchat_no_aff,
        leave_and_remove_conversation,
        leave_and_remain_conversation
      ]}
  ].

suite() ->
  escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
  ok = dynamic_modules:ensure_modules(domain(), required_modules()),
  InboxOptions = inbox_opts(),
  Config1 = escalus:init_per_suite(Config),
  Config2 = [{inbox_opts, InboxOptions} | Config1],
  escalus:create_users(Config2, escalus:get_users([alice, bob, kate, mike, carol])).

required_modules() ->
  [
    {mod_mam, [{archive_chat_markers, true}]},
    {mod_mam_muc, [{archive_chat_markers, true}]},
    {mod_muc_light, [{host, binary_to_list(muclight_domain())},
                     {backend, odbc}]},
    {mod_inbox, inbox_opts()}
  ].

inbox_opts() ->
  [{backend, odbc}, {aff_changes, true}, {remove_on_kicked, true}, {groupchat, [muclight]},  {markers, [displayed]}].

domain() ->
  ct:get_config({hosts, mim, domain}).

muclight_domain() ->
  Domain = domain(),
  <<"muclight.", Domain/binary>>.

end_per_suite(Config) ->
  Host = ct:get_config({hosts, mim, domain}),
  Config1 = escalus:delete_users(Config, escalus:get_users([alice, bob, kate, mike])),
  dynamic_modules:stop(Host, mod_inbox),
  dynamic_modules:stop(Host, mod_muc_light),
  escalus:end_per_suite(Config1).

init_per_group(muclight, Config) ->
  create_room(?ROOM, muclight_domain(), alice, [bob, kate], Config, ver(1));
init_per_group(_GroupName, Config) ->
  Config.

end_per_group(muclight, Config) ->
  muc_light_helper:clear_db(),
  Config;
end_per_group(_GroupName, Config) ->
  Config.


init_per_testcase(create_groupchat_no_aff, Config) ->
  clear_inbox_all(),
  reload_inbox_option(Config, aff_changes, false),
  escalus:init_per_testcase(create_groupchat_no_aff, Config);
init_per_testcase(leave_and_remove_conversation, Config) ->
  clear_inbox_all(),
  create_room(?ROOM2, muclight_domain(), alice, [bob, kate], Config, ver(1)),
  escalus:init_per_testcase(kick_and_remove_conversation, Config);
init_per_testcase(leave_and_remain_conversation, Config) ->
  clear_inbox_all(),
  reload_inbox_option(Config, remove_on_kicked, false),
  escalus:init_per_testcase(leave_and_remain_conversation, Config);
init_per_testcase(CaseName, Config) ->
  clear_inbox_all(),
  escalus:init_per_testcase(CaseName, Config).

end_per_testcase(leave_and_remove_conversation, Config) ->
  clear_inbox_all(),
  muc_light_helper:clear_db(),
  restore_inbox_option(Config),
  escalus:end_per_testcase(leave_and_remove_conversation, Config);
end_per_testcase(create_groupchat_no_aff, Config) ->
  clear_inbox_all(),
  restore_inbox_option(Config),
  escalus:end_per_testcase(create_groupchat_no_aff, Config);
end_per_testcase(leave_and_remain_conversation, Config) ->
  clear_inbox_all(),
  restore_inbox_option(Config),
  escalus:end_per_testcase(leave_and_remain_conversation, Config);
end_per_testcase(CaseName, Config) ->
  clear_inbox_all(),
  escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Inbox tests one-to-one
%%--------------------------------------------------------------------

simple_msg(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
    Msg1 = escalus_stanza:chat_to(Bob, <<"Hello">>),
    BobJid = lbin(escalus_client:full_jid(Bob)),
    AliceJid = lbin(escalus_client:full_jid(Alice)),
    escalus:send(Alice, Msg1),
    M = escalus:wait_for_stanza(Bob),
    escalus:assert(is_chat_message, M),
    check_inbox(Alice, <<"1">>, [{<<"0">>, AliceJid, BobJid,<<"Hello">>}]),
    check_inbox(Bob, <<"1">>, [{<<"1">>, AliceJid, BobJid,<<"Hello">>}])
                                                end).

two_conversations(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    Msg1 = escalus_stanza:chat_to(Bob, <<"Hello Bob">>),
    Msg2 = escalus_stanza:chat_to(Kate, <<"Hello Kate">>),
    BobJid = lbin(escalus_client:full_jid(Bob)),
    AliceJid = lbin(escalus_client:full_jid(Alice)),
    KateJid = lbin(escalus_client:full_jid(Kate)),
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

to_offline(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
    mongoose_helper:logout_user(Config, Bob),
    Msg1 = escalus_stanza:chat_to(Bob, <<"test">>),
    BobJid = lbin(escalus_client:full_jid(Bob)),
    AliceJid = lbin(escalus_client:full_jid(Alice)),
    escalus:send(Alice, Msg1),
    {ok, NewBob} = escalus_client:start(Config, bob, <<"new-session">>),
    escalus:send(NewBob, escalus_stanza:presence(<<"available">>)),
    Stanzas = escalus:wait_for_stanzas(NewBob, 2),
    escalus_new_assert:mix_match
    ([is_presence, fun(Stanza) -> escalus_pred:is_chat_message(<<"test">>, Stanza) end],
      Stanzas),
    check_inbox(Alice, <<"1">>, [{<<"0">>, AliceJid, BobJid,<<"test">>}]),
    check_inbox(NewBob, <<"1">>, [{<<"1">>, AliceJid, BobJid,<<"test">>}])
                                                end).

to_not_existing(Config) ->
  escalus:story(Config, [{alice, 1}], fun(Alice) ->
    Msg1 = escalus_stanza:chat_to(<<"not_existing_user@localhost">>, <<"test">>),
    AliceJid = lbin(escalus_client:full_jid(Alice)),
    escalus:send(Alice, Msg1),
    ServiceUnavailable = escalus:wait_for_stanza(Alice),
    escalus_pred:is_error(<<"cancel">>,<<"service-unavailable">>, ServiceUnavailable),
    check_inbox(Alice, <<"1">>, [{<<"0">>, AliceJid, <<"not_existing_user@localhost">>,<<"test">>}])
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
    %% sender is full JID
    KateJid = lbin(escalus_client:full_jid(Kate)),
    %% receiver is full JID
    MikeJid = lbin(escalus_client:full_jid(Mike)),
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
    MsgId =  <<"123123">>,
    Msg1 = escalus_stanza:set_id(escalus_stanza:chat_to(Mike, <<"Hi mike">>), MsgId),
    %% sender is full JID
    KateJid = lbin(escalus_client:full_jid(Kate)),
    MikeJid = lbin(escalus_client:full_jid(Mike)),
    escalus:send(Kate, Msg1),
    M1 = escalus:wait_for_stanza(Mike),
    escalus:assert(is_chat_message, M1),
    check_inbox(Mike, <<"1">>, [{<<"1">>, KateJid, MikeJid, <<"Hi mike">>}]),
    ChatMarker = escalus_stanza:chat_marker(KateJid, <<"displayed">>, MsgId),
    escalus:send(Mike, ChatMarker),
    %% Now Mike asks for inbox second time
    check_inbox(Mike, <<"1">>, [{<<"0">>, KateJid, MikeJid, <<"Hi mike">>}])
                                                end).

mark_unread_bad_id(Config) ->
  escalus:story(Config, [{kate, 1}, {mike, 1}], fun(Kate, Mike) ->
    Msg1 = escalus_stanza:set_id(escalus_stanza:chat_to(Mike, <<"okey dockey">>), <<"111">>),
    %% sender is bare JID
    KateJid = lbin(escalus_client:full_jid(Kate)),
    MikeJid = lbin(escalus_client:full_jid(Mike)),
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
    KateJid = lbin(escalus_client:short_jid(Kate)),
    BobJid = lbin(escalus_client:short_jid(Bob)),
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
    check_inbox(Alice, <<"1">>, [{<<"0">>, AliceRoomJid,  AliceJid, Msg}]),
    check_inbox(Bob, <<"1">>, [{<<"1">>, AliceRoomJid,  BobJid, Msg}]),
    check_inbox(Kate, <<"1">>, [{<<"1">>, AliceRoomJid,  KateJid, Msg}])
                                                           end).

advanced_muclight(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    Msg1 = <<"Hi Room!">>,
    Msg2 = <<"How are you?">>,
    Id = <<"MyID">>,
    BobJid = lbin(escalus_client:short_jid(Bob)),
    AliceJid = lbin(escalus_client:short_jid(Alice)),
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
    check_inbox(Alice, <<"1">>, [{<<"1">>, BobRoomJid,  AliceJid, Msg2}]),
    check_inbox(Bob, <<"1">>, [{<<"0">>, BobRoomJid,  BobJid, Msg2}]),
    check_inbox(Kate, <<"1">>, [{<<"2">>, BobRoomJid,  KateJid, Msg2}])
                                                           end).
groupchat_markers_simple(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    RoomName = <<"markers_room">>,
    AliceJid = lbin(escalus_client:short_jid(Alice)),
    RoomJid = room_bin_jid(RoomName),
    AliceRoomJid = <<RoomJid/binary,"/", AliceJid/binary>>,
    Msg = <<"Mark me!">>,
    create_room_send_msg_check_inbox(Alice, [Bob, Kate], RoomName, Msg, <<"some-id">>),
    [mark_last_muclight_message(U, [Alice, Bob, Kate], <<"1">>) || U <- [Bob, Kate]],
    foreach_check_inbox([Bob, Kate, Alice], <<"1">>, <<"0">>, AliceRoomJid, Msg)
                                                           end).

groupchat_markers_advanced(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    Msg = <<"Welcome guys">>,
    RoomName = <<"markers_room2">>,
    RoomJid = room_bin_jid(RoomName),
    AliceJid = lbin(escalus_client:short_jid(Alice)),
    AliceRoomJid = <<RoomJid/binary,"/", AliceJid/binary>>,
    BobJid = lbin(escalus_client:short_jid(Bob)),
    KateJid = lbin(escalus_client:short_jid(Kate)),
    create_room_send_msg_check_inbox(Alice, [Bob, Kate], RoomName, Msg, <<"another-id">>),
    %% Now Bob sends marker
    mark_last_muclight_message(Bob, [Alice, Bob, Kate], <<"1">>),
    %% The crew ask for inbox second time. Only Kate has unread messages
    check_inbox(Alice, <<"1">>, [{<<"0">>, AliceRoomJid, AliceJid, Msg}]),
    check_inbox(Kate, <<"1">>, [{<<"1">>, AliceRoomJid,  KateJid, Msg}]),
    check_inbox(Bob, <<"1">>, [{<<"0">>,  AliceRoomJid,  BobJid, Msg}])

                                                           end).
create_groupchat(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    RoomNode = <<"bobroom">>,
    create_room_and_check_inbox(Bob, [Alice, Kate], RoomNode)
                                                           end).

create_groupchat_no_aff(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    InitOccupants = [{Alice, member}, {Kate, member}],
    FinalOccupants = [{Bob, owner} | InitOccupants],
    Msg = <<"Hi all!">>,
    InitConfig = [{<<"roomname">>, <<"Bob's room2">>}],
    RoomNode = <<"bobroom2">>,
    RoomJid = room_bin_jid(RoomNode),
    AliceJid = lbin(escalus_client:short_jid(Alice)),
    AliceRoomJid = <<RoomJid/binary,"/", AliceJid/binary>>,
    BobJid = lbin(escalus_client:short_jid(Bob)),
    KateJid = lbin(escalus_client:short_jid(Kate)),
    %% Bob creates room
    escalus:send(Bob, stanza_create_room(RoomNode, InitConfig, InitOccupants)),
    verify_aff_bcast(FinalOccupants, FinalOccupants),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),
    %% affiliation change messages are not stored in inbox
    check_inbox(Alice, <<"0">>, []),
    check_inbox(Bob, <<"0">>, []),
    check_inbox(Kate, <<"0">>, []),
    Stanza = escalus_stanza:set_id(
      escalus_stanza:groupchat_to(RoomJid, Msg), <<"123">>),
    %% Alice sends a message
    escalus:send(Alice, Stanza),
    R0 = escalus:wait_for_stanza(Alice),
    R1 = escalus:wait_for_stanza(Bob),
    R2 = escalus:wait_for_stanza(Kate),
    escalus:assert(is_groupchat_message, R0),
    escalus:assert(is_groupchat_message, R1),
    escalus:assert(is_groupchat_message, R2),
    %% now inboxes are not empty
    check_inbox(Alice, <<"1">>, [{<<"0">>, AliceRoomJid,  AliceJid, Msg}]),
    check_inbox(Bob, <<"1">>, [{<<"1">>, AliceRoomJid,  BobJid, Msg}]),
    check_inbox(Kate, <<"1">>, [{<<"1">>, AliceRoomJid,  KateJid, Msg}])
                                                           end).

leave_and_remove_conversation(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    Msg = <<"Hi Room!">>,
    Id = <<"MyID">>,
    AliceJid = lbin(escalus_client:short_jid(Alice)),
    KateJid = lbin(escalus_client:short_jid(Kate)),
    RoomJid = room_bin_jid(?ROOM2),
    AliceRoomJid = <<RoomJid/binary,"/", AliceJid/binary>>,
    Stanza = escalus_stanza:set_id(
      escalus_stanza:groupchat_to(RoomJid, Msg), Id),
    %% Alice sends msg to room
    escalus:send(Alice, Stanza),
    R0 = escalus:wait_for_stanza(Alice),
    R1 = escalus:wait_for_stanza(Bob),
    R2 = escalus:wait_for_stanza(Kate),
    escalus:assert(is_groupchat_message, R0),
    escalus:assert(is_groupchat_message, R1),
    escalus:assert(is_groupchat_message, R2),
    %% Bob leaves the room
    muc_light_helper:user_leave(?ROOM2, Bob, [{Alice, owner}, {Kate, member}]),
    %% Alice and Kate have one message
    check_inbox(Alice, <<"1">>, [{<<"0">>, AliceRoomJid,  AliceJid, Msg}]),
    check_inbox(Kate, <<"1">>, [{<<"1">>, AliceRoomJid,  KateJid, Msg}]),
    %% Bob doesn't have conversation in his inbox
    check_inbox(Bob, <<"0">>, [])
                                                           end).

leave_and_remain_conversation(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    RoomName = <<"kicking-room">>,
    AliceJid = lbin(escalus_client:short_jid(Alice)),
    BobJid = lbin(escalus_client:short_jid(Bob)),
    KateJid = lbin(escalus_client:short_jid(Kate)),
    RoomJid = room_bin_jid(RoomName),
    AliceRoomJid = <<RoomJid/binary,"/", AliceJid/binary>>,
    Msg = <<"Hi all">>,
    %% Alice creates a room and send msg
    create_room_send_msg_check_inbox(Alice, [Bob, Kate], RoomName, Msg, <<"leave-id">>),
    %% Bob leaves room
    muc_light_helper:user_leave(RoomName, Bob, [{Alice, owner}, {Kate, member}]),
    check_inbox(Alice, <<"1">>, [{<<"0">>, AliceRoomJid,  AliceJid, Msg}]),
    check_inbox(Kate, <<"1">>, [{<<"1">>, AliceRoomJid,  KateJid, Msg}]),
    %% Bob still has a conversation in inbox
    check_inbox(Bob, <<"1">>, [{<<"1">>, AliceRoomJid,  BobJid, Msg}])
                                                           end).

%%%%
%% Helpers

create_room_and_check_inbox(Owner, MemberList, RoomName) ->
  InitOccupants = [{M, member} || M <- MemberList],
  FinalOccupants = [{Owner, owner} | InitOccupants],
  InitConfig = [{<<"roomname">>, <<"Just room name">>}],
  OwnerJid = lbin(escalus_client:short_jid(Owner)),
  MembersJids = [lbin(escalus_client:short_jid(M)) || M <- MemberList],
  MemberAndJids = lists:zip(MemberList, MembersJids),
  MembersAndOwner = [Owner | MemberList],
  %% Owner creates room
  escalus:send(Owner, stanza_create_room(RoomName, InitConfig, InitOccupants)),
  verify_aff_bcast(FinalOccupants, FinalOccupants),
  escalus:assert(is_iq_result, escalus:wait_for_stanza(Owner)),
  %% check for the owner. Unread from owner is affiliation change to owner
  check_inbox(Owner, <<"1">>, [{<<"1">>, room_bin_jid(RoomName), OwnerJid, <<>>}]),
  %% check for the members. Every has affiliation change to member
  [check_inbox(Member, <<"1">>, [{<<"1">>, room_bin_jid(RoomName), Jid, <<>>}])
    || {Member, Jid} <- MemberAndJids],
  %% Each room participant send chat marker
  [begin mark_last_muclight_system_message(U, <<"1">>),
         foreach_recipient(MembersAndOwner, fun(_Stanza) -> ok end) end || U <- MembersAndOwner],
  %% counter is reset for owner
  check_inbox(Owner, <<"1">>, [{<<"0">>, room_bin_jid(RoomName), OwnerJid, <<>>}]),
  %% counter is reset for members
  [check_inbox(Member, <<"1">>, [{<<"0">>, room_bin_jid(RoomName), Jid, <<>>}])
    || {Member, Jid} <- MemberAndJids].


mark_last_muclight_message(User, AllUsers, ExpectedCount) ->
  mark_last_muclight_message(User, AllUsers, ExpectedCount, <<"displayed">>).


mark_last_muclight_message(User, AllUsers, ExpectedCount, MarkerType) ->
  GetInbox = get_inbox_stanza(),
  escalus:send(User, GetInbox),
  Stanzas = escalus:wait_for_stanzas(User, binary_to_integer(ExpectedCount)),
  ResIQ = escalus:wait_for_stanza(User),
  ExpectedCount = get_inbox_count(ResIQ),
  LastMsg = lists:last(Stanzas),
  [InnerMsg] = get_inner_msg(LastMsg),
  MsgId = exml_query:attr(InnerMsg, <<"id">>),
  From = exml_query:attr(InnerMsg, <<"from">>),
  FromBare = escalus_utils:get_short_jid(From),
  ChatMarker = set_type(escalus_stanza:chat_marker(FromBare,MarkerType, MsgId), <<"groupchat">>),
  escalus:send(User, ChatMarker),
  foreach_recipient(AllUsers, fun(Marker) ->
    true = escalus_pred:is_chat_marker(MarkerType, MsgId, Marker)
                              end).


mark_last_muclight_system_message(User, ExpectedCount) ->
  mark_last_muclight_system_message(User, ExpectedCount, <<"displayed">>).

mark_last_muclight_system_message(User, ExpectedCount, MarkerType) ->
  GetInbox = get_inbox_stanza(),
  escalus:send(User, GetInbox),
  Stanzas = escalus:wait_for_stanzas(User, binary_to_integer(ExpectedCount)),
  ResIQ = escalus:wait_for_stanza(User),
  ExpectedCount = get_inbox_count(ResIQ),
  LastMsg = lists:last(Stanzas),
  [InnerMsg] = get_inner_msg(LastMsg),
  MsgId = exml_query:attr(InnerMsg, <<"id">>),
  From = exml_query:attr(InnerMsg, <<"from">>),
  ChatMarker =
    set_type(escalus_stanza:chat_marker(From,MarkerType, MsgId), <<"groupchat">>),
  escalus:send(User, ChatMarker).



create_room_send_msg_check_inbox(Owner, MemberList, RoomName, Msg, Id) ->
  RoomJid = room_bin_jid(RoomName),
  OwnerJid = lbin(escalus_client:short_jid(Owner)),
  create_room_and_check_inbox(Owner, MemberList, RoomName),
  Stanza = escalus_stanza:set_id(
    escalus_stanza:groupchat_to(RoomJid, Msg), Id),
  escalus:send(Owner, Stanza),
  foreach_recipient([Owner | MemberList],
    fun(Stanza) ->
    escalus:assert(is_groupchat_message, Stanza)
    end),
  %% send chat marker per each
  OwnerRoomJid = <<RoomJid/binary,"/", OwnerJid/binary>>,
  %% Owner sent the message so he has unread set to 0
  check_inbox(Owner, <<"1">>, [{<<"0">>, OwnerRoomJid, OwnerJid, Msg}]),
  foreach_check_inbox(MemberList, <<"1">>, <<"1">>, OwnerRoomJid, Msg).


foreach_check_inbox(Users, Total, Unread, SenderJid, Msg) ->
  [begin
     UserJid = lbin(escalus_client:short_jid(U)),
     check_inbox(U, Total, [{Unread, SenderJid,  UserJid, Msg}])
    end || U <- Users].


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
 [process_inbox_message(M, Unread, FromJid, ToJid, Content)
    || {M, {Unread, FromJid, ToJid, Content}} <- Merged],
  %% Apply additional asserts
  [AdditionalCheck(M) || M <- Stanzas].


process_inbox_message(Message, Unread, FromJid, ToJid, Content) ->
  Unread = get_unread_count(Message),
  escalus:assert(is_message, Message),
  Unread = get_unread_count(Message),
  [InnerMsg] = get_inner_msg(Message),
  FromJid = lbin(exml_query:attr(InnerMsg, <<"from">>)),
  ToJid = lbin(exml_query:attr(InnerMsg, <<"to">>)),
  InnerContent = exml_query:path(InnerMsg, [{element, <<"body">>}, cdata], []),
  Content = InnerContent.

set_type(Msg, Type) ->
  Attrs = lists:keystore(<<"type">>, 1, Msg#xmlel.attrs, {<<"type">>, Type}),
  Msg#xmlel{attrs = Attrs}.



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


clear_inbox_all() ->
  Host = ct:get_config({hosts, mim, domain}),
  clear_inboxes([alice, bob, kate, mike], Host).

clear_inboxes(UserList, Host) ->
  JIDs = [escalus_users:get_jid(escalus_users:get_users(UserList),U) || U <- UserList],
  [escalus_ejabberd:rpc(mod_inbox_utils, clear_inbox, [JID,Host]) || JID <- JIDs].


reload_inbox_option(Config, Key, Value) ->
  Host = domain(),
  Args = proplists:get_value(inbox_opts, Config),
  Args1 = lists:keyreplace(Key, 1, Args, {Key, Value}),
  dynamic_modules:restart(Host, mod_inbox, Args1).

restore_inbox_option(Config) ->
  Host = domain(),
  Args = proplists:get_value(inbox_opts, Config),
  dynamic_modules:restart(Host, mod_inbox, Args).