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
-export([msg_sent_stored_in_inbox/1,
         user_has_empty_inbox/1,
         user_has_two_unread_messages/1,
         reset_unread_counter/1,
         try_to_reset_unread_counter_with_bad_marker/1,
         user_has_two_conversations/1,
         msg_sent_to_offline_user/1,
         msg_sent_to_not_existing_user/1,
         simple_groupchat_stored_in_all_inbox/1,
         advanced_groupchat_stored_in_all_inbox/1,
         groupchat_markers_one_reset/1,
         create_groupchat/1,
         create_groupchat_no_affiliation_stored/1,
         leave_and_remove_conversation/1,
         leave_and_store_conversation/1,
         groupchat_markers_one_reset_room_created/1,
         groupchat_markers_all_reset_room_created/1,
         no_aff_stored_and_remove_on_kicked/1,
         no_stored_and_remain_after_kicked/1,
         simple_groupchat_stored_in_all_inbox_muc/1,
         simple_groupchat_stored_in_offline_users_inbox_muc/1,
         unread_count_is_the_same_after_going_online_again/1,
         unread_count_is_reset_after_sending_chatmarker/1,
         private_messages_are_/1
        ]).

-import(muc_helper, [foreach_occupant/3, foreach_recipient/2]).
-import(muc_light_helper, [bin_aff_users/1, aff_msg_verify_fun/1, gc_message_verify_fun/3, ver/1,
                           lbin/1, room_bin_jid/1, verify_aff_bcast/2, verify_aff_bcast/3,
                           verify_aff_users/2, kv_el/2, to_lus/2, stanza_create_room/3,
                           create_room/6, stanza_aff_set/2, default_config/0]).

-define(NS_ESL_INBOX, <<"erlang-solutions.com:xmpp:inbox:0">>).
-define(ROOM, <<"testroom1">>).
-define(ROOM2, <<"testroom2">>).
-define(ROOM3, <<"testroom3">>).
-define(ROOM4, <<"testroom4">>).
-define(ROOM_MARKERS, <<"room_markers">>).
-define(MUC_ROOM, <<"some_muc_room">>).
-define(MUC_ROOM2, <<"some_muc_room2">>).
-define(MUC_ROOM3, <<"some_muc_room3">>).
-define(MUC_ROOM4, <<"some_muc_room4">>).
-define(MUC_ROOM5, <<"some_muc_room5">>).
-define(MUC_DOMAIN, <<"muc.localhost">>).
-record(conv, {unread, from, to, content = <<>>, verify = fun(C, Stanza) -> ok end}).
-record(inbox, {total, convs = []}).
%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    case is_odbc_enabled(domain()) of
      true ->
          tests();
      false ->
          {skip, require_odbc}
    end.

tests() ->
  [
      {group, one_to_one},
      {group, muclight},
      {group, muc}
  ].

groups() ->
    G = [
         {one_to_one, [sequence],
          [
           user_has_empty_inbox,
           msg_sent_stored_in_inbox,
           user_has_two_conversations,
           msg_sent_to_offline_user,
           msg_sent_to_not_existing_user,
           user_has_two_unread_messages,
           reset_unread_counter,
           try_to_reset_unread_counter_with_bad_marker
          ]},
         {muclight, [sequence],
          [
           simple_groupchat_stored_in_all_inbox,
           advanced_groupchat_stored_in_all_inbox,
           groupchat_markers_one_reset,
           create_groupchat,
           create_groupchat_no_affiliation_stored,
           leave_and_remove_conversation,
           leave_and_store_conversation,
           no_aff_stored_and_remove_on_kicked,
           no_stored_and_remain_after_kicked,
           groupchat_markers_one_reset_room_created,
           groupchat_markers_all_reset_room_created
          ]},
         {muc, [sequence],
          [
           simple_groupchat_stored_in_all_inbox_muc,
           simple_groupchat_stored_in_offline_users_inbox_muc,
           unread_count_is_the_same_after_going_online_again,
           unread_count_is_reset_after_sending_chatmarker,
           private_messages_are_
          ]}
        ].
    %ct_helper:repeat_all_until_all_ok(G).

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
  escalus:create_users(Config2, escalus:get_users([alice, bob, kate, mike])).

is_odbc_enabled(Host) ->
  mongoose_helper:is_odbc_enabled(Host).

required_modules() ->
  [
    {mod_muc_light, [{host, binary_to_list(muclight_domain())},
                     {backend, odbc}]},
    {mod_inbox, inbox_opts()}
  ].

inbox_opts() ->
  [{backend, odbc},
   {aff_changes, true},
   {remove_on_kicked, true},
   {groupchat, [muclight]},
   {markers, [displayed]}].

domain() ->
  ct:get_config({hosts, mim, domain}).

muclight_domain() ->
  Domain = domain(),
  <<"muclight.", Domain/binary>>.

muc_domain() ->
    Domain = domain(),
    <<"muc.", Domain/binary>>.

end_per_suite(Config) ->
  Host = ct:get_config({hosts, mim, domain}),
  Config1 = escalus:delete_users(Config, escalus:get_users([alice, bob, kate, mike])),
  dynamic_modules:stop(Host, mod_inbox),
  dynamic_modules:stop(Host, mod_muc_light),
  muc_light_helper:clear_db(),
  escalus:end_per_suite(Config1).

init_per_group(one_to_one, Config) ->
  reload_inbox_option(Config, groupchat, []);
init_per_group(muclight, Config) ->
  reload_inbox_option(Config, groupchat, [muclight]),
  create_room(?ROOM, muclight_domain(), alice, [bob, kate], Config, ver(1));
init_per_group(muc, Config) ->
  muc_helper:load_muc(muc_domain()),
  reload_inbox_option(Config, groupchat, [muc]);
init_per_group(_GroupName, Config) ->
  Config.

end_per_group(muclight, Config) ->
  muc_light_helper:clear_db(),
  Config;
end_per_group(muc, Config) ->
    Config;
end_per_group(_GroupName, Config) ->
  Config.


init_per_testcase(create_groupchat_no_affiliation_stored, Config) ->
  clear_inbox_all(),
  reload_inbox_option(Config, aff_changes, false),
  escalus:init_per_testcase(create_groupchat_no_affiliation_stored, Config);
init_per_testcase(groupchat_markers_one_reset, Config) ->
  clear_inbox_all(),
  create_room(?ROOM_MARKERS, muclight_domain(), alice, [bob, kate], Config, ver(1)),
  escalus:init_per_testcase(groupchat_markers_one_reset, Config);
init_per_testcase(leave_and_remove_conversation, Config) ->
  clear_inbox_all(),
  create_room(?ROOM2, muclight_domain(), alice, [bob, kate], Config, ver(1)),
  escalus:init_per_testcase(leave_and_remove_conversation, Config);
init_per_testcase(leave_and_store_conversation, Config) ->
  clear_inbox_all(),
  reload_inbox_option(Config, remove_on_kicked, false),
  escalus:init_per_testcase(leave_and_store_conversation, Config);
init_per_testcase(no_aff_stored_and_remove_on_kicked, Config) ->
    clear_inbox_all(),
    create_room(?ROOM3, muclight_domain(), alice, [bob, kate], Config, ver(1)),
    reload_inbox_option(Config, [{remove_on_kicked, true}, {aff_changes, false}]),
    escalus:init_per_testcase(no_aff_stored_and_remove_on_kicked, Config);
init_per_testcase(no_stored_and_remain_after_kicked, Config) ->
  clear_inbox_all(),
  create_room(?ROOM4, muclight_domain(), alice, [bob, kate], Config, ver(1)),
  reload_inbox_option(Config, [{remove_on_kicked, false}, {aff_changes, true}]),
  escalus:init_per_testcase(no_stored_and_remain_after_kicked, Config);
init_per_testcase(simple_groupchat_stored_in_all_inbox_muc = TC, Config) ->
  clear_inbox_all(),
  [User | _] = ?config(escalus_users, Config), % probably change this line as it should always take Alice to create the room
  Config2 = muc_helper:start_room(Config, User, ?MUC_ROOM, <<"some_friendly_name">>, default),
  escalus:init_per_testcase(TC, Config2);
init_per_testcase(simple_groupchat_stored_in_offline_users_inbox_muc = TC, Config) ->
  clear_inbox_all(),
  [User | _] = ?config(escalus_users, Config), % probably change this line as it should always take Alice to create the room
  Config2 = muc_helper:start_room(Config, User, ?MUC_ROOM2, <<"some_friendly_name">>, default),
  escalus:init_per_testcase(TC, Config2);
init_per_testcase(unread_count_is_the_same_after_going_online_again = TC, Config) ->
  clear_inbox_all(),
  [User | _] = ?config(escalus_users, Config), % probably change this line as it should always take Alice to create the room
  Config2 = muc_helper:start_room(Config, User, ?MUC_ROOM3, <<"some_friendly_name">>, default),
  escalus:init_per_testcase(TC, Config2);
init_per_testcase(unread_count_is_reset_after_sending_chatmarker = TC, Config) ->
  clear_inbox_all(),
  [User | _] = ?config(escalus_users, Config), % probably change this line as it should always take Alice to create the room
  Config2 = muc_helper:start_room(Config, User, ?MUC_ROOM4, <<"some_friendly_name">>, default),
  escalus:init_per_testcase(TC, Config2);
init_per_testcase(private_messages_are_ = TC, Config) ->
  clear_inbox_all(),
  [User | _] = ?config(escalus_users, Config), % probably change this line as it should always take Alice to create the room
  Config2 = muc_helper:start_room(Config, User, ?MUC_ROOM5, <<"some_friendly_name">>, default),
  escalus:init_per_testcase(TC, Config2);

init_per_testcase(CaseName, Config) ->
  clear_inbox_all(),
  escalus:init_per_testcase(CaseName, Config).

end_per_testcase(groupchat_markers_one_reset, Config) ->
  clear_inbox_all(),
  restore_inbox_option(Config),
  escalus:end_per_testcase(groupchat_markers_one_reset, Config);
end_per_testcase(leave_and_remove_conversation, Config) ->
  clear_inbox_all(),
  restore_inbox_option(Config),
  escalus:end_per_testcase(leave_and_remove_conversation, Config);
end_per_testcase(create_groupchat_no_affiliation_stored, Config) ->
  clear_inbox_all(),
  restore_inbox_option(Config),
  escalus:end_per_testcase(create_groupchat_no_affiliation_stored, Config);
end_per_testcase(leave_and_store_conversation, Config) ->
  clear_inbox_all(),
  restore_inbox_option(Config),
  escalus:end_per_testcase(leave_and_store_conversation, Config);
end_per_testcase(no_aff_stored_and_remove_on_kicked, Config) ->
    clear_inbox_all(),
    restore_inbox_option(Config),
    escalus:end_per_testcase(no_aff_stored_and_remove_on_kicked, Config);
end_per_testcase(no_stored_and_remain_after_kicked, Config) ->
  clear_inbox_all(),
  restore_inbox_option(Config),
  escalus:end_per_testcase(no_stored_and_remain_after_kicked, Config);
end_per_testcase(msg_sent_to_not_existing_user, Config) ->
  Host = ct:get_config({hosts, mim, domain}),
  escalus_ejabberd:rpc(mod_inbox_utils, clear_inbox, [<<"not_existing_user@localhost">>,Host]),
  escalus:end_per_testcase(msg_sent_to_not_existing_user, Config);
end_per_testcase(TC, Config) when TC =:= simple_groupchat_stored_in_all_inbox_muc,
                                  TC =:= simple_groupchat_stored_in_offline_users_inbox_muc,
                                  TC =:= unread_count_is_the_same_after_going_online_again,
                                  TC =:= unread_count_is_reset_after_sending_chatmarker,
                                  TC =:= private_messages_are_ ->
    muc_helper:destroy_room(Config);
end_per_testcase(CaseName, Config) ->
  clear_inbox_all(),
  escalus:end_per_testcase(CaseName, Config).




%%--------------------------------------------------------------------
%% Inbox tests one-to-one
%%--------------------------------------------------------------------

user_has_empty_inbox(Config) ->
  escalus:story(Config, [{kate, 1}], fun(Kate) ->
    Stanza = get_inbox_stanza(),
    %% Kate logs in for first time and ask for inbox
    escalus:send(Kate, Stanza),
    [ResIQ] = escalus:wait_for_stanzas(Kate, 1),
    true = escalus_pred:is_iq_result(ResIQ),
    %% Inbox is empty
    TotalCount = get_inbox_count(ResIQ),
    0 = TotalCount
                                     end).


msg_sent_stored_in_inbox(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
    Msg1 = escalus_stanza:chat_to(Bob, <<"Hello">>),
    BobJid = lbin(escalus_client:full_jid(Bob)),
    AliceJid = lbin(escalus_client:full_jid(Alice)),
    %% Alice sends msg to Bob
    escalus:send(Alice, Msg1),
    M = escalus:wait_for_stanza(Bob),
    escalus:assert(is_chat_message, M),
    %% Both check inbox
    check_inbox(Alice, #inbox{
      total = 1,
      convs = [#conv{unread = 0, from = AliceJid, to = BobJid, content = <<"Hello">>}]}),
    check_inbox(Bob, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = AliceJid, to = BobJid, content = <<"Hello">>}]})
                                                end).

user_has_two_conversations(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    Msg1 = escalus_stanza:chat_to(Bob, <<"Hello Bob">>),
    Msg2 = escalus_stanza:chat_to(Kate, <<"Hello Kate">>),
    BobJid = lbin(escalus_client:full_jid(Bob)),
    AliceJid = lbin(escalus_client:full_jid(Alice)),
    KateJid = lbin(escalus_client:full_jid(Kate)),
    %% Alice sends messages to Bob and Kate
    escalus:send(Alice, Msg1),
    escalus:send(Alice, Msg2),
    M1 = escalus:wait_for_stanza(Bob),
    M2 = escalus:wait_for_stanza(Kate),
    escalus:assert(is_chat_message, M1),
    escalus:assert(is_chat_message, M2),
    %% Alice has two conversations in her inbox (no unread messages)
    check_inbox(Alice, #inbox{
      total = 2,
      convs =
      [#conv{unread = 0, from = AliceJid, to = BobJid, content = <<"Hello Bob">>},
        #conv{unread = 0, from = AliceJid, to = KateJid, content = <<"Hello Kate">>}]}),
    %% Kate has one conversation with one unread
    check_inbox(Kate, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = AliceJid, to = KateJid, content = <<"Hello Kate">>}]}),
    %% Bob has one conversation with one unread
    check_inbox(Bob, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = AliceJid, to = BobJid, content = <<"Hello Bob">>}]})

                                                           end).

msg_sent_to_offline_user(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
    %% Bob goes offline
    mongoose_helper:logout_user(Config, Bob),
    Msg1 = escalus_stanza:chat_to(Bob, <<"test">>),
    BobJid = lbin(escalus_client:full_jid(Bob)),
    AliceJid = lbin(escalus_client:full_jid(Alice)),
    %% Alice sends a message to Bob
    escalus:send(Alice, Msg1),
    %% Bob goes online again
    {ok, NewBob} = escalus_client:start(Config, bob, <<"new-session">>),
    escalus:send(NewBob, escalus_stanza:presence(<<"available">>)),
    Stanzas = escalus:wait_for_stanzas(NewBob, 2),
    escalus_new_assert:mix_match
    ([is_presence, fun(Stanza) -> escalus_pred:is_chat_message(<<"test">>, Stanza) end],
      Stanzas),
    %% Alice has conversation
    check_inbox(Alice, #inbox{
      total = 1,
      convs = [#conv{unread = 0, from = AliceJid, to = BobJid, content = <<"test">>}]}),
    %% Both check inbox and has a conversation
    check_inbox(NewBob, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = AliceJid, to = BobJid, content = <<"test">>}]})
                                                end).

msg_sent_to_not_existing_user(Config) ->
  escalus:story(Config, [{alice, 1}], fun(Alice) ->
    Msg1 = escalus_stanza:chat_to(<<"not_existing_user@localhost">>, <<"test2">>),
    AliceJid = lbin(escalus_client:full_jid(Alice)),
    %% Alice sends message to user that doesnt exist
    escalus:send(Alice, Msg1),
    %% Alice receives error
    ServiceUnavailable = escalus:wait_for_stanza(Alice),
    escalus_pred:is_error(<<"cancel">>,<<"service-unavailable">>, ServiceUnavailable),
    %% Alice has this conversation in inbox.
    check_inbox(Alice, #inbox{
      total = 1,
      convs = [#conv{unread = 0,
                     from = AliceJid,
                     to = <<"not_existing_user@localhost">>,
                     content = <<"test2">>}]})
                                      end).

user_has_two_unread_messages(Config) ->
  escalus:story(Config, [{kate, 1}, {mike, 1}], fun(Kate, Mike) ->
    Msg1 = escalus_stanza:chat_to(Mike, <<"Hello">>),
    Msg2 = escalus_stanza:chat_to(Mike, <<"How are you">>),
    KateJid = lbin(escalus_client:full_jid(Kate)),
    MikeJid = lbin(escalus_client:full_jid(Mike)),
    %% Kate sends 2 messages to Mike
    escalus:send(Kate, Msg1),
    escalus:send(Kate, Msg2),
    [M1, M2] = escalus:wait_for_stanzas(Mike, 2),
    escalus:assert(is_chat_message, M1),
    escalus:assert(is_chat_message, M2),
    %% Mike has two unread messages in conversation with Kate
    check_inbox(Mike, #inbox{
      total = 1,
      convs = [#conv{unread = 2, from = KateJid, to = MikeJid, content = <<"How are you">>}]}),
    %% Kate has one conv in her inbox (no unread messages)
    check_inbox(Kate, #inbox{
      total = 1,
      convs = [#conv{unread = 0, from = KateJid, to = MikeJid, content = <<"How are you">>}]})
                                                end).

reset_unread_counter(Config) ->
  escalus:story(Config, [{kate, 1}, {mike, 1}], fun(Kate, Mike) ->
    MsgId =  <<"123123">>,
    Msg1 = escalus_stanza:set_id(escalus_stanza:chat_to(Mike, <<"Hi mike">>), MsgId),
    KateJid = lbin(escalus_client:full_jid(Kate)),
    MikeJid = lbin(escalus_client:full_jid(Mike)),
    %% Kate sends message to Mike
    escalus:send(Kate, Msg1),
    M1 = escalus:wait_for_stanza(Mike),
    escalus:assert(is_chat_message, M1),
    %% Mike has one unread message
    check_inbox(Mike, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = KateJid, to = MikeJid, content = <<"Hi mike">>}]}),
    ChatMarker = escalus_stanza:chat_marker(KateJid, <<"displayed">>, MsgId),
    %% Mike sends "displayed" chat marker to Kate
    escalus:send(Mike, ChatMarker),
    %% Now Mike asks for inbox second time. He has 0 unread messages now
    check_inbox(Mike, #inbox{
      total = 1,
      convs = [#conv{unread = 0, from = KateJid, to = MikeJid, content = <<"Hi mike">>}]})
                                                end).

try_to_reset_unread_counter_with_bad_marker(Config) ->
  escalus:story(Config, [{kate, 1}, {mike, 1}], fun(Kate, Mike) ->
    Msg1 = escalus_stanza:set_id(escalus_stanza:chat_to(Mike, <<"okey dockey">>), <<"111">>),
    KateJid = lbin(escalus_client:full_jid(Kate)),
    MikeJid = lbin(escalus_client:full_jid(Mike)),
    %% Kate sends message to Mike
    escalus:send(Kate, Msg1),
    M1 = escalus:wait_for_stanza(Mike),
    escalus:assert(is_chat_message, M1),
    %% Mike has one unread message
    check_inbox(Mike, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = KateJid, to = MikeJid, content = <<"okey dockey">>}]}),
    MsgId = <<"badId">>,
    ChatMarker = escalus_stanza:chat_marker(KateJid, <<"displayed">>, MsgId),
    %% Mike sends "displayed" chat marker but 'id' field is wrong
    escalus:send(Mike, ChatMarker),
    %% Now Mike asks for inbox second time. Unread count should be still the same
    check_inbox(Mike, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = KateJid, to = MikeJid, content = <<"okey dockey">>}]})
                                                end).

%%--------------------------------------------------------------------
%% Inbox tests muclight
%%--------------------------------------------------------------------

simple_groupchat_stored_in_all_inbox(Config) ->
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
    %% Alice sends message to a room
    escalus:send(Alice, Stanza),
    R0 = escalus:wait_for_stanza(Alice),
    R1 = escalus:wait_for_stanza(Bob),
    R2 = escalus:wait_for_stanza(Kate),
    escalus:assert(is_groupchat_message, R0),
    escalus:assert(is_groupchat_message, R1),
    escalus:assert(is_groupchat_message, R2),
    %% Alice has 0 unread messages
    check_inbox(Alice, #inbox{
      total = 1,
      convs = [#conv{unread = 0, from = AliceRoomJid, to = AliceJid, content = Msg}]}),
    %% Bob and Kate have one conv with 1 unread message
    check_inbox(Bob, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = AliceRoomJid, to = BobJid, content = Msg}]}),
    check_inbox(Kate, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = AliceRoomJid, to = KateJid, content = Msg}]})
                                                           end).

advanced_groupchat_stored_in_all_inbox(Config) ->
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
    %% Alice sends msg to room
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
    %% Alice have one unread message (from Bob)
    check_inbox(Alice, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = BobRoomJid, to = AliceJid, content = Msg2}]}),
    %% Bob has 0 unread messages because he sent the last message
    check_inbox(Bob, #inbox{
      total = 1,
      convs = [#conv{unread = 0, from = BobRoomJid, to = BobJid, content = Msg2}]}),
    %% Kate has 2 unread messages (from Alice and Bob)
    check_inbox(Kate, #inbox{
      total = 1,
      convs = [#conv{unread = 2, from = BobRoomJid, to = KateJid, content = Msg2}]})
                                                           end).

groupchat_markers_one_reset(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    AliceJid = lbin(escalus_client:short_jid(Alice)),
    BobJid = lbin(escalus_client:short_jid(Bob)),
    KateJid = lbin(escalus_client:short_jid(Kate)),
    RoomJid = room_bin_jid(?ROOM_MARKERS),
    AliceRoomJid = <<RoomJid/binary,"/", AliceJid/binary>>,
    Id = <<"markerId">>,
    Stanza1 = escalus_stanza:set_id(
      escalus_stanza:groupchat_to(RoomJid, <<"marker time!">>), Id),
    escalus:send(Alice, Stanza1),
    R0 = escalus:wait_for_stanza(Alice),
    R1 = escalus:wait_for_stanza(Bob),
    R2 = escalus:wait_for_stanza(Kate),
    escalus:assert(is_groupchat_message, R0),
    escalus:assert(is_groupchat_message, R1),
    escalus:assert(is_groupchat_message, R2),
    ChatMarker = set_type(escalus_stanza:chat_marker(RoomJid,<<"displayed">>, Id), <<"groupchat">>),
    %% User marks last message
    escalus:send(Bob, ChatMarker),
    %% participants receive marker
    foreach_recipient([Alice, Bob, Kate], fun(Marker) ->
      true = escalus_pred:is_chat_marker(<<"displayed">>, Id, Marker) end),
    %% Bob has 0 unread messages because he reset his counter
    check_inbox(Bob, #inbox{
        total = 1,
        convs = [#conv{unread = 0, from = AliceRoomJid, to = BobJid, content = <<"marker time!">>}]}),
    %% Alice has 0 unread messages because she was the sender
    check_inbox(Alice, #inbox{
      total = 1,
      convs = [#conv{unread = 0, from = AliceRoomJid, to = AliceJid, content = <<"marker time!">>}]}),
    %% Kate still has unread message
    check_inbox(Kate, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = AliceRoomJid, to = KateJid, content = <<"marker time!">>}]})
                                                           end).

%% this test combines options:
%% ...
%%{aff_changes, true},
%%{remove_on_kicked, true},
create_groupchat(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    RoomNode = <<"bobroom">>,
    create_room_and_check_inbox(Bob, [Alice, Kate], RoomNode)
                                                           end).


%% this test combines options:
%% ...
%%{aff_changes, false},
%%{remove_on_kicked, true},
create_groupchat_no_affiliation_stored(Config) ->
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
    check_inbox(Alice, #inbox{
      total = 0,
      convs = []}),
    check_inbox(Bob, #inbox{
      total = 0,
      convs = []}),
    check_inbox(Kate, #inbox{
      total = 0,
      convs = []}),
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
    %% Alice has 0 unread because she sent the last message
    check_inbox(Alice, #inbox{
      total = 1,
      convs = [#conv{unread = 0, from = AliceRoomJid, to = AliceJid, content = Msg}]}),
    %% Bob and Kate have one unread message
    check_inbox(Bob, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = AliceRoomJid, to = BobJid, content = Msg}]}),
    check_inbox(Kate, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = AliceRoomJid, to = KateJid, content = Msg}]})
                                                           end).


%% this test combines options:
%% ...
%%{aff_changes, true},
%%{remove_on_kicked, true},
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
    check_inbox(Alice, #inbox{
      total = 1,
      convs = [#conv{unread = 0, from = AliceRoomJid, to = AliceJid, content = Msg}]}),
    check_inbox(Kate, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = AliceRoomJid, to = KateJid, content = Msg}]}),
    %% Bob doesn't have conversation in his inbox
    check_inbox(Bob, #inbox{total = 0, convs = []})
                                                           end).

%% this test combines options:
%% ...
%%{aff_changes, true},
%%{remove_on_kicked, false},
leave_and_store_conversation(Config) ->
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
    %% Alice and Kate have conversation
    check_inbox(Alice, #inbox{
      total = 1,
      convs = [#conv{unread = 0, from = AliceRoomJid, to = AliceJid, content = Msg}]}),
    check_inbox(Kate, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = AliceRoomJid, to = KateJid, content = Msg}]}),
    %% Bob still has a conversation in inbox. Two unread - first is invitation, the second is the leaving affiliation
    check_inbox(Bob, #inbox{
      total = 1,
      convs = [#conv{unread = 2, from = RoomJid, to = BobJid, verify = fun verify_is_none_aff_change/2}]})
                                                           end).

%% this test combines options:
%% ...
%%{aff_changes, false},
%%{remove_on_kicked, true},
no_aff_stored_and_remove_on_kicked(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    AliceJid = lbin(escalus_client:short_jid(Alice)),
    KateJid = lbin(escalus_client:short_jid(Kate)),
    RoomJid = room_bin_jid(?ROOM3),
    AliceRoomJid = <<RoomJid/binary, "/", AliceJid/binary>>,
    Msg = <<"Hi all">>,
    Stanza = escalus_stanza:set_id(
      escalus_stanza:groupchat_to(RoomJid, Msg), <<"33">>),
    %% Alice sends a message
    escalus:send(Alice, Stanza),
    R0 = escalus:wait_for_stanza(Alice),
    R1 = escalus:wait_for_stanza(Bob),
    R2 = escalus:wait_for_stanza(Kate),
    escalus:assert(is_groupchat_message, R0),
    escalus:assert(is_groupchat_message, R1),
    escalus:assert(is_groupchat_message, R2),
    %% Bob leaves the room
    muc_light_helper:user_leave(?ROOM3, Bob, [{Alice, owner}, {Kate, member}]),
    %% Alice and Kate have message in groupchats
    check_inbox(Alice, #inbox{
      total = 1,
      convs = [#conv{unread = 0, from = AliceRoomJid, to = AliceJid, content = Msg}]}),
    check_inbox(Kate, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = AliceRoomJid, to = KateJid, content = Msg}]}),
    %% Bob doesnt have a conversation in inbox
    check_inbox(Bob, #inbox{total = 0, convs = []})
                                                           end).


%% this test combines options:
%% ...
%%{aff_changes, true},
%%{remove_on_kicked, false},
no_stored_and_remain_after_kicked(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    AliceJid = lbin(escalus_client:short_jid(Alice)),
    KateJid = lbin(escalus_client:short_jid(Kate)),
    BobJid = lbin(escalus_client:short_jid(Bob)),
    RoomJid = room_bin_jid(?ROOM4),
    AliceRoomJid = <<RoomJid/binary, "/", AliceJid/binary>>,
    Msg = <<"Hi all">>,
    Stanza = escalus_stanza:set_id(
      escalus_stanza:groupchat_to(RoomJid, Msg), <<"33">>),
    %% Alice sends a message
    escalus:send(Alice, Stanza),
    R0 = escalus:wait_for_stanza(Alice),
    R1 = escalus:wait_for_stanza(Bob),
    R2 = escalus:wait_for_stanza(Kate),
    escalus:assert(is_groupchat_message, R0),
    escalus:assert(is_groupchat_message, R1),
    escalus:assert(is_groupchat_message, R2),
    %% Bob leaves the room
    muc_light_helper:user_leave(?ROOM4, Bob, [{Alice, owner}, {Kate, member}]),
    %% Alice and Kate have message in groupchats
    check_inbox(Alice, #inbox{
      total = 1,
      convs = [#conv{unread = 0, from = AliceRoomJid, to = AliceJid, content = Msg}]}),
    check_inbox(Kate, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = AliceRoomJid, to = KateJid, content = Msg}]}),
    %% Bob have a conversation in inbox. First unread is message from Alice, the second the affiliation change
    check_inbox(Bob, #inbox{total = 1, convs = [#conv{unread = 2,
                                                      from = RoomJid,
                                                      to = BobJid,
                                                      verify = fun verify_is_none_aff_change/2}]})
                                                           end).


groupchat_markers_one_reset_room_created(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    Msg = <<"Welcome guys">>,
    RoomName = <<"markers_room">>,
    RoomJid = room_bin_jid(RoomName),
    AliceJid = lbin(escalus_client:short_jid(Alice)),
    AliceRoomJid = <<RoomJid/binary,"/", AliceJid/binary>>,
    BobJid = lbin(escalus_client:short_jid(Bob)),
    KateJid = lbin(escalus_client:short_jid(Kate)),
    create_room_send_msg_check_inbox(Alice, [Bob, Kate], RoomName, Msg, <<"1-id">>),
    %% Now Bob sends marker
    mark_last_muclight_message(Bob, [Alice, Bob, Kate]),
    %% The crew ask for inbox second time. Only Kate has unread messages
    check_inbox(Alice, #inbox{
      total = 1,
      convs = [#conv{unread = 0, from = AliceRoomJid, to = AliceJid, content = Msg}]}),
    check_inbox(Kate, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = AliceRoomJid, to = KateJid, content = Msg}]}),
    check_inbox(Bob, #inbox{
      total = 1,
      convs = [#conv{unread = 0, from = AliceRoomJid, to = BobJid, content = Msg}]})

                                                           end).

groupchat_markers_all_reset_room_created(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    RoomName = <<"markers_room2">>,
    AliceJid = lbin(escalus_client:short_jid(Alice)),
    RoomJid = room_bin_jid(RoomName),
    AliceRoomJid = <<RoomJid/binary,"/", AliceJid/binary>>,
    Msg = <<"Mark me!">>,
    create_room_send_msg_check_inbox(Alice, [Bob, Kate], RoomName, Msg, <<"2-id">>),
    [mark_last_muclight_message(U, [Alice, Bob, Kate]) || U <- [Bob, Kate]],
    foreach_check_inbox([Bob, Kate, Alice], 1, 0, AliceRoomJid, Msg)
                                                           end).

%%--------------------------------------------------------------------
%% legacy MUC tests
%%--------------------------------------------------------------------

simple_groupchat_stored_in_all_inbox_muc(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    Users = [Alice, Bob, Kate],
    Msg = <<"Hi Room!">>,
    Id = <<"MyID">>,
    Room = ?config(room, Config),
    RoomAddr = muc_room_address(Room),

    enter_room(Room, Users),
    make_members(Room, Alice, Users -- [Alice]),
    Stanza = escalus_stanza:set_id(
      escalus_stanza:groupchat_to(RoomAddr, Msg), Id),
    escalus:send(Bob, Stanza),
    wait_for_groupchat_msg(Users),
    [AliceJid, BobJid, KateJid] = lists:map(fun to_bare_lower/1, Users),
    BobRoomJid = muc_room_address(Room, nick(Bob)),
    %% Bob has 0 unread messages
    check_inbox(Bob, #inbox{
      total = 1,
      convs = [#conv{unread = 0, from = BobRoomJid, to = BobJid,
                     content = Msg}]}, #{case_sensitive => true}),
    %% Alice and Kate have one conv with 1 unread message
    check_inbox(Alice, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = BobRoomJid, to = AliceJid, content = Msg}]}, #{case_sensitive => true}),
    check_inbox(Kate, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = BobRoomJid, to = KateJid, content = Msg}]}, #{case_sensitive => true})
    end).

simple_groupchat_stored_in_offline_users_inbox_muc(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    Users = [Alice, Bob, Kate],
    Msg = <<"Hi Room!">>,
    Id = <<"MyID">>,
    Room = ?config(room, Config),
    RoomAddr = muc_room_address(Room),

    enter_room(Room, Users),
    make_members(Room, Alice, Users -- [Alice]),
    go_offline(Kate, Room, Users),
    Stanza = escalus_stanza:set_id(
      escalus_stanza:groupchat_to(RoomAddr, Msg), Id),
    escalus:send(Bob, Stanza),
    wait_for_groupchat_msg(Users -- [Kate]),

    [AliceJid, BobJid, KateJid] = lists:map(fun to_bare_lower/1, Users),
    BobRoomJid = muc_room_address(Room, nick(Bob)),
    %% Bob has 0 unread messages
    check_inbox(Bob, #inbox{
      total = 1,
      convs = [#conv{unread = 0, from = BobRoomJid, to = BobJid,
                     content = Msg}]}, #{case_sensitive => true}),
    %% Alice and Kate have one conv with 1 unread message
    check_inbox(Alice, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = BobRoomJid, to = AliceJid, content = Msg}]}, #{case_sensitive => true}),
    check_inbox(Kate, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = BobRoomJid, to = KateJid, content = Msg}]}, #{case_sensitive => true})
    end).


unread_count_is_the_same_after_going_online_again(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    Users = [Alice, Bob, Kate],
    Msg = <<"Hi Room!">>,
    Id = <<"MyID">>,
    Room = ?config(room, Config),
    RoomAddr = muc_room_address(Room),

    enter_room(Room, Users),
    make_members(Room, Alice, Users -- [Alice]),
    go_offline(Kate, Room, Users),
    Stanza = escalus_stanza:set_id(
      escalus_stanza:groupchat_to(RoomAddr, Msg), Id),
    escalus:send(Bob, Stanza),
    wait_for_groupchat_msg(Users -- [Kate]),
    enter_room(Room, Kate, Users -- [Kate], 1),
    [AliceJid, BobJid, KateJid] = lists:map(fun to_bare_lower/1, Users),
    BobRoomJid = muc_room_address(Room, nick(Bob)),
    %% Bob has 0 unread messages
    check_inbox(Bob, #inbox{
      total = 1,
      convs = [#conv{unread = 0, from = BobRoomJid, to = BobJid,
                     content = Msg}]}, #{case_sensitive => true}),
    %% Alice and Kate have one conv with 1 unread message
    check_inbox(Alice, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = BobRoomJid, to = AliceJid, content = Msg}]}, #{case_sensitive => true}),
    check_inbox(Kate, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = BobRoomJid, to = KateJid, content = Msg}]}, #{case_sensitive => true})
    end).

unread_count_is_reset_after_sending_chatmarker(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    Users = [Alice, Bob, Kate],
    Msg = <<"Hi Room!">>,
    Id = <<"MyID">>,
    Room = ?config(room, Config),
    RoomAddr = muc_room_address(Room),

    enter_room(Room, Users),
    make_members(Room, Alice, Users -- [Alice]),
    Stanza = escalus_stanza:set_id(
      escalus_stanza:groupchat_to(RoomAddr, Msg), Id),
    escalus:send(Bob, Stanza),
    wait_for_groupchat_msg(Users),
    ChatMarker = set_type(escalus_stanza:chat_marker(RoomAddr, <<"displayed">>, Id), <<"groupchat">>),
    %% User marks last message
    escalus:send(Kate, ChatMarker),
    wait_for_groupchat_msg(Users),

    [AliceJid, BobJid, KateJid] = lists:map(fun to_bare_lower/1, Users),
    BobRoomJid = muc_room_address(Room, nick(Bob)),
    %% Bob has 0 unread messages
    check_inbox(Bob, #inbox{
      total = 1,
      convs = [#conv{unread = 0, from = BobRoomJid, to = BobJid,
                     content = Msg}]}, #{case_sensitive => true}),
    %% Alice have one conv with 1 unread message
    check_inbox(Alice, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = BobRoomJid, to = AliceJid, content = Msg}]}, #{case_sensitive => true}),
    %% Kate has 0 unread messages
    check_inbox(Kate, #inbox{
      total = 1,
      convs = [#conv{unread = 0, from = BobRoomJid, to = KateJid, content = Msg}]}, #{case_sensitive => true})
    end).

private_messages_are_(Config) ->
  escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
    Users = [Alice, Bob, Kate],
    Msg = <<"Hi Room!">>,
    Id = <<"MyID">>,
    Room = ?config(room, Config),
    RoomAddr = muc_room_address(Room),
    BobRoomJid = muc_room_address(Room, nick(Bob)),
    KateRoomJid = muc_room_address(Room, nick(Kate)),

    enter_room(Room, Users),
    make_members(Room, Alice, Users -- [Alice]),
    Stanza = escalus_stanza:set_id(
      escalus_stanza:chat_to(BobRoomJid, Msg), Id),
    escalus:send(Kate, Stanza),
    BobsPrivMsg = escalus:wait_for_stanza(Bob),

    [_AliceJid, BobJid, KateJid] = lists:map(fun to_bare_lower/1, Users),
    %% Bob has 1 unread message
    check_inbox(Bob, #inbox{
      total = 1,
      convs = [#conv{unread = 1, from = KateRoomJid, to = BobRoomJid,
                     content = Msg}]}, #{case_sensitive => true}),
    %% Alice gets nothing
    check_inbox(Alice, #inbox{
      total = 0,
      convs = []}),
    %% Kate has 1 conv with 0 unread messages
    check_inbox(Kate, #inbox{
      total = 1,
      convs = [#conv{unread = 0, from = KateJid, to = BobRoomJid, content = Msg}]},
                #{case_sensitive => true, check_resource => false})
    end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers

go_offline(User, Room, Occupants) ->
    UnavailavbleStanza = escalus_stanza:presence(<<"unavailable">>),
    Stanza = muc_helper:stanza_to_room(UnavailavbleStanza, Room, nick(User)),
    escalus:send(User, Stanza),
    lists:foreach(fun(User) -> A = escalus:wait_for_stanza(User), ct:pal("A: ~p", [A]) end,
                  Occupants).

go_online(User, Room, Occupants) ->
    UnavailavbleStanza = escalus_stanza:presence(<<"unavailable">>),
    Stanza = muc_helper:stanza_to_room(UnavailavbleStanza, Room, nick(User)),
    escalus:send(User, Stanza),
    lists:foreach(fun(User) -> A = escalus:wait_for_stanza(User), ct:pal("A: ~p", [A]) end,
                  Occupants).

wait_for_groupchat_msg(Users) ->
    Resps = lists:map(fun(User) -> escalus:wait_for_stanza(User) end,
              Users),
    lists:foreach(fun(Resp) -> escalus:assert(is_groupchat_message, Resp) end,
                  Resps).


to_bare_lower(User) ->
    lbin(escalus_client:short_jid(User)).

make_members(Room, Admin, Users) ->
    Items = lists:map(fun(User) -> {escalus_utils:get_short_jid(User),<<"member">>} end,
                      Users),
    escalus:send(Admin, stanza_set_affiliations(Room, Items)),
    SuccesResp = escalus:wait_for_stanzas(Admin, 1 + length(Users)), % gets iq result and affs changes from all users
    ct:pal("SuccessResp: ~p", [SuccesResp]),
    lists:foreach(fun(User) -> escalus:wait_for_stanzas(User, length(Users)) end, Users). % Everybody gets aff changes of everybody

% All users enter the room
enter_room(Room, Users) ->
    lists:foreach(fun(User) ->
                          escalus:send(User, stanza_muc_enter_room(Room, nick(User))) end,
                  Users),
    lists:foreach(fun(User) ->
                          A = escalus:wait_for_stanzas(User, length(Users) + 1), % everybody gets presence from everybody + a subject message
                          ct:pal("For ~p: ~p", [escalus_client:short_jid(User), A])
                  end,
                  Users).

% `User` enters the room `Room` where `Users` are occupants
enter_room(Room, User, Users, DelayedMessagesCount) ->
    escalus:send(User, stanza_muc_enter_room(Room, nick(User))),
    lists:foreach(fun(User) ->
                          A = escalus:wait_for_stanza(User), ct:pal("~p: Got presence from user: ~p", [escalus_client:short_jid(User), A]) end,
                  Users),
    Subj = escalus:wait_for_stanzas(User, length(Users) + 1 + 1 + DelayedMessagesCount), % User gets subject message and presences from everybody including himself
    ct:pal("Subj and presence messages: ~p", [Subj]).


stanza_set_affiliations(Room, List) ->
    Payload = lists:map(fun({JID, Affiliation}) ->
        #xmlel{name = <<"item">>,
        attrs = [{<<"jid">>, JID}, {<<"affiliation">>, Affiliation}]};
    ({JID, Affiliation, Reason}) ->
        #xmlel{name = <<"item">>,
        attrs = [{<<"jid">>, JID}, {<<"affiliation">>, Affiliation}],
        children = [#xmlel{
            name = <<"reason">>,
            children = [#xmlcdata{content = Reason}]}
        ]}
    end, List),
    muc_helper:stanza_to_room(escalus_stanza:iq_set(?NS_MUC_ADMIN, Payload), Room).


nick(User) -> escalus_utils:get_username(User).



stanza_muc_enter_room(Room, Nick) ->
    stanza_to_room(
        escalus_stanza:presence(<<"available">>,
                                [#xmlel{name = <<"x">>,
                                        attrs=[{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}]}
                                ]),
        Room, Nick).

stanza_to_room(Stanza, Room, Nick) ->
    escalus_stanza:to(Stanza, muc_room_address(Room, Nick)).


muc_room_address(Room) ->
    RoomJid = <<Room/binary, $@, ?MUC_DOMAIN/binary>>.

muc_room_address(Room, Nick) ->
    RoomJid = <<Room/binary, $@, ?MUC_DOMAIN/binary, $/, Nick/binary>>.

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
  check_inbox(Owner, #inbox{
    total = 1,
    convs = [#conv{unread = 1,
                   from = room_bin_jid(RoomName),
                   to = OwnerJid,
                   verify = fun verify_is_owner_aff_change/2}]}),
  %% check for the members. Every member has affiliation change to member
  [check_inbox(Member, #inbox{
      total = 1,
      convs = [#conv{unread = 1,
                     from = room_bin_jid(RoomName),
                     to = Jid,
                     verify = fun verify_is_member_aff_change/2}]})
    || {Member, Jid} <- MemberAndJids],
  %% Each room participant send chat marker
  [begin mark_last_muclight_system_message(U, 1),
         foreach_recipient(MembersAndOwner, fun(_Stanza) -> ok end) end || U <- MembersAndOwner],
  %% counter is reset for owner
  check_inbox(Owner, #inbox{
    total = 1,
    convs = [#conv{unread = 0,
                   from = room_bin_jid(RoomName),
                   to = OwnerJid,
                   verify = fun verify_is_owner_aff_change/2}]}),
  %% counter is reset for members
  [check_inbox(Member, #inbox{
      total = 1,
      convs = [#conv{unread = 0,
                     from = room_bin_jid(RoomName),
                     to = Jid,
                     verify = fun verify_is_member_aff_change/2}]})
    || {Member, Jid} <- MemberAndJids].

%% assume there is only one conversation
mark_last_muclight_message(User, AllUsers) ->
  mark_last_muclight_message(User, AllUsers, <<"displayed">>).


mark_last_muclight_message(User, AllUsers, MarkerType) ->
  %% User ask for inbox in order to get id of last message
  GetInbox = get_inbox_stanza(),
  escalus:send(User, GetInbox),
  Stanza = escalus:wait_for_stanza(User),
  ResIQ = escalus:wait_for_stanza(User),
  1 = get_inbox_count(ResIQ),
  [InnerMsg] = get_inner_msg(Stanza),
  MsgId = exml_query:attr(InnerMsg, <<"id">>),
  From = exml_query:attr(InnerMsg, <<"from">>),
  FromBare = escalus_utils:get_short_jid(From),
  ChatMarker = set_type(escalus_stanza:chat_marker(FromBare,MarkerType, MsgId), <<"groupchat">>),
  %% User marks last message
  escalus:send(User, ChatMarker),
  %% participants receive marker
  foreach_recipient(AllUsers, fun(Marker) ->
    true = escalus_pred:is_chat_marker(MarkerType, MsgId, Marker)
                              end).


mark_last_muclight_system_message(User, ExpectedCount) ->
  mark_last_muclight_system_message(User, ExpectedCount, <<"displayed">>).

mark_last_muclight_system_message(User, ExpectedCount, MarkerType) ->
  GetInbox = get_inbox_stanza(),
  escalus:send(User, GetInbox),
  Stanzas = escalus:wait_for_stanzas(User, ExpectedCount),
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
  check_inbox(Owner, #inbox{
    total = 1,
    convs = [#conv{unread = 0, from = OwnerRoomJid, to = OwnerJid, content = Msg}]}),
  foreach_check_inbox(MemberList, 1, 1, OwnerRoomJid, Msg).


% Checks case insesitive
foreach_check_inbox(Users, Total, Unread, SenderJid, Msg) ->
  [begin
     UserJid = lbin(escalus_client:short_jid(U)),
     check_inbox(U, #inbox{total = Total, convs = [#conv{unread = Unread, from = SenderJid, to = UserJid, content = Msg}]})
    end || U <- Users].

check_inbox(Client, Inbox) ->
    check_inbox(Client, Inbox, #{}).

% @doc Opts are:
%       * case_sensitive - should jids be checked case
%                          sensitively
%       * check_resource - should resource 
check_inbox(Client, #inbox{total = Total, convs = Convs}, Opts0) -> 
  Opts = fill_options(Opts0),
  check_inbox(Client, integer_to_binary(Total), Convs, Opts).


fill_options(Opts) ->
    #{case_sensitive => maps:get(case_sensitive, Opts, false),
      check_resource => maps:get(check_resource, Opts, true)}.



% This should be only called by some wrapper
%
check_inbox(Client, ExpectedCount, MsgCheckList, Opts) when is_binary(ExpectedCount) ->
  check_inbox(Client, binary_to_integer(ExpectedCount), MsgCheckList, Opts);
check_inbox(Client, ExpectedCount, MsgCheckList, Opts) ->
  GetInbox = get_inbox_stanza(),
  escalus:send(Client, GetInbox),
  Stanzas = escalus:wait_for_stanzas(Client, ExpectedCount),
  ResIQ = escalus:wait_for_stanza(Client),
  ExpectedCount = get_inbox_count(ResIQ),
  %% TODO: Replace with commented code when inbox starts sorting by timestamp by default
  %% Merged = lists:zip(Stanzas, MsgCheckList),
  %% [process_inbox_message(Client, M, ConvCheck) || {M, ConvCheck} <- Merged].
  process_inbox_messages(Client, Stanzas, MsgCheckList, [], Opts).

process_inbox_messages(Client, [], [], [], _) ->
    ok;
process_inbox_messages(Client, [], UnmatchedConvs, UnmatchedItems, #{case_sensitive := IsCaseSensitive, check_resource := CheckResource}) ->
    ct:fail(#{ reason => inbox_mismatch,
               unmatched_convs => UnmatchedConvs,
               unmatched_result_items => UnmatchedItems,
               resources_match_checked => CheckResource,
               jids_checked_casesesitive => IsCaseSensitive});
process_inbox_messages(Client, [Stanza | RStanzas], MsgCheckList, UnmatchedItems, Opts) ->
    Pred = fun(Conv) -> (catch process_inbox_message(Client, Stanza, Conv, Opts)) == ok end,
    case lists:partition(Pred, MsgCheckList) of
        {[], _NoConvSatisfiedPred} ->
            process_inbox_messages(Client, RStanzas, MsgCheckList, [Stanza | UnmatchedItems], Opts);
        {[_MatchedConv], RConvs} ->
            process_inbox_messages(Client, RStanzas, RConvs, UnmatchedItems, Opts)
    end.

process_inbox_message(Client, Stanza, Conv,
                      #{case_sensitive := IsCaseSensitive, check_resource := CheckResource} = Opts) ->
    do_process_inbox_message(Client, Stanza, Conv, check_jid_fun(IsCaseSensitive, CheckResource)).

check_jid_fun(true, true) ->
    fun(InnerMsg, Expected, El) -> Expected = exml_query:attr(InnerMsg, El) end;
check_jid_fun(false, true) ->
    fun(InnerMsg, Expected0, El) ->
            Expected = lbin(Expected0),
            Expected = lbin(exml_query:attr(InnerMsg, El)) end;
check_jid_fun(true, false) ->
    fun(InnerMsg, Expected, El) ->
            NoResExpected = bin_to_bare(Expected),
            NoResExpected = bin_to_bare(exml_query:attr(InnerMsg, El))
    end;
check_jid_fun(false, false) ->
    fun(InnerMsg, Expected, El) ->
            NoResExpected0 = escalus_client:short_jid(Expected),
            NoResExpected = lbin(NoResExpected0),
            NoResExpected = lbin(exml_query:attr(InnerMsg, El)) end.


bin_to_bare(Jid) ->
    case binary:split(Jid, <<"/">>) of
        [Bare, _Res] -> Bare;
        [Bare] -> Bare
    end.

do_process_inbox_message(Client, Message, #conv{unread = Unread, from = FromJid,
                                             to = ToJid, content = Content, verify = Fun}, CheckJidFun) ->
  Unread = get_unread_count(Message),
  escalus:assert(is_message, Message),
  Unread = get_unread_count(Message),
  [InnerMsg] = get_inner_msg(Message),
  CheckJidFun(InnerMsg, FromJid, <<"from">>),
  CheckJidFun(InnerMsg, ToJid, <<"to">>),
  InnerContent = exml_query:path(InnerMsg, [{element, <<"body">>}, cdata], []),
  Content = InnerContent,
  Fun(Client, InnerMsg),
  ok.


verify_is_owner_aff_change(Client, Msg) ->
  verify_muc_light_aff_msg(Msg, [{Client,  owner}]).

verify_is_member_aff_change(Client, Msg) ->
  verify_muc_light_aff_msg(Msg, [{Client, member}]).

verify_is_none_aff_change(Client, Msg) ->
  verify_muc_light_aff_msg(Msg, [{Client, none}]).

verify_muc_light_aff_msg(Msg, AffUsersChanges) ->
  BinAffUsersChanges = muc_light_helper:bin_aff_users(AffUsersChanges),
  ProperNS = muc_light_helper:ns_muc_light_affiliations(),
  SubEl = exml_query:path(Msg, [{element_with_ns, ProperNS}]),
  undefined = exml_query:subelement(Msg, <<"prev-version">>),
  Items = exml_query:subelements(SubEl, <<"user">>),
  muc_light_helper:verify_aff_users(Items, BinAffUsersChanges).

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
  binary_to_integer(Val).

get_inbox_count(Packet) ->
  [Val] = exml_query:paths(Packet, [{element_with_ns, ?NS_ESL_INBOX}, cdata]),
  case Val of
    <<>> ->
      {error, no_unread_count};
    _ ->
      binary_to_integer(Val)
  end.



clear_inbox_all() ->
  Host = ct:get_config({hosts, mim, domain}),
  clear_inboxes([alice, bob, kate, mike], Host).

clear_inboxes(UserList, Host) ->
  JIDs = [escalus_users:get_jid(escalus_users:get_users(UserList),U) || U <- UserList],
  [escalus_ejabberd:rpc(mod_inbox_utils, clear_inbox, [JID,Host]) || JID <- JIDs].

reload_inbox_option(Config, KeyValueList) ->
    Host = domain(),
    Args = proplists:get_value(inbox_opts, Config),
    Args2 = lists:foldl(fun({K, V}, AccIn) ->
        lists:keyreplace(K, 1, AccIn, {K, V})
                end, Args, KeyValueList),
    dynamic_modules:restart(Host, mod_inbox, Args2),
    lists:keyreplace(inbox_opts, 1, Config, {inbox_opts, Args2}).

reload_inbox_option(Config, Key, Value) ->
  Host = domain(),
  Args = proplists:get_value(inbox_opts, Config),
  Args1 = lists:keyreplace(Key, 1, Args, {Key, Value}),
  dynamic_modules:restart(Host, mod_inbox, Args1),
  lists:keyreplace(inbox_opts, 1, Config, {inbox_opts, Args1}).

restore_inbox_option(Config) ->
  Host = domain(),
  Args = proplists:get_value(inbox_opts, Config),
  dynamic_modules:restart(Host, mod_inbox, Args).

