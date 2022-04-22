-module(inbox_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("jid/include/jid.hrl").
-include_lib("inbox.hrl").

%% tests
-import(muc_light_helper, [room_bin_jid/1]).
-import(inbox_helper, [
                       inbox_modules/0,
                       muclight_modules/0,
                       inbox_opts/0,
                       muc_domain/0,
                       parse_form_iq/1,
                       check_inbox/2, check_inbox/3,
                       check_inbox/4,
                       clear_inbox_all/0,
                       given_conversations_between/2,
                       assert_invalid_inbox_form_value_error/3,
                       assert_invalid_reset_inbox/4,
                       extract_user_specs/1
                      ]).

-define(ROOM3, <<"testroom3">>).
-define(ROOM4, <<"testroom4">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    inbox_helper:skip_or_run_inbox_tests(tests()).

tests() ->
    [
     {group, regular},
     {group, async_pools}
    ].

groups() ->
    Gs = [
     {generic, [],
      [
       disco_service,
       returns_valid_form,
       returns_error_when_first_bad_form_field_encountered,
       returns_error_when_bad_form_field_start_sent,
       returns_error_when_bad_form_field_end_sent,
       returns_error_when_bad_form_field_order_sent,
       returns_error_when_bad_form_field_hidden_read_sent,
       returns_error_when_bad_reset_field_jid,
       returns_error_when_no_reset_field_jid,
       returns_error_when_unknown_field_sent
      ]},
     {one_to_one, [],
      [
       user_has_empty_inbox,
       msg_sent_stored_in_inbox,
       msg_sent_stored_in_inbox_iq_id_as_queryid_fallback,
       msg_sent_stored_in_inbox_queryid,
       msg_with_no_store_is_not_stored_in_inbox,
       msg_with_store_hint_is_always_stored,
       carbons_are_not_stored,
       user_has_two_conversations,
       msg_sent_to_offline_user,
       msg_sent_to_not_existing_user,
       user_has_two_unread_messages,
       other_resources_do_not_interfere,
       reset_unread_counter_with_reset_chat_marker,
       reset_unread_counter_with_reset_stanza,
       try_to_reset_unread_counter_with_bad_marker,
       non_reset_marker_should_not_affect_inbox,
       user_has_only_unread_messages_or_only_read,
       reset_unread_counter_and_show_only_unread,
       check_total_unread_count_and_active_conv_count,
       check_total_unread_count_when_there_are_no_active_conversations,
       total_unread_count_and_active_convs_are_zero_at_no_activity
      ]},
     {muclight, [],
      [
       simple_groupchat_stored_in_all_inbox,
       advanced_groupchat_stored_in_all_inbox,
       groupchat_markers_one_reset,
       non_reset_marker_should_not_affect_muclight_inbox,
       groupchat_reset_stanza_resets_inbox,
       create_groupchat,
       leave_and_remove_conversation,
       groupchat_markers_one_reset_room_created,
       groupchat_markers_all_reset_room_created,
       inbox_does_not_trigger_does_user_exist
      ]},
     {muclight_config, [sequence],
      [
       create_groupchat_no_affiliation_stored,
       leave_and_store_conversation,
       no_aff_stored_and_remove_on_kicked,
       no_stored_and_remain_after_kicked,
       system_message_is_correctly_avoided
      ]},
     {muc, [],
      [
       simple_groupchat_stored_in_all_inbox_muc,
       simple_groupchat_stored_in_offline_users_inbox_muc,
       unread_count_is_the_same_after_going_online_again,
       unread_count_is_reset_after_sending_chatmarker,
       non_reset_marker_should_not_affect_muc_inbox,
       unread_count_is_reset_after_sending_reset_stanza,
       private_messages_are_handled_as_one2one
      ]},
     {timestamps, [],
      [
       timestamp_is_updated_on_new_message,
       order_by_timestamp_ascending,
       get_by_timestamp_range,
       get_with_start_timestamp,
       get_with_end_timestamp
      ]},
     {bin, [],
      [
       timeout_cleaner_flush_all,
       rest_api_bin_flush_all,
       rest_api_bin_flush_user,
       xmpp_bin_flush,
       bin_is_not_included_by_default
      ]},
     {regular, [], test_groups()},
     {async_pools, [], [{group, bin} | test_groups()]}
    ],
    inbox_helper:maybe_run_in_parallel(Gs).

test_groups() ->
    [
     {group, generic},
     {group, one_to_one},
     {group, muclight},
     {group, muclight_config},
     {group, muc},
     {group, timestamps}
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    mongoose_helper:inject_module(?MODULE),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(GroupName, Config) when GroupName =:= regular; GroupName =:= async_pools ->
    HostType = domain_helper:host_type(),
    SecHostType = domain_helper:secondary_host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config),
    Config2 = dynamic_modules:save_modules(SecHostType, Config1),
    InboxOptions = inbox_helper:inbox_opts(GroupName),
    ok = dynamic_modules:ensure_modules(HostType,
           inbox_helper:inbox_modules(GroupName)
           ++ inbox_helper:muclight_modules()
           ++ [{mod_offline, config_parser_helper:default_mod_config(mod_offline)}]),
    ok = dynamic_modules:ensure_modules(SecHostType,
           [{mod_inbox, InboxOptions#{aff_changes := false}}]),
    [{inbox_opts, InboxOptions} | Config2];

init_per_group(muclight, Config) ->
    ok = dynamic_modules:ensure_modules(domain_helper:host_type(), muclight_modules()),
    inbox_helper:reload_inbox_option(Config, groupchat, [muclight]);
init_per_group(muclight_config, Config) ->
    ok = dynamic_modules:ensure_modules(domain_helper:host_type(), muclight_modules()),
    Config1 = inbox_helper:reload_inbox_option(Config, groupchat, [muclight]),
    escalus:create_users(Config1, escalus:get_users([alice, alice_bis, bob, kate, mike]));
init_per_group(muc, Config) ->
    muc_helper:load_muc(),
    inbox_helper:reload_inbox_option(Config, groupchat, [muc]);
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(GroupName, Config) when GroupName =:= regular; GroupName =:= async_pools ->
    muc_light_helper:clear_db(domain_helper:host_type()),
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config);
end_per_group(muclight_config, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, alice_bis, bob, kate, mike]));
end_per_group(muc, Config) ->
    inbox_helper:restore_inbox_option(Config),
    muc_helper:unload_muc();
end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(timeout_cleaner_flush_all, Config) ->
    clear_inbox_all(),
    inbox_helper:reload_inbox_option(Config, [{bin_ttl, 0}, {bin_clean_after, 10}]),
    escalus:init_per_testcase(timeout_cleaner_flush_all, Config);
init_per_testcase(TS, Config)
  when TS =:= create_groupchat_no_affiliation_stored;
       TS =:= system_message_is_correctly_avoided ->
    clear_inbox_all(),
    inbox_helper:reload_inbox_option(Config, aff_changes, false),
    escalus:init_per_testcase(TS, Config);
init_per_testcase(leave_and_store_conversation, Config) ->
    clear_inbox_all(),
    inbox_helper:reload_inbox_option(Config, remove_on_kicked, false),
    escalus:init_per_testcase(leave_and_store_conversation, Config);
init_per_testcase(no_aff_stored_and_remove_on_kicked, Config) ->
    clear_inbox_all(),
    muc_light_helper:create_room(?ROOM3, muc_light_helper:muc_host(), alice, [bob, kate],
                                 Config, muc_light_helper:ver(1)),
    inbox_helper:reload_inbox_option(Config, [{remove_on_kicked, true}, {aff_changes, false}]),
    escalus:init_per_testcase(no_aff_stored_and_remove_on_kicked, Config);
init_per_testcase(no_stored_and_remain_after_kicked, Config) ->
    clear_inbox_all(),
    muc_light_helper:create_room(?ROOM4, muc_light_helper:muc_host(), alice, [bob, kate],
                                 Config, muc_light_helper:ver(1)),
    inbox_helper:reload_inbox_option(Config, [{remove_on_kicked, false}, {aff_changes, true}]),
    escalus:init_per_testcase(no_stored_and_remain_after_kicked, Config);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(timeout_cleaner_flush_all, Config) ->
    inbox_helper:restore_inbox_option(Config),
    escalus:end_per_testcase(timeout_cleaner_flush_all, Config);
end_per_testcase(TS, Config)
  when TS =:= create_groupchat_no_affiliation_stored;
       TS =:= system_message_is_correctly_avoided ->
    clear_inbox_all(),
    inbox_helper:restore_inbox_option(Config),
    escalus:end_per_testcase(TS, Config);
end_per_testcase(leave_and_store_conversation, Config) ->
    clear_inbox_all(),
    inbox_helper:restore_inbox_option(Config),
    escalus:end_per_testcase(leave_and_store_conversation, Config);
end_per_testcase(no_aff_stored_and_remove_on_kicked, Config) ->
    clear_inbox_all(),
    inbox_helper:restore_inbox_option(Config),
    escalus:end_per_testcase(no_aff_stored_and_remove_on_kicked, Config);
end_per_testcase(no_stored_and_remain_after_kicked, Config) ->
    clear_inbox_all(),
    inbox_helper:restore_inbox_option(Config),
    escalus:end_per_testcase(no_stored_and_remain_after_kicked, Config);
end_per_testcase(msg_sent_to_not_existing_user, Config) ->
    HostType = domain_helper:host_type(),
    escalus_ejabberd:rpc(mod_inbox_utils, clear_inbox, [HostType, <<"not_existing_user">>,<<"localhost">>]),
    escalus:end_per_testcase(msg_sent_to_not_existing_user, Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% Generic Inbox tests
%%--------------------------------------------------------------------

disco_service(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
            Server = escalus_client:server(Alice),
            escalus:send(
              Alice, escalus_stanza:to(escalus_stanza:iq_get(?NS_DISCO_INFO, []), Server)),
            Stanza = escalus:wait_for_stanza(Alice),
            Features = exml_query:paths(Stanza, [{element, <<"query">>},
                                                 {element, <<"feature">>},
                                                 {attr, <<"var">>}]),
            ?assertEqual(true, lists:member(inbox_helper:inbox_ns(), Features))
        end).

returns_valid_form(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        escalus:send(Alice, inbox_helper:get_inbox_form_stanza()),
        ResIQ = escalus:wait_for_stanza(Alice),
        InboxNS = inbox_helper:inbox_ns(),
        #{ field_count := 7 } = Form = parse_form_iq(ResIQ),
        #{ <<"FORM_TYPE">> := #{ type := <<"hidden">>,
                                 value := InboxNS } } = Form,
        #{ <<"start">> := #{ type := <<"text-single">> } } = Form,
        #{ <<"end">> := #{ type := <<"text-single">> } } = Form,
        #{ <<"archive">> := #{ type := <<"boolean">>,
                               value := <<"false">> } } = Form,
        #{ <<"box">> := #{ type := <<"list-single">>,
                           value := <<"all">>,
                           options := BoxOptions } } = Form,
        #{ <<"order">> := #{ type := <<"list-single">>,
                             value := <<"desc">>,
                             options := OrderOptions } } = Form,
        [<<"asc">>, <<"desc">>] = lists:sort(OrderOptions),
        [<<"all">>, <<"archive">>, <<"bin">>, <<"inbox">>, <<"other">>] = lists:sort(BoxOptions),
        #{ <<"hidden_read">> := #{ type := <<"text-single">> } } = Form
      end).

returns_error_when_bad_form_field_order_sent(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        assert_invalid_inbox_form_value_error(Alice, <<"order">>, <<"invalid">>)
      end).

returns_error_when_bad_form_field_start_sent(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        assert_invalid_inbox_form_value_error(Alice, <<"start">>, <<"invalid">>)
      end).

returns_error_when_bad_form_field_end_sent(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
      assert_invalid_inbox_form_value_error(Alice, <<"end">>, <<"invalid">>)
    end).

returns_error_when_bad_form_field_hidden_read_sent(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
      assert_invalid_inbox_form_value_error(Alice, <<"hidden_read">>, <<"invalid">>)
    end).

returns_error_when_bad_reset_field_jid(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
      assert_invalid_reset_inbox(
        Alice, <<"$@/">>, <<"jid">>, <<"invalid-jid">>)
    end).

returns_error_when_no_reset_field_jid(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
      assert_invalid_reset_inbox(
        Alice, undefined, <<"jid">>, <<"jid-required">>)
    end).


returns_error_when_first_bad_form_field_encountered(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        Stanza = inbox_helper:make_inbox_stanza(#{<<"start">> => <<"invalid">>,
                                                  <<"end">> => <<"invalid">>}, false),
        escalus:send(Alice, Stanza),
        [ResIQ] = escalus:wait_for_stanzas(Alice, 1),
        escalus_pred:is_iq_error(ResIQ),
        ErrorMsg = inbox_helper:get_error_message(ResIQ),
        inbox_helper:assert_message_content(ErrorMsg, <<"field=end">>, <<"value=invalid">>)
      end).

returns_error_when_unknown_field_sent(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        Stanza = inbox_helper:make_inbox_stanza(#{<<"unknown_field">> => <<"unknown_field_value">>}, false),
        escalus:send(Alice, Stanza),
        [ResIQ] = escalus:wait_for_stanzas(Alice, 1),
        escalus_pred:is_iq_error(ResIQ),
        ErrorMsg = inbox_helper:get_error_message(ResIQ),
        inbox_helper:assert_message_content(ErrorMsg, <<"field=unknown_field">>, <<"value=unknown_field_value">>)
      end).


%%--------------------------------------------------------------------
%% Inbox tests one-to-one
%%--------------------------------------------------------------------

user_has_empty_inbox(Config) ->
    escalus:fresh_story(Config, [{kate, 1}], fun(Kate) ->
        Stanza = inbox_helper:make_inbox_stanza(),
        %% Kate logs in for first time and ask for inbox
        escalus:send(Kate, Stanza),
        [ResIQ] = escalus:wait_for_stanzas(Kate, 1),
        true = escalus_pred:is_iq_result(ResIQ),
        %% Inbox is empty
        TotalCount = inbox_helper:get_result_el(ResIQ, <<"count">>),
        0 = TotalCount
      end).

msg_sent_stored_in_inbox(Config) ->
    test_msg_stored_in_inbox(Config, undefined).

msg_sent_stored_in_inbox_iq_id_as_queryid_fallback(Config) ->
    test_msg_stored_in_inbox(Config, iq_id).

msg_sent_stored_in_inbox_queryid(Config) ->
    test_msg_stored_in_inbox(Config, queryid).

test_msg_stored_in_inbox(Config, QueryId) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        #{ Alice := AliceConvs, Bob := BobConvs } = given_conversations_between(Alice, [Bob]),
        %% Both check inbox
        check_inbox(Alice, AliceConvs, inbox_helper:maybe_make_queryid(QueryId)),
        check_inbox(Bob, BobConvs, inbox_helper:maybe_make_queryid(QueryId))
      end).

msg_with_no_store_is_not_stored_in_inbox(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %% Alice sends a message to Bob with a no-store hint
        Body = <<"test">>,
        Msg1 = escalus_stanza:chat_to(Bob, Body),
        Msg2 = escalus_stanza:set_id(Msg1, escalus_stanza:id()),
        Msg3 = mam_helper:add_nostore_hint(Msg2),
        escalus:send(Alice, Msg3),
        MsgSent = escalus:wait_for_stanza(Bob),
        escalus:assert(is_chat_message, MsgSent),
        %% Bob has no unread messages in conversation with Alice
        check_inbox(Bob, []),
        %% Alice has no conv in her inbox either
        check_inbox(Alice, [])
      end).

msg_with_store_hint_is_always_stored(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %% Alice sends a message to Bob with a store hint, that would otherwise be ignored
        Msg1 = escalus_stanza:to(#xmlel{name = <<"message">>}, Bob),
        Msg2 = escalus_stanza:set_id(Msg1, escalus_stanza:id()),
        Msg3 = mam_helper:add_store_hint(Msg2),
        escalus:send(Alice, Msg3),
        escalus:wait_for_stanza(Bob),
        %% Alice and Bob has a body-less message in their inbox
        check_inbox(Bob, [#conv{unread = 1, from = Alice, to = Bob, content = <<>>}]),
        check_inbox(Alice, [#conv{unread = 0, from = Alice, to = Bob, content = <<>>}])
      end).

carbons_are_not_stored(Config) ->
    escalus:fresh_story(Config, [{alice, 2}, {bob, 2}], fun(Alice1, Alice2, Bob1, Bob2) ->
        mongoose_helper:enable_carbons([Alice1, Alice2, Bob1, Bob2]),
        #{ Alice1 := AliceConvs, Bob1 := BobConvs } = given_conversations_between(Alice1, [Bob1]),
        %% Both check inbox and carbons aren't there
        check_inbox(Alice1, AliceConvs),
        check_inbox(Bob1, BobConvs)
      end).

user_has_two_conversations(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        #{ Alice := AliceConvs, Bob := BobConvs, Kate := KateConvs } =
        given_conversations_between(Alice, [Bob, Kate]),

        %% Alice has two conversations in her inbox (no unread messages)
        check_inbox(Alice, AliceConvs),

        %% Kate has one conversation with one unread
        check_inbox(Kate, KateConvs),

        %% Bob has one conversation with one unread
        check_inbox(Bob, BobConvs)
      end).

user_has_only_unread_messages_or_only_read(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        given_conversations_between(Alice, [Bob, Kate]),
        % Alice has no unread messages, but requests all conversations
        inbox_helper:get_inbox(Alice, #{ hidden_read => false }, #{count => 2}),

        % Requests only conversations with unread messages
        inbox_helper:get_inbox(Alice, #{ hidden_read => true }, #{count => 0}),
        % Bob has only one, unread message
        inbox_helper:get_inbox(Bob, #{ hidden_read => true }, #{count => 1})
      end).

msg_sent_to_offline_user(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %% Bob goes offline
        mongoose_helper:logout_user(Config, Bob),

        %% Alice sends a message to Bob
        Msg1 = escalus_stanza:chat_to(Bob, <<"test">>),
        escalus:send(Alice, Msg1),

        %% Bob goes online again
        {_, BobSpecs} = extract_user_specs(Bob),
        {ok, NewBob} = escalus_client:start(Config, BobSpecs, <<"new-session">>),
        escalus:send(NewBob, escalus_stanza:presence(<<"available">>)),
        Stanzas = escalus:wait_for_stanzas(NewBob, 2),

        escalus_new_assert:mix_match
        ([is_presence, fun(Stanza) -> escalus_pred:is_chat_message(<<"test">>, Stanza) end],
          Stanzas),

        %% Alice has conversation
        check_inbox(Alice, [#conv{unread = 0, from = Alice, to = Bob, content = <<"test">>}]),

        %% Both check inbox and has a conversation
        check_inbox(NewBob, [#conv{unread = 1, from = Alice, to = Bob, content = <<"test">>}])
      end).

msg_sent_to_not_existing_user(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        %% Alice sends message to user that doesnt exist
        Msg1 = escalus_stanza:chat_to(<<"not_existing_user@localhost">>, <<"test2">>),
        escalus:send(Alice, Msg1),

        %% Alice receives error
        ServiceUnavailable = escalus:wait_for_stanza(Alice),
        escalus_pred:is_error(<<"cancel">>,<<"service-unavailable">>, ServiceUnavailable),

        %% Alice has this conversation in inbox.
        check_inbox(Alice,[#conv{unread = 0,
                                 from = Alice,
                                 to = <<"not_existing_user@localhost">>,
                                 content = <<"test2">>}])
      end).

user_has_two_unread_messages(Config) ->
    escalus:fresh_story(Config, [{kate, 1}, {mike, 1}], fun(Kate, Mike) ->
        inbox_helper:send_msg(Kate, Mike, "Hello"),
        inbox_helper:send_msg(Kate, Mike, "How are you"),
        %% Mike has two unread messages in conversation with Kate
        check_inbox(Mike, [#conv{unread = 2, from = Kate, to = Mike, content = <<"How are you">>}]),
        %% Kate has one conv in her inbox (no unread messages)
        check_inbox(Kate, [#conv{unread = 0, from = Kate, to = Mike, content = <<"How are you">>}])
      end).

other_resources_do_not_interfere(Config) ->
    %% regression test
    escalus:fresh_story(Config, [{kate, 2}, {mike, 1}], fun(Kate, Kate2, Mike) ->
        Prio = #xmlel{name = <<"priority">>, children = [#xmlcdata{content = <<"100">>}]},
        escalus_client:send(Kate2, escalus_stanza:presence(<<"available">>, [Prio])),
        escalus_client:wait_for_stanza(Kate),
        inbox_helper:send_msg(Kate, Mike, "Hello"),
        inbox_helper:send_msg(Kate, Mike, "How are you"),
        %% Mike has two unread messages in conversation with Kate
        check_inbox(Mike, [#conv{unread = 2, from = Kate, to = Mike, content = <<"How are you">>}]),
        %% Kate has one conv in her inbox (no unread messages)
        check_inbox(Kate, [#conv{unread = 0, from = Kate, to = Mike, content = <<"How are you">>}])
                                                  end).

reset_unread_counter_with_reset_chat_marker(Config) ->
    escalus:fresh_story(Config, [{kate, 1}, {mike, 1}], fun(Kate, Mike) ->
        Msg = inbox_helper:send_msg(Kate, Mike, <<"Hi mike">>),
        MsgId = exml_query:attr(Msg, <<"id">>),

        %% Mike has one unread message
        check_inbox(Mike, [#conv{unread = 1, from = Kate, to = Mike, content = <<"Hi mike">>}]),
        ChatMarker = escalus_stanza:chat_marker(Kate, <<"displayed">>, MsgId),
        %% Mike sends "displayed" chat marker to Kate
        escalus:send(Mike, ChatMarker),
        %% Now Mike asks for inbox second time. He has 0 unread messages now
        check_inbox(Mike, [#conv{unread = 0, from = Kate, to = Mike, content = <<"Hi mike">>}])
      end).

reset_unread_counter_with_reset_stanza(Config) ->
    escalus:fresh_story(Config, [{kate, 1}, {mike, 1}], fun(Kate, Mike) ->
        _Msg = inbox_helper:send_msg(Kate, Mike, <<"Hi mike">>),

        %% Mike has one unread message
        check_inbox(Mike, [#conv{unread = 1, from = Kate, to = Mike, content = <<"Hi mike">>}]),
        %% Mike sends "reset" stanza
        inbox_helper:reset_inbox(Mike, Kate),
        %% Now Mike asks for inbox second time. He has 0 unread messages now
        check_inbox(Mike, [#conv{unread = 0, from = Kate, to = Mike, content = <<"Hi mike">>}]),
        %% Kate should not be receiving this stanza
        inbox_helper:assert_has_no_stanzas(Kate)
      end).

reset_unread_counter_and_show_only_unread(Config) ->
    escalus:fresh_story(Config, [{kate, 1}, {mike, 1}, {alice, 1}], fun(Kate, Mike, Alice) ->
        Msg = inbox_helper:send_msg(Kate, Mike),
        MsgId = exml_query:attr(Msg, <<"id">>),
        inbox_helper:get_inbox(Mike, #{ hidden_read => true }, #{count => 1}),

        ChatMarker = escalus_stanza:chat_marker(Kate, <<"displayed">>, MsgId),
        escalus:send(Mike, ChatMarker),

        inbox_helper:get_inbox(Mike, #{ hidden_read => true }, #{count => 0}),

        inbox_helper:send_msg(Alice, Mike),
        % Mike has two conversations, one with unread messages
        inbox_helper:get_inbox(Mike, #{ hidden_read => true }, #{count => 1}),
        inbox_helper:get_inbox(Mike, #{ hidden_read => false }, #{count => 2})
      end).

check_total_unread_count_and_active_conv_count(Config) ->
    escalus:fresh_story(Config, [{kate, 1}, {mike, 1}, {alice, 1}], fun(Kate, Mike, Alice) ->
        inbox_helper:send_and_mark_msg(Kate, Mike),
        inbox_helper:send_msg(Alice, Mike),
        inbox_helper:send_msg(Alice, Mike),
        inbox_helper:send_msg(Kate, Mike),
        inbox_helper:get_inbox(Mike, #{count => 2, unread_messages => 3, active_conversations => 2})
      end).

check_total_unread_count_when_there_are_no_active_conversations(Config) ->
    escalus:fresh_story(Config, [{kate, 1}, {mike, 1}], fun(Kate, Mike) ->
        inbox_helper:send_and_mark_msg(Kate, Mike),
        inbox_helper:get_inbox(Mike, #{count => 1, unread_messages => 0, active_conversations => 0})
    end).

total_unread_count_and_active_convs_are_zero_at_no_activity(Config) ->
    escalus:fresh_story(Config, [{kate, 1}], fun(Kate) ->
        inbox_helper:get_inbox(Kate, #{count => 0, unread_messages => 0, active_conversations => 0})
    end).

try_to_reset_unread_counter_with_bad_marker(Config) ->
    escalus:fresh_story(Config, [{kate, 1}, {mike, 1}], fun(Kate, Mike) ->
        Msg1 = escalus_stanza:set_id(escalus_stanza:chat_to(Mike, <<"okey dockey">>), <<"111">>),
        %% Kate sends message to Mike
        escalus:send(Kate, Msg1),
        M1 = escalus:wait_for_stanza(Mike),
        escalus:assert(is_chat_message, M1),
        %% Mike has one unread message
        check_inbox(Mike, [#conv{unread = 1, from = Kate, to = Mike, content = <<"okey dockey">>}]),
        MsgId = <<"badId">>,
        ChatMarker = escalus_stanza:chat_marker(Kate, <<"displayed">>, MsgId),
        %% Mike sends "displayed" chat marker but 'id' field is wrong
        escalus:send(Mike, ChatMarker),
        %% Now Mike asks for inbox second time. Unread count should be still the same
        check_inbox(Mike, [#conv{unread = 1, from = Kate, to = Mike, content = <<"okey dockey">>}])
      end).

non_reset_marker_should_not_affect_inbox(Config) ->
    escalus:fresh_story(Config, [{kate, 1}, {mike, 1}], fun(Kate, Mike) ->
        MsgId = <<"kate_to_mike">>,
        MsgBody = <<"okey dockey">>,
        Msg = escalus_stanza:set_id(escalus_stanza:chat_to(Mike, MsgBody), MsgId),
        %% Kate sends message to Mike
        escalus:send(Kate, Msg),
        M1 = escalus:wait_for_stanza(Mike),
        escalus:assert(is_chat_message, M1),
        %% Mike has one unread message
        check_inbox(Mike, [#conv{unread = 1, from = Kate, to = Mike, content = MsgBody}]),
        ChatMarker = escalus_stanza:chat_marker(Kate, <<"received">>, MsgId),
        %% Mike sends "received" chat marker, which is not a reset_marker
        escalus:send(Mike, ChatMarker),
        CM = escalus:wait_for_stanza(Kate),
        escalus:assert(is_message, CM),
        %% Now Mike asks for inbox second time. Unread count should be still the same
        check_inbox(Mike, [#conv{unread = 1, from = Kate, to = Mike, content = MsgBody}]),
        check_inbox(Kate, [], #{hidden_read => true}, #{})
      end).

%%--------------------------------------------------------------------
%% Inbox tests muclight
%%--------------------------------------------------------------------

simple_groupchat_stored_in_all_inbox(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        Msg = <<"Hi Room!">>,
        Id = <<"MyID">>,
        Users = [Alice, Bob, Kate],
        Room = inbox_helper:create_room(Alice, [Bob, Kate]),
        AliceJid = inbox_helper:to_bare_lower(Alice),
        KateJid = inbox_helper:to_bare_lower(Kate),
        BobJid = inbox_helper:to_bare_lower(Bob),
        RoomJid = room_bin_jid(Room),
        AliceRoomJid = <<RoomJid/binary, "/", AliceJid/binary>>,
        Stanza = escalus_stanza:set_id(
                   escalus_stanza:groupchat_to(RoomJid, Msg), Id),
        %% Alice sends message to a room
        escalus:send(Alice, Stanza),
        inbox_helper:wait_for_groupchat_msg(Users),
        %% Alice has 0 unread messages
        check_inbox(Alice, [#conv{unread = 0, from = AliceRoomJid, to = AliceJid, content = Msg}]),
        %% Bob and Kate have one conv with 1 unread message
        check_inbox(Bob, [#conv{unread = 1, from = AliceRoomJid, to = BobJid, content = Msg}]),
        check_inbox(Kate, [#conv{unread = 1, from = AliceRoomJid, to = KateJid, content = Msg}])
      end).

advanced_groupchat_stored_in_all_inbox(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        Msg1 = <<"Hi Room!">>,
        Msg2 = <<"How are you?">>,
        Id1 = <<"id-1">>,
        Id2 = <<"id-2">>,
        Users = [Alice, Bob, Kate],
        Room = pubsub_tools:pubsub_node_name(),
        inbox_helper:create_room_and_check_inbox(Alice, [Bob, Kate], Room),
        BobJid = inbox_helper:to_bare_lower(Bob),
        AliceJid = inbox_helper:to_bare_lower(Alice),
        KateJid = inbox_helper:to_bare_lower(Kate),
        RoomJid = room_bin_jid(Room),
        BobRoomJid = <<RoomJid/binary, "/", BobJid/binary>>,
        Stanza1 = escalus_stanza:set_id(
                    escalus_stanza:groupchat_to(RoomJid, Msg1), Id1),
        %% Alice sends msg to room
        escalus:send(Alice, Stanza1),
        inbox_helper:wait_for_groupchat_msg(Users),
        %% Bob sends second message
        Stanza2 = escalus_stanza:set_id(
                    escalus_stanza:groupchat_to(RoomJid, Msg2), Id2),
        escalus:send(Bob, Stanza2),
        inbox_helper:wait_for_groupchat_msg(Users),
        %% Alice have one unread message (from Bob)
        check_inbox(Alice, [#conv{unread = 1, from = BobRoomJid, to = AliceJid, content = Msg2}]),
        %% Bob has 0 unread messages because he sent the last message
        check_inbox(Bob, [#conv{unread = 0, from = BobRoomJid, to = BobJid, content = Msg2}]),
        %% Kate has 2 unread messages (from Alice and Bob)
        check_inbox(Kate, [#conv{unread = 2, from = BobRoomJid, to = KateJid, content = Msg2}])
      end).

groupchat_markers_one_reset(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        AliceJid = inbox_helper:to_bare_lower(Alice),
        BobJid = inbox_helper:to_bare_lower(Bob),
        KateJid = inbox_helper:to_bare_lower(Kate),
        Room = inbox_helper:create_room(Alice, [Bob, Kate]),
        RoomJid = room_bin_jid(Room),
        AliceRoomJid = <<RoomJid/binary, "/", AliceJid/binary>>,
        Id = <<"markerId">>,
        Users = [Alice, Bob, Kate],
        Stanza1 = escalus_stanza:set_id(
                    escalus_stanza:groupchat_to(RoomJid, <<"marker time!">>), Id),
        escalus:send(Alice, Stanza1),
        inbox_helper:wait_for_groupchat_msg(Users),
        ChatMarkerWOType = escalus_stanza:chat_marker(RoomJid,<<"displayed">>, Id),
        ChatMarker = escalus_stanza:setattr(ChatMarkerWOType, <<"type">>, <<"groupchat">>),
        %% User marks last message
        escalus:send(Bob, ChatMarker),
        %% participants receive marker
        muc_helper:foreach_recipient([Alice, Bob, Kate], fun(Marker) ->
          true = escalus_pred:is_chat_marker(<<"displayed">>, Id, Marker) end),
        %% Bob has 0 unread messages because he reset his counter
        check_inbox(Bob, [#conv{unread = 0, from = AliceRoomJid,
                                to = BobJid, content = <<"marker time!">>}]),
        %% Alice has 0 unread messages because she was the sender
        check_inbox(Alice, [#conv{unread = 0, from = AliceRoomJid,
                                  to = AliceJid, content = <<"marker time!">>}]),
        %% Kate still has unread message
        check_inbox(Kate, [#conv{unread = 1, from = AliceRoomJid,
                                 to = KateJid, content = <<"marker time!">>}])
      end).

non_reset_marker_should_not_affect_muclight_inbox(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        % %% GIVEN
        AliceJid = inbox_helper:to_bare_lower(Alice),
        BobJid = inbox_helper:to_bare_lower(Bob),
        KateJid = inbox_helper:to_bare_lower(Kate),
        % %% WHEN DONE
        Id = <<"some_ID">>,
        Msg = <<"marker time!">>,
        RoomName = pubsub_tools:pubsub_node_name(),
        RoomJid = inbox_helper:create_room_send_msg_check_inbox(Alice, [Bob, Kate], RoomName, Msg, Id),
        AliceRoomJid = <<RoomJid/binary, "/", AliceJid/binary>>,
        % %% AND MARKED WRONG
        inbox_helper:mark_last_muclight_message(Bob, [Alice, Bob, Kate], <<"received">>),
        inbox_helper:mark_last_muclight_message(Kate, [Alice, Bob, Kate], <<"acknowledged">>),
        % %% THEN
        %% Alice has 0 unread messages because she was the sender
        check_inbox(Alice, [#conv{unread = 0, from = AliceRoomJid,
                                  to = AliceJid, content = Msg}]),
        %% Bob still has unread message
        check_inbox(Bob, [#conv{unread = 1, from = AliceRoomJid,
                                to = BobJid, content = Msg}]),
        %% Kate still has unread message
        check_inbox(Kate, [#conv{unread = 1, from = AliceRoomJid,
                                 to = KateJid, content = Msg}])
      end).

groupchat_reset_stanza_resets_inbox(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        % %% WITH
        Id = <<"some_ID">>,
        Msg = <<"marker time!">>,
        RoomName = pubsub_tools:pubsub_node_name(),
        AliceJid = inbox_helper:to_bare_lower(Alice),
        BobJid = inbox_helper:to_bare_lower(Bob),
        KateJid = inbox_helper:to_bare_lower(Kate),
        % %% WHEN A MESSAGE IS SENT
        RoomJid = inbox_helper:create_room_send_msg_check_inbox(Alice, [Bob, Kate], RoomName, Msg, Id),
        AliceRoomJid = <<RoomJid/binary, "/", AliceJid/binary>>,
        % %% AND WHEN SEND RESET FOR ROOM
        inbox_helper:reset_inbox(Bob, RoomJid),
        % %% THEN INBOX IS RESET FOR BOB, WITHOUT FORWARDING
        %% Bob has 0 unread messages because he reset his counter
        check_inbox(Bob, [#conv{unread = 0, from = AliceRoomJid,
                                to = BobJid, content = <<"marker time!">>}]),
        %% Alice has 0 unread messages because she was the sender
        check_inbox(Alice, [#conv{unread = 0, from = AliceRoomJid,
                                  to = AliceJid, content = <<"marker time!">>}]),
        %% Kate still has unread message
        check_inbox(Kate, [#conv{unread = 1, from = AliceRoomJid,
                                 to = KateJid, content = <<"marker time!">>}]),
        %% And nobody received any other stanza
        inbox_helper:assert_has_no_stanzas([Alice, Bob, Kate])
      end).

%% this test combines options:
%% ...
%%{aff_changes, true},
%%{remove_on_kicked, true},
create_groupchat(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        RoomName = pubsub_tools:pubsub_node_name(),
        inbox_helper:create_room_and_check_inbox(Bob, [Alice, Kate], RoomName)
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
        Users = [Alice, Bob, Kate],
        InitConfig = [{<<"roomname">>, <<"Bob's room2">>}],
        RoomNode = <<"bobroom2">>,
        RoomJid = room_bin_jid(RoomNode),
        AliceJid = inbox_helper:to_bare_lower(Alice),
        AliceRoomJid = <<RoomJid/binary, "/", AliceJid/binary>>,
        BobJid = inbox_helper:to_bare_lower(Bob),
        KateJid = inbox_helper:to_bare_lower(Kate),
        %% Bob creates room
        escalus:send(Bob, muc_light_helper:stanza_create_room(RoomNode, InitConfig, InitOccupants)),
        muc_light_helper:verify_aff_bcast(FinalOccupants, FinalOccupants),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),
        %% affiliation change messages are not stored in inbox
        [ check_inbox(User, []) || User <- Users ],
        Stanza = escalus_stanza:set_id(
                   escalus_stanza:groupchat_to(RoomJid, Msg), <<"123">>),
        %% Alice sends a message
        escalus:send(Alice, Stanza),
        inbox_helper:wait_for_groupchat_msg(Users),
        %% Alice has 0 unread because she sent the last message
        check_inbox(Alice, [#conv{unread = 0, from = AliceRoomJid, to = AliceJid, content = Msg}]),
        %% Bob and Kate have one unread message
        check_inbox(Bob, [#conv{unread = 1, from = AliceRoomJid, to = BobJid, content = Msg}]),
        check_inbox(Kate, [#conv{unread = 1, from = AliceRoomJid, to = KateJid, content = Msg}])
    end).

%% this test combines options:
%% ...
%%{aff_changes, true},
%%{remove_on_kicked, true},
leave_and_remove_conversation(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        RoomName = pubsub_tools:pubsub_node_name(),
        AliceJid = inbox_helper:to_bare_lower(Alice),
        BobJid = inbox_helper:to_bare_lower(Bob),
        KateJid = inbox_helper:to_bare_lower(Kate),
        %% Alice creates a room and send msg
        Msg = <<"Hi all">>,
        Id = <<"leave-id">>,
        RoomJid = inbox_helper:create_room_send_msg_check_inbox(Alice, [Bob, Kate], RoomName, Msg, Id),
        AliceRoomJid = <<RoomJid/binary, "/", AliceJid/binary>>,
        %% Bob leaves the room
        muc_light_helper:user_leave(RoomName, Bob, [{Alice, owner}, {Kate, member}]),
        %% Alice and Kate have one message
        check_inbox(Alice, [#conv{unread = 0, from = AliceRoomJid, to = AliceJid, content = Msg}]),
        check_inbox(Kate, [#conv{unread = 1, from = AliceRoomJid, to = KateJid, content = Msg}]),
        %% Bob doesn't have conversation in his inbox
        check_inbox(Bob, [], #{box => inbox}),
        if_async_check_bin(Config, Bob, [#conv{unread = 2, from = RoomJid, to = BobJid,
                                               verify = fun inbox_helper:verify_is_none_aff_change/2}])
    end).

%% this test combines options:
%% ...
%%{aff_changes, true},
%%{remove_on_kicked, false},
leave_and_store_conversation(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        RoomName = pubsub_tools:pubsub_node_name(),
        AliceJid = inbox_helper:to_bare_lower(Alice),
        BobJid = inbox_helper:to_bare_lower(Bob),
        KateJid = inbox_helper:to_bare_lower(Kate),
        %% Alice creates a room and send msg
        Msg = <<"Hi all">>,
        Id = <<"leave-id">>,
        RoomJid = inbox_helper:create_room_send_msg_check_inbox(Alice, [Bob, Kate], RoomName, Msg, Id),
        AliceRoomJid = <<RoomJid/binary, "/", AliceJid/binary>>,
        %% Bob leaves room
        muc_light_helper:user_leave(RoomName, Bob, [{Alice, owner}, {Kate, member}]),
        %% Alice and Kate have conversation
        check_inbox(Alice, [#conv{unread = 0, from = AliceRoomJid, to = AliceJid, content = Msg}]),
        check_inbox(Kate, [#conv{unread = 1, from = AliceRoomJid, to = KateJid, content = Msg}]),
        %% Bob still has a conversation in inbox. Two unread - first is invitation,
        %% the second is the leaving affiliation
        check_inbox(Bob, [#conv{unread = 2, from = RoomJid, to = BobJid,
                                verify = fun inbox_helper:verify_is_none_aff_change/2}])
      end).

%% this test combines options:
%% ...
%%{aff_changes, false},
%%{remove_on_kicked, true},
no_aff_stored_and_remove_on_kicked(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        AliceJid = inbox_helper:to_bare_lower(Alice),
        BobJid = inbox_helper:to_bare_lower(Bob),
        KateJid = inbox_helper:to_bare_lower(Kate),
        RoomJid = room_bin_jid(?ROOM3),
        AliceRoomJid = <<RoomJid/binary, "/", AliceJid/binary>>,
        Msg = <<"Hi all">>,
        Users = [Alice, Bob, Kate],
        Stanza = escalus_stanza:set_id(
          escalus_stanza:groupchat_to(RoomJid, Msg), <<"33">>),
        %% Alice sends a message
        escalus:send(Alice, Stanza),
        inbox_helper:wait_for_groupchat_msg(Users),
        %% Bob leaves the room
        muc_light_helper:user_leave(?ROOM3, Bob, [{Alice, owner}, {Kate, member}]),
        %% Alice and Kate have message in groupchats
        check_inbox(Alice, [#conv{unread = 0, from = AliceRoomJid, to = AliceJid, content = Msg}]),
        check_inbox(Kate, [#conv{unread = 1, from = AliceRoomJid, to = KateJid, content = Msg}]),
        %% Bob doesnt have a conversation in inbox
        check_inbox(Bob, [], #{box => inbox}),
        if_async_check_bin(Config, Bob, [#conv{unread = 1, from = AliceRoomJid, to = BobJid, content = Msg}])
      end).

%% this test combines options:
%% ...
%%{aff_changes, true},
%%{remove_on_kicked, false},
no_stored_and_remain_after_kicked(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        AliceJid = inbox_helper:to_bare_lower(Alice),
        KateJid = inbox_helper:to_bare_lower(Kate),
        BobJid = inbox_helper:to_bare_lower(Bob),
        RoomJid = room_bin_jid(?ROOM4),
        AliceRoomJid = <<RoomJid/binary, "/", AliceJid/binary>>,
        Msg = <<"Hi all">>,
        Users = [Alice, Bob, Kate],
        Stanza = escalus_stanza:set_id(
          escalus_stanza:groupchat_to(RoomJid, Msg), <<"33">>),
        %% Alice sends a message
        escalus:send(Alice, Stanza),
        inbox_helper:wait_for_groupchat_msg(Users),
        %% Bob leaves the room
        muc_light_helper:user_leave(?ROOM4, Bob, [{Alice, owner}, {Kate, member}]),
        %% Alice and Kate have message in groupchats
        check_inbox(Alice, [#conv{unread = 0, from = AliceRoomJid, to = AliceJid, content = Msg}]),
        check_inbox(Kate, [#conv{unread = 1, from = AliceRoomJid, to = KateJid, content = Msg}]),
        %% Bob have a conversation in inbox. First unread is message from Alice, the second the affiliation change
        check_inbox(Bob, [#conv{unread = 2, from = RoomJid, to = BobJid,
                                verify = fun inbox_helper:verify_is_none_aff_change/2}])
      end).

groupchat_markers_one_reset_room_created(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        Id = <<"random-id">>,
        Msg = <<"Welcome guys">>,
        RoomName = pubsub_tools:pubsub_node_name(),
        AliceJid = inbox_helper:to_bare_lower(Alice),
        BobJid = inbox_helper:to_bare_lower(Bob),
        KateJid = inbox_helper:to_bare_lower(Kate),
        RoomJid = inbox_helper:create_room_send_msg_check_inbox(Alice, [Bob, Kate], RoomName, Msg, Id),
        AliceRoomJid = <<RoomJid/binary, "/", AliceJid/binary>>,
        %% Now Bob sends marker
        inbox_helper:mark_last_muclight_message(Bob, [Alice, Bob, Kate]),
        %% The crew ask for inbox second time. Only Kate has unread messages
        check_inbox(Alice,[#conv{unread = 0, from = AliceRoomJid, to = AliceJid, content = Msg}]),
        check_inbox(Kate, [#conv{unread = 1, from = AliceRoomJid, to = KateJid, content = Msg}]),
        check_inbox(Bob, [#conv{unread = 0, from = AliceRoomJid, to = BobJid, content = Msg}])
      end).

groupchat_markers_all_reset_room_created(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        Id = <<"random-id">>,
        Msg = <<"Mark me!">>,
        RoomName = pubsub_tools:pubsub_node_name(),
        AliceJid = inbox_helper:to_bare_lower(Alice),
        RoomJid = inbox_helper:create_room_send_msg_check_inbox(Alice, [Bob, Kate], RoomName, Msg, Id),
        AliceRoomJid = <<RoomJid/binary, "/", AliceJid/binary>>,
        [inbox_helper:mark_last_muclight_message(U, [Alice, Bob, Kate]) || U <- [Bob, Kate]],
        inbox_helper:foreach_check_inbox([Bob, Kate, Alice], 0, AliceRoomJid, Msg)
      end).

inbox_does_not_trigger_does_user_exist(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        Msg = <<"Mark me!">>,
        RoomName = inbox_helper:create_room(Alice, [Bob, Kate]),
        RoomJid = room_bin_jid(RoomName),
        HookHandlerExtra = start_hook_listener(),
        Stanza = escalus_stanza:groupchat_to(RoomJid, Msg),
        %% Alice sends message to a room
        escalus:send(Alice, Stanza),
        [escalus:wait_for_stanza(User) || User <- [Alice, Bob, Kate]],
        stop_hook_listener(HookHandlerExtra),
        verify_hook_listener(RoomName)
      end).

system_message_is_correctly_avoided(Config) ->
    escalus:story(Config, [{alice, 1}, {alice_bis, 1}, {bob, 1}], fun(Alice, AliceBis, Bob) ->
        %% Variables
        Id1 = <<"id-1">>,
        Id2 = <<"id-2">>,
        Msg1 = <<"Hi Room!">>,
        Msg2 = <<"How are you?">>,
        Users = [Alice, AliceBis, Bob],
        InitOccupants = [{M, member} || M <- [AliceBis, Bob]],
        Room = atom_to_binary(?FUNCTION_NAME, utf8),
        BobJid = inbox_helper:to_bare_lower(Bob),
        AliceJid = inbox_helper:to_bare_lower(Alice),
        AliceBisJid = inbox_helper:to_bare_lower(AliceBis),
        RoomJid = room_bin_jid(Room),
        %% Given a room
        muc_light_helper:given_muc_light_room(Room, Alice, InitOccupants),
        BobRoomJid = <<RoomJid/binary, "/", BobJid/binary>>,
        Stanza1 = escalus_stanza:set_id(escalus_stanza:groupchat_to(RoomJid, Msg1), Id1),
        %% Alice sends msg to room
        escalus:send(Alice, Stanza1),
        inbox_helper:wait_for_groupchat_msg(Users),
        %% Bob sends second message
        Stanza2 = escalus_stanza:set_id(escalus_stanza:groupchat_to(RoomJid, Msg2), Id2),
        escalus:send(Bob, Stanza2),
        inbox_helper:wait_for_groupchat_msg(Users),
        %% Alice has one unread message (from Bob)
        check_inbox(Alice, [#conv{unread = 1, from = BobRoomJid, to = AliceJid, content = Msg2}]),
        %% Bob has 0 unread messages because he sent the last message
        check_inbox(Bob, [#conv{unread = 0, from = BobRoomJid, to = BobJid, content = Msg2}]),
        %% AliceBis has 2 unread messages (from Alice and Bob) and does not have the affiliation
        check_inbox(AliceBis, [#conv{unread = 2, from = BobRoomJid, to = AliceBisJid, content = Msg2}])
      end).

%%--------------------------------------------------------------------
%% Classic MUC tests
%%--------------------------------------------------------------------

simple_groupchat_stored_in_all_inbox_muc(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        RoomConfig = muc_helper:start_room(Config, extract_user_specs(Alice), muc_helper:fresh_room_name(), <<"some_friendly_name">>, default),
        Users = [Alice, Bob, Kate],
        Msg = <<"Hi Room!">>,
        Id = <<"MyID">>,
        Room = ?config(room, RoomConfig),
        RoomAddr = muc_helper:room_address(Room),

        inbox_helper:enter_room(Room, Users),
        inbox_helper:make_members(Room, Alice, Users -- [Alice]),
        Stanza = escalus_stanza:set_id(
          escalus_stanza:groupchat_to(RoomAddr, Msg), Id),
        escalus:send(Bob, Stanza),
        inbox_helper:wait_for_groupchat_msg(Users),
        [AliceJid, BobJid, KateJid] = lists:map(fun inbox_helper:to_bare_lower/1, Users),
        BobRoomJid = muc_helper:room_address(Room, inbox_helper:nick(Bob)),
        %% Bob has 0 unread messages
        check_inbox(Bob, [#conv{unread = 0, from = BobRoomJid, to = BobJid, content = Msg}],
                    #{}, #{case_sensitive => true}),
        %% Alice and Kate have one conv with 1 unread message
        check_inbox(Alice,[#conv{unread = 1, from = BobRoomJid, to = AliceJid, content = Msg}],
                    #{}, #{case_sensitive => true}),
        check_inbox(Kate, [#conv{unread = 1, from = BobRoomJid, to = KateJid, content = Msg}],
                    #{}, #{case_sensitive => true})
      end).

simple_groupchat_stored_in_offline_users_inbox_muc(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        RoomConfig = muc_helper:start_room(Config, extract_user_specs(Alice), muc_helper:fresh_room_name(), <<"some_friendly_name">>, default),
        Users = [Alice, Bob, Kate],
        Msg = <<"Hi Room!">>,
        Id = <<"MyID">>,
        Room = ?config(room, RoomConfig),
        RoomAddr = muc_helper:room_address(Room),

        inbox_helper:enter_room(Room, Users),
        inbox_helper:make_members(Room, Alice, Users -- [Alice]),
        inbox_helper:leave_room(Kate, Room, Users),
        Stanza = escalus_stanza:set_id(
          escalus_stanza:groupchat_to(RoomAddr, Msg), Id),
        escalus:send(Bob, Stanza),
        inbox_helper:wait_for_groupchat_msg(Users -- [Kate]),

        [AliceJid, BobJid, KateJid] = lists:map(fun inbox_helper:to_bare_lower/1, Users),
        BobRoomJid = muc_helper:room_address(Room, inbox_helper:nick(Bob)),
        %% Bob has 0 unread messages
        check_inbox(Bob, [#conv{unread = 0, from = BobRoomJid, to = BobJid, content = Msg}],
                    #{}, #{case_sensitive => true}),
        %% Alice and Kate have one conv with 1 unread message
        check_inbox(Alice, [#conv{unread = 1, from = BobRoomJid, to = AliceJid, content = Msg}],
                    #{}, #{case_sensitive => true}),
        check_inbox(Kate, [#conv{unread = 1, from = BobRoomJid, to = KateJid, content = Msg}],
                    #{}, #{case_sensitive => true})
      end).


unread_count_is_the_same_after_going_online_again(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        RoomConfig = muc_helper:start_room(Config, extract_user_specs(Alice), muc_helper:fresh_room_name(), <<"some_friendly_name">>, default),
        Users = [Alice, Bob, Kate],
        Msg = <<"Hi Room!">>,
        Id = <<"MyID">>,
        Room = ?config(room, RoomConfig),
        RoomAddr = muc_helper:room_address(Room),

        inbox_helper:enter_room(Room, Users),
        inbox_helper:make_members(Room, Alice, Users -- [Alice]),
        inbox_helper:leave_room(Kate, Room, Users),
        Stanza = escalus_stanza:set_id(
          escalus_stanza:groupchat_to(RoomAddr, Msg), Id),
        escalus:send(Bob, Stanza),
        inbox_helper:wait_for_groupchat_msg(Users -- [Kate]),
        inbox_helper:enter_room(Room, Kate, Users -- [Kate], 1),
        [AliceJid, BobJid, KateJid] = lists:map(fun inbox_helper:to_bare_lower/1, Users),
        BobRoomJid = muc_helper:room_address(Room, inbox_helper:nick(Bob)),
        %% Bob has 0 unread messages
        check_inbox(Bob, [#conv{unread = 0, from = BobRoomJid, to = BobJid, content = Msg}],
                    #{}, #{case_sensitive => true}),
        %% Alice and Kate have one conv with 1 unread message
        check_inbox(Alice, [#conv{unread = 1, from = BobRoomJid, to = AliceJid, content = Msg}],
                    #{}, #{case_sensitive => true}),
        check_inbox(Kate, [#conv{unread = 1, from = BobRoomJid, to = KateJid, content = Msg}],
                    #{}, #{case_sensitive => true})
    end).

unread_count_is_reset_after_sending_chatmarker(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        RoomConfig = muc_helper:start_room(Config, extract_user_specs(Alice), muc_helper:fresh_room_name(), <<"some_friendly_name">>, default),
        Users = [Alice, Bob, Kate],
        Msg = <<"Hi Room!">>,
        Id = <<"MyID">>,
        Room = ?config(room, RoomConfig),
        RoomAddr = muc_helper:room_address(Room),

        inbox_helper:enter_room(Room, Users),
        inbox_helper:make_members(Room, Alice, Users -- [Alice]),
        Stanza = escalus_stanza:set_id(
          escalus_stanza:groupchat_to(RoomAddr, Msg), Id),
        escalus:send(Bob, Stanza),
        inbox_helper:wait_for_groupchat_msg(Users),
        ChatMarkerWOType = escalus_stanza:chat_marker(RoomAddr, <<"displayed">>, Id),
        ChatMarker = escalus_stanza:setattr(ChatMarkerWOType, <<"type">>, <<"groupchat">>),
        %% User marks last message
        escalus:send(Kate, ChatMarker),
        inbox_helper:wait_for_groupchat_msg(Users),

        [AliceJid, BobJid, KateJid] = lists:map(fun inbox_helper:to_bare_lower/1, Users),
        BobRoomJid = muc_helper:room_address(Room, inbox_helper:nick(Bob)),
        %% Bob has 0 unread messages
        check_inbox(Bob, [#conv{unread = 0, from = BobRoomJid, to = BobJid, content = Msg}],
                    #{}, #{case_sensitive => true}),
        %% Alice have one conv with 1 unread message
        check_inbox(Alice, [#conv{unread = 1, from = BobRoomJid, to = AliceJid, content = Msg}],
                    #{}, #{case_sensitive => true}),
        %% Kate has 0 unread messages
        check_inbox(Kate, [#conv{unread = 0, from = BobRoomJid, to = KateJid, content = Msg}],
                    #{}, #{case_sensitive => true})
      end).

non_reset_marker_should_not_affect_muc_inbox(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        RoomConfig = muc_helper:start_room(Config, extract_user_specs(Alice), muc_helper:fresh_room_name(), <<"some_friendly_name">>, default),
        % %% GIVEN
        Users = [Alice, Bob, Kate],
        Msg = <<"Hi Room!">>,
        Id = <<"MyID">>,
        Room = ?config(room, RoomConfig),
        RoomAddr = muc_helper:room_address(Room),

        % %% WHEN
        inbox_helper:enter_room(Room, Users),
        inbox_helper:make_members(Room, Alice, Users -- [Alice]),
        Stanza = escalus_stanza:set_id(escalus_stanza:groupchat_to(RoomAddr, Msg), Id),
        escalus:send(Bob, Stanza),
        inbox_helper:wait_for_groupchat_msg(Users),
        % %% AND MARKED WRONG
        KateChatMarkerWOType = escalus_stanza:chat_marker(RoomAddr,<<"acknowledged">>, Id),
        KateChatMarker = escalus_stanza:setattr(KateChatMarkerWOType, <<"type">>, <<"groupchat">>),
        escalus:send(Kate, KateChatMarker),
        inbox_helper:wait_for_groupchat_msg(Users),

        [AliceJid, BobJid, KateJid] = lists:map(fun inbox_helper:to_bare_lower/1, Users),
        BobRoomJid = muc_helper:room_address(Room, inbox_helper:nick(Bob)),
        % %% THEN
        %% Bob has 0 unread messages
        check_inbox(Bob, [#conv{unread = 0, from = BobRoomJid, to = BobJid, content = Msg}],
                    #{}, #{case_sensitive => true}),
        %% Alice have one conv with 1 unread message
        check_inbox(Alice, [#conv{unread = 1, from = BobRoomJid, to = AliceJid, content = Msg}],
                    #{}, #{case_sensitive => true}),
        %% Kate has 1 unread messages
        check_inbox(Kate, [#conv{unread = 1, from = BobRoomJid, to = KateJid, content = Msg}],
                    #{}, #{case_sensitive => true})
      end).

unread_count_is_reset_after_sending_reset_stanza(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        RoomConfig = muc_helper:start_room(Config, extract_user_specs(Alice), muc_helper:fresh_room_name(), <<"some_friendly_name">>, default),
        % %% WITH
        Users = [Alice, Bob, Kate],
        Msg = <<"Hi Room!">>,
        Id = <<"MyID">>,
        Room = ?config(room, RoomConfig),
        RoomAddr = muc_helper:room_address(Room),
        % %% PROVIDED
        inbox_helper:enter_room(Room, Users),
        inbox_helper:make_members(Room, Alice, Users -- [Alice]),
        % %% WHEN A MESSAGE IS SENT
        Stanza = escalus_stanza:set_id(
          escalus_stanza:groupchat_to(RoomAddr, Msg), Id),
        escalus:send(Bob, Stanza),
        inbox_helper:wait_for_groupchat_msg(Users),
        [AliceJid, BobJid, KateJid] = lists:map(fun inbox_helper:to_bare_lower/1, Users),
        BobRoomJid = muc_helper:room_address(Room, inbox_helper:nick(Bob)),
        %% Make sure Kate gets her inbox updated
        check_inbox(Kate, [#conv{unread = 1, from = BobRoomJid, to = KateJid, content = Msg}],
                    #{}, #{case_sensitive => true}),
        % %% AND WHEN SEND RESET FOR ROOM
        inbox_helper:reset_inbox(Kate, RoomAddr),

        %% Bob has 0 unread messages
        check_inbox(Bob, [#conv{unread = 0, from = BobRoomJid, to = BobJid, content = Msg}],
                    #{}, #{case_sensitive => true}),
        %% Alice have one conv with 1 unread message
        check_inbox(Alice, [#conv{unread = 1, from = BobRoomJid, to = AliceJid, content = Msg}],
                    #{}, #{case_sensitive => true}),
        %% Kate has 0 unread messages
        check_inbox(Kate, [#conv{unread = 0, from = BobRoomJid, to = KateJid, content = Msg}],
                    #{}, #{case_sensitive => true}),
        inbox_helper:assert_has_no_stanzas([Alice, Bob, Kate])
      end).

private_messages_are_handled_as_one2one(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        RoomConfig = muc_helper:start_room(Config, extract_user_specs(Alice), muc_helper:fresh_room_name(), <<"some_friendly_name">>, default),
        Users = [Alice, Bob, Kate],
        Msg = <<"Hi Room!">>,
        Id = <<"MyID">>,
        Room = ?config(room, RoomConfig),
        BobRoomJid = muc_helper:room_address(Room, inbox_helper:nick(Bob)),
        KateRoomJid = muc_helper:room_address(Room, inbox_helper:nick(Kate)),

        inbox_helper:enter_room(Room, Users),
        inbox_helper:make_members(Room, Alice, Users -- [Alice]),
        Stanza = escalus_stanza:set_id(
          escalus_stanza:chat_to(BobRoomJid, Msg), Id),
        escalus:send(Kate, Stanza),
        _BobsPrivMsg = escalus:wait_for_stanza(Bob),

        KateJid = inbox_helper:to_bare_lower(Kate),
        %% Bob has 1 unread message
        check_inbox(Bob, [#conv{unread = 1, from = KateRoomJid, to = BobRoomJid, content = Msg}],
                    #{}, #{case_sensitive => true}),
        %% Alice gets nothing
        check_inbox(Alice, []),
        %% Kate has 1 conv with 0 unread messages
        check_inbox(Kate, [#conv{unread = 0, from = KateJid, to = BobRoomJid, content = Msg}],
                    #{}, #{case_sensitive => true, check_resource => false})
      end).

%%--------------------------------------------------------------------
%% Timestamp-related tests
%%--------------------------------------------------------------------

timestamp_is_updated_on_new_message(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
         Body1 = <<"Hello Bob">>,
         Body2 = <<"Are you there?">>,
         %% We capture the timestamp after the first message
         escalus:send(Alice, escalus_stanza:chat_to(Bob, Body1)),
         _M1 = escalus:wait_for_stanza(Bob),
         [Item1] = check_inbox(Bob, [#conv{unread = 1, from = Alice, to = Bob, content = Body1}]),
         TStamp1 = inbox_helper:timestamp_from_item(Item1),
         %% We capture the timestamp after the second message
         escalus:send(Alice, escalus_stanza:chat_to(Bob, Body2)),
         _M2 = escalus:wait_for_stanza(Bob),
         [Item2] = check_inbox(Bob, [#conv{unread = 2, from = Alice, to = Bob, content = Body2}]),
         TStamp2 = inbox_helper:timestamp_from_item(Item2),
         %% Timestamp after second message must be higher
         case TStamp2 > TStamp1 of
             true -> ok;
             false -> error(#{ type => timestamp_is_not_greater,
                               item1 => Item1,
                               item2 => Item2,
                               tstamp1 => TStamp1,
                               tstamp2 => TStamp2 })
         end
  end).

order_by_timestamp_ascending(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        #{ Alice := AliceConvs, Bob := BobConvs, Kate := KateConvs } =
        given_conversations_between(Alice, [Bob, Kate]),

        %% Alice has two conversations in her inbox (no unread messages)
        %% check_inbox will reverse conversation list automatically
        check_inbox(Alice, AliceConvs, #{ order => asc }, #{}),

        %% Kate has one conversation with one unread
        check_inbox(Kate, KateConvs),

        %% Bob has one conversation with one unread
        check_inbox(Bob, BobConvs)
      end).

get_by_timestamp_range(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        #{ Alice := AliceConvs, time_before := TimeBefore } =
        given_conversations_between(Alice, [Bob, Kate]),

        ConvWithBob = lists:keyfind(Bob, #conv.to, AliceConvs),
        TimeAfterBob = ConvWithBob#conv.time_after,

        %% Between given timestamps we have only one conversation: with Bob
        check_inbox(Alice, [ConvWithBob], #{ start => TimeBefore, 'end' => TimeAfterBob }, #{})
    end).

get_with_start_timestamp(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        #{ Alice := AliceConvs } =
        given_conversations_between(Alice, [Bob, Kate]),

        ConvWithBob = lists:keyfind(Bob, #conv.to, AliceConvs),
        TimeAfterBob = ConvWithBob#conv.time_after,
        ConvWithKate = lists:keyfind(Kate, #conv.to, AliceConvs),

        %% After given timestamp we have only one conversation: with Kate
        check_inbox(Alice, [ConvWithKate], #{ start => TimeAfterBob }, #{})
    end).

get_with_end_timestamp(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        #{ Alice := AliceConvs } =
        given_conversations_between(Alice, [Bob, Kate]),

        ConvWithBob = lists:keyfind(Bob, #conv.to, AliceConvs),
        TimeAfterBob = ConvWithBob#conv.time_after,
        %% Before given timestamp we have only one conversation: with Bob
        %% TODO: Improve this test to store 3+ conversations in Alice's inbox
        check_inbox(Alice, [ConvWithBob], #{ 'end' => TimeAfterBob }, #{})
    end).


%% Bin flushes tests
bin_is_not_included_by_default(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        RoomName = create_room_and_make_users_leave(Alice, Bob, Kate),
        RoomJid = room_bin_jid(RoomName),
        BobJid = inbox_helper:to_bare_lower(Bob),
        Convs = [#conv{unread = 1, from = RoomJid, to = BobJid,
                       verify = fun inbox_helper:verify_is_none_aff_change/2}],
        %% Fetching all does include it
        check_inbox(Bob, Convs, #{box => all}),
        %% Fetching without explicit box name skips the bin
        check_inbox(Bob, [], #{})
    end).

rest_api_bin_flush_user(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        create_room_and_make_users_leave(Alice, Bob, Kate),
        %% It is not in his bin anymore after triggering a bin flush
        BobName = escalus_utils:get_username(Bob),
        BobDomain = escalus_utils:get_server(Bob),
        Path = <<"/inbox", "/", (BobDomain)/binary, "/", (BobName)/binary, "/0/bin">>,
        {{<<"200">>, <<"OK">>}, NumOfRows} = rest_helper:delete(admin, Path),
        ?assertEqual(1, NumOfRows),
        check_inbox(Bob, [], #{box => bin})
    end).

rest_api_bin_flush_all(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        create_room_and_make_users_leave(Alice, Bob, Kate),
        %% It is not in any bin anymore after triggering a bin flush
        HostTypePath = uri_string:normalize(#{path => domain_helper:host_type()}),
        Path = <<"/inbox/", HostTypePath/binary, "/0/bin">>,
        {{<<"200">>, <<"OK">>}, NumOfRows} = rest_helper:delete(admin, Path),
        ?assertEqual(2, NumOfRows),
        check_inbox(Bob, [], #{box => bin}),
        check_inbox(Kate, [], #{box => bin})
    end).

timeout_cleaner_flush_all(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        create_room_and_make_users_leave(Alice, Bob, Kate),
        %% It is eventually not in any bin thanks to the periodic cleanouts
        check_inbox(Bob, [], #{box => bin}),
        check_inbox(Kate, [], #{box => bin})
    end).

xmpp_bin_flush(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        create_room_and_make_users_leave(Alice, Bob, Kate),
        %% It is eventually not in any bin thanks to the periodic cleanouts
        %% Bob requests flush through xmpp
        Iq = escalus_stanza:iq(<<"set">>,
                               [#xmlel{name = <<"empty-bin">>,
                                       attrs = [{<<"xmlns">>, inbox_helper:inbox_ns()}],
                                       children = []}]),
        escalus:send(Bob, Iq),
        escalus:assert(is_iq_result, [Iq], escalus:wait_for_stanza(Bob)),
        check_inbox(Bob, [], #{box => bin})
    end).


%% helpers
create_room_and_make_users_leave(Alice, Bob, Kate) ->
    RoomName = pubsub_tools:pubsub_node_name(),
    inbox_helper:create_room_and_check_inbox(Alice, [Bob, Kate], RoomName),
    %% Bob leaves the room
    muc_light_helper:user_leave(RoomName, Bob, [{Alice, owner}, {Kate, member}]),
    muc_light_helper:user_leave(RoomName, Kate, [{Alice, owner}]),
    %% Bob doesn't have conversation in his inbox, nor Kate
    check_inbox(Bob, [], #{box => inbox}),
    check_inbox(Kate, [], #{box => inbox}),
    RoomName.

if_async_check_bin(Config, Bob, Convs) ->
    case maps:get(backend, ?config(inbox_opts, Config), rdbms) of
        rdbms -> ok;
        rdbms_async ->
            check_inbox(Bob, Convs, #{box => bin})
    end.

start_hook_listener() ->
    TestCasePid = self(),
    distributed_helper:rpc(distributed_helper:mim(), ?MODULE, rpc_start_hook_handler, [TestCasePid, domain_helper:host_type()]).

stop_hook_listener(HookExtra) ->
    distributed_helper:rpc(distributed_helper:mim(), ?MODULE, rpc_stop_hook_handler, [HookExtra, domain_helper:host_type()]).

rpc_start_hook_handler(TestCasePid, HostType) ->
    Extra = #{test_case_pid => TestCasePid},
    gen_hook:add_handler(does_user_exist, HostType, fun ?MODULE:hook_handler_fn/3, Extra, 1),
    Extra.

rpc_stop_hook_handler(HookExtra, HostType) ->
    gen_hook:delete_handler(does_user_exist, HostType, fun ?MODULE:hook_handler_fn/3, HookExtra, 1).

hook_handler_fn(Acc,
                #{args := [_HostType, User, _Stored]} = _Params,
                #{test_case_pid := Pid} = _Extra) ->
    Pid ! {input, User#jid.user},
    {ok, Acc}.

verify_hook_listener(RoomName) ->
    receive
        {input, RoomName} ->
            ct:fail("does_user_exist was called with a room jid");
        {input, _} ->
            verify_hook_listener(RoomName)
    after 100 ->
              ct:pal("OK")
    end.
