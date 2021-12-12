-module(inbox_extensions_SUITE).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("inbox.hrl").

-define(ROOM_MARKERS_RESET, <<"room_markers_reset">>).
-define(HOUR, 3600).
-define(VALID_JID, <<"mike@localhost">>).
-define(INVALID_JID, <<"$@/">>).
-define(INVALID_VALUE, <<"invalid">>).

-export([all/0,
         groups/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% ERRORS
-export([
         % General
         returns_error_when_no_jid_provided/1,
         returns_error_when_invalid_jid_provided/1,
         returns_error_when_valid_jid_but_no_property/1,
         % Set-unread errors
         returns_error_when_read_invalid_value/1,
         returns_error_when_read_valid_request_but_not_in_inbox/1,
         % Archiving errors
         returns_error_when_archive_invalid_value/1,
         returns_error_when_archive_valid_request_but_not_in_inbox/1,
         % Muting errors
         returns_error_when_mute_invalid_value/1,
         returns_error_when_mute_invalid_seconds/1,
         returns_error_when_mute_valid_request_but_not_in_inbox/1,
         % Form errors
         returns_error_when_archive_field_is_invalid/1,
         returns_error_when_max_is_not_a_number/1
        ]).
%% SUCCESSES
-export([
         % read
         read_unread_entry_set_to_read/1,
         read_read_entry_set_to_unread/1,
         read_unread_entry_with_two_messages_when_set_unread_then_unread_count_stays_in_two/1,
         % archive
         archive_active_entry_gets_archived/1,
         archive_archived_entry_gets_active_on_request/1,
         archive_archived_entry_gets_active_for_the_sender_on_new_message/1,
         archive_archived_entry_gets_active_for_the_receiver_on_new_message/1,
         archive_active_unread_entry_gets_archived_and_still_unread/1,
         archive_full_archive_can_be_fetched/1,
         % mute
         mute_unmuted_entry_gets_muted/1,
         mute_muted_entry_gets_unmuted/1,
         mute_after_timestamp_gets_unmuted/1,
         mute_muted_conv_restarts_timestamp/1,
         % other
         returns_valid_properties_form/1,
         properties_can_be_get/1,
         properties_many_can_be_set/1,
         max_queries_can_be_limited/1,
         max_queries_can_fetch_ahead/1,
         timestamp_is_not_reset_with_setting_properties/1
        ]).
%% Groupchats
-export([
         groupchat_setunread_stanza_sets_inbox/1 % muclight
        ]).


-import(inbox_helper, [
                       inbox_modules/0,
                       muclight_modules/0,
                       inbox_opts/0
                      ]).


all() ->
    inbox_helper:skip_or_run_inbox_tests(tests()).

tests() ->
    [
     {group, generic},
     {group, one_to_one},
     {group, muclight}
    ].

suite() ->
    escalus:suite().

groups() ->
    Gs = [
     {generic, [], [
        % General errors
        returns_error_when_no_jid_provided,
        returns_error_when_invalid_jid_provided,
        returns_error_when_valid_jid_but_no_property,
        % Set-unread errors
        returns_error_when_read_invalid_value,
        returns_error_when_read_valid_request_but_not_in_inbox,
        % Archiving errors
        returns_error_when_archive_invalid_value,
        returns_error_when_archive_valid_request_but_not_in_inbox,
        % Muting errors
        returns_error_when_mute_invalid_value,
        returns_error_when_mute_invalid_seconds,
        returns_error_when_mute_valid_request_but_not_in_inbox,
        % Form errors
        returns_error_when_archive_field_is_invalid,
        returns_error_when_max_is_not_a_number
      ]},
     {one_to_one, [], [
        % read
        read_unread_entry_set_to_read,
        read_read_entry_set_to_unread,
        read_unread_entry_with_two_messages_when_set_unread_then_unread_count_stays_in_two,
        % archive
        archive_active_entry_gets_archived,
        archive_archived_entry_gets_active_on_request,
        archive_archived_entry_gets_active_for_the_sender_on_new_message,
        archive_archived_entry_gets_active_for_the_receiver_on_new_message,
        archive_active_unread_entry_gets_archived_and_still_unread,
        archive_full_archive_can_be_fetched,
        % mute
        mute_unmuted_entry_gets_muted,
        mute_muted_entry_gets_unmuted,
        mute_after_timestamp_gets_unmuted,
        mute_muted_conv_restarts_timestamp,
        % other
        returns_valid_properties_form,
        properties_can_be_get,
        properties_many_can_be_set,
        max_queries_can_be_limited,
        max_queries_can_fetch_ahead,
        timestamp_is_not_reset_with_setting_properties
      ]},
     {muclight, [], [
        groupchat_setunread_stanza_sets_inbox
      ]}
    ],
    inbox_helper:maybe_run_in_parallel(Gs).

init_per_suite(Config) ->
    ok = dynamic_modules:ensure_modules(domain_helper:host_type(mim), inbox_modules()),
    InboxOptions = inbox_opts(),
    Config1 = escalus:init_per_suite(Config),
    Config2 = [{inbox_opts, InboxOptions} | Config1],
    escalus:create_users(Config2, escalus:get_users([alice, bob, kate, mike])).

end_per_suite(Config) ->
    Config1 = escalus:delete_users(Config, escalus:get_users([alice, bob, kate, mike])),
    HostType = domain_helper:host_type(mim),
    dynamic_modules:stop(HostType, mod_inbox),
    muc_light_helper:clear_db(HostType),
    escalus_fresh:clean(),
    escalus:end_per_suite(Config1).

init_per_group(muclight, Config) ->
    ok = dynamic_modules:ensure_modules(domain_helper:host_type(mim), muclight_modules()),
    inbox_helper:reload_inbox_option(Config, groupchat, [muclight]),
    Config;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(muclight, Config) ->
    HostType = domain_helper:host_type(mim),
    muc_light_helper:clear_db(HostType),
    dynamic_modules:stop(HostType, mod_muc_light),
    Config;
end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(groupchat_setunread_stanza_sets_inbox, Config) ->
    inbox_helper:clear_inbox_all(),
    muc_light_helper:create_room(?ROOM_MARKERS_RESET, muc_light_helper:muc_host(), alice, [bob, kate],
                                 Config, muc_light_helper:ver(1)),
    escalus:init_per_testcase(groupchat_setunread_stanza_sets_inbox, Config);
init_per_testcase(TestCase, Config) ->
    escalus:init_per_testcase(TestCase, Config).

end_per_testcase(groupchat_setunread_stanza_sets_inbox, Config) ->
    inbox_helper:clear_inbox_all(),
    inbox_helper:restore_inbox_option(Config),
    escalus:end_per_testcase(groupchat_setunread_stanza_sets_inbox, Config);
end_per_testcase(TestCase, Config) ->
    escalus:end_per_testcase(TestCase, Config).


%%--------------------------------------------------------------------
%% tests
%%--------------------------------------------------------------------

% General
returns_error_when_no_jid_provided(Config) ->
    Stanza = make_inbox_iq_request(undefined, anything, anything),
    returns_error(Config, Stanza, <<"jid-required">>).

returns_error_when_invalid_jid_provided(Config) ->
    Stanza = make_inbox_iq_request(?INVALID_JID, anything, anything),
    returns_error(Config, Stanza, <<"invalid-jid">>).

returns_error_when_valid_jid_but_no_property(Config) ->
    Stanza = make_inbox_iq_request(?VALID_JID, undefined, anything),
    returns_error(Config, Stanza, <<"no-property">>).

% Set-unread errors
returns_error_when_read_invalid_value(Config) ->
    Stanza = make_inbox_iq_request(?VALID_JID, read, ?INVALID_VALUE),
    returns_error(Config, Stanza, <<"bad-request">>).

returns_error_when_read_valid_request_but_not_in_inbox(Config) ->
    Stanza = make_inbox_iq_request(?VALID_JID, read, true),
    returns_error(Config, Stanza, <<"item-not-found">>).

% Archiving errors
returns_error_when_archive_invalid_value(Config) ->
    Stanza = make_inbox_iq_request(?VALID_JID, archive, ?INVALID_VALUE),
    returns_error(Config, Stanza, <<"bad-request">>).

returns_error_when_archive_valid_request_but_not_in_inbox(Config) ->
    Stanza = make_inbox_iq_request(?VALID_JID, archive, true),
    returns_error(Config, Stanza, <<"item-not-found">>).

% Muting errors
returns_error_when_mute_invalid_value(Config) ->
    Stanza = make_inbox_iq_request(?VALID_JID, mute, ?INVALID_VALUE),
    returns_error(Config, Stanza, <<"bad-request">>).

returns_error_when_mute_invalid_seconds(Config) ->
    Stanza = make_inbox_iq_request(?VALID_JID, mute, -?HOUR),
    returns_error(Config, Stanza, <<"bad-request">>).

returns_error_when_mute_valid_request_but_not_in_inbox(Config) ->
    Stanza = make_inbox_iq_request(?VALID_JID, mute, ?HOUR),
    returns_error(Config, Stanza, <<"item-not-found">>).

% Form errors
returns_error_when_archive_field_is_invalid(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
      inbox_helper:assert_invalid_inbox_form_value_error(Alice, <<"archive">>, <<"invalid">>)
    end).

returns_error_when_max_is_not_a_number(Config) ->
    Stanza = inbox_helper:make_inbox_stanza(#{box => both, limit => <<"NaN">>}),
    returns_error(Config, Stanza, <<"bad-request">>).

returns_error(Config, Stanza, Value) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        assert_invalid_request(Alice, Stanza, Value)
    end).

% read
read_unread_entry_set_to_read(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        % Alice sends a message to Bob
        Body = <<"Hi Bob">>,
        inbox_helper:send_msg(Alice, Bob, Body),
        % Bob has one unread message
        inbox_helper:check_inbox(Bob, [#conv{unread = 1, from = Alice, to = Bob, content = Body}]),
        % Then Bob decides to mark it as read
        set_inbox_properties(Bob, Alice, [{read, true}]),
        % Bob's inbox has no unread messages
        inbox_helper:check_inbox(Bob, [#conv{unread = 0, from = Alice, to = Bob, content = Body}])
    end).

read_read_entry_set_to_unread(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        % Alice sends a message to Bob and Bob reads it
        Body = <<"Hi Bob">>,
        inbox_helper:send_and_mark_msg(Alice, Bob, Body),
        % Bob has no unread messages
        inbox_helper:check_inbox(Bob, [#conv{unread = 0, from = Alice, to = Bob, content = Body}]),
        % Then Bob decides to mark it again as unread
        set_inbox_properties(Bob, Alice, [{read, false}]),
        % Bob's inbox has something unread
        inbox_helper:check_inbox(Bob, [#conv{unread = 1, from = Alice, to = Bob, content = Body}])
    end).

read_unread_entry_with_two_messages_when_set_unread_then_unread_count_stays_in_two(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        % Alice sends a message to Bob and Bob reads it
        Body = <<"Hi again Bob">>,
        inbox_helper:send_msg(Alice, Bob, <<"Hi Bob">>),
        inbox_helper:send_msg(Alice, Bob, Body),
        % Bob has some unread messages
        inbox_helper:check_inbox(Bob, [#conv{unread = 2, from = Alice, to = Bob, content = Body}]),
        % Then Bob decides to mark it again as unread
        set_inbox_properties(Bob, Alice, [{read, false}]),
        % And the count didn't really change, to prevent losing higher counts
        inbox_helper:check_inbox(Bob, [#conv{unread = 2, from = Alice, to = Bob, content = Body}])
    end).

% archive
archive_active_entry_gets_archived(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        % Alice sends a message to Bob
        Body = <<"Hi Bob">>,
        inbox_helper:send_and_mark_msg(Alice, Bob, Body),
        inbox_helper:check_inbox(Bob, [#conv{unread = 0, from = Alice, to = Bob, content = Body}]),
        % Then Bob decides to archive it
        set_inbox_properties(Bob, Alice, [{archive, true}]),
        % Then the conversation is in the archive and not in the active box
        inbox_helper:check_inbox(Bob, [], #{box => active}),
        inbox_helper:check_inbox(Bob, [#conv{unread = 0, from = Alice, to = Bob, content = Body}], #{box => archive})
    end).

archive_archived_entry_gets_active_on_request(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        % Alice sends a message to Bob and Bob archives it immediately
        Body = <<"Hi Bob">>,
        inbox_helper:send_msg(Alice, Bob, Body),
        inbox_helper:check_inbox(Bob, [#conv{unread = 1, from = Alice, to = Bob, content = Body}]),
        set_inbox_properties(Bob, Alice, [{archive, true}]),
        % Then bob decides to recover the conversation
        set_inbox_properties(Bob, Alice, [{archive, false}]),
        % Then the conversation is in the active and not in the archive box
        inbox_helper:check_inbox(Bob, [#conv{unread = 1, from = Alice, to = Bob, content = Body}], #{box => active}),
        inbox_helper:check_inbox(Bob, [], #{box => archive})
    end).

archive_archived_entry_gets_active_for_the_receiver_on_new_message(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        % Alice sends a message to Bob and Bob archives it immediately
        Body = <<"Hi Bob">>,
        inbox_helper:send_msg(Alice, Bob, Body),
        inbox_helper:check_inbox(Bob, [#conv{unread = 1, from = Alice, to = Bob, content = Body}]),
        set_inbox_properties(Bob, Alice, [{archive, true}]),
        % But then Alice keeps writing:
        inbox_helper:send_msg(Alice, Bob, Body),
        % Then the conversation is automatically in the active and not in the archive box
        inbox_helper:check_inbox(Bob, [#conv{unread = 2, from = Alice, to = Bob, content = Body}], #{box => active}),
        inbox_helper:check_inbox(Bob, [], #{box => archive})
    end).

archive_archived_entry_gets_active_for_the_sender_on_new_message(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        % Alice sends a message to Bob and then she archives the conversation
        Body = <<"Hi Bob">>,
        inbox_helper:send_msg(Alice, Bob, Body),
        inbox_helper:check_inbox(Bob, [#conv{unread = 1, from = Alice, to = Bob, content = Body}]),
        set_inbox_properties(Alice, Bob, [{archive, true}]),
        % But then Alice keeps writing
        inbox_helper:send_msg(Alice, Bob, Body),
        % Then the conversation is automatically in the active and not in the archive box
        inbox_helper:check_inbox(Alice, [], #{box => archive}),
        inbox_helper:check_inbox(Alice, [#conv{unread = 0, from = Alice, to = Bob, content = Body}], #{box => active})
    end).

archive_active_unread_entry_gets_archived_and_still_unread(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        % Alice sends a message to Bob, but Bob archives it without reading it
        Body = <<"Hi Bob">>,
        inbox_helper:send_msg(Alice, Bob, Body),
        inbox_helper:check_inbox(Bob, [#conv{unread = 1, from = Alice, to = Bob, content = Body}]),
        set_inbox_properties(Bob, Alice, [{archive, true}]),
        inbox_helper:check_inbox(Bob, [], #{box => active}),
        % Then Bob queries his archive and the conversation is there still unread
        inbox_helper:check_inbox(Bob, [#conv{unread = 1, from = Alice, to = Bob, content = Body}], #{box => archive})
    end).

archive_full_archive_can_be_fetched(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}, {mike, 1}], fun(Alice, Bob, Kate, Mike) ->
        % Several people write to Alice, and Alice reads and archives all of them
        inbox_helper:check_inbox(Alice, [], #{box => archive}),
        #{Alice := AliceConvs} = inbox_helper:given_conversations_between(Alice, [Bob, Kate, Mike]),
        inbox_helper:check_inbox(Alice, AliceConvs),
        set_inbox_properties(Alice, Bob, [{archive, true}]),
        set_inbox_properties(Alice, Kate, [{archive, true}]),
        set_inbox_properties(Alice, Mike, [{archive, true}]),
        % Then Alice queries her archive and the conversations are there and not in the active box
        inbox_helper:check_inbox(Alice, [], #{box => active}),
        inbox_helper:check_inbox(Alice, AliceConvs, #{box => archive})
    end).

% mute
mute_unmuted_entry_gets_muted(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        % Alice sends a message to Bob and Bob reads it
        Body = <<"Hi Bob">>,
        inbox_helper:send_and_mark_msg(Alice, Bob, Body),
        inbox_helper:check_inbox(Bob, [#conv{unread = 0, from = Alice, to = Bob, content = Body}]),
        % Then Bob decides to mute the conversation
        set_inbox_properties(Bob, Alice, [{mute, 24*?HOUR}]),
        % Alice keeps writing
        inbox_helper:send_msg(Alice, Bob, Body),
        % Bob's inbox has an unread message, but it's marked as muted
        inbox_helper:check_inbox(Bob, [#conv{unread = 1, from = Alice, to = Bob, content = Body,
                                             verify = fun(_, _, Outer) -> muted_status(23*?HOUR, Outer) end}])
    end).

mute_muted_entry_gets_unmuted(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        % Alice sends a message to Bob and Bob reads it
        Body = <<"Hi Bob">>,
        inbox_helper:send_and_mark_msg(Alice, Bob, Body),
        inbox_helper:check_inbox(Bob, [#conv{unread = 0, from = Alice, to = Bob, content = Body}]),
        % Then Bob decides to mute the conversation
        set_inbox_properties(Bob, Alice, [{mute, 24*?HOUR}]),
        % Alice keeps writing
        inbox_helper:send_msg(Alice, Bob, Body),
        % And Bob unmutes it again because he's now interested
        set_inbox_properties(Bob, Alice, [{mute, 0}]),
        % Bob's inbox has an unread message immediately unmuted
        inbox_helper:check_inbox(
          Bob, [#conv{unread = 1, from = Alice, to = Bob, content = Body,
                      verify = fun(_, _, Outer) -> muted_status(unmuted, Outer) end}])
    end).

mute_after_timestamp_gets_unmuted(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        % Alice sends a message to Bob and Bob reads it
        Body = <<"Hi Bob">>,
        inbox_helper:send_and_mark_msg(Alice, Bob, Body),
        inbox_helper:check_inbox(Bob, [#conv{unread = 0, from = Alice, to = Bob, content = Body}]),
        % Then Bob decides to mute the conversation (for a very short 1 second for testing purposes)
        set_inbox_properties(Bob, Alice, [{mute, 1}]),
        % Alice keeps writing
        inbox_helper:send_msg(Alice, Bob, Body),
        % Inbox eventually returns unmuted
        Fun = fun() ->
                      try inbox_helper:check_inbox(
                            Bob, [#conv{unread = 1, from = Alice, to = Bob, content = Body,
                                        verify = fun(_, _, Outer) -> muted_status(unmuted, Outer) end}]),
                          ok
                      catch _:_ -> not_unmuted_yet
                      end
              end,
        mongoose_helper:wait_until(Fun, ok, #{name => verify_its_unmuted, sleep_time => 250})
    end).

mute_muted_conv_restarts_timestamp(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        % Alice sends a message to Bob and Bob reads it
        Body = <<"Hi Bob">>,
        inbox_helper:send_and_mark_msg(Alice, Bob, Body),
        inbox_helper:check_inbox(Bob, [#conv{unread = 0, from = Alice, to = Bob, content = Body}]),
        % Then Bob decides to mute the conversation
        set_inbox_properties(Bob, Alice, [{mute, ?HOUR}]),
        % Alice keeps writing
        inbox_helper:send_and_mark_msg(Alice, Bob, Body),
        % Then Bob decides to mute the conversation for way longer
        set_inbox_properties(Bob, Alice, [{mute, 24*?HOUR}]),
        % Muted timestamp is way longer than before
        inbox_helper:check_inbox(
          Bob, [#conv{unread = 0, from = Alice, to = Bob, content = Body,
                      verify = fun(_, _, Outer) -> muted_status(23*?HOUR, Outer) end}])
    end).

% other
returns_valid_properties_form(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        InboxConversationNS = inbox_helper:inbox_ns_conversation(),
        escalus:send(Alice, escalus_stanza:iq_get(InboxConversationNS, [])),
        ResIQ = escalus:wait_for_stanza(Alice),
        #{field_count := 4} = Form = inbox_helper:parse_form_iq(ResIQ),
        #{<<"FORM_TYPE">> := #{type := <<"hidden">>, value := InboxConversationNS}} = Form,
        #{<<"archive">> := #{type := <<"boolean">>, value := <<"false">>}} = Form,
        #{<<"read">> := #{type := <<"boolean">>, value := <<"false">>}} = Form,
        #{<<"mute">> := #{type := <<"text-single">>, value := <<"0">>}} = Form
      end).

properties_can_be_get(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        % Alice sends a message to Bob
        Body = <<"Hi Bob">>,
        inbox_helper:send_and_mark_msg(Alice, Bob, Body),
        inbox_helper:check_inbox(Bob, [#conv{unread = 0, from = Alice, to = Bob, content = Body}]),
        % Then Bob can just query the properties of this entry at will
        query_properties(Bob, Alice, [{archive, false}, {read, true}, {mute, 0}])
    end).

properties_many_can_be_set(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        % Alice sends a message to Bob, and Bob sets a bunch of properties about it
        Body = <<"Hi Bob">>,
        inbox_helper:send_msg(Alice, Bob, Body),
        inbox_helper:check_inbox(Bob, [#conv{unread = 1, from = Alice, to = Bob, content = Body}]),
        set_inbox_properties(Bob, Alice, [{archive, true}, {read, true}, {mute, 24*?HOUR}]),
        % Then Bob queries his boxes and everything is as expected
        inbox_helper:check_inbox(Bob, [], #{box => active}),
        inbox_helper:check_inbox(Bob, [#conv{unread = 0, from = Alice, to = Bob, content = Body,
                                       verify = fun(_, _, Outer) -> muted_status(23*?HOUR, Outer)
                                                end}], #{box => archive})
    end).

max_queries_can_be_limited(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}, {mike, 1}],
                        fun(Alice, Bob, Kate, Mike) ->
        % Several people write to Alice
        #{Alice := AliceConvs} =
            inbox_helper:given_conversations_between(Alice, [Bob, Kate, Mike]),
        % Alice has some messages in her inbox
        inbox_helper:check_inbox(Alice, AliceConvs),
        % Then Alice queries her inbox setting a limit to only one conversation,
        % and she gets the newest one
        ConvWithMike = lists:keyfind(Mike, #conv.to, AliceConvs),
        inbox_helper:check_inbox(Alice, [ConvWithMike], #{limit => 1, box => active}),
        % And a limit to two also works fine
        ConvWithKate = lists:keyfind(Kate, #conv.to, AliceConvs),
        inbox_helper:check_inbox(Alice, [ConvWithMike, ConvWithKate], #{limit => 2, box => active})
    end).

max_queries_can_fetch_ahead(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}, {mike, 1}],
                        fun(Alice, Bob, Kate, Mike) ->
        #{Alice := AliceConvs} =
            inbox_helper:given_conversations_between(Alice, [Bob, Kate, Mike]),
        ConvWithBob = lists:keyfind(Bob, #conv.to, AliceConvs),
        ConvWithKate = lists:keyfind(Kate, #conv.to, AliceConvs),
        % ConvWithMike = lists:keyfind(Mike, #conv.to, AliceConvs),
        TimeAfterKate = ConvWithKate#conv.time_after,
        inbox_helper:check_inbox(Alice, [ConvWithKate, ConvWithBob],
                  #{limit => 2, 'end' => TimeAfterKate, box => active})
    end).

timestamp_is_not_reset_with_setting_properties(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        % Alice sends a message to Bob
        inbox_helper:send_msg(Alice, Bob),
        %% We capture the timestamp
        [Item1] = inbox_helper:get_inbox(Bob, #{count => 1}),
        TStamp1 = inbox_helper:timestamp_from_item(Item1),
        % Bob sets a bunch of properties
        set_inbox_properties(Bob, Alice, [{read, true}, {mute, 24*?HOUR}]),
        % Bob gets the inbox again, and timestamp should be the same
        [Item2] = inbox_helper:get_inbox(Bob, #{count => 1}),
        TStamp2 = inbox_helper:timestamp_from_item(Item2),
        ?assertEqual(TStamp1, TStamp2)
  end).

% muclight
groupchat_setunread_stanza_sets_inbox(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        %%% DATA
        MsgBody = <<"marker time!">>,
        AliceJid = inbox_helper:to_bare_lower(Alice),
        BobJid = inbox_helper:to_bare_lower(Bob),
        KateJid = inbox_helper:to_bare_lower(Kate),
        RoomJid = muc_light_helper:room_bin_jid(?ROOM_MARKERS_RESET),
        AliceRoomJid = <<RoomJid/binary,"/", AliceJid/binary>>,
        %%% WHEN A MESSAGE IS SENT (two times the same message)
        MsgStanza = escalus_stanza:set_id(escalus_stanza:groupchat_to(RoomJid, MsgBody), <<"some_ID">>),
        escalus:send(Alice, MsgStanza),
        inbox_helper:wait_for_groupchat_msg([Alice, Bob, Kate]),
        escalus:send(Alice, MsgStanza),
        inbox_helper:wait_for_groupchat_msg([Alice, Bob, Kate]),
        % verify that Bob has the message on inbox, reset it, and verify is still there but read
        inbox_helper:check_inbox(Bob, [#conv{unread = 2, from = AliceRoomJid, to = BobJid, content = MsgBody}]),
        set_inbox_properties(Bob, RoomJid, [{read, true}]),
        inbox_helper:check_inbox(Bob, [#conv{unread = 0, from = AliceRoomJid, to = BobJid, content = MsgBody}]),
        % Bob sets the inbox as unread again and has so in his inbox
        set_inbox_properties(Bob, RoomJid, [{read, false}]),
        inbox_helper:check_inbox(Bob, [#conv{unread = 1, from = AliceRoomJid, to = BobJid, content = MsgBody}]),
        %% Alice has 0 unread messages because she was the sender
        inbox_helper:check_inbox(Alice, [#conv{unread = 0, from = AliceRoomJid, to = AliceJid, content = MsgBody}]),
        %% Kate still has unread messages, and setting the entry as unread keeps the count to two
        set_inbox_properties(Kate, RoomJid, [{read, false}]),
        inbox_helper:check_inbox(Kate, [#conv{unread = 2, from = AliceRoomJid, to = KateJid, content = MsgBody}]),
        %% And nobody received any other stanza
        inbox_helper:assert_has_no_stanzas([Alice, Bob, Kate])
    end).


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
-type maybe_client() :: undefined | escalus:client().
-type box() :: both | active | archive.

-spec query_properties(escalus:client(), escalus:client(), proplists:proplist()) -> [exml:element()].
query_properties(From, To, Expected) ->
    Stanza = make_inbox_get_properties(To),
    escalus:send(From, Stanza),
    Result = escalus:wait_for_stanza(From),
    ?assert(escalus_pred:is_iq_result(Stanza, Result)),
    [Props] = exml_query:subelements(Result, <<"query">>),
    ?assertEqual(inbox_helper:inbox_ns_conversation(), exml_query:attr(Props, <<"xmlns">>)),
    lists:foreach(fun({Key, Val}) -> assert_property(Props, Key, Val) end, Expected).

-spec make_inbox_get_properties(escalus:client()) -> exml:element().
make_inbox_get_properties(To) ->
    Query = escalus_stanza:query_el(inbox_helper:inbox_ns_conversation(), jid_attr(To), []),
    escalus_stanza:iq(<<"get">>, [Query]).

-spec set_inbox_properties(escalus:client(), escalus:client(), proplists:proplist()) -> ok.
set_inbox_properties(From, To, Properties) ->
    Stanza = make_inbox_iq_request(To, Properties),
    escalus:send(From, Stanza),
    check_message_with_properties(From, Stanza, Properties),
    check_iq_result_for_property(From, Stanza).

-spec check_message_with_properties(escalus:client(), exml:element(), proplists:proplist()) -> ok.
check_message_with_properties(From, Stanza, Properties) ->
    Message = escalus:wait_for_stanza(From),
    ?assert(escalus_pred:is_message(Message)),
    ?assert(has_same_id(Stanza, Message)),
    [X] = exml_query:subelements(Message, <<"x">>),
    ?assertEqual(inbox_helper:inbox_ns_conversation(), exml_query:attr(X, <<"xmlns">>)),
    lists:foreach(fun({Key, Val}) -> assert_property(X, Key, Val) end, Properties).

-spec check_iq_result_for_property(escalus:client(), exml:element()) -> ok.
check_iq_result_for_property(From, Stanza) ->
    Result = escalus:wait_for_stanza(From),
    ?assert(escalus_pred:is_iq_result(Stanza, Result)).

-spec make_inbox_iq_request(maybe_client(), atom(), atom()) -> exml:element().
make_inbox_iq_request(ToClient, Key, Value) ->
    make_inbox_iq_request(ToClient, [{Key, Value}]).

-spec make_inbox_iq_request(maybe_client(), proplists:proplist()) -> exml:element().
make_inbox_iq_request(ToClient, Properties) when is_list(Properties) ->
    JidAttr = jid_attr(ToClient),
    Children = props_to_children(Properties),
    Query = escalus_stanza:query_el(inbox_helper:inbox_ns_conversation(), JidAttr, Children),
    escalus_stanza:iq(<<"set">>, [Query]).

assert_invalid_request(From, Stanza, Value) ->
    inbox_helper:assert_invalid_form(From, Stanza, Value, Value).

-spec jid_attr(maybe_client()) -> proplists:proplist().
jid_attr(undefined) -> [];
jid_attr(Client) -> [{<<"jid">>, escalus_utils:get_short_jid(Client)}].

props_to_children(L) -> props_to_children(L, []).
props_to_children([], Acc) -> Acc;
props_to_children([{undefined, _} | Rest], Acc) ->
    props_to_children(Rest, Acc);
props_to_children([{Key, undefined} | Rest], Acc) ->
    props_to_children(Rest, [#xmlel{name = Key} | Acc]);
props_to_children([{Key, Value} | Rest], Acc) ->
    props_to_children(Rest,
      [#xmlel{name = to_bin(Key), children = [#xmlcdata{content = to_bin(Value)}]} | Acc]).

assert_property(X, read, Val) ->
    ?assertEqual(to_bin(Val), exml_query:path(X, [{element, <<"read">>}, cdata]));
assert_property(X, archive, Val) ->
    ?assertEqual(to_bin(Val), exml_query:path(X, [{element, <<"archive">>}, cdata]));
assert_property(X, mute, 0) ->
    ?assertEqual(0, binary_to_integer(exml_query:path(X, [{element, <<"mute">>}, cdata])));
assert_property(X, mute, _) ->
    Cal = binary_to_list(exml_query:path(X, [{element, <<"mute">>}, cdata])),
    calendar:rfc3339_to_system_time(Cal, [{unit, microsecond}]).

-spec to_bin(term()) -> binary().
to_bin(Value) when is_binary(Value) -> Value;
to_bin(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
to_bin(Value) when is_integer(Value) -> integer_to_binary(Value).

-spec has_same_id(exml:element(), exml:element()) -> boolean().
has_same_id(OrigStanza, Stanza) ->
    OrigId = exml_query:attr(OrigStanza, <<"id">>),
    Id = exml_query:attr(Stanza, <<"id">>),
    OrigId =:= Id.

muted_status(unmuted, Outer) ->
    Res = exml_query:path(Outer, [{element, <<"result">>}, {element, <<"mute">>}, cdata]),
    ?assertEqual(<<"0">>, Res);
muted_status(MutedOrUnmuted, Outer) ->
    GivenRfcTimestamp = exml_query:path(Outer, [{element, <<"result">>}, {element, <<"mute">>}, cdata]),
    GivenMutedUntil = calendar:rfc3339_to_system_time(binary_to_list(GivenRfcTimestamp), [{offset, "Z"}, {unit, microsecond}]),
    Now = erlang:system_time(microsecond),
    case MutedOrUnmuted of
        unmuted ->
            ?assert(Now > GivenMutedUntil);
        MutedDiff ->
            Diff = erlang:convert_time_unit(MutedDiff, second, microsecond),
            ?assert(Now + Diff < GivenMutedUntil)
    end.
