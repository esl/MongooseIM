%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(privacy_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("escalus/include/escalus_xmlns.hrl").

-define(SLEEP_TIME, 50).

-import(privacy_helper, [assert_privacy_get_event/1,
                         assert_privacy_get_event/2,
                         assert_privacy_set_event/2,
                         assert_privacy_check_packet_event/3,
                         assert_privacy_push_item_event/2,
                         assert_privacy_push_item_event/3]).

-import(distributed_helper, [mim/0, rpc/4]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, blocking},
     {group, allowing}
    ].

groups() ->
    [
     {blocking, [parallel], blocking_test_cases()},
     {allowing, [parallel], allowing_test_cases()}
    ].

blocking_test_cases() ->
    [
     block_jid_message,
     block_group_message,
     block_subscription_message,
     block_all_message,
     block_jid_presence_in,
     block_jid_presence_out,
     block_jid_iq,
     block_jid_all,
     block_jid_message_but_not_presence,
     newly_blocked_presense_jid_by_new_list,
     newly_blocked_presense_jid_by_list_change,
     newly_blocked_presence_not_notify_self,
     iq_reply_doesnt_crash_user_process,
     iq_with_to_attribute_is_treated_as_regular_one
    ].

allowing_test_cases() ->
    [allow_subscription_to_from_message,
     allow_subscription_both_message].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config0) ->
    HostType = domain_helper:host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config0),
    ModConfig = [{mod_privacy, config_parser_helper:mod_config_with_auto_backend(mod_privacy)}],
    dynamic_modules:ensure_modules(HostType, ModConfig),
    instrument_helper:start(instrument_helper:declared_events(mod_privacy)),
    [{escalus_no_stanzas_after_story, true} | escalus:init_per_suite(Config1)].

end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config),
    instrument_helper:stop().

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

%% In terms of server response to blocked communication, we strive to implement the following
%% as defined in XEP-0016:
%% If someone I block tries to communicate with me, then the following rules apply:
%%  * For presence stanzas (including notifications, subscriptions, and probes), the server MUST NOT respond and MUST NOT
%%    return an error.
%%  * For message stanzas, the server SHOULD return an error, which SHOULD be <service-unavailable/>.
%%  * For IQ stanzas of type "get" or "set", the server MUST return an error, which SHOULD be <service-unavailable/>. IQ
%%    stanzas of other types MUST be silently dropped by the server.
%% If I want to communicate with someone I block, then:
%%  * If the user attempts to send an outbound stanza to a contact and that stanza type is blocked, the user's server MUST
%%    NOT route the stanza to the contact but instead MUST return a <not-acceptable/> error:

%% TODO later:
%% - big picture:
%%   - blocking can be done on jids, roster groups,
%%     subscription type or globally
%%   - a blocking rule may block one or more of {message, presence-in,
%%     presence-out, iqs} by specifying these as children to the list item
%%     or block all of them, when the item has no children
%% - blocking: messages, presence (in/out), iqs, all

block_jid_message(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% Alice should receive message
        escalus_client:send(Bob,
            escalus_stanza:chat_to(Alice, <<"Hi! What's your name?">>)),
        escalus_assert:is_chat_message(<<"Hi! What's your name?">>,
            escalus_client:wait_for_stanza(Alice)),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Alice, {<<"deny_client_message">>, Bob}),

        %% Alice should NOT receive message, while Bob gets error message
        privacy_helper:send_and_check_blocked_message(Bob, Alice),

        %% Blocking only applies to incoming messages, so Alice can still send
        %% Bob a message
        %% and Bob gets nothing
        TS = instrument_helper:timestamp(),
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi, Bobbb!">>)),
        escalus_assert:is_chat_message(<<"Hi, Bobbb!">>, escalus_client:wait_for_stanza(Bob)),
        assert_privacy_check_packet_event(Alice, #{dir => out}, TS),
        assert_privacy_check_packet_event(Bob, #{dir => in}, TS)

        end).

block_group_message(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% Alice should receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        escalus_assert:is_chat_message(<<"Hi!">>, escalus_client:wait_for_stanza(Alice)),

        %% add Bob to Alices' group 'ignored'
        add_sample_contact(Alice, Bob, [<<"ignored">>], <<"Ugly Bastard">>),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Alice, <<"deny_group_message">>),

        %% Alice should NOT receive message
        privacy_helper:send_and_check_blocked_message(Bob, Alice)

        end).

block_subscription_message(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% Alice should receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        escalus_assert:is_chat_message(<<"Hi!">>,
            escalus_client:wait_for_stanza(Alice)),

        %% Alice sends unsubscribe
        Stanza = escalus_stanza:presence_direct(escalus_utils:get_short_jid(Bob),
                                                <<"unsubscribe">>),
        escalus_client:send(Alice, Stanza),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Alice, <<"deny_unsubscribed_message">>),

        %% Alice should NOT receive message
        privacy_helper:send_and_check_blocked_message(Bob, Alice)

        end).

allow_subscription_to_from_message(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %% deny all message but not from subscribed "to"
        privacy_helper:set_and_activate(Alice, <<"deny_all_message_but_subscription_to">>),

        %% deny all message but not from subscribed "from"
        privacy_helper:set_and_activate(Bob, <<"deny_all_message_but_subscription_from">>),

        %% Bob and Alice cannot sent to each other now
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi, Alice XYZ!">>)),
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi, Bob XYZ!">>)),

        ct:sleep(?SLEEP_TIME),
        %% they received just rejection msgs
        privacy_helper:gets_error(Alice, <<"service-unavailable">>),
        escalus_assert:has_no_stanzas(Alice),
        privacy_helper:gets_error(Bob, <<"service-unavailable">>),
        escalus_assert:has_no_stanzas(Bob),

        %% Alice subscribes to Bob
        subscribe_from_to(Alice, Bob, false),

        %% Now Alice is subscribed "to" Bob
        %% And Bob is subscribed "from" Alice

        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi, Alice XYZ!">>)),
        escalus_assert:is_chat_message(<<"Hi, Alice XYZ!">>,
                                       escalus_client:wait_for_stanza(Alice)),

        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi, Bob XYZ!">>)),
        escalus_assert:is_chat_message(<<"Hi, Bob XYZ!">>,
                                       escalus_client:wait_for_stanza(Bob))

    end).

allow_subscription_both_message(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% Alice subscribes to Bob
        subscribe_from_to(Alice, Bob, false),

        %% deny all message but not from subscribed "to"
        privacy_helper:set_and_activate(Alice, <<"deny_all_message_but_subscription_both">>),

        %% deny all message but not from subscribed "from"
        privacy_helper:set_and_activate(Bob, <<"deny_all_message_but_subscription_both">>),

        %% Bob and Alice cannot write to each other now
        %% Even though they are in subscription "to" and "from" respectively
        escalus_client:send(Bob, escalus_stanza:chat_to(
                                   Alice, <<"Hi, Alice XYZ!">>)),
        escalus_client:send(Alice, escalus_stanza:chat_to(
                                     Bob, <<"Hi, Bob XYZ 1!">>)),

        privacy_helper:gets_error(Alice, <<"service-unavailable">>),
        privacy_helper:gets_error(Bob, <<"service-unavailable">>),
        escalus_assert:has_no_stanzas(Alice),
        escalus_assert:has_no_stanzas(Bob),

        %% Bob subscribes to Alice
        subscribe_from_to(Bob, Alice, true),

        %% Now their subscription is in state "both"
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi, Alice XYZ!">>)),
        escalus_assert:is_chat_message(<<"Hi, Alice XYZ!">>,
                                       escalus_client:wait_for_stanza(Alice)),

        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi, Bob XYZ! 2">>)),
        escalus_assert:is_chat_message(<<"Hi, Bob XYZ! 2">>,
                                       escalus_client:wait_for_stanza(Bob))

    end).

block_all_message(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% Alice should receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        escalus_assert:is_chat_message(<<"Hi!">>,
                                       escalus_client:wait_for_stanza(Alice)),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Alice, <<"deny_all_message">>),

        %% Alice should NOT receive message
        privacy_helper:send_and_check_blocked_message(Bob, Alice)

        end).

block_jid_presence_in(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% Alice should receive presence in
        Presence1 =  escalus_stanza:presence_direct(escalus_utils:get_short_jid(Alice),
                                                    <<"available">>),
        escalus_client:send(Bob, Presence1),
        Received = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_presence, Received),
        escalus_assert:is_stanza_from(Bob, Received),

        privacy_helper:set_and_activate(Alice, {<<"deny_client_presence_in">>, Bob}),

        %% Alice should NOT receive presence in
        Presence2 = escalus_stanza:presence_direct(escalus_utils:get_short_jid(Alice),
                                                   <<"available">>),
        escalus_client:send(Bob, Presence2),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Alice),
        %% and Bob should NOT receive any response
        escalus_assert:has_no_stanzas(Bob)


        end).

block_jid_presence_out(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        BobBareJID = escalus_utils:get_short_jid(Bob),

        %% Bob should receive presence in
        Presence1 = escalus_stanza:presence_direct(BobBareJID, <<"available">>),
        escalus_client:send(Alice, Presence1),

        Received = escalus_client:wait_for_stanza(Bob),
        escalus:assert(is_presence, Received),
        escalus_assert:is_stanza_from(Alice, Received),

        privacy_helper:set_and_activate(Alice, {<<"deny_client_presence_out">>, Bob}),

        %% Bob should NOT receive presence in
        Presence2 = escalus_stanza:presence_direct(BobBareJID, <<"available">>),
        escalus_client:send(Alice, Presence2),

        %% Alice gets an error back from mod_privacy
        ErrorPresence = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_presence_with_type, [<<"error">>], ErrorPresence),

        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Bob)

        end).

version_iq(Type, From, To) ->
    Req = escalus_stanza:iq(Type, [escalus_stanza:query_el(<<"jabber:iq:version">>, [])]),
    Req1 = escalus_stanza:to(Req, To),
    Req2 = escalus_stanza:from(Req1, From),
    Req2.

iq_reply_doesnt_crash_user_process(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        QueryWithPrivacyNS = escalus_stanza:query_el(?NS_PRIVACY, []),
        %% Send IQ reply with privacy ns
        %% Send message to check user process still alive
        privacy_helper:does_user_process_crash(Alice,
            Bob,
            <<"error">>,
            QueryWithPrivacyNS,
            <<"Hello, Bob">>),

        privacy_helper:does_user_process_crash(Bob,
            Alice,
            <<"result">>,
            QueryWithPrivacyNS,
            <<"Hello, Alice">>)
    end).

%% This test checks an edge case where a privacy IQ is sent to another user
%% This isn't allowed by the XEP, but the test ensures MIM handles it correctly
iq_with_to_attribute_is_treated_as_regular_one(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, Kate) ->
            %% Alice sends a privacy IQ addressed to Bob
            St = escalus_stanza:privacy_set_list(
                privacy_helper:privacy_list({<<"deny_jid_all">>, Kate})),
            StanzaPriv = escalus_stanza:to(St, Bob),
            escalus_client:send(Alice, StanzaPriv),
            %% Bob should receive the privacy IQ sent by Alice
            StanzaReceived = escalus:wait_for_stanza(Bob),
            escalus:assert(is_iq_set, StanzaReceived),
            %% Alice shouldn't receive any response from the server
            [] = escalus:wait_for_stanzas(Alice, 1, 100),
            escalus_assert:has_no_stanzas(Alice)
        end).

block_jid_iq(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        LServer = escalus_utils:get_server(Bob),
        privacy_helper:set_list(Alice, {<<"deny_server_iq">>, LServer}),
        %% activate it (this is actually redundant, but asserts iq result always comes through)
        Stanza = escalus_stanza:privacy_activate(<<"deny_server_iq">>),
        escalus_client:send(Alice, Stanza),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)), % we should get a result

        %% bob queries for version and gets an error, Alice doesn't receive the query
        escalus_client:send(Bob, version_iq(<<"get">>, Bob, Alice)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Alice),
        privacy_helper:gets_error(Bob, <<"service-unavailable">>),
        %% this stanza does not make much sense, but is routed and rejected correctly
        escalus_client:send(Bob, version_iq(<<"set">>, Bob, Alice)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Alice),
        privacy_helper:gets_error(Bob, <<"service-unavailable">>),
        %% but another type, like result, is silently dropped
        escalus_client:send(Bob, version_iq(<<"result">>, Bob, Alice)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Alice),
        escalus_assert:has_no_stanzas(Bob),

        %% assert iqs upon my request are received
        RStanza = escalus_stanza:roster_add_contact(Bob, [], <<"a random guy">>),
        escalus:send(Alice, RStanza),
        Received = escalus:wait_for_stanzas(Alice, 2),
        escalus:assert_many([is_roster_set, is_iq_result], Received),

        %% assert direct iq from server is received
        ServerStanza = version_iq(<<"get">>, <<"localhost">>, escalus_utils:get_jid(Alice)),
        rpc(mim(), ejabberd_router, route, [jid:from_binary(<<"localhost">>),
                                            jid:from_binary(escalus_utils:get_jid(Alice)),
                                            ServerStanza]),
        escalus:assert(is_iq_get, escalus:wait_for_stanza(Alice)) ,

        ok
        end).

block_jid_all(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        privacy_helper:set_list(Alice, {<<"deny_jid_all">>, Bob}),

        %% Alice blocks Bob
        Stanza = escalus_stanza:privacy_activate(<<"deny_jid_all">>),
        escalus_client:send(Alice, Stanza),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)), % we should get a result

        %% IQ response is blocked;
        %% do magic wait for the request to take effect
        timer:sleep(200),

        %% From now on nothing whatsoever sent by Bob should reach Alice.

        %% Alice should NOT receive message, Bob receives err msg
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        privacy_helper:gets_error(Bob, <<"service-unavailable">>),

        %% Alice should NOT receive presence-in from Bob, no err msg
        AliceBareJID = escalus_utils:get_short_jid(Alice),
        Presence1 = escalus_stanza:presence_direct(AliceBareJID, <<"available">>),
        escalus_client:send(Bob, Presence1),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Bob),

        %% Bob should NOT receive presence-in from Alice, Alice receives err msg
        BobBareJID = escalus_utils:get_short_jid(Bob),
        Presence2 = escalus_stanza:presence_direct(BobBareJID, <<"available">>),
        escalus_client:send(Alice, Presence2),
        timer:sleep(?SLEEP_TIME),
        privacy_helper:gets_error(Alice, <<"not-acceptable">>),

        %% Just set the toy list and en~sure that only
        %% the notification push comes back.
        privacy_helper:send_set_list(Alice, {<<"deny_client">>, Bob}),

        %% verify
        timer:sleep(?SLEEP_TIME),
        %% ...that nothing else reached Bob
        escalus_assert:has_no_stanzas(Bob),
        %% ...that Alice got a privacy push
        Responses = escalus_client:wait_for_stanzas(Alice, 2),
        escalus:assert_many([fun privacy_helper:is_privacy_list_push/1, is_iq_result], Responses),
        %% and Alice didn't get anything else
        escalus_assert:has_no_stanzas(Alice)

        end).

block_jid_message_but_not_presence(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% Alice should receive message
        Msg =  escalus_stanza:chat_to(Alice, <<"Hi! What's your name?">>),
        escalus_client:send(Bob, Msg),
        escalus_assert:is_chat_message(<<"Hi! What's your name?">>,
                                       escalus_client:wait_for_stanza(Alice)),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Alice, {<<"deny_client_message">>, Bob}),

        %% Alice should NOT receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi, Alice!">>)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Alice),
        privacy_helper:gets_error(Bob, <<"service-unavailable">>),

        %% ...but should receive presence in
        escalus_client:send(Bob,
                            escalus_stanza:presence_direct(Alice, <<"available">>)),
        Received = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_presence, Received),
        escalus_assert:is_stanza_from(Bob, Received)

        end).

newly_blocked_presense_jid_by_new_list(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        add_sample_contact(Alice, Bob, [<<"My Friends">>], <<"Bobbie">>),
        subscribe(Bob, Alice),

        %% Alice gets notification that a roster contact is now subscribed
        escalus:assert(is_roster_set, escalus_client:wait_for_stanza(Alice)),

        %% Bob should receive presence in
        Presence = escalus_stanza:presence_direct(escalus_client:short_jid(Bob),
                                                  <<"available">>),
        escalus_client:send(Alice, Presence),
        Received = escalus_client:wait_for_stanza(Bob),
        escalus:assert(is_presence, Received),
        escalus_assert:is_stanza_from(Alice, Received),

        privacy_helper:set_list(
          Alice, <<"deny_bob_presence_out">>,
          [escalus_stanza:privacy_list_jid_item(<<"1">>, <<"deny">>,
                                                escalus_client:short_jid(Bob),
                                                [<<"presence-out">>])]),
        privacy_helper:activate_list(Alice, <<"deny_bob_presence_out">>),

        %% Bob should receive unavailable, per XEP-0016 2.11
        Received2 = escalus_client:wait_for_stanza(Bob),
        escalus:assert(is_presence_with_type, [<<"unavailable">>], Received2),
        escalus_assert:is_stanza_from(Alice, Received2)

        end).

newly_blocked_presense_jid_by_list_change(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        add_sample_contact(Alice, Bob, [<<"My Friends">>], <<"Bobbie">>),
        subscribe(Bob, Alice),

        %% Alice gets notification that a roster contact is now subscribed
        escalus:assert(is_roster_set, escalus_client:wait_for_stanza(Alice)),

        %% Alice sets up an initially empty privacy list
        privacy_helper:set_and_activate(Alice, <<"noop_list">>),

        %% Bob should receive presence in
        escalus_client:send(Alice,
            escalus_stanza:presence_direct(Bob, <<"available">>)),
        Received = escalus_client:wait_for_stanza(Bob),
        escalus:assert(is_presence, Received),
        escalus_assert:is_stanza_from(Alice, Received),

        %% Alice now adds Bob to her currently active privacy list
        privacy_helper:set_list(
          Alice, <<"noop_list">>,
          [escalus_stanza:privacy_list_jid_item(<<"1">>, <<"deny">>,
                                                escalus_client:short_jid(Bob),
                                                [<<"presence-out">>])]),

        %% Bob should receive unavailable, per XEP-0016 2.11
        Received2 = escalus_client:wait_for_stanza(Bob),
        escalus:assert(is_presence_with_type, [<<"unavailable">>], Received2),
        escalus_assert:is_stanza_from(Alice, Received2)

        end).

newly_blocked_presence_not_notify_self(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        %% Alice sets up an initially empty privacy list
        privacy_helper:set_and_activate(Alice,
                                        <<"deny_not_both_presence_out">>),

        %% Alice should not receive an 'unavailable' because she's not a
        %% contact of herself.
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Alice)

        end).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

add_sample_contact(Who, Whom, Groups, Nick) ->
    escalus_client:send(Who,
                        escalus_stanza:roster_add_contact(Whom,
                                                          Groups,
                                                          Nick)),
    Received = escalus_client:wait_for_stanza(Who),
    escalus_assert:is_roster_set(Received),
    escalus_client:send(Who, escalus_stanza:iq_result(Received)),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(Who)).

subscribe(Who, Whom) ->
    % 'Who' sends a subscribe request to 'Whom'
    escalus:send(Who, escalus_stanza:presence_direct(
                        escalus_client:short_jid(Whom), <<"subscribe">>)),
    PushReq = escalus:wait_for_stanza(Who),
    escalus:assert(is_roster_set, PushReq),
    escalus:send(Who, escalus_stanza:iq_result(PushReq)),

    %% 'Whom' receives subscription request
    Received = escalus:wait_for_stanza(Whom),
    escalus:assert(is_presence_with_type, [<<"subscribe">>], Received),

    %% 'Whom' adds new contact to their roster
    escalus:send(Whom, escalus_stanza:roster_add_contact(Who,
                                                        [<<"enemies">>],
                                                        <<"Enemy1">>)),
    PushReq2 = escalus:wait_for_stanza(Whom),
    escalus:assert(is_roster_set, PushReq2),
    escalus:send(Whom, escalus_stanza:iq_result(PushReq2)),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(Whom)),

    %% 'Whom' sends subscribed presence
    escalus:send(Whom, escalus_stanza:presence_direct(
                         escalus_client:short_jid(Who), <<"subscribed">>)),

    %% 'Who' receives subscribed
    Stanzas = escalus:wait_for_stanzas(Who, 2),

    check_subscription_stanzas(Stanzas, <<"subscribed">>),
    escalus:assert(is_presence, escalus:wait_for_stanza(Who)).

check_subscription_stanzas(Stanzas, Type) ->
    IsPresWithType = fun(S) ->
                         escalus_pred:is_presence_with_type(Type, S)
                     end,
    escalus:assert_many([is_roster_set, IsPresWithType], Stanzas).

subscribe_from_to(From, To, IsSecondSubscription) ->
    %% From subscribes to To
    ToBareJid = escalus_utils:get_short_jid(To),
    SubStanza = escalus_stanza:presence_direct(ToBareJid, <<"subscribe">>),
    escalus_client:send(From, SubStanza),
    PushReq = escalus_client:wait_for_stanza(From),
    escalus:assert(is_roster_set, PushReq),
    Received = escalus_client:wait_for_stanza(To),
    %% To accepts From
    FromBareJid = escalus_utils:get_short_jid(From),
    SubConfirmStanza = escalus_stanza:presence_direct(FromBareJid, <<"subscribed">>),
    escalus_client:send(To, SubConfirmStanza),
    case IsSecondSubscription of
        true ->
            escalus:assert(is_roster_set, Received),
            escalus_client:wait_for_stanzas(To, 2);
        false ->
            escalus:assert(is_presence_with_type, [<<"subscribe">>], Received),
            escalus_client:wait_for_stanza(To)
    end,
    escalus_client:wait_for_stanzas(From, 3).
