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
-compile(export_all).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-define(SLEEP_TIME, 50).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, management},
     {group, blocking},
     {group, allowing}
    ].

groups() ->
    [{management, [sequence], management_test_cases()},
     {blocking, [sequence], blocking_test_cases()},
        {my, [sequence], mytest()},
     {allowing, [sequence], allowing_test_cases()}
    ].
mytest() ->
    [block_jid_message_but_not_presence].
management_test_cases() ->
    [get_all_lists,
     get_existing_list,
     get_many_lists,
     get_nonexistent_list,
     set_list,
     activate,
     activate_nonexistent,
     deactivate,
     default,
     %default_conflict,  % fails, as of bug #7073
     default_nonexistent,
     no_default,
     remove_list,
     get_all_lists_with_active,
     get_all_lists_with_default
    ].

blocking_test_cases() ->
    [block_jid_message,
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
     newly_blocked_presence_not_notify_self
    ].

allowing_test_cases() ->
    [allow_subscription_to_from_message,
     allow_subscription_both_message].


suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    [{escalus_no_stanzas_after_story, true} |
     escalus:init_per_suite(Config)].

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, bob])).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob])).

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


%% TODO:
%% x get all privacy lists
%% x get single privacy list
%%   x that exists
%%   x that doesn't exist (ensure server returns item-not-found)
%%   x request more than one at a time (ensure server returns bad-request)
%% x set new/edit privacy list (ensure server pushes notifications
%%   to all resources)
%% - remove existing list
%%   x remove existing list (ensure server push)
%%   - remove, but check conflict case
%% x manage active list(s)
%%   x activate
%%   x activate nonexistent (ensure item-not-found)
%%   x deactivate by sending empty <active />
%% - manage default list
%%   x set default
%%   - set default, but check the conflict case, i.e.:
%%     "Client attempts to change the default list but that list is in use
%%     by another resource",
%%     !!! ejabberd doesn't support this, bug filed (#7073)
%%   x set nonexistent default list
%%   x use domain's routing, i.e. no default list -> send empty <default />
%%   - set no default list, but check conflict case,
%%     when a resource currently uses the default list
%%
%% TODO later:
%% - big picture:
%%   - blocking can be done on jids, roster groups,
%%     subscription type or globally
%%   - a blocking rule may block one or more of {message, presence-in,
%%     presence-out, iqs} by specifying these as children to the list item
%%     or block all of them, when the item has no children
%% - blocking: messages, presence (in/out), iqs, all

get_all_lists(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        escalus:send(Alice, escalus_stanza:privacy_get_all()),
        escalus:assert(is_privacy_result, escalus:wait_for_stanza(Alice))

        end).

get_all_lists_with_active(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, _Bob) ->

        privacy_helper:set_and_activate(Alice, <<"deny_bob">>),

        escalus:send(Alice, escalus_stanza:privacy_get_all()),
        escalus:assert(is_privacy_result_with_active, [<<"deny_bob">>],
                       escalus:wait_for_stanza(Alice))

        end).

get_all_lists_with_default(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, _Bob) ->

        privacy_helper:set_list(Alice, <<"deny_bob">>),
        privacy_helper:set_list(Alice, <<"allow_bob">>),
        privacy_helper:set_default_list(Alice, <<"allow_bob">>),

        escalus:send(Alice, escalus_stanza:privacy_get_all()),
        escalus:assert(is_privacy_result_with_default,
                       escalus:wait_for_stanza(Alice))

        end).

get_nonexistent_list(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        escalus_client:send(Alice,
            escalus_stanza:privacy_get_lists([<<"public">>])),
        escalus_assert:is_privacy_list_nonexistent_error(
            escalus_client:wait_for_stanza(Alice))

        end).

get_many_lists(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        Request = escalus_stanza:privacy_get_lists([<<"public">>, <<"private">>]),
        escalus_client:send(Alice, Request),
        Response = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_error(Response, <<"modify">>, <<"bad-request">>)

        end).

get_existing_list(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, _Bob) ->

        privacy_helper:set_list(Alice, <<"deny_bob">>),

        escalus:send(Alice, escalus_stanza:privacy_get_lists([<<"deny_bob">>])),
        Response = escalus:wait_for_stanza(Alice),

        <<"deny_bob">> = exml_query:path(Response, [{element, <<"query">>},
                                                    {element, <<"list">>},
                                                    {attr, <<"name">>}])

        end).

activate(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, _Bob) ->

        privacy_helper:set_list(Alice, <<"deny_bob">>),

        Request = escalus_stanza:privacy_activate(<<"deny_bob">>),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Response)

        end).

activate_nonexistent(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        Request = escalus_stanza:privacy_activate(<<"some_list">>),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_error, [<<"cancel">>, <<"item-not-found">>], Response)

        end).

deactivate(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        Request = escalus_stanza:privacy_deactivate(),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Response)

        end).

default(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, _Bob) ->

        privacy_helper:set_list(Alice, <<"deny_bob">>),

        Request = escalus_stanza:privacy_set_default(<<"deny_bob">>),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Response)

        end).

default_conflict(Config) ->
    escalus:story(Config, [{alice, 2}, {bob, 1}], fun(Alice, Alice2, _Bob) ->

        %% testcase setup
        %% setup list on server
        privacy_helper:send_set_list(Alice, <<"deny_bob">>),
        privacy_helper:send_set_list(Alice, <<"allow_bob">>),
        %% skip responses
        escalus_client:wait_for_stanzas(Alice, 4),
        %% make a default list for Alice2
        R1 = escalus_stanza:privacy_set_default(Alice2, <<"deny_bob">>),
        escalus_client:send(Alice2, R1),
        escalus:assert_many([is_privacy_set, is_privacy_set, is_iq_result],
                            escalus_client:wait_for_stanzas(Alice2, 3)),
        %% setup done

        Request = escalus_stanza:privacy_set_default(<<"allow_bob">>),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        %% TODO: should fail on this (result) and receive error
        %%       this is a bug and was filed to the esl redmine as Bug #7073
        %true = exmpp_iq:is_result(Response),
        %% but this should pass just fine
        escalus:assert(is_error, [<<"cancel">>, <<"conflict">>], Response)

        end).

default_nonexistent(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        Request = escalus_stanza:privacy_set_default(<<"some_list">>),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_error(Response, <<"cancel">>, <<"item-not-found">>)

        end).

no_default(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        Request = escalus_stanza:privacy_no_default(),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Response)

        end).

set_list(Config) ->
    escalus:story(Config, [{alice, 3}, {bob, 1}], fun(Alice, Alice2, Alice3, _Bob) ->

        privacy_helper:send_set_list(Alice, <<"deny_bob">>),

        %% Verify that original Alice gets iq result and notification.
        %% It's a bit quirky as these come in undefined order
        %% (actually, they always came with 'push' first and then 'result',
        %% but I suppose it's not mandatory).
        AliceResponses = escalus_client:wait_for_stanzas(Alice, 2),
        escalus:assert_many([
            fun escalus_pred:is_iq_result/1,
            fun privacy_helper:is_privacy_list_push/1
        ], AliceResponses),

        %% Verify that other resources also get the push.
        escalus:assert(fun privacy_helper:is_privacy_list_push/1,
                       escalus:wait_for_stanza(Alice2)),
        escalus:assert(fun privacy_helper:is_privacy_list_push/1,
                       escalus:wait_for_stanza(Alice3))

        %% All in all, the spec requires the resources to reply
        %% (as to every iq), but it's omitted here.

        end).

remove_list(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, _Bob) ->

        privacy_helper:send_set_list(Alice, <<"deny_bob">>),

        %% These are the pushed notification and iq result.
        escalus_client:wait_for_stanzas(Alice, 2),

        %% Request list deletion by sending an empty list.
        RemoveRequest = escalus_stanza:privacy_set_list(
                escalus_stanza:privacy_list(<<"someList">>, [])),
        escalus_client:send(Alice, RemoveRequest),

        %% These too are the pushed notification and iq result.
        escalus:assert_many([
            fun privacy_helper:is_privacy_list_push/1,
            is_iq_result
        ], escalus_client:wait_for_stanzas(Alice, 2)),

        escalus_client:send(Alice,
            escalus_stanza:privacy_get_lists([<<"someList">>])),

        %% Finally ensure that the list doesn't exist anymore.
        escalus_assert:is_privacy_list_nonexistent_error(
            escalus_client:wait_for_stanza(Alice))

        end).

block_jid_message(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% Alice should receive message
        escalus_client:send(Bob,
            escalus_stanza:chat_to(Alice, <<"Hi! What's your name?">>)),
        escalus_assert:is_chat_message(<<"Hi! What's your name?">>,
            escalus_client:wait_for_stanza(Alice)),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Alice, <<"deny_bob_message">>),

        %% Alice should NOT receive message, while Bob gets error message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi, Alice!">>)),
        privacy_helper:gets_error(Bob, <<"service-unavailable">>),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Alice),

        %% now Alice try to send a msg to Bob, whom she had blocked, and gets error
        %% and Bob gets nothing
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi, Bobbb!">>)),
        privacy_helper:gets_error(Alice, <<"not-acceptable">>),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Bob)

        end).

block_group_message(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% Alice should receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        escalus_assert:is_chat_message(<<"Hi!">>,
            escalus_client:wait_for_stanza(Alice)),

        %% add Bob to Alices' group 'ignored'
        add_sample_contact(Alice, Bob, [<<"ignored">>], <<"Ugly Bastard">>),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Alice, <<"deny_group_message">>),

        %% Alice should NOT receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi, blocked group!">>)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Alice),
        privacy_helper:gets_error(Bob, <<"service-unavailable">>)

        end).

block_subscription_message(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% Alice should receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        escalus_assert:is_chat_message(<<"Hi!">>,
            escalus_client:wait_for_stanza(Alice)),

        %% Alice sends unsubscribe
        escalus_client:send(Alice,
            escalus_stanza:presence_direct(bob, <<"unsubscribe">>)),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Alice, <<"deny_unsubscribed_message">>),

        %% Alice should NOT receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Alice),
        privacy_helper:gets_error(Bob, <<"service-unavailable">>)

        end).

allow_subscription_to_from_message(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %% deny all message but not from subscribed "to"
        privacy_helper:set_and_activate(Alice, <<"deny_all_message_but_subscription_to">>),

        %% deny all message but not from subscribed "from"
        privacy_helper:set_and_activate(Bob, <<"deny_all_message_but_subscription_from">>),

        %% Bob and Alice cannot sent to each other now
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi, Alice XYZ!">>)),
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi, Bob XYZ!">>)),

        ct:sleep(?SLEEP_TIME),
        %% they received just rejection msgs
        privacy_helper:gets_error(Alice, <<"not-acceptable">>),
        escalus_assert:has_no_stanzas(Alice),
        privacy_helper:gets_error(Bob, <<"not-acceptable">>),
        escalus_assert:has_no_stanzas(Bob),

        %% Alice subscribes to Bob
        escalus_client:send(Alice,
                            escalus_stanza:presence_direct(bob, <<"subscribe">>)),
        escalus_client:wait_for_stanza(Alice),
        escalus_client:wait_for_stanza(Bob),

        %% Bob accepts Alice
        escalus_client:send(Bob, escalus_stanza:presence_direct(alice, <<"subscribed">>)),
        escalus_client:wait_for_stanza(Bob),
        escalus_client:wait_for_stanzas(Alice, 3),

        %% Now Alice is subscirbed "to" Bob
        %% And Bob is subscribed "from" Alice

        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi, Alice XYZ!">>)),
        escalus_assert:is_chat_message(<<"Hi, Alice XYZ!">>,
            escalus_client:wait_for_stanza(Alice)),

        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi, Bob XYZ!">>)),
        escalus_assert:is_chat_message(<<"Hi, Bob XYZ!">>,
            escalus_client:wait_for_stanza(Bob))

    end).


allow_subscription_both_message(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        [{_, Spec}] = escalus_users:get_users([bob]),
        {ok, Bob, _Spec2, _Features} = escalus_connection:start(Spec),
        %escalus_story:send_initial_presence(Alice),
        escalus_story:send_initial_presence(Bob),
        escalus_client:wait_for_stanza(Alice),
        escalus_client:wait_for_stanza(Bob),
        %% deny all message but not from subscribed "to"
        privacy_helper:set_and_activate(Alice, <<"deny_all_message_but_subscription_both">>),

        %% deny all message but not from subscribed "from"
        privacy_helper:set_and_activate(Bob, <<"deny_all_message_but_subscription_both">>),

        %% Bob and Alice cannot sent to each other now
        %% Even though they are in subscription "to" and "from" respectively
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi, Alice XYZ!">>)),
        escalus_client:send(Alice, escalus_stanza:chat_to(bob, <<"Hi, Bob XYZ!">>)),

        ct:sleep(?SLEEP_TIME),
        privacy_helper:gets_error(Alice, <<"not-acceptable">>),
        privacy_helper:gets_error(Bob, <<"not-acceptable">>),
        escalus_assert:has_no_stanzas(Alice),
        escalus_assert:has_no_stanzas(Bob),

        %% Alice subscribes to Bob
        escalus_client:send(Bob,
                            escalus_stanza:presence_direct(alice, <<"subscribe">>)),
        escalus_client:wait_for_stanza(Alice),
        escalus_client:wait_for_stanza(Bob),

        %% Bob accepts Alice
        escalus_client:send(Alice, escalus_stanza:presence_direct(bob, <<"subscribed">>)),
        escalus_client:wait_for_stanzas(Alice, 2),
        escalus_client:wait_for_stanzas(Bob, 3),

        %% Now their subscription is in state "both"
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi, Alice XYZ!">>)),
        escalus_assert:is_chat_message(<<"Hi, Alice XYZ!">>,
            escalus_client:wait_for_stanza(Alice)),

        escalus_client:send(Alice, escalus_stanza:chat_to(bob, <<"Hi, Bob XYZ!">>)),
        escalus_assert:is_chat_message(<<"Hi, Bob XYZ!">>,
            escalus_client:wait_for_stanza(Bob))

    end).

block_all_message(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% Alice should receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        escalus_assert:is_chat_message(<<"Hi!">>,
            escalus_client:wait_for_stanza(Alice)),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Alice, <<"deny_all_message">>),

        %% Alice should NOT receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Alice),
        privacy_helper:gets_error(Bob, <<"service-unavailable">>)

        end).

block_jid_presence_in(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% Alice should receive presence in
        escalus_client:send(Bob,
            escalus_stanza:presence_direct(alice, <<"available">>)),
        Received = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_presence, Received),
        escalus_assert:is_stanza_from(Bob, Received),

        privacy_helper:set_and_activate(Alice, <<"deny_bob_presence_in">>),

        %% Alice should NOT receive presence in
        escalus_client:send(Bob,
            escalus_stanza:presence_direct(alice, <<"available">>)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Alice),
        %% and Bob should NOT receive any response
        escalus_assert:has_no_stanzas(Bob)


        end).

block_jid_presence_out(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% Bob should receive presence in
        escalus_client:send(Alice,
            escalus_stanza:presence_direct(bob, <<"available">>)),
        Received = escalus_client:wait_for_stanza(Bob),
        escalus:assert(is_presence, Received),
        escalus_assert:is_stanza_from(Alice, Received),

        privacy_helper:set_and_activate(Alice, <<"deny_bob_presence_out">>),

        %% Bob should NOT receive presence in
        escalus_client:send(Alice,
            escalus_stanza:presence_direct(bob, <<"available">>)),

        %% Alice gets an error back from mod_privacy
        Presence = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_presence_with_type, [<<"error">>], Presence),

        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Bob)

        end).

version_iq(Type, From, To) ->
    Req = escalus_stanza:iq(Type, [escalus_stanza:query_el(<<"jabber:iq:version">>, [])]),
    Req1 = escalus_stanza:to(Req, To),
    Req2= escalus_stanza:from(Req1, From),
    Req2.

block_jid_iq(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        privacy_helper:set_list(Alice, <<"deny_localhost_iq">>),
        %% activate it
        escalus_client:send(Alice,
            escalus_stanza:privacy_activate(<<"deny_localhost_iq">>)),
        timer:sleep(500), %% we must let it sink in
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
        escalus_assert:has_no_stanzas(Bob)

        end).

block_jid_all(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        privacy_helper:set_list(Alice, <<"deny_jid_all">>),

        %% Alice blocks Bob
        escalus_client:send(Alice,
            escalus_stanza:privacy_activate(<<"deny_jid_all">>)),
        %% IQ response is blocked;
        %% do magic wait for the request to take effect
        timer:sleep(200),

        %% From now on nothing whatsoever sent by Bob should reach Alice.

        %% Alice should NOT receive message, Bob receives err msg
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        privacy_helper:gets_error(Bob, <<"service-unavailable">>),

        %% Alice should NOT receive presence-in from Bob, no err msg
        escalus_client:send(Bob,
            escalus_stanza:presence_direct(alice, <<"available">>)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Bob),

        %% Bob should NOT receive presence-in from Alice, Alice receives err msg
        escalus_client:send(Alice,
            escalus_stanza:presence_direct(bob, <<"available">>)),
        timer:sleep(?SLEEP_TIME),
        privacy_helper:gets_error(Alice, <<"not-acceptable">>),

        %% Just set the toy list and ensure that only
        %% the notification push comes back.
        privacy_helper:send_set_list(Alice, <<"deny_bob">>),

        %% verify
        timer:sleep(?SLEEP_TIME),
        %% ...that nothing else reached Bob
        escalus_assert:has_no_stanzas(Bob),
        %% ...that Alice got a privacy push
        Responses = escalus_client:wait_for_stanza(Alice),
        escalus:assert(fun privacy_helper:is_privacy_list_push/1, Responses),
        %% and Alice didn't get anything else
        escalus_assert:has_no_stanzas(Alice)

        end).

block_jid_message_but_not_presence(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% Alice should receive message
        escalus_client:send(Bob,
            escalus_stanza:chat_to(Alice, <<"Hi! What's your name?">>)),
        escalus_assert:is_chat_message(<<"Hi! What's your name?">>,
            escalus_client:wait_for_stanza(Alice)),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Alice, <<"deny_bob_message">>),

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
        escalus_client:send(Alice,
            escalus_stanza:presence_direct(escalus_client:short_jid(Bob),
                                           <<"available">>)),
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

    %% 'Whom' receives subscription reqest
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
