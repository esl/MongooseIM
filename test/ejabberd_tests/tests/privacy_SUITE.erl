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
     {group, blocking}
    ].

groups() ->
    [{management, [sequence], management_test_cases()},
     {blocking, [sequence], blocking_test_cases()}
    ].
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
    get_all_lists_with_active
    %get_all_lists_with_default
    % not implemented (see testcase)
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
    block_jid_message_but_not_presence
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    [{escalus_no_stanzas_after_story, true} |
     escalus:init_per_suite(Config)].

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, {by_name, [alice, bob]}).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, {by_name, [alice, bob]}).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

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
    escalus:story(Config, [1], fun(Alice) ->

        escalus:send(Alice, escalus_stanza:privacy_get_all()),
        escalus:assert(is_privacy_result, escalus:wait_for_stanza(Alice))

        end).

get_all_lists_with_active(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, _Bob) ->

        privacy_helper:set_and_activate(Alice, <<"deny_bob">>),

        escalus:send(Alice, escalus_stanza:privacy_get_all()),
        escalus:assert(is_privacy_result_with_active, [<<"deny_bob">>],
                       escalus:wait_for_stanza(Alice))

        end).

%% Black box testing showed that this feature is not implemented,
%% i.e. the <default /> element is never returned by ejabberd.
%% However, I'm not 100% sure, as I didn't look inside the source.
get_all_lists_with_default(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, _Bob) ->

        privacy_helper:set_list(Alice, <<"deny_bob">>),
        privacy_helper:set_and_activate(Alice, <<"allow_bob">>),

        escalus:send(Alice, escalus_stanza:privacy_get_all()),
        escalus:assert(is_privacy_result_with_default,
                       escalus:wait_for_stanza(Alice))

        end).

get_nonexistent_list(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        escalus_client:send(Alice,
            escalus_stanza:privacy_get_lists([<<"public">>])),
        escalus_assert:is_privacy_list_nonexistent_error(
            escalus_client:wait_for_stanza(Alice))

        end).

get_many_lists(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        Request = escalus_stanza:privacy_get_lists([<<"public">>, <<"private">>]),
        escalus_client:send(Alice, Request),
        Response = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_error(Response, <<"modify">>, <<"bad-request">>)

        end).

get_existing_list(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, _Bob) ->

        privacy_helper:set_list(Alice, <<"deny_bob">>),

        escalus:send(Alice, escalus_stanza:privacy_get_lists([<<"deny_bob">>])),
        Response = escalus:wait_for_stanza(Alice),

        <<"deny_bob">> = exml_query:path(Response, [{element, <<"query">>},
                                                    {element, <<"list">>},
                                                    {attr, <<"name">>}])

        end).

activate(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, _Bob) ->

        privacy_helper:set_list(Alice, <<"deny_bob">>),

        Request = escalus_stanza:privacy_activate(<<"deny_bob">>),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Response)

        end).

activate_nonexistent(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        Request = escalus_stanza:privacy_activate(<<"some_list">>),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_error, [<<"cancel">>, <<"item-not-found">>], Response)

        end).

deactivate(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        Request = escalus_stanza:privacy_deactivate(),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Response)

        end).

default(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, _Bob) ->

        privacy_helper:set_list(Alice, <<"deny_bob">>),

        Request = escalus_stanza:privacy_set_default(<<"deny_bob">>),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Response)

        end).

default_conflict(Config) ->
    escalus:story(Config, [2, 1], fun(Alice, Alice2, _Bob) ->

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
    escalus:story(Config, [1], fun(Alice) ->

        Request = escalus_stanza:privacy_set_default(<<"some_list">>),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_error(Response, <<"cancel">>, <<"item-not-found">>)

        end).

no_default(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        Request = escalus_stanza:privacy_no_default(),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Response)

        end).

set_list(Config) ->
    escalus:story(Config, [3, 1], fun(Alice, Alice2, Alice3, _Bob) ->

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
    escalus:story(Config, [1, 1], fun(Alice, _Bob) ->

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
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

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
        escalus_assert:has_no_stanzas(Alice)

        end).

block_group_message(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        %% Alice should receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        escalus_assert:is_chat_message(<<"Hi!">>,
            escalus_client:wait_for_stanza(Alice)),

        %% add Bob to Alices' group 'ignored'
        add_sample_contact(Alice, Bob, [<<"ignored">>], <<"Ugly Bastard">>),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Alice, <<"deny_bob_message">>),

        %% Alice should NOT receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Alice)

        end).

block_subscription_message(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

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
        escalus_assert:has_no_stanzas(Alice)

        end).

block_all_message(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        %% Alice should receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        escalus_assert:is_chat_message(<<"Hi!">>,
            escalus_client:wait_for_stanza(Alice)),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Alice, <<"deny_all_message">>),

        %% Alice should NOT receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Alice)

        end).

block_jid_presence_in(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

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
        escalus_assert:has_no_stanzas(Alice)

        end).

block_jid_presence_out(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

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

block_jid_iq(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, _Bob) ->

        privacy_helper:set_list(Alice, <<"deny_localhost_iq">>),
        %% activate it
        escalus_client:send(Alice,
            escalus_stanza:privacy_activate(<<"deny_localhost_iq">>)),
        %% From now on no iq replies should reach Alice.
        %% That's also the reason why we couldn't use
        %% the privacy_helper:set_and_activate helper - it waits for all replies.

        %% Just set the toy list and ensure that only
        %% the notification push comes back.
        privacy_helper:send_set_list(Alice, <<"deny_bob">>),
        Response = escalus_client:wait_for_stanza(Alice),
        escalus:assert(fun privacy_helper:is_privacy_list_push/1, Response),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Alice)

        end).

block_jid_all(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        privacy_helper:set_list(Alice, <<"deny_jid_all">>),

        %% Alice blocks Bob
        escalus_client:send(Alice,
            escalus_stanza:privacy_activate(<<"deny_jid_all">>)),
        %% IQ response is blocked;
        %% do magic wait for the request to take effect
        timer:sleep(200),

        %% From now on nothing whatsoever sent by Bob should reach Alice.

        %% Alice should NOT receive message
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),

        %% Alice should NOT receive presence-in from Bob
        escalus_client:send(Bob,
            escalus_stanza:presence_direct(alice, <<"available">>)),

        %% Bob should NOT receive presence-in from Alice
        escalus_client:send(Alice,
            escalus_stanza:presence_direct(bob, <<"available">>)),

        %% Just set the toy list and ensure that only
        %% the notification push comes back.
        privacy_helper:send_set_list(Alice, <<"deny_bob">>),

        %% verify
        timer:sleep(?SLEEP_TIME),
        %% ...that nothing reached Bob
        escalus_assert:has_no_stanzas(Bob),
        %% ...that Alice got exactly two responses
        Responses = escalus_client:wait_for_stanzas(Alice, 2),
        %% one of which is a push and the other a presence error
        escalus:assert_many([
            fun privacy_helper:is_privacy_list_push/1,
            fun privacy_helper:is_presence_error/1
        ], Responses),
        %% and Alice didn't get anything else
        escalus_assert:has_no_stanzas(Alice)

        end).

block_jid_message_but_not_presence(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

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

        %% ...but should receive presence in
        escalus_client:send(Bob,
            escalus_stanza:presence_direct(Alice, <<"available">>)),
        Received = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_presence, Received),
        escalus_assert:is_stanza_from(Bob, Received)

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
