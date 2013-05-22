%%==============================================================================
%% Copyright 2013 Erlang Solutions Ltd.
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

-module(metrics_c2s_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-define(WAIT_TIME, 100).

-import(metrics_helper, [assert_counter/2,
                         assert_rest_counters/3,
                         get_rest_counter_values/1,
                         get_counter_value/1]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, single},
     {group, multiple},
     {group, drop},
     {group, errors},
     {group, count},
     {group, single_rest}].

groups() ->
    [{single, [sequence], [message_one,
                           stanza_one,
                           presence_one,
                           presence_direct_one,
                           iq_one]},
     {multiple, [sequence], [messages]},
     {drop, [sequence], [bounced
                         ]},
     {errors, [sequence], [error_total,
                           error_mesg,
                           error_iq,
                           error_presence]},
     {count, [sequence], [stanza_count]},
     {single_rest, [sequence], [message_one_rest,
                                stanza_one_rest,
                                presence_one_rest,
                                presence_direct_one_rest,
                                iq_one_rest]}].

suite() ->
    [{require, ejabberd_node} | escalus:suite()].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    metrics_helper:start_lhttpc(),
    Config1 = dynamic_modules:stop_running(mod_offline, Config),
    escalus:init_per_suite(Config1).

end_per_suite(Config) ->
    metrics_helper:stop_lhttpc(),
    dynamic_modules:start_running(Config),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------


message_one(Config) ->
    {value, MesgSent} = get_counter_value(xmppMessageSent),
    {value, MesgReceived} = get_counter_value(xmppMessageReceived),
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),
        escalus_client:wait_for_stanza(Bob),

        assert_counter(MesgSent + 1, xmppMessageSent),
        assert_counter(MesgReceived + 1, xmppMessageReceived)

        end).

stanza_one(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        {value, StanzaSent} = get_counter_value(xmppStanzaSent),
        {value, StanzaReceived} = get_counter_value(xmppStanzaReceived),

        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),
        escalus_client:wait_for_stanza(Bob),

        assert_counter(StanzaSent + 1, xmppStanzaSent),
        assert_counter(StanzaReceived + 1, xmppStanzaReceived)

        end).

presence_one(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        {value, PresenceSent} = get_counter_value(xmppPresenceSent),
        {value, PresenceReceived} = get_counter_value(xmppPresenceReceived),
        {value, StanzaSent} = get_counter_value(xmppStanzaSent),
        {value, StanzaReceived} = get_counter_value(xmppStanzaReceived),

        escalus:send(Alice, escalus_stanza:presence(<<"available">>)),
        escalus:wait_for_stanza(Alice),

        assert_counter(PresenceSent + 1, xmppPresenceSent),
        assert_counter(PresenceReceived + 1, xmppPresenceReceived),
        assert_counter(StanzaSent + 1, xmppStanzaSent),
        assert_counter(StanzaReceived + 1, xmppStanzaReceived)

        end).

presence_direct_one(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        {value, PresenceSent} = get_counter_value(xmppPresenceSent),
        {value, PresenceReceived} = get_counter_value(xmppPresenceReceived),
        {value, StanzaSent} = get_counter_value(xmppStanzaSent),
        {value, StanzaReceived} = get_counter_value(xmppStanzaReceived),

        Presence = escalus_stanza:presence_direct(bob, <<"available">>),
        escalus:send(Alice, Presence),
        escalus:wait_for_stanza(Bob),

        assert_counter(PresenceSent + 1, xmppPresenceSent),
        assert_counter(PresenceReceived + 1, xmppPresenceReceived),
        assert_counter(StanzaSent + 1, xmppStanzaSent),
        assert_counter(StanzaReceived + 1, xmppStanzaReceived)

        end).

iq_one(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        {value, IqSent} = get_counter_value(xmppIqSent),
        {value, IqReceived} = get_counter_value(xmppIqReceived),
        {value, StanzaSent} = get_counter_value(xmppStanzaSent),
        {value, StanzaReceived} = get_counter_value(xmppStanzaReceived),

        escalus_client:send(Alice,
                            escalus_stanza:roster_get()),
        escalus_client:wait_for_stanza(Alice),

        assert_counter(IqSent + 1, xmppIqSent),
        assert_counter(StanzaSent + 1, xmppStanzaSent),
        assert_counter(StanzaReceived + 1, xmppStanzaReceived),
        assert_counter(IqReceived + 1, xmppIqReceived)

        end).

messages(Config) ->
    {value, MesgSent} = get_counter_value(xmppMessageSent),
    {value, MesgReceived} = get_counter_value(xmppMessageReceived),
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),
        escalus_client:wait_for_stanza(Bob),
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        escalus_client:wait_for_stanza(Alice),
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),
        escalus_client:wait_for_stanza(Bob),
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        escalus_client:wait_for_stanza(Alice),

        assert_counter(MesgSent + 4, xmppMessageSent),
        assert_counter(MesgReceived + 4, xmppMessageReceived)

        end).

bounced(Config) ->
    {value, MesgBounced} = get_counter_value(xmppMessageBounced),
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        escalus_client:stop(Bob),
        timer:sleep(?WAIT_TIME),

        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),
        timer:sleep(?WAIT_TIME),

        assert_counter(MesgBounced + 1, xmppMessageBounced)

        end).

stanza_count(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        {value, OldStanzaCount} = get_counter_value(xmppStanzaCount),

        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),
        escalus_client:wait_for_stanza(Bob),
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        escalus_client:wait_for_stanza(Alice),
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),
        escalus_client:wait_for_stanza(Bob),
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        escalus_client:wait_for_stanza(Alice),

        {value, StanzaCount} = get_counter_value(xmppStanzaCount),
        true = StanzaCount >= OldStanzaCount + 4

        end).


%%-----------------------------------------------------
%% Error tests
%%-----------------------------------------------------

error_total(Config) ->
    {value, Errors} = get_counter_value(xmppErrorTotal),
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        escalus_client:stop(Bob),
        timer:sleep(?WAIT_TIME),

        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),
        timer:sleep(?WAIT_TIME),

        assert_counter(Errors + 1, xmppErrorTotal)

        end).

error_mesg(Config) ->
    {value, Errors} = get_counter_value(xmppErrorMessage),
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        escalus_client:stop(Bob),
        timer:sleep(?WAIT_TIME),

        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),
        timer:sleep(?WAIT_TIME),

        assert_counter(Errors + 1, xmppErrorMessage)

        end).

error_presence(Config) ->
    {value, Errors} = get_counter_value(xmppErrorPresence),
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        escalus:send(Alice, escalus_stanza:presence_direct(bob, <<"available">>)),
        escalus:wait_for_stanza(Bob),

        ErrorElt = escalus_stanza:error_element(<<"cancel">>, <<"gone">>),
        Presence = escalus_stanza:presence_direct(alice, <<"error">>, ErrorElt),
        escalus:send(Bob, Presence),
        escalus:wait_for_stanza(Alice),

        assert_counter(Errors + 1, xmppErrorPresence)

        end).

error_iq(Config) ->
    {value, Errors} = get_counter_value(xmppErrorIq),

    Alice = escalus_users:get_user_by_name(alice),
    escalus_users:create_user(Config, Alice),

    timer:sleep(?WAIT_TIME),

    assert_counter(Errors + 1, xmppErrorIq).

message_one_rest(Config) ->
    {value, MesgSentH, MesgSentT} = get_rest_counter_values(xmppMessageSent),
    {value, MesgReceivedH, MesgReceivedT} = get_rest_counter_values(xmppMessageReceived),
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),
        escalus_client:wait_for_stanza(Bob),

        assert_rest_counters(MesgSentH + 1, MesgSentT + 1, xmppMessageSent),
        assert_rest_counters(MesgReceivedH + 1, MesgReceivedT + 1, xmppMessageReceived)

        end).

stanza_one_rest(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        {value, StanzaSentH, StanzaSentT} = get_rest_counter_values(xmppStanzaSent),
        {value, StanzaReceivedH, StanzaReceivedT} = get_rest_counter_values(xmppStanzaReceived),

        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),
        escalus_client:wait_for_stanza(Bob),

        assert_rest_counters(StanzaSentH + 1, StanzaSentT + 1,  xmppStanzaSent),
        assert_rest_counters(StanzaReceivedH + 1, StanzaReceivedT + 1, xmppStanzaReceived)

        end).

presence_one_rest(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        {value, PresenceSentH, PresenceSentT} = get_rest_counter_values(xmppPresenceSent),
        {value, PresenceReceivedH, PresenceReceivedT} = get_rest_counter_values(xmppPresenceReceived),
        {value, StanzaSentH, StanzaSentT} = get_rest_counter_values(xmppStanzaSent),
        {value, StanzaReceivedH, StanzaReceivedT} = get_rest_counter_values(xmppStanzaReceived),

        escalus:send(Alice, escalus_stanza:presence(<<"available">>)),
        escalus:wait_for_stanza(Alice),

        assert_rest_counters(PresenceSentH + 1, PresenceSentT + 1, xmppPresenceSent),
        assert_rest_counters(PresenceReceivedH + 1, PresenceReceivedT + 1, xmppPresenceReceived),
        assert_rest_counters(StanzaSentH + 1, StanzaSentT + 1, xmppStanzaSent),
        assert_rest_counters(StanzaReceivedH + 1, StanzaReceivedT + 1, xmppStanzaReceived)

        end).

presence_direct_one_rest(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        {value, PresenceSentH, PresenceSentT} = get_rest_counter_values(xmppPresenceSent),
        {value, PresenceReceivedH, PresenceReceivedT} = get_rest_counter_values(xmppPresenceReceived),
        {value, StanzaSentH, StanzaSentT} = get_rest_counter_values(xmppStanzaSent),
        {value, StanzaReceivedH, StanzaReceivedT} = get_rest_counter_values(xmppStanzaReceived),

        Presence = escalus_stanza:presence_direct(bob, <<"available">>),
        escalus:send(Alice, Presence),
        escalus:wait_for_stanza(Bob),

        assert_rest_counters(PresenceSentH + 1, PresenceSentT + 1, xmppPresenceSent),
        assert_rest_counters(PresenceReceivedH + 1, PresenceReceivedT + 1, xmppPresenceReceived),
        assert_rest_counters(StanzaSentH + 1, StanzaSentT + 1, xmppStanzaSent),
        assert_rest_counters(StanzaReceivedH + 1, StanzaReceivedT + 1, xmppStanzaReceived)

        end).

iq_one_rest(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        {value, IqSentH, IqSentT} = get_rest_counter_values(xmppIqSent),
        {value, IqReceivedH, IqReceivedT} = get_rest_counter_values(xmppIqReceived),
        {value, StanzaSentH, StanzaSentT} = get_rest_counter_values(xmppStanzaSent),
        {value, StanzaReceivedH, StanzaReceivedT} = get_rest_counter_values(xmppStanzaReceived),

        escalus_client:send(Alice,
                            escalus_stanza:roster_get()),
        escalus_client:wait_for_stanza(Alice),

        assert_rest_counters(IqSentH + 1, IqSentT + 1, xmppIqSent),
        assert_rest_counters(StanzaSentH + 1, StanzaSentT + 1, xmppStanzaSent),
        assert_rest_counters(StanzaReceivedH + 1, StanzaReceivedT + 1, xmppStanzaReceived),
        assert_rest_counters(IqReceivedH + 1, IqReceivedT + 1, xmppIqReceived)

        end).
