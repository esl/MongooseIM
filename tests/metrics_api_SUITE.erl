%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
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
-module(metrics_api_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
all() ->
    [{group, metrics},
     {group, single_values}].

groups() ->
    [{metrics, [], [message_flow]},
     {single_values, [], [message_one,
                          stanza_one,
                          presence_one,
                          presence_direct_one,
                          iq_one]}].

init_per_suite(Config) ->
    Config1 = dynamic_modules:stop_running(mod_offline, Config),
    Config2 = escalus:init_per_suite(Config1),
    katt_helper:init_per_suite(Config2).

end_per_suite(Config) ->
    katt_helper:end_per_suite(Config),
    escalus:end_per_suite(Config),
    dynamic_modules:start_running(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, {by_name, [alice, bob]}).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, {by_name, [alice, bob]}).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% metrics_api tests
%%--------------------------------------------------------------------
message_flow(Config) ->
    katt_helper:run(metrics, Config).

%%--------------------------------------------------------------------
%% metric update tests
%%--------------------------------------------------------------------
message_one(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
                MessageSent1 = fetch_counter_value(xmppMessageSent, Config),
                MessageRecv1 = fetch_counter_value(xmppMessageReceived, Config),

                Chat = escalus_stanza:chat_to(Bob, <<"Hi!">>),
                escalus_client:send(Alice, Chat),
                escalus_client:wait_for_stanza(Bob),

                MessageSent2 = fetch_counter_value(xmppMessageSent, Config),
                assert_counter_inc(1, MessageSent1, MessageSent2),
                MessageRecv2 = fetch_counter_value(xmppMessageReceived, Config),
                assert_counter_inc(1, MessageRecv1, MessageRecv2)
        end).

stanza_one(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
                StanzaSent1 = fetch_counter_value(xmppStanzaSent, Config),
                StanzaRecv1 = fetch_counter_value(xmppStanzaReceived, Config),

                Chat = escalus_stanza:chat_to(Bob, <<"Hi!">>),
                escalus_client:send(Alice, Chat),
                escalus_client:wait_for_stanza(Bob),

                StanzaSent2 = fetch_counter_value(xmppStanzaSent, Config),
                assert_counter_inc(1, StanzaSent1, StanzaSent2),
                StanzaRecv2 = fetch_counter_value(xmppStanzaReceived, Config),
                assert_counter_inc(1, StanzaRecv1, StanzaRecv2)
        end).

presence_one(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
                PresSent1 = fetch_counter_value(xmppPresenceSent, Config),
                PresRecv1 = fetch_counter_value(xmppPresenceReceived, Config),
                StanzaSent1 = fetch_counter_value(xmppStanzaSent, Config),
                StanzaRecv1 = fetch_counter_value(xmppStanzaReceived, Config),

                Presence = escalus_stanza:presence(<<"available">>),
                escalus:send(Alice, Presence),
                escalus:wait_for_stanza(Alice),

                PresSent2 = fetch_counter_value(xmppPresenceSent, Config),
                assert_counter_inc(1, PresSent1, PresSent2),
                PresRecv2 = fetch_counter_value(xmppPresenceReceived, Config),
                assert_counter_inc(1, PresRecv1, PresRecv2),
                StanzaSent2 = fetch_counter_value(xmppStanzaSent, Config),
                assert_counter_inc(1, StanzaSent1, StanzaSent2),
                StanzaRecv2 = fetch_counter_value(xmppStanzaReceived, Config),
                assert_counter_inc(1, StanzaRecv1, StanzaRecv2)
        end).

presence_direct_one(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
                PresSent1 = fetch_counter_value(xmppPresenceSent, Config),
                PresRecv1 = fetch_counter_value(xmppPresenceReceived, Config),
                StanzaSent1 = fetch_counter_value(xmppStanzaSent, Config),
                StanzaRecv1 = fetch_counter_value(xmppStanzaReceived, Config),

                Presence = escalus_stanza:presence_direct(bob, <<"available">>),
                escalus:send(Alice, Presence),
                escalus:wait_for_stanza(Bob),

                PresSent2 = fetch_counter_value(xmppPresenceSent, Config),
                assert_counter_inc(1, PresSent1, PresSent2),
                PresRecv2 = fetch_counter_value(xmppPresenceReceived, Config),
                assert_counter_inc(1, PresRecv1, PresRecv2),
                StanzaSent2 = fetch_counter_value(xmppStanzaSent, Config),
                assert_counter_inc(1, StanzaSent1, StanzaSent2),
                StanzaRecv2 = fetch_counter_value(xmppStanzaReceived, Config),
                assert_counter_inc(1, StanzaRecv1, StanzaRecv2)
        end).

iq_one(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
                IqSent1 = fetch_counter_value(xmppIqSent, Config),
                IqRecv1 = fetch_counter_value(xmppIqReceived, Config),
                StanzaSent1 = fetch_counter_value(xmppStanzaSent, Config),
                StanzaRecv1 = fetch_counter_value(xmppStanzaReceived, Config),

                RosterIq = escalus_stanza:roster_get(),
                escalus_client:send(Alice, RosterIq),
                escalus_client:wait_for_stanza(Alice),

                IqSent2 = fetch_counter_value(xmppIqSent, Config),
                assert_counter_inc(1, IqSent1, IqSent2),
                IqRecv2 = fetch_counter_value(xmppIqReceived, Config),
                assert_counter_inc(1, IqRecv1, IqRecv2),
                StanzaSent2 = fetch_counter_value(xmppStanzaSent, Config),
                assert_counter_inc(1, StanzaSent1, StanzaSent2),
                StanzaRecv2 = fetch_counter_value(xmppStanzaReceived, Config),
                assert_counter_inc(1, StanzaRecv1, StanzaRecv2)
        end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
fetch_counter_value(Counter, Config) ->
    Params = [{host, ct:get_config(ejabberd_domain)},
              {metric, atom_to_list(Counter)}],
    {_, _, _, Vars, _} = katt_helper:run(metric, Config, Params),
    HostValue = proplists:get_value("value_host", Vars),
    HostValueList = proplists:get_value("value_host_list", Vars),
    TotalValue = proplists:get_value("value_total", Vars),
    TotalValueList = proplists:get_value("value_total_list", Vars),
    [HostValue, HostValueList, TotalValue, TotalValueList].

assert_counter_inc(Inc, Counters1, Counters2) ->
    Counters2 = [Counter+Inc || Counter <- Counters1].
