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
     {group, stories},
     {group, global}
    ].

groups() ->
    [{metrics, [], [message_flow]},
     {stories, [], [one_client_just_logs_in,
                    two_clients_just_log_in,
                    one_message_sent,
                    one_direct_presence_sent,
                    one_iq_sent,
                    one_message_error,
                    one_iq_error,
                    one_presence_error
                   ]},
     {global, [], [session_counters]}
    ].

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

one_client_just_logs_in(Config) ->
    instrumented_story
        (Config, [{alice, 1}],
         fun(_Alice) -> end_of_story end,
         %% A list of metrics and their expected relative increase
         [{xmppIqSent, 0},
          {xmppIqReceived, 0},
          {xmppMessageSent, 0},
          {xmppMessageReceived, 0},
          {xmppPresenceSent, 0 + user_alpha(1)},
          {xmppPresenceReceived, 0 + user_alpha(1)},
          {xmppStanzaSent, 0 + user_alpha(1)},
          {xmppStanzaReceived, 0 + user_alpha(1)},
          {sessionSuccessfulLogins, 0 + user_alpha(1)},
          {sessionLogouts, 0 + user_alpha(1)}
         ]).

two_clients_just_log_in(Config) ->
    instrumented_story
        (Config, [{alice, 1}, {bob, 1}],
         fun(_Alice, _Bob) -> end_of_story end,
         [{xmppMessageSent, 0},
          {xmppMessageReceived, 0},
          {xmppStanzaSent, 0 + user_alpha(2)},
          {xmppStanzaReceived, 0 + user_alpha(2)},
          {xmppPresenceSent, 0 + user_alpha(2)},
          {xmppPresenceReceived, 0 + user_alpha(2)},
          {sessionSuccessfulLogins, 0 + user_alpha(2)},
          {sessionLogouts, 0 + user_alpha(2)}
         ]).

one_message_sent(Config) ->
    instrumented_story
      (Config, [{alice, 1}, {bob, 1}],
       fun(Alice, Bob) ->
               Chat = escalus_stanza:chat_to(Bob, <<"Hi!">>),
               escalus_client:send(Alice, Chat),
               escalus_client:wait_for_stanza(Bob)
       end,
       [{xmppMessageSent,     1},
        {xmppMessageReceived, 1}]).

one_direct_presence_sent(Config) ->
    instrumented_story
      (Config, [{alice, 1}, {bob, 1}],
       fun(Alice, Bob) ->
               Presence = escalus_stanza:presence_direct(bob, <<"available">>),
               escalus:send(Alice, Presence),
               escalus:wait_for_stanza(Bob)
        end,
       [{xmppPresenceSent, 1 + user_alpha(2)},
        {xmppPresenceReceived, 1 + user_alpha(2)},
        {xmppStanzaSent, 1 + user_alpha(2)},
        {xmppStanzaReceived, 1 + user_alpha(2)}]).

one_iq_sent(Config) ->
    instrumented_story
      (Config, [{alice, 1}],
       fun(Alice) ->
               RosterIq = escalus_stanza:roster_get(),
               escalus_client:send(Alice, RosterIq),
               escalus_client:wait_for_stanza(Alice)
        end,
       [{xmppIqSent, 1},
        {xmppIqReceived, 1},
        {modRosterGets, 1},
        {xmppStanzaSent, 1 + user_alpha(1)},
        {xmppStanzaReceived, 1 + user_alpha(1)}]).

one_message_error(Config) ->
    instrumented_story
      (Config, [{alice, 1}],
       fun(Alice) ->
               Chat = escalus_stanza:chat_to
                        (<<"nobody@localhost">>, <<"Hi!">>),
               escalus_client:send(Alice, Chat),
               escalus_client:wait_for_stanza(Alice)
        end,
       [{xmppErrorTotal, 1},
        {xmppErrorIq, 0},
        {xmppErrorMessage, 1},
        {xmppErrorPresence, 0}]).

one_iq_error(Config) ->
    instrumented_story
      (Config, [{alice, 1}],
       fun(Alice) ->
               BadIQ = escalus_stanza:iq_set(<<"BadNS">>, []),
               escalus_client:send(Alice, BadIQ),
               escalus_client:wait_for_stanza(Alice)
        end,
       [{xmppErrorTotal, 1},
        {xmppErrorIq, 1},
        {xmppErrorMessage, 0},
        {xmppErrorPresence, 0}]).

one_presence_error(Config) ->
    instrumented_story
      (Config, [{alice, 1}],
       fun(Alice) ->
               BadPres = escalus_stanza:presence_direct
                           (<<"nbody@wronghost">>,<<"subscribed">>,[]),
               escalus_client:send(Alice, BadPres),
               escalus_client:wait_for_stanza(Alice)
        end,
       [{xmppErrorTotal, 1},
        {xmppErrorIq, 0},
        {xmppErrorMessage, 0},
        {xmppErrorPresence, 1}]).

session_counters(Config) ->
    escalus:story
      (Config,
       [{alice, 2}, {bob, 1}],
       fun(_Alice1, _Alice2, _Bob) ->
               3 = fetch_global_counter_value(totalSessionCount, Config),
               2 = fetch_global_counter_value(uniqueSessionCount, Config),
               3 = fetch_global_counter_value(nodeSessionCount, Config)
       end).


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

user_alpha(NumberOfUsers) ->
    %% This represents the overhead of logging in N users via escalus:story/3
    %% For each user,
    %%     xmppStanza(sent|received)
    %%     and
    %%     xmppPresence(sent|recieved)
    %% will be bumped by +1 at login.
    NumberOfUsers.

instrumented_story(Config, UsersSpecs, StoryFun, CounterSpecs) ->
    Befores = fetch_all(Config, CounterSpecs),
    StoryResult = escalus:story(Config, UsersSpecs, StoryFun),
    Afters =  fetch_all(Config, CounterSpecs),
    [ assert_counter_inc(Name, N, find(Name, Befores), find(Name, Afters))
      || {Name, N} <- CounterSpecs ],
    StoryResult.

fetch_all(Config, CounterSpecs) ->
    [ {Counter, fetch_counter_value(Counter, Config)}
      || {Counter, _} <- CounterSpecs ].

find(CounterName, CounterList) ->
    case lists:keyfind(CounterName, 1, CounterList) of
        false -> error(counter_defined_incorrectly);
        {CounterName, Val} -> Val end.

fetch_counter_value(Counter, Config) ->
    Params = [{host, ct:get_config(ejabberd_domain)},
              {metric, atom_to_list(Counter)}],
    {_, _, _, Vars, _} = katt_helper:run(metric, Config, Params),
    HostValue = proplists:get_value("value_host", Vars),
    HostValueList = proplists:get_value("value_host_list", Vars),
    TotalValue = proplists:get_value("value_total", Vars),
    TotalValueList = proplists:get_value("value_total_list", Vars),
    [HostValue, HostValueList, TotalValue, TotalValueList].

fetch_global_counter_value(Counter, Config) ->
    Params = [{metric, atom_to_list(Counter)}],

    {_, _, _, Vars, _} = katt_helper:run(global, Config, Params),

    Value = proplists:get_value("value", Vars),
    Value = proplists:get_value("value_list", Vars).

assert_counter_inc(Name, Inc, Counters1, Counters2) ->
    ExpectedCounters = [Counter+Inc || Counter <- Counters1],
    case ExpectedCounters == Counters2 of
        false ->
            ct:comment("Expected ~w, got: ~w", [ExpectedCounters, Counters2]),
            error({unexpected_values, Name, get_diffs(ExpectedCounters, Counters2)});
        true -> ok
    end.

get_diffs(L1,L2) ->
    lists:zip(L1, L2).
