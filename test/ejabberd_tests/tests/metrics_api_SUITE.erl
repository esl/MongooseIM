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

-define(assert_equal(E, V), (
    [ct:fail("assert_equal(~s, ~s)~n\tExpected ~p~n\tValue ~p~n",
             [(??E), (??V), (E), (V)])
     || (E) =/= (V)]
    )).

-define(assert_equal_extra(E, V, Extra), (
    [ct:fail("assert_equal_extra(~s, ~s)~n\tExpected ~p~n\tValue ~p~nExtra ~p~n",
             [(??E), (??V), (E), (V), (Extra)])
     || (E) =/= (V)]
    )).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
all() ->
    [
     {group, metrics},
     {group, all_metrics_are_global},
     {group, global}
    ].

-define(METRICS_CASES, [
                        message_flow,
                        one_client_just_logs_in,
                        two_clients_just_log_in,
                        one_message_sent,
                        one_direct_presence_sent,
                        one_iq_sent,
                        one_message_error,
                        one_iq_error,
                        one_presence_error
                       ]).

groups() ->
    [
     {metrics, [], ?METRICS_CASES},
     {all_metrics_are_global, [], ?METRICS_CASES},
     {global, [], [session_counters,
                   node_uptime]}
    ].

init_per_suite(Config) ->
    Config1 = dynamic_modules:stop_running(mod_offline, Config),
    Config2 = escalus:init_per_suite(Config1),
    katt_helper:init_per_suite(Config2).

end_per_suite(Config) ->
    katt_helper:end_per_suite(Config),
    escalus:end_per_suite(Config),
    dynamic_modules:start_running(Config).

init_per_group(GroupName, Config) ->
    metrics_helper:prepare_by_all_metrics_are_global(Config, GroupName =:= all_metrics_are_global).

end_per_group(GroupName, Config) ->
    metrics_helper:finalise_by_all_metrics_are_global(Config, GroupName =:= all_metrics_are_global).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% metrics_api tests
%%--------------------------------------------------------------------

message_flow(Config) ->
    case metrics_helper:all_metrics_are_global(Config) of
        true -> katt_helper:run(metrics_only_global, Config,
                                [{port, escalus_ct:get_config(ejabberd2_metrics_rest_port)}]);
        _ -> katt_helper:run(metrics, Config)
    end.

one_client_just_logs_in(Config) ->
    instrumented_story
        (Config, metrics_helper:userspec(1, Config),
         fun(_User1) -> end_of_story end,
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
        (Config, metrics_helper:userspec(1, 1, Config),
         fun(_User1, _User2) -> end_of_story end,
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
      (Config, metrics_helper:userspec(1, 1, Config),
       fun(User1, User2) ->
               Chat = escalus_stanza:chat_to(User2, <<"Hi!">>),
               escalus_client:send(User1, Chat),
               escalus_client:wait_for_stanza(User2)
       end,
       [{xmppMessageSent,     1},
        {xmppMessageReceived, 1}]).

one_direct_presence_sent(Config) ->
    Userspec = metrics_helper:userspec(1, 1, Config),
    instrumented_story
      (Config, Userspec,
       fun(User1, User2) ->
               Presence = escalus_stanza:presence_direct(User2, <<"available">>),
               escalus:send(User1, Presence),
               escalus:wait_for_stanza(User2)
        end,
       [{xmppPresenceSent, 1 + user_alpha(2)},
        {xmppPresenceReceived, 1 + user_alpha(2)},
        {xmppStanzaSent, 1 + user_alpha(2)},
        {xmppStanzaReceived, 1 + user_alpha(2)}]).

one_iq_sent(Config) ->
    instrumented_story
      (Config, metrics_helper:userspec(1, Config),
       fun(User1) ->
               RosterIq = escalus_stanza:roster_get(),
               escalus_client:send(User1, RosterIq),
               escalus_client:wait_for_stanza(User1)
        end,
       [{xmppIqSent, 1},
        {xmppIqReceived, 1},
        {modRosterGets, 1},
        {xmppStanzaSent, 1 + user_alpha(1)},
        {xmppStanzaReceived, 1 + user_alpha(1)}]).

one_message_error(Config) ->
    instrumented_story
      (Config, metrics_helper:userspec(1, Config),
       fun(User1) ->
               Chat = escalus_stanza:chat_to
                        (<<"nobody@localhost">>, <<"Hi!">>),
               escalus_client:send(User1, Chat),
               escalus_client:wait_for_stanza(User1)
        end,
       [{xmppErrorTotal, 1},
        {xmppErrorIq, 0},
        {xmppErrorMessage, 1},
        {xmppErrorPresence, 0}]).

one_iq_error(Config) ->
    instrumented_story
      (Config, metrics_helper:userspec(1, Config),
       fun(User1) ->
               BadIQ = escalus_stanza:iq_set(<<"BadNS">>, []),
               escalus_client:send(User1, BadIQ),
               escalus_client:wait_for_stanza(User1)
        end,
       [{xmppErrorTotal, 1},
        {xmppErrorIq, 1},
        {xmppErrorMessage, 0},
        {xmppErrorPresence, 0}]).

one_presence_error(Config) ->
    instrumented_story
      (Config, metrics_helper:userspec(1, Config),
       fun(User1) ->
               BadPres = escalus_stanza:presence_direct
                           (<<"localhost/no-such-resource">>, <<"subscribed">>, []),
               escalus_client:send(User1, BadPres),
               escalus_client:wait_for_stanza(User1)
        end,
       [{xmppErrorTotal, 1},
        {xmppErrorIq, 0},
        {xmppErrorMessage, 0},
        {xmppErrorPresence, 1}]).

session_counters(Config) ->
    escalus:story
      (Config, [{alice, 2}, {bob, 1}],
       fun(_User11, _User12, _User2) ->
               ?assert_equal(3, fetch_global_gauge_value(totalSessionCount, Config)),
               ?assert_equal(2, fetch_global_gauge_value(uniqueSessionCount, Config)),
               ?assert_equal(3, fetch_global_gauge_value(nodeSessionCount, Config))
       end).

node_uptime(Config) ->
      X = fetch_global_incrementing_gauge_value(nodeUpTime, Config),
      timer:sleep(timer:seconds(1)),
      Y = fetch_global_incrementing_gauge_value(nodeUpTime, Config),
      ?assert_equal_extra(true, Y > X, [{counter, nodeUpTime}, {first, X}, {second, Y}]).

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
    FetchCounterFun = case metrics_helper:all_metrics_are_global(Config) of
                          true -> fun fetch_global_spiral_values/2;
                          _ -> fun fetch_counter_value/2
                      end,
    [ {Counter, FetchCounterFun(Counter, Config)}
      || {Counter, _} <- CounterSpecs ].

find(CounterName, CounterList) ->
    case lists:keyfind(CounterName, 1, CounterList) of
        false -> error(counter_defined_incorrectly);
        {CounterName, Val} -> Val end.

fetch_counter_value(Counter, Config) ->
    Params = [{metric, atom_to_list(Counter)}, {host, ct:get_config(ejabberd_domain)}],
    {_, _, _, Vars, _} = katt_helper:run(metric, Config, Params),
    HostValue = proplists:get_value("value_host", Vars),
    HostValueList = proplists:get_value("value_host_list", Vars),
    TotalValue = proplists:get_value("value_total", Vars),
    TotalValueList = proplists:get_value("value_total_list", Vars),
    [HostValue, HostValueList, TotalValue, TotalValueList].

%% @doc Fetch counter that is static
fetch_global_gauge_value(Counter, Config) ->
    [Value, ValueList] = fetch_global_gauge_values(Counter, Config),
    ?assert_equal_extra(Value, ValueList, [{counter, Counter}]),
    Value.

%% @doc Fetch counter that can be incremented by server between two API requests
%%
%% Returns last actual value
fetch_global_incrementing_gauge_value(Counter, Config) ->
    [Value, ValueList] = fetch_global_gauge_values(Counter, Config),
    ?assert_equal_extra(true, Value =< ValueList, [{counter, Counter},
                                                   {value, Value},
                                                   {value_list, ValueList}]),
    ValueList.

fetch_global_gauge_values(Counter, Config) ->
    fetch_global_counter_values(global_gauge, Counter, Config).

fetch_global_spiral_values(Counter, Config) ->
    fetch_global_counter_values(global_spiral, Counter, Config).

fetch_global_counter_values(Blueprint, Counter, Config) ->
    ParamsBase = case metrics_helper:all_metrics_are_global(Config) of
                     true -> [{port, escalus_ct:get_config(ejabberd2_metrics_rest_port)}];
                     _ -> []
                 end,
    Params = [{metric, atom_to_list(Counter)} | ParamsBase],

    {_, _, _, Vars, _} = katt_helper:run(Blueprint, Config, Params),

    Value = proplists:get_value("value", Vars),
    ValueList = proplists:get_value("value_list", Vars),
    [Value, ValueList].

assert_counter_inc(Name, Inc, Counters1, Counters2) when is_list(Counters1) ->
    ExpectedCounters = [Counter+Inc || Counter <- Counters1],
    case ExpectedCounters == Counters2 of
        false ->
            ct:comment("Expected ~w, got: ~w", [ExpectedCounters, Counters2]),
            error({unexpected_values, Name, get_diffs(ExpectedCounters, Counters2)});
        true -> ok
    end;
assert_counter_inc(_Name, Inc, Counter1, Counter2) when Counter1 + Inc =:= Counter2 ->
    ok.

get_diffs(L1,L2) ->
    lists:zip(L1, L2).

