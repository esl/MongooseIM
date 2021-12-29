%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
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
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

-import(distributed_helper, [mim/0, rpc/4]).
-import(rest_helper, [assert_status/2, simple_request/2, simple_request/3, simple_request/4]).
-define(PORT, (ct:get_config({hosts, mim, metrics_rest_port}))).

-include_lib("eunit/include/eunit.hrl").

-import(domain_helper, [host_type/0, domain/0]).

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
                   node_uptime,
                   cluster_size
                  ]}
    ].

init_per_suite(Config) ->
    HostType = host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config),
    dynamic_modules:ensure_stopped(HostType, [mod_offline]),
    escalus:init_per_suite(Config1).

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(GroupName, Config) ->
    metrics_helper:prepare_by_all_metrics_are_global(Config, GroupName =:= all_metrics_are_global).

end_per_group(GroupName, Config) ->
    metrics_helper:finalise_by_all_metrics_are_global(Config, GroupName =:= all_metrics_are_global).

init_per_testcase(cluster_size = CN, Config) ->
    Config1 = ensure_nodes_not_clustered(Config),
    escalus:init_per_testcase(CN, Config1);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(cluster_size = CN, Config) ->
    Config1 = ensure_nodes_clustered(Config),
    escalus:end_per_testcase(CN, Config1);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% metrics_api tests
%%--------------------------------------------------------------------

message_flow(Config) ->
    case metrics_helper:all_metrics_are_global(Config) of
        true -> metrics_only_global(Config);
        _ -> metrics_msg_flow(Config)
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
                        (<<"nobody@", (domain())/binary>>, <<"Hi!">>),
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
                           (<<(domain())/binary, "/no-such-resource">>, <<"subscribed">>, []),
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
               ?assertEqual(3, fetch_global_gauge_value(totalSessionCount, Config)),
               ?assertEqual(2, fetch_global_gauge_value(uniqueSessionCount, Config)),
               ?assertEqual(3, fetch_global_gauge_value(nodeSessionCount, Config))
       end).

node_uptime(Config) ->
      X = fetch_global_incrementing_gauge_value(nodeUpTime, Config),
      timer:sleep(timer:seconds(1)),
      Y = fetch_global_incrementing_gauge_value(nodeUpTime, Config),
      ?assertEqual(true, Y > X, [{counter, nodeUpTime}, {first, X}, {second, Y}]).

cluster_size(Config) ->
      SingleNodeClusterState =
            fetch_global_incrementing_gauge_value(clusterSize, Config),
      ?assertEqual(1, SingleNodeClusterState),

      distributed_helper:add_node_to_cluster(Config),
      TwoNodesClusterState =
            fetch_global_incrementing_gauge_value(clusterSize, Config),
      ?assertEqual(2, TwoNodesClusterState),

      distributed_helper:remove_node_from_cluster(Config),
      SingleNodeClusterState2 =
            fetch_global_incrementing_gauge_value(clusterSize, Config),
      ?assertEqual(1, SingleNodeClusterState2).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

metrics_only_global(_Config) ->
    Port = ct:get_config({hosts, mim2, metrics_rest_port}),
    % 0. GET is the only implemented allowed method
    % (both OPTIONS and HEAD are for free then)
    Res = simple_request(<<"OPTIONS">>, "/metrics/", Port),
    {_S, H, _B} = Res,
    assert_status(200, Res),
    V = proplists:get_value(<<"allow">>, H),
    Opts = string:split(V, ", ", all),
    ?assertEqual([<<"GET">>,<<"HEAD">>,<<"OPTIONS">>], lists:sort(Opts)),

    % List of host types and metrics
    Res2 = simple_request(<<"GET">>, "/metrics/", Port),
    {_S2, _H2, B2} = Res2,
    assert_status(200, Res2),
    #{<<"host_types">> := [_ExampleHostType | _],
      <<"metrics">> := [],
      <<"global">> := [ExampleGlobal | _]} = B2,

    % All global metrics
    Res3 = simple_request(<<"GET">>, "/metrics/global", Port),
    {_S3, _H3, B3} = Res3,
    assert_status(200, Res3),
    #{<<"metrics">> := _ML} = B3,
    ?assertEqual(1, maps:size(B3)),

    % An example global metric
    Res4 = simple_request(<<"GET">>,
                          unicode:characters_to_list(["/metrics/global/", ExampleGlobal]),
                          Port),
    {_S4, _H4, B4} = Res4,
    #{<<"metric">> := _} = B4,
    ?assertEqual(1, maps:size(B4)).

metrics_msg_flow(_Config) ->
    % 0. GET is the only implemented allowed method
    % (both OPTIONS and HEAD are for free then)
    Res = simple_request(<<"OPTIONS">>, "/metrics/", ?PORT),
    {_S, H, _B} = Res,
    assert_status(200, Res),
    V = proplists:get_value(<<"allow">>, H),
    Opts = string:split(V, ", ", all),
    ?assertEqual([<<"GET">>,<<"HEAD">>,<<"OPTIONS">>], lists:sort(Opts)),

    % List of host types and metrics
    Res2 = simple_request(<<"GET">>, "/metrics/", ?PORT),
    {_S2, _H2, B2} = Res2,
    assert_status(200, Res2),
    #{<<"host_types">> := [ExampleHostType | _],
      <<"metrics">> := [ExampleMetric | _],
      <<"global">> := [ExampleGlobal | _]} = B2,

    % Sum of all metrics
    Res3 = simple_request(<<"GET">>, "/metrics/all", ?PORT),
    {_S3, _H3, B3} = Res3,
    assert_status(200, Res3),
    #{<<"metrics">> := _ML} = B3,
    ?assertEqual(1, maps:size(B3)),

    % Sum for a given metric
    Res4 = simple_request(<<"GET">>,
                          unicode:characters_to_list(["/metrics/all/", ExampleMetric]),
                          ?PORT),
    {_S4, _H4, B4} = Res4,
    #{<<"metric">> := #{<<"one">> := _, <<"count">> := _} = IM} = B4,
    ?assertEqual(2, maps:size(IM)),
    ?assertEqual(1, maps:size(B4)),

    % Negative case for a non-existent given metric
    Res5 = simple_request(<<"GET">>, "/metrics/all/nonExistentMetric", ?PORT),
    assert_status(404, Res5),

    % All metrics for an example host type
    Res6 = simple_request(<<"GET">>,
                          unicode:characters_to_list(["/metrics/host_type/", ExampleHostType]),
                          ?PORT),
    {_S6, _H6, B6} = Res6,
    #{<<"metrics">> := _} = B6,
    ?assertEqual(1, maps:size(B6)),

    % Negative case for a non-existent host type
    Res7 = simple_request(<<"GET">>, "/metrics/host_type/nonExistentHostType", ?PORT),
    assert_status(404, Res7),

    % An example metric for an example host type
    Res8 = simple_request(<<"GET">>,
                          unicode:characters_to_list(["/metrics/host_type/", ExampleHostType,
                                               "/", ExampleMetric]),
                          ?PORT),
    {_S8, _H8, B8} = Res8,
    #{<<"metric">> := #{<<"one">> := _, <<"count">> := _} = IM2} = B8,
    ?assertEqual(2, maps:size(IM2)),
    ?assertEqual(1, maps:size(B8)),

    % Negative case for a non-existent (host type, metric) pair
    Res9 = simple_request(<<"GET">>,
                          unicode:characters_to_list(["/metrics/host_type/", ExampleHostType,
                                               "/nonExistentMetric"]),
                          ?PORT),
    assert_status(404, Res9),

    % All global metrics
    Res10 = simple_request(<<"GET">>, "/metrics/global", ?PORT),
    {_, _, B10} = Res10,
    #{<<"metrics">> := _} = B10,
    ?assertEqual(1, maps:size(B10)),

    Res11 = simple_request(<<"GET">>,
                           unicode:characters_to_list(["/metrics/global/", ExampleGlobal]),
                           ?PORT),
    {_, _, B11} = Res11,
    #{<<"metric">> := _} = B11,
    ?assertEqual(1, maps:size(B11)).

user_alpha(NumberOfUsers) ->
    %% This represents the overhead of logging in N users via escalus:story/3
    %% For each user,
    %%     xmppStanza(sent|received)
    %%     and
    %%     xmppPresence(sent|received)
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

fetch_counter_value(Counter, _Config) ->
    Metric = atom_to_binary(Counter, utf8),

    HostType = host_type(),
    HostTypeName = metrics_helper:make_host_type_name(HostType),

    Result = simple_request(<<"GET">>,
                            unicode:characters_to_list(["/metrics/host_type/", HostTypeName, "/", Metric]),
                            ?PORT),
    {_S, _H, B} = Result,
    assert_status(200, Result),
    #{<<"metric">> := #{<<"count">> := HostTypeValue}} = B,

    Result2 = simple_request(<<"GET">>,
                             unicode:characters_to_list(["/metrics/host_type/", HostTypeName]),
                             ?PORT),
    {_S2, _H2, B2} = Result2,
    assert_status(200, Result2),
    #{<<"metrics">> := #{Metric := #{<<"count">> := HostTypeValueList}}} = B2,

    Result3 = simple_request(<<"GET">>,
                             unicode:characters_to_list(["/metrics/all/", Metric]),
                             ?PORT),
    {_S3, _H3, B3} = Result3,
    assert_status(200, Result3),
    #{<<"metric">> := #{<<"count">> := TotalValue}} = B3,

    Result4 = simple_request(<<"GET">>, "/metrics/all/", ?PORT),
    {_S4, _H4, B4} = Result4,
    assert_status(200, Result4),
    #{<<"metrics">> := #{Metric := #{<<"count">> := TotalValueList}}} = B4,

    [HostTypeValue, HostTypeValueList, TotalValue, TotalValueList].

%% @doc Fetch counter that is static.
fetch_global_gauge_value(Counter, Config) ->
    [Value, ValueList] = fetch_global_gauge_values(Counter, Config),
    ?assertEqual(Value, ValueList, [{counter, Counter}]),
    Value.

%% @doc Fetch counter that can be incremented by server between two API requests.
%%
%% Returns last actual value
fetch_global_incrementing_gauge_value(Counter, Config) ->
    [Value, ValueList] = fetch_global_gauge_values(Counter, Config),
    ?assertEqual(true, Value =< ValueList, [{counter, Counter},
                                                   {value, Value},
                                                   {value_list, ValueList}]),
    ValueList.

fetch_global_gauge_values(Counter, Config) ->
    fetch_global_counter_values(<<"value">>, Counter, Config).

fetch_global_spiral_values(Counter, Config) ->
    % Spirals have two values associated with the metric: "one" and "count".
    % We are interested in the latter.
    fetch_global_counter_values(<<"count">>, Counter, Config).

fetch_global_counter_values(MetricKey, Counter, Config) ->
    Metric = atom_to_binary(Counter, utf8),

    Port = case metrics_helper:all_metrics_are_global(Config) of
               true ->
                   ct:get_config({hosts, mim2, metrics_rest_port});
               _ -> ct:get_config({hosts, mim, metrics_rest_port})
           end,

    Result = simple_request(<<"GET">>,
                            unicode:characters_to_list(["/metrics/global/", Metric]),
                            Port),
    assert_status(200, Result),
    {_S, H, B} = Result,
    #{<<"metric">> := #{MetricKey := Value}} = B,
    ?assertEqual(<<"application/json">>, proplists:get_value(<<"content-type">>, H)),
    ?assertEqual(1, maps:size(B)),

    Result2 = simple_request(<<"GET">>,
                             unicode:characters_to_list(["/metrics/global/"]),
                             Port),
    assert_status(200, Result2),
    {_S2, H2, B2} = Result2,
    ?assertEqual(<<"application/json">>, proplists:get_value(<<"content-type">>, H2)),
    #{<<"metrics">> := #{Metric := #{MetricKey := ValueList}}} = B2,
    ?assertEqual(1, maps:size(B2)),

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

get_diffs(L1, L2) ->
    lists:zip(L1, L2).

ensure_nodes_not_clustered(Config) ->
    #{node := Node1Name} = RPCNode = mim(),
    Nodes1 = rpc(RPCNode, mnesia, system_info, [running_db_nodes]),

    Nodes = [Node || Node <- Nodes1, Node =/= Node1Name],
    [distributed_helper:remove_node_from_cluster(#{node => N}, Config) || N <- Nodes],
    Config ++ [{nodes_clustered, Nodes}].

ensure_nodes_clustered(Config) ->
    NodesToBeClustered = proplists:get_value(nodes_clustered, Config),
    [distributed_helper:add_node_to_cluster(N, Config)
     || N <- NodesToBeClustered],
    Config.
