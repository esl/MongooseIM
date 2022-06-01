-module(graphql_metric_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [execute_auth/2, init_admin_handler/1]).

suite() ->
    MIM2NodeName = maps:get(node, distributed_helper:mim2()),
    %% Ensure nodes are connected
    mongoose_helper:successful_rpc(net_kernel, connect_node, [MIM2NodeName]),
    require_rpc_nodes([mim, mim2]) ++ escalus:suite().

all() ->
     [{group, metrics}].

groups() ->
     [{metrics, [], metrics_handler()}].

metrics_handler() ->
    [get_metrics,
     get_global_erlang_metrics,
     get_vm_stats_memory,
     get_metrics_as_dicts,
     get_metrics_as_dicts_with_key_one,
     get_cluster_metrics,
     get_mim2_cluster_metrics].

init_per_suite(Config) ->
    escalus:init_per_suite(init_admin_handler(Config)).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
     escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
     escalus:end_per_testcase(CaseName, Config).

get_metrics(Config) ->
    %% Get all metrics
    Result = execute_auth(#{query => get_all_metrics_call(),
                            variables => #{}, operationName => <<"Q1">>}, Config),
    ParsedResult = ok_result(<<"metric">>, <<"getMetrics">>, Result),
    Map = maps:from_list([{Name, X} || X = #{<<"name">> := Name} <- ParsedResult]),
    ReadsKey = [<<"global">>, <<"backends">>, <<"mod_roster">>, <<"read_roster_version">>],
    Reads = maps:get(ReadsKey, Map),
    %% Histogram integer keys have p prefix
    check_histogram_p(Reads),
    #{<<"type">> := <<"histogram">>} = Reads.

get_global_erlang_metrics(Config) ->
    %% Filter by name works
    Result = execute_auth(#{query => get_metrics_call_with_args(<<"(name: [\"global\", \"erlang\"])">>),
                            variables => #{}, operationName => <<"Q1">>}, Config),
    ParsedResult = ok_result(<<"metric">>, <<"getMetrics">>, Result),
    Map = maps:from_list([{Name, X} || X = #{<<"name">> := Name} <- ParsedResult]),
    Info = maps:get([<<"global">>,<<"erlang">>, <<"system_info">>], Map),
    #{<<"type">> := <<"vm_system_info">>} = Info,
    Keys = [<<"ets_limit">>, <<"port_count">>, <<"port_limit">>,
            <<"process_count">>, <<"process_limit">>],
    [true = is_integer(maps:get(Key, Info)) || Key <- Keys],
    ReadsKey = [<<"global">>, <<"backends">>, <<"mod_roster">>, <<"read_roster_version">>],
    %% Other metrics are filtered out
    undef = maps:get(ReadsKey, Map, undef).

get_vm_stats_memory(Config) ->
    Result = execute_auth(#{query => get_metrics_call_with_args(<<"(name: [\"global\"])">>),
                            variables => #{}, operationName => <<"Q1">>}, Config),
    ParsedResult = ok_result(<<"metric">>, <<"getMetrics">>, Result),
    Map = maps:from_list([{Name, X} || X = #{<<"name">> := Name} <- ParsedResult]),
    Mem = maps:get([<<"global">>, <<"erlang">>, <<"memory">>], Map),
    #{<<"type">> := <<"vm_stats_memory">>} = Mem,
    Keys = [<<"atom_used">>, <<"binary">>, <<"ets">>,
            <<"processes_used">>, <<"system">>, <<"total">>],
    [true = is_integer(maps:get(Key, Mem)) || Key <- Keys].

get_metrics_as_dicts(Config) ->
    Result = execute_auth(#{query => get_all_metrics_as_dicts_call(), variables => #{},
                            operationName => <<"Q1">>}, Config),
    ParsedResult = ok_result(<<"metric">>, <<"getMetricsAsDicts">>, Result),
    check_node_result_is_valid(ParsedResult, false).

get_metrics_as_dicts_with_key_one(Config) ->
    Result = execute_auth(#{query => get_all_metrics_as_dicts_with_key_one_call(),
                            variables => #{},
                            operationName => <<"Q1">>}, Config),
    ParsedResult = ok_result(<<"metric">>, <<"getMetricsAsDicts">>, Result),
    Map = dict_objects_to_map(ParsedResult),
    SentName = [domain_helper:host_type(), <<"xmppStanzaSent">>],
    [#{<<"key">> := <<"one">>, <<"value">> := One}] = maps:get(SentName, Map),
    true = is_integer(One).

get_cluster_metrics(Config) ->
    %% We will have at least these two nodes
    Node1 = atom_to_binary(maps:get(node, distributed_helper:mim())),
    Node2 = atom_to_binary(maps:get(node, distributed_helper:mim2())),
    Result = execute_auth(#{query => get_all_cluster_metrics_as_dicts_call(),
                            variables => #{},
                            operationName => <<"Q1">>}, Config),
    ParsedResult = ok_result(<<"metric">>, <<"getClusterMetricsAsDicts">>, Result),
    #{Node1 := Res1, Node2 := Res2} = node_objects_to_map(ParsedResult),
    check_node_result_is_valid(Res1, false),
    check_node_result_is_valid(Res2, true).

get_mim2_cluster_metrics(Config) ->
    Node = atom_to_binary(maps:get(node, distributed_helper:mim2())),
    Result = execute_auth(#{query => get_node_cluster_metrics_as_dicts_call(Node),
                            variables => #{},
                            operationName => <<"Q1">>}, Config),
    ParsedResult = ok_result(<<"metric">>, <<"getClusterMetricsAsDicts">>, Result),
    [#{<<"node">> := Node, <<"result">> := ResList}] = ParsedResult,
    check_node_result_is_valid(ResList, true).

check_node_result_is_valid(ResList, MetricsAreGlobal) ->
    %% Check that result contains something
    Map = dict_objects_to_map(ResList),
    SentName = case MetricsAreGlobal of
            true -> [<<"global">>, <<"xmppStanzaSent">>];
            false -> [domain_helper:host_type(), <<"xmppStanzaSent">>]
        end,
    [#{<<"key">> := <<"count">>, <<"value">> := Count},
     #{<<"key">> := <<"one">>, <<"value">> := One}] =
        maps:get(SentName, Map),
    true = is_integer(Count),
    true = is_integer(One),
    [#{<<"key">> := <<"value">>,<<"value">> := V}] =
        maps:get([<<"global">>,<<"uniqueSessionCount">>], Map),
    true = is_integer(V),
    HistObjects = maps:get([<<"global">>, <<"data">>, <<"xmpp">>,
                            <<"sent">>, <<"compressed_size">>], Map),
    check_histogram(kv_objects_to_map(HistObjects)).

check_histogram(Map) ->
    Keys = [<<"n">>, <<"mean">>,  <<"min">>,  <<"max">>,  <<"median">>,
            <<"50">>, <<"75">>, <<"90">>, <<"95">>,  <<"99">>, <<"999">>],
    [true = is_integer(maps:get(Key, Map)) || Key <- Keys].

check_histogram_p(Map) ->
    Keys = [<<"n">>, <<"mean">>,  <<"min">>,  <<"max">>,  <<"median">>,
            <<"p50">>, <<"p75">>, <<"p90">>, <<"p95">>,  <<"p99">>, <<"p999">>],
    [true = is_integer(maps:get(Key, Map)) || Key <- Keys].

dict_objects_to_map(List) ->
    KV = [{Name, Dict} || #{<<"name">> := Name, <<"dict">> := Dict} <- List],
    maps:from_list(KV).

node_objects_to_map(List) ->
    KV = [{Name, Value} || #{<<"node">> := Name, <<"result">> := Value} <- List],
    maps:from_list(KV).

kv_objects_to_map(List) ->
    KV = [{Key, Value} || #{<<"key">> := Key, <<"value">> := Value} <- List],
    maps:from_list(KV).

get_all_metrics_call() ->
    get_metrics_call_with_args(<<>>).

get_metrics_call_with_args(Args) ->
    <<"query Q1
           {metric
               {getMetrics", Args/binary, " {
                     ... on HistogramMetric
                     { name type n mean min max median p50 p75 p90 p95 p99 p999 }
                     ... on CounterMetric
                     { name type value ms_since_reset }
                     ... on SpiralMetric
                     { name type one count }
                     ... on GaugeMetric
                     { name type value }
                     ... on MergedInetStatsMetric
                     { name type connections recv_cnt recv_max recv_oct
                       send_cnt send_max send_oct send_pend }
                     ... on VMStatsMemoryMetric
                     { name type total processes_used atom_used binary ets system }
                     ... on VMSystemInfoMetric
                     { name type port_count port_limit process_count process_limit ets_limit }
                     ... on ProbeQueuesMetric
                     { name type type fsm regular total }
                 }
               }
           }">>.

get_all_metrics_as_dicts_call() ->
    <<"query Q1
           {metric
               {getMetricsAsDicts { name dict { key value }}}}">>.

get_all_metrics_as_dicts_with_key_one_call() ->
    <<"query Q1
           {metric
               {getMetricsAsDicts(keys: [\"one\"]) { name dict { key value }}}}">>.

get_all_cluster_metrics_as_dicts_call() ->
    <<"query Q1
           {metric
               {getClusterMetricsAsDicts {node result { name dict { key value }}}}}">>.

get_node_cluster_metrics_as_dicts_call(NodeBin) ->
    <<"query Q1
           {metric
               {getClusterMetricsAsDicts(nodes: [\"", NodeBin/binary, "\"]) "
               "{node result { name dict { key value }}}}}">>.

%% Helpers
ok_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"data">> := Data}}) ->
    maps:get(What2, maps:get(What1, Data)).

error_result(ErrorNumber, {{<<"200">>, <<"OK">>}, #{<<"errors">> := Errors}}) ->
    lists:nth(ErrorNumber, Errors).
