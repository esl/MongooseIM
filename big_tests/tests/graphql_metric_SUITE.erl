-module(graphql_metric_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [execute_command/4, get_ok_value/2, get_unauthorized/1,
                         get_err_msg/1, get_err_code/1]).

suite() ->
    MIM2NodeName = maps:get(node, distributed_helper:mim2()),
    %% Ensure nodes are connected
    mongoose_helper:successful_rpc(net_kernel, connect_node, [MIM2NodeName]),
    require_rpc_nodes([mim, mim2]) ++ escalus:suite().

all() ->
     [{group, metrics_http},
      {group, metrics_cli},
      {group, domain_admin_metrics}].

groups() ->
     [{metrics_http, [], metrics_tests()},
      {metrics_cli, [], metrics_tests()},
      {domain_admin_metrics, [], domain_admin_metrics_tests()}].

metrics_tests() ->
    [get_all_metrics,
     get_all_metrics_check_by_type,
     get_by_name_global_erlang_metrics,
     get_metrics_by_name_empty_args,
     get_metrics_by_name_empty_string,
     get_metrics_by_nonexistent_name,
     get_metrics_for_specific_host_type,
     get_process_queue_length,
     get_inet_stats,
     get_vm_stats_memory,
     get_cets_system,
     get_all_metrics_as_dicts,
     get_by_name_metrics_as_dicts,
     get_metrics_as_dicts_by_nonexistent_name,
     get_metrics_as_dicts_with_key_one,
     get_metrics_as_dicts_with_nonexistent_key,
     get_metrics_as_dicts_empty_args,
     get_metrics_as_dicts_empty_strings,
     get_cluster_metrics,
     get_by_name_cluster_metrics_as_dicts,
     get_mim2_cluster_metrics,
     get_cluster_metrics_for_nonexistent_nodes,
     get_cluster_metrics_by_nonexistent_name,
     get_cluster_metrics_with_nonexistent_key,
     get_cluster_metrics_empty_args,
     get_cluster_metrics_empty_strings].

domain_admin_metrics_tests() ->
    [domain_admin_get_metrics,
     domain_admin_get_metrics_as_dicts,
     domain_admin_get_metrics_as_dicts_by_name,
     domain_admin_get_metrics_as_dicts_with_keys,
     domain_admin_get_cluster_metrics_as_dicts,
     domain_admin_get_cluster_metrics_as_dicts_by_name,
     domain_admin_get_cluster_metrics_as_dicts_for_nodes].

init_per_suite(Config) ->
    Config1 = ejabberd_node_utils:init(mim(), Config),
    escalus:init_per_suite(Config1).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(metrics_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(metrics_cli, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(domain_admin_metrics, Config) ->
    graphql_helper:init_domain_admin_handler(Config).

end_per_group(_GroupName, _Config) ->
    graphql_helper:clean().

init_per_testcase(get_cets_system = CaseName, Config) ->
     case is_cets_enabled() of
         true ->
             escalus:init_per_testcase(CaseName, Config);
         false ->
             {skip, cets_not_enabled}
     end;
init_per_testcase(CaseName, Config) ->
     escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
     escalus:end_per_testcase(CaseName, Config).

get_all_metrics(Config) ->
    Result = get_metrics(Config),
    ParsedResult = get_ok_value([data, metric, getMetrics], Result),
    Map = maps:from_list([{Name, X} || X = #{<<"name">> := Name} <- ParsedResult]),
    ReadsKey = [<<"global">>, <<"backends">>, <<"mod_roster">>, <<"read_roster_version">>],
    Reads = maps:get(ReadsKey, Map),
    %% Histogram integer keys have p prefix
    check_histogram_p(Reads),
    %% HistogramMetric type
    #{<<"type">> := <<"histogram">>} = Reads.

get_all_metrics_check_by_type(Config) ->
    Result = get_metrics(Config),
    ParsedResult = get_ok_value([data, metric, getMetrics], Result),
    lists:foreach(fun check_metric_by_type/1, ParsedResult).

check_metric_by_type(#{<<"type">> := Type} = Map) ->
    values_are_integers(Map, type_to_keys(Type)).

type_to_keys(<<"histogram">>) ->
    [<<"n">>, <<"mean">>,  <<"min">>,  <<"max">>,  <<"median">>,
     <<"p50">>, <<"p75">>, <<"p90">>, <<"p95">>,  <<"p99">>, <<"p999">>];
type_to_keys(<<"counter">>) ->
    [<<"value">>, <<"ms_since_reset">>];
type_to_keys(<<"spiral">>) ->
    [<<"one">>, <<"count">>];
type_to_keys(<<"gauge">>) ->
    [<<"value">>];
type_to_keys(<<"merged_inet_stats">>) ->
    [<<"connections">>, <<"recv_cnt">>, <<"recv_max">>, <<"recv_oct">>,
     <<"send_cnt">>, <<"send_max">>, <<"send_oct">>, <<"send_pend">>];
type_to_keys(<<"rdbms_stats">>) ->
    [<<"workers">>, <<"recv_cnt">>, <<"recv_max">>, <<"recv_oct">>,
     <<"send_cnt">>, <<"send_max">>, <<"send_oct">>, <<"send_pend">>];
type_to_keys(<<"vm_stats_memory">>) ->
    [<<"atom_used">>, <<"binary">>, <<"ets">>,
     <<"processes_used">>, <<"system">>, <<"total">>];
type_to_keys(<<"vm_system_info">>) ->
    [<<"ets_limit">>, <<"port_count">>, <<"port_limit">>,
     <<"process_count">>, <<"process_limit">>];
type_to_keys(<<"probe_queues">>) ->
    [<<"fsm">>, <<"regular">>, <<"total">>];
type_to_keys(<<"cets_system">>) ->
    [<<"available_nodes">>, <<"unavailable_nodes">>,
    <<"remote_nodes_without_disco">>, <<"joined_nodes">>,
    <<"remote_nodes_with_unknown_tables">>, <<"remote_unknown_tables">>,
    <<"remote_nodes_with_missing_tables">>, <<"remote_missing_tables">>,
    <<"conflict_nodes">>, <<"conflict_tables">>,
    <<"discovered_nodes">>, <<"discovery_works">>].

get_by_name_global_erlang_metrics(Config) ->
    %% Filter by name works
    Result = get_metrics([<<"global">>, <<"erlang">>], Config),
    ParsedResult = get_ok_value([data, metric, getMetrics], Result),
    Map = maps:from_list([{Name, X} || X = #{<<"name">> := Name} <- ParsedResult]),
    Info = maps:get([<<"global">>, <<"erlang">>, <<"system_info">>], Map),
    %% VMSystemInfoMetric type
    #{<<"type">> := <<"vm_system_info">>} = Info,
    check_metric_by_type(Info),
    ReadsKey = [<<"global">>, <<"backends">>, <<"mod_roster">>, <<"read_roster_version">>],
    %% Other metrics are filtered out
    undef = maps:get(ReadsKey, Map, undef).

get_metrics_by_name_empty_args(Config) ->
    Result = get_metrics([], Config),
    ParsedResult = get_ok_value([data, metric, getMetrics], Result),
    lists:foreach(fun check_metric_by_type/1, ParsedResult),
    [_|_] = ParsedResult.

get_metrics_by_name_empty_string(Config) ->
    Result = get_metrics([<<>>], Config),
    ParsedResult = get_ok_value([data, metric, getMetrics], Result),
    [] = ParsedResult.

get_metrics_by_nonexistent_name(Config) ->
    Result = get_metrics([<<"not_existing">>], Config),
    ParsedResult = get_ok_value([data, metric, getMetrics], Result),
    [] = ParsedResult.

get_metrics_for_specific_host_type(Config) ->
    Result = get_metrics([<<"dummy auth">>], Config),
    ParsedResult = get_ok_value([data, metric, getMetrics], Result),
    lists:foreach(fun check_metric_by_type/1, ParsedResult),
    [_|_] = ParsedResult.

get_process_queue_length(Config) ->
    Result = get_metrics([<<"global">>, <<"processQueueLengths">>], Config),
    ParsedResult = get_ok_value([data, metric, getMetrics], Result),
    Map = maps:from_list([{Name, X} || X = #{<<"name">> := Name} <- ParsedResult]),
    Lens = maps:get([<<"global">>, <<"processQueueLengths">>], Map),
    %% ProbeQueuesMetric type
    #{<<"type">> := <<"probe_queues">>} = Lens,
    check_metric_by_type(Lens).

get_inet_stats(Config) ->
    Result = get_metrics([<<"global">>, <<"data">>, <<"dist">>], Config),
    ParsedResult = get_ok_value([data, metric, getMetrics], Result),
    Map = maps:from_list([{Name, X} || X = #{<<"name">> := Name} <- ParsedResult]),
    Stats = maps:get([<<"global">>, <<"data">>, <<"dist">>], Map),
    %% MergedInetStatsMetric type
    #{<<"type">> := <<"merged_inet_stats">>} = Stats,
    check_metric_by_type(Stats).

get_vm_stats_memory(Config) ->
    Result = get_metrics([<<"global">>], Config),
    ParsedResult = get_ok_value([data, metric, getMetrics], Result),
    Map = maps:from_list([{Name, X} || X = #{<<"name">> := Name} <- ParsedResult]),
    Mem = maps:get([<<"global">>, <<"erlang">>, <<"memory">>], Map),
    %% VMStatsMemoryMetric type
    #{<<"type">> := <<"vm_stats_memory">>} = Mem,
    check_metric_by_type(Mem).

get_cets_system(Config) ->
    Result = get_metrics([<<"global">>, <<"cets">>, <<"system">>], Config),
    ParsedResult = get_ok_value([data, metric, getMetrics], Result),
    [#{<<"type">> := <<"cets_system">>} = Sys] = ParsedResult,
    check_metric_by_type(Sys).

get_all_metrics_as_dicts(Config) ->
    Result = get_metrics_as_dicts(Config),
    ParsedResult = get_ok_value([data, metric, getMetricsAsDicts], Result),
    check_node_result_is_valid(ParsedResult, false).

get_by_name_metrics_as_dicts(Config) ->
    Result = get_metrics_as_dicts_by_name([<<"_">>, <<"xmppStanzaSent">>], Config),
    ParsedResult = get_ok_value([data, metric, getMetricsAsDicts], Result),
    [_|_] = ParsedResult,
    %% Only xmppStanzaSent type
    lists:foreach(fun(#{<<"dict">> := Dict, <<"name">> := [_, <<"xmppStanzaSent">>]}) ->
                          check_spiral_dict(Dict)
            end, ParsedResult).

get_metrics_as_dicts_by_nonexistent_name(Config) ->
    Result = get_metrics_as_dicts_by_name([<<"not_existing">>], Config),
    ParsedResult = get_ok_value([data, metric, getMetricsAsDicts], Result),
    [] = ParsedResult.

get_metrics_as_dicts_with_key_one(Config) ->
    Result = get_metrics_as_dicts_with_keys([<<"one">>], Config),
    ParsedResult = get_ok_value([data, metric, getMetricsAsDicts], Result),
    Map = dict_objects_to_map(ParsedResult),
    SentName = [metric_host_type(), <<"xmppStanzaSent">>],
    [#{<<"key">> := <<"one">>, <<"value">> := One}] = maps:get(SentName, Map),
    ?assert(is_integer(One)).

get_metrics_as_dicts_with_nonexistent_key(Config) ->
    Result = get_metrics_as_dicts_with_keys([<<"not_existing">>], Config),
    ParsedResult = get_ok_value([data, metric, getMetricsAsDicts], Result),
    Map = dict_objects_to_map(ParsedResult),
    SentName = [<<"global">>, <<"data">>, <<"xmpp">>, <<"received">>, <<"xml_stanza_size">>],
    [] = maps:get(SentName, Map).

get_metrics_as_dicts_empty_args(Config) ->
    %% Empty name
    Result = get_metrics_as_dicts([], [<<"median">>], Config),
    ParsedResult = get_ok_value([data, metric, getMetricsAsDicts], Result),
    Map = dict_objects_to_map(ParsedResult),
    SentName = [<<"global">>, <<"data">>, <<"xmpp">>, <<"received">>, <<"xml_stanza_size">>],
    [#{<<"key">> := <<"median">>, <<"value">> := Median}] = maps:get(SentName, Map),
    ?assert(is_integer(Median)),
    %% Empty keys
    Result2 = get_metrics_as_dicts([<<"global">>, <<"erlang">>], [], Config),
    ParsedResult2 = get_ok_value([data, metric, getMetricsAsDicts], Result2),
    ?assertEqual(length(ParsedResult2), 2).

get_metrics_as_dicts_empty_strings(Config) ->
    %% Name is an empty string
    Result = get_metrics_as_dicts([<<>>], [<<"median">>], Config),
    ParsedResult = get_ok_value([data, metric, getMetricsAsDicts], Result),
    [] = ParsedResult,
    %% Key is an empty string
    Result2 = get_metrics_as_dicts([<<"global">>, <<"erlang">>], [<<>>], Config),
    ParsedResult2 = get_ok_value([data, metric, getMetricsAsDicts], Result2),
    [_|_] = ParsedResult2.

get_cluster_metrics(Config) ->
    %% We will have at least these two nodes
    Node1 = atom_to_binary(maps:get(node, distributed_helper:mim())),
    Node2 = atom_to_binary(maps:get(node, distributed_helper:mim2())),
    Result = get_cluster_metrics_as_dicts(Config),
    ParsedResult = get_ok_value([data, metric, getClusterMetricsAsDicts], Result),
    #{Node1 := Res1, Node2 := Res2} = node_objects_to_map(ParsedResult),
    check_node_result_is_valid(Res1, false),
    check_node_result_is_valid(Res2, true).

get_by_name_cluster_metrics_as_dicts(Config) ->
    Result = get_cluster_metrics_as_dicts_by_name([<<"_">>, <<"xmppStanzaSent">>], Config),
    NodeResult = get_ok_value([data, metric, getClusterMetricsAsDicts], Result),
    Map = node_objects_to_map(NodeResult),
    %% Contains data for at least two nodes
    ?assert(maps:size(Map) > 1),
    %% Only xmppStanzaSent type
    maps:map(fun(_Node, [_|_] = NodeRes) ->
        lists:foreach(fun(#{<<"dict">> := Dict,
                            <<"name">> := [_, <<"xmppStanzaSent">>]}) ->
                              check_spiral_dict(Dict)
                end, NodeRes) end, Map).

get_mim2_cluster_metrics(Config) ->
    Node = atom_to_binary(maps:get(node, distributed_helper:mim2())),
    Result = get_cluster_metrics_as_dicts_for_nodes([Node], Config),
    ParsedResult = get_ok_value([data, metric, getClusterMetricsAsDicts], Result),
    [#{<<"node">> := Node, <<"result">> := ResList}] = ParsedResult,
    check_node_result_is_valid(ResList, true).

get_cluster_metrics_for_nonexistent_nodes(Config) ->
    Result = get_cluster_metrics_as_dicts_for_nodes([<<"nonexistent">>], Config),
    ParsedResult = get_ok_value([data, metric, getClusterMetricsAsDicts], Result),
    [#{<<"node">> := _, <<"result">> := ResList}] = ParsedResult,
    [#{<<"dict">> := [], <<"name">> := ErrorResult}] = ResList,
    ?assert(ErrorResult == [<<"error">>, <<"nodedown">>]).

get_cluster_metrics_by_nonexistent_name(Config) ->
    Result = get_cluster_metrics_as_dicts_by_name([<<"nonexistent">>], Config),
    ParsedResult = get_ok_value([data, metric, getClusterMetricsAsDicts], Result),
    [#{<<"node">> := _, <<"result">> := []},
     #{<<"node">> := _, <<"result">> := []}|_] = ParsedResult. %% two or three nodes.

get_cluster_metrics_with_nonexistent_key(Config) ->
    Result = get_cluster_metrics_as_dicts_with_keys([<<"nonexistent">>], Config),
    ParsedResult = get_ok_value([data, metric, getClusterMetricsAsDicts], Result),
    [#{<<"node">> := _, <<"result">> := [_|_]},
     #{<<"node">> := _, <<"result">> := [_|_]}|_] = ParsedResult.

get_cluster_metrics_empty_args(Config) ->
    Node = atom_to_binary(maps:get(node, distributed_helper:mim2())),
    %% Empty name
    Result = get_cluster_metrics_as_dicts([], [<<"one">>], [Node], Config),
    ParsedResult = get_ok_value([data, metric, getClusterMetricsAsDicts], Result),
    [#{<<"node">> := Node, <<"result">> := ResList}] = ParsedResult,
    Map = dict_objects_to_map(ResList),
    SentName = [<<"global">>, <<"xmppStanzaSent">>],
    [#{<<"key">> := <<"one">>, <<"value">> := One}] = maps:get(SentName, Map),
    ?assert(is_integer(One)),
    %% Empty keys
    Result2 = get_cluster_metrics_as_dicts([<<"_">>], [], [Node], Config),
    ParsedResult2 = get_ok_value([data, metric, getClusterMetricsAsDicts], Result2),
    [#{<<"node">> := Node, <<"result">> := ResList2}] = ParsedResult2,
    check_node_result_is_valid(ResList2, true),
    %% Empty nodes
    Result3 = get_cluster_metrics_as_dicts([<<"_">>, <<"erlang">>], [<<"ets_limit">>], [], Config),
    ParsedResult3 = get_ok_value([data, metric, getClusterMetricsAsDicts], Result3),
    NodeMap = node_objects_to_map(ParsedResult3),
    ?assert(maps:size(NodeMap) > 1).

get_cluster_metrics_empty_strings(Config) ->
    Node = atom_to_binary(maps:get(node, distributed_helper:mim2())),
    %% Name is an empty string
    Result = get_cluster_metrics_as_dicts([<<>>], [<<"median">>], [Node], Config),
    ParsedResult = get_ok_value([data, metric, getClusterMetricsAsDicts], Result),
    [#{<<"node">> := Node, <<"result">> := []}] = ParsedResult,
    %% Key is an empty string
    Result2 = get_cluster_metrics_as_dicts([<<"_">>], [<<>>], [Node], Config),
    ParsedResult2 = get_ok_value([data, metric, getClusterMetricsAsDicts], Result2),
    [#{<<"node">> := Node, <<"result">> := [_|_]}] = ParsedResult2,
    %% Node is an empty string
    Result3 = get_cluster_metrics_as_dicts([<<"_">>], [<<"median">>], [<<>>], Config),
    ParsedResult3 = get_ok_value([data, metric, getClusterMetricsAsDicts], Result3),
    [#{<<"node">> := _, <<"result">> := ResList}] = ParsedResult3,
    [#{<<"dict">> := [], <<"name">> := ErrorResult}] = ResList,
    ?assert(ErrorResult == [<<"error">>, <<"nodedown">>]).

check_node_result_is_valid(ResList, MetricsAreGlobal) ->
    %% Check that result contains something
    Map = dict_objects_to_map(ResList),
    SentName = case MetricsAreGlobal of
            true -> [<<"global">>, <<"xmppStanzaSent">>];
            false -> [metric_host_type(), <<"xmppStanzaSent">>]
        end,
    check_spiral_dict(maps:get(SentName, Map)),
    [#{<<"key">> := <<"value">>,<<"value">> := V}] =
        maps:get([<<"global">>,<<"uniqueSessionCount">>], Map),
    ?assert(is_integer(V)),
    HistObjects = maps:get([<<"global">>, <<"data">>, <<"xmpp">>,
                            <<"sent">>, <<"xml_stanza_size">>], Map),
    check_histogram(kv_objects_to_map(HistObjects)).

check_histogram(Map) ->
    Keys = [<<"n">>, <<"mean">>,  <<"min">>,  <<"max">>,  <<"median">>,
            <<"50">>, <<"75">>, <<"90">>, <<"95">>,  <<"99">>, <<"999">>],
    values_are_integers(Map, Keys).

check_histogram_p(Map) ->
    Keys = type_to_keys(<<"histogram">>),
    values_are_integers(Map, Keys).

dict_objects_to_map(List) ->
    KV = [{Name, Dict} || #{<<"name">> := Name, <<"dict">> := Dict} <- List],
    maps:from_list(KV).

node_objects_to_map(List) ->
    KV = [{Name, Value} || #{<<"node">> := Name, <<"result">> := Value} <- List],
    maps:from_list(KV).

kv_objects_to_map(List) ->
    KV = [{Key, Value} || #{<<"key">> := Key, <<"value">> := Value} <- List],
    maps:from_list(KV).

%% Domain admin test cases

domain_admin_get_metrics(Config) ->
    get_unauthorized(get_metrics(Config)).

domain_admin_get_metrics_as_dicts(Config) ->
    get_unauthorized(get_metrics_as_dicts(Config)).

domain_admin_get_metrics_as_dicts_by_name(Config) ->
    get_unauthorized(get_metrics_as_dicts_by_name([<<"_">>], Config)).

domain_admin_get_metrics_as_dicts_with_keys(Config) ->
    get_unauthorized(get_metrics_as_dicts_with_keys([<<"one">>], Config)).

domain_admin_get_cluster_metrics_as_dicts(Config) ->
    get_unauthorized(get_cluster_metrics_as_dicts(Config)).

domain_admin_get_cluster_metrics_as_dicts_by_name(Config) ->
    get_unauthorized(get_cluster_metrics_as_dicts_by_name([<<"_">>], Config)).

domain_admin_get_cluster_metrics_as_dicts_for_nodes(Config) ->
    Node = atom_to_binary(maps:get(node, distributed_helper:mim2())),
    get_unauthorized(get_cluster_metrics_as_dicts_for_nodes([Node], Config)).

%% Admin commands

get_metrics(Config) ->
    execute_command(<<"metric">>, <<"getMetrics">>, #{}, Config).

get_metrics(Name, Config) ->
    Vars = #{<<"name">> => Name},
    execute_command(<<"metric">>, <<"getMetrics">>, Vars, Config).

get_metrics_as_dicts(Config) ->
    execute_command(<<"metric">>, <<"getMetricsAsDicts">>, #{}, Config).

get_metrics_as_dicts(Name, Keys, Config) ->
    Vars = #{<<"name">> => Name, <<"keys">> => Keys},
    execute_command(<<"metric">>, <<"getMetricsAsDicts">>, Vars, Config).

get_metrics_as_dicts_by_name(Name, Config) ->
    Vars = #{<<"name">> => Name},
    execute_command(<<"metric">>, <<"getMetricsAsDicts">>, Vars, Config).

get_metrics_as_dicts_with_keys(Keys, Config) ->
    Vars = #{<<"keys">> => Keys},
    execute_command(<<"metric">>, <<"getMetricsAsDicts">>, Vars, Config).

get_cluster_metrics_as_dicts(Config) ->
    execute_command(<<"metric">>, <<"getClusterMetricsAsDicts">>, #{}, Config).

get_cluster_metrics_as_dicts(Name, Keys, Nodes, Config) ->
    Vars = #{<<"name">> => Name, <<"nodes">> => Nodes, <<"keys">> => Keys},
    execute_command(<<"metric">>, <<"getClusterMetricsAsDicts">>, Vars, Config).

get_cluster_metrics_as_dicts_by_name(Name, Config) ->
    Vars = #{<<"name">> => Name},
    execute_command(<<"metric">>, <<"getClusterMetricsAsDicts">>, Vars, Config).

get_cluster_metrics_as_dicts_for_nodes(Nodes, Config) ->
    Vars = #{<<"nodes">> => Nodes},
    execute_command(<<"metric">>, <<"getClusterMetricsAsDicts">>, Vars, Config).

get_cluster_metrics_as_dicts_with_keys(Keys, Config) ->
    Vars = #{<<"keys">> => Keys},
    execute_command(<<"metric">>, <<"getClusterMetricsAsDicts">>, Vars, Config).

%% Helpers

check_spiral_dict(Dict) ->
    [#{<<"key">> := <<"count">>, <<"value">> := Count},
     #{<<"key">> := <<"one">>, <<"value">> := One}] = Dict,
    ?assert(is_integer(Count)),
    ?assert(is_integer(One)).

values_are_integers(Map, Keys) ->
    case lists:all(fun(Key) -> is_integer(maps:get(Key, Map)) end, Keys) of
        true ->
            ok;
        false ->
            ct:fail({values_are_integers, Keys, Map})
    end.

metric_host_type() ->
    binary:replace(domain_helper:host_type(), <<" ">>, <<"_">>, [global]).

is_cets_enabled() ->
    case rpc(mim(), mongoose_config, lookup_opt, [[internal_databases, cets]]) of
        {ok, _} ->
            true;
        _ ->
            false
    end.
