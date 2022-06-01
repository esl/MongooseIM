-module(mongoose_graphql_metric_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose_logger.hrl").

-import(mongoose_graphql_helper, [make_error/2, format_result/2]).

-type metric_result() :: term().

execute(_Ctx, _Obj, <<"getMetrics">>, Args) ->
    get_metrics(Args);
execute(_Ctx, _Obj, <<"getMetricsAsDicts">>, Args) ->
    get_metrics_as_dicts(Args);
execute(_Ctx, _Obj, <<"getClusterMetricsAsDicts">>, Args) ->
    get_cluster_metrics_as_dicts(Args).

-spec get_metrics(mongoose_graphql:args()) ->
    {ok, [metric_result()]} | {error, resolver_error()}.
get_metrics(Args) ->
    Name = maps:get(<<"name">>, Args, []),
    Values = exometer:get_values(prepare_name(Name)),
    {ok, lists:map(fun make_metric_result/1, Values)}.

get_metrics_as_dicts(Args) ->
    Name = maps:get(<<"name">>, Args, []),
    Keys = prepare_keys(maps:get(<<"keys">>, Args, null)),
    Values = exometer:get_values(prepare_name(Name)),
    {ok, [make_metric_dict_result(V, Keys) || V <- Values]}.

get_cluster_metrics_as_dicts(Args) ->
    Name = maps:get(<<"name">>, Args, []),
    PrepName = prepare_name(Name),
    Keys = prepare_keys(maps:get(<<"keys">>, Args, null)),
    Nodes = prepare_nodes(maps:get(<<"nodes">>, Args, null)),
    AllNodes = [node()|nodes()],
    F = fun(Node) -> rpc:call(Node, exometer, get_values, [PrepName]) end,
    FilteredNodes = filter_nodes(AllNodes, Nodes),
    Results = mongoose_lib:pmap(F, FilteredNodes),
    Zip = lists:zip(FilteredNodes, Results),
    {ok, [make_node_result(Node, Result, Keys) || {Node, Result} <- Zip]}.

make_node_result(Node, {ok, Values}, Keys) ->
    {ok, #{<<"node">> => Node,
           <<"result">> => [make_metric_dict_result(V, Keys) || V <- Values]}};
make_node_result(Node, Other, _Keys) ->
    ?LOG_ERROR(#{what => metric_get_failed,
                 remote_node => Node, reason => Other}),
    {error, <<"Failed to get metrics">>}.

prepare_keys([]) ->
    null;
prepare_keys(null) ->
    null;
prepare_keys(Keys) ->
    lists:map(fun prepare_key/1, Keys).

prepare_nodes([]) ->
    null;
prepare_nodes(null) ->
    null;
prepare_nodes(Nodes) ->
    lists:map(fun binary_to_atom/1, Nodes).

filter_nodes(AllNodes, null) ->
    AllNodes;
filter_nodes(AllNodes, AllowedNodes) ->
    [Node || Node <- AllNodes, lists:member(Node, AllowedNodes)].

prepare_key(X) when is_binary(X) ->
    binary_to_atom(X);
prepare_key(X) when is_integer(X) -> %% For percentiles
    X.

prepare_name(null) ->
    [];
prepare_name([<<"global">> | T]) ->
    [global | prepare_name2(T)];
prepare_name([H | T]) ->
    [binary_to_atom(H) | prepare_name2(T)];
prepare_name([]) ->
    [].

prepare_name2(Segments) ->
    lists:map(fun binary_to_atom/1, Segments).

make_metric_result({Name, Dict}) ->
    PreparedName = format_name(Name),
    Map = format_dict(Dict),
    {ok, Map#{<<"name">> => PreparedName}}.

make_metric_dict_result({Name, Dict}, Keys) ->
    PreparedName = format_name(Name),
    {ok, #{<<"name">> => PreparedName, <<"dict">> => format_dict_entries(Dict, Keys)}}.

format_dict_entries(Dict, Keys) ->
    [{ok, #{<<"key">> => Key, <<"value">> => Value}}
     || {Key, Value} <- filter_keys(Dict, Keys)].

filter_keys(Dict, null) ->
    Dict;
filter_keys(Dict, Keys) ->
    [KV || KV = {Key, _} <- Dict, lists:member(Key, Keys)].

format_name(Name) ->
    lists:map(fun format_name_segment/1, Name).

format_name_segment(Segment) when is_atom(Segment) ->
    {ok, atom_to_binary(Segment)};
format_name_segment(Segment) when is_binary(Segment) ->
    {ok, Segment}.

format_dict(Dict) ->
    format_dict2(maps:from_list(Dict)).

format_dict2(#{one := _} = Dict) ->
    format_spiral(Dict);
format_dict2(#{ms_since_reset := _} = Dict) ->
    format_counter(Dict);
format_dict2(#{value := _} = Dict) ->
    format_gauge(Dict);
format_dict2(#{median := _} = Dict) ->
    format_histogram(Dict);
format_dict2(#{connections := _, recv_cnt := _} = Dict) ->
    format_merged_inet_stats(Dict);
format_dict2(#{processes_used := _} = Dict) ->
    format_vm_stats_memory(Dict);
format_dict2(#{port_count := _} = Dict) ->
    format_vm_system_info(Dict);
format_dict2(#{fsm := _, regular := _} = Dict) ->
    format_probe_queues(Dict).

format_spiral(#{one := One, count := Count}) ->
    #{<<"type">> => <<"spiral">>, <<"one">> => One, <<"count">> => Count}.

format_counter(#{value := Value, ms_since_reset := MS}) ->
    #{<<"type">> => <<"counter">>, <<"value">> => Value, <<"ms_since_reset">> => MS}.

format_gauge(#{value := Value}) ->
    #{<<"type">> => <<"gauge">>, <<"value">> => Value}.

format_histogram(#{n := N, mean := Mean, min := Min, max := Max, median := Median,
                   50 := P50, 75 := P75, 90 := P90, 95 := P95,
                   99 := P99, 999 := P999}) ->
    #{<<"type">> => <<"histogram">>, <<"n">> => N, <<"mean">> => Mean,
      <<"min">> => Min, <<"max">> => Max, <<"median">> => Median,
      <<"p50">> => P50, <<"p75">> => P75, <<"p90">> => P90, <<"p95">> => P95,
      <<"p99">> => P99, <<"p999">> => P999}.

format_merged_inet_stats(#{connections := Cons,
                           recv_cnt := RCnt, recv_max := RMax, recv_oct := ROct,
                           send_cnt := SCnt, send_max := SMax, send_oct := SOct,
                           send_pend := SPend}) ->
    %% Metrics from a pool of connections
    #{<<"type">> => <<"merged_inet_stats">>, <<"connections">> => Cons,
      <<"recv_cnt">> => RCnt, recv_max => RMax, recv_oct => ROct,
      <<"send_cnt">> => SCnt, send_max => SMax, send_oct => SOct,
      <<"send_pend">> => SPend}.

format_vm_stats_memory(#{total := Total, processes_used := P,
                         atom_used := A, binary := B, ets := E, system := S}) ->
    #{<<"type">> => <<"vm_stats_memory">>,
      <<"total">> => Total, <<"processes_used">> => P, <<"atom_used">> => A,
      <<"binary">> => B, <<"ets">> => E, <<"system">> => S}.

format_vm_system_info(#{port_count := PortCount, port_limit := PortLimit,
                        process_count := ProcessCount, process_limit := ProcessLimit,
                        ets_limit := EtsLimit}) ->
    #{<<"type">> => <<"vm_system_info">>,
      <<"port_count">> => PortCount, <<"port_limit">> => PortLimit,
      <<"process_count">> => ProcessCount, <<"process_limit">> => ProcessLimit,
      <<"ets_limit">> => EtsLimit}.

format_probe_queues(#{fsm := FSM, regular := Regular, total := Total}) ->
    #{<<"type">> => <<"probe_queues">>,
      <<"fsm">> => FSM, <<"regular">> => Regular, <<"total">> => Total}.
