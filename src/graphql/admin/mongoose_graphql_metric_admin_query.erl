-module(mongoose_graphql_metric_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-import(mongoose_graphql_helper, [make_error/3]).

execute(_Ctx, _Obj, <<"getMetrics">>, Args) ->
    Name = get_name(Args),
    mongoose_metrics_api:get_metrics(Name);
execute(_Ctx, _Obj, <<"getMetricsAsDicts">>, Args) ->
    Name = get_name(Args),
    Keys = get_metric_keys(Args),
    mongoose_metrics_api:get_metrics_as_dicts(Name, Keys);
execute(_Ctx, _Obj, <<"getClusterMetricsAsDicts">>, Args) ->
    Name = get_name(Args),
    Keys = get_metric_keys(Args),
    Nodes = get_nodes(Args),
    {ok, Results} = mongoose_metrics_api:get_cluster_metrics_as_dicts(Name, Keys, Nodes),
    {ok, lists:map(fun format_node_result/1, Results)}.

format_node_result({metric_get_failed, Node, Msg}) ->
    make_error(metric_get_failed, Msg, #{node => Node});
format_node_result(Result) ->
    Result.

%% Internal

get_name(Args) ->
    get_list(<<"name">>, Args).

get_metric_keys(Args) ->
    get_list(<<"keys">>, Args).

get_nodes(Args) ->
    get_list(<<"nodes">>, Args).

get_list(Key, Map) ->
    null_as_empty(maps:get(Key, Map, [])).

null_as_empty(null) -> [];
null_as_empty(X) -> X.
