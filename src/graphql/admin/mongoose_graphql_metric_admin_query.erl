-module(mongoose_graphql_metric_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

execute(_Ctx, _Obj, <<"getMetrics">>, Args) ->
    Name = get_name(Args),
    mongoose_metrics_api:get_metrics(Name);
execute(_Ctx, _Obj, <<"getMetricsAsDicts">>, Args) ->
    Name = get_name(Args),
    Keys = get_keys2(Args),
    mongoose_metrics_api:get_metrics_as_dicts(Name, Keys);
execute(_Ctx, _Obj, <<"getClusterMetricsAsDicts">>, Args) ->
    Name = get_name(Args),
    Keys = get_keys2(Args),
    Nodes = get_nodes(Args),
    mongoose_metrics_api:get_cluster_metrics_as_dicts(Name, Keys, Nodes).

%% get_keys is a BIF, so we have a name conflict
get_keys2(Args) ->
    Keys = get_list(<<"keys">>, Args),
    lists:map(fun prepare_key/1, Keys).

prepare_key(X) when is_binary(X) ->
    binary_to_atom(X);
prepare_key(X) when is_integer(X) -> %% For percentiles
    X.

get_name(Args) ->
    Segments = get_list(<<"name">>, Args),
    lists:map(fun binary_to_atom/1, Segments).

get_nodes(Args) ->
    Nodes = get_list(<<"nodes">>, Args),
    lists:map(fun binary_to_atom/1, Nodes).

get_list(Key, Map) ->
    null_as_empty(maps:get(Key, Map, [])).

null_as_empty(null) -> [];
null_as_empty(X) -> X.
