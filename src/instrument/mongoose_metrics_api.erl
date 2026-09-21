-module(mongoose_metrics_api).

-export([get_metrics/1,
         get_metrics_as_dicts/2,
         get_cluster_metrics_as_dicts/3]).

-include("mongoose_logger.hrl").
-include("mongoose.hrl").

-type name() :: [binary()].
-type key() :: binary().
%% Binary-keyed maps; list elements are wrapped as {ok, Value} | {error, Reason}
-type result_map() :: #{binary() => term()}.
-type metric_result() :: {ok, result_map()} | {error, atom()}.
-type metric_dict_result() :: {ok, result_map()}.
-type metric_node_dict_result() :: {ok, result_map()} | {error, binary()}.

-spec get_metrics(Name :: name()) -> {ok, [metric_result()]}.
get_metrics(Name) ->
    {ok, lists:map(fun make_metric_result/1, get_metric_values(Name))}.

-spec get_metrics_as_dicts(Name :: name(), Keys :: [key()]) ->
    {ok, [metric_dict_result()]}.
get_metrics_as_dicts(Name, Keys) ->
    {ok, [make_metric_dict_result(V, Keys) || V <- get_metric_values(Name)]}.

%% An empty list of nodes means all nodes of the cluster
-spec get_cluster_metrics_as_dicts(Name :: name(), Keys :: [key()],
                                   Nodes :: [node()]) ->
    {ok, [metric_node_dict_result()]}.
get_cluster_metrics_as_dicts(Name, Keys, Nodes) ->
    Nodes2 = prepare_nodes_arg(Nodes),
    Results = get_node_values(prepare_name(Name), Nodes2),
    {ok, [make_node_result(Node, Result, Keys)
          || {Node, Result} <- lists:zip(Nodes2, Results)]}.

%% A name that cannot match any metric is not passed on to exometer
get_metric_values(Name) ->
    case prepare_name(Name) of
        {ok, PrepName} ->
            mongoose_instrument_exometer:get_metric_values(PrepName);
        error ->
            []
    end.

get_node_values({ok, PrepName}, Nodes) ->
    F = fun(Node) ->
            case rpc:call(Node, mongoose_instrument_exometer, get_metric_values, [PrepName]) of
            {badrpc, Reason} ->
                [{[error, Reason], []}];
            Result ->
                Result
            end
        end,
    mongoose_lib:pmap(F, Nodes);
get_node_values(error, Nodes) ->
    [{ok, []} || _Node <- Nodes].

make_node_result(Node, {ok, Values}, Keys) ->
    {ok, #{<<"node">> => Node,
           <<"result">> => [make_metric_dict_result(V, Keys) || V <- Values]}};
make_node_result(Node, Other, _Keys) ->
    ?LOG_ERROR(#{what => metric_get_failed,
                 remote_node => Node, reason => Other}),
    {error, <<"Failed to get metrics">>}.

filter_keys(Dict, []) ->
    Dict;
filter_keys(Dict, Keys) ->
    [KV || KV = {Key, _} <- Dict, lists:member(key_to_binary(Key), Keys)].

%% Datapoint keys are atoms (e.g. count) or integers (e.g. histogram percentile 50)
key_to_binary(Key) when is_atom(Key) ->
    atom_to_binary(Key);
key_to_binary(Key) when is_integer(Key) ->
    integer_to_binary(Key).

prepare_nodes_arg([]) ->
    [node() | nodes()];
prepare_nodes_arg(Nodes) ->
    Nodes.

make_metric_result({Name, Dict}) ->
    case format_dict(Dict) of
        {error, Reason} ->
            {error, Reason};
        Map ->
            {ok, Map#{~"name" => format_name(Name)}}
    end.

make_metric_dict_result({Name, Dict}, Keys) ->
    PreparedName = format_name(Name),
    {ok, #{<<"name">> => PreparedName, <<"dict">> => format_dict_entries(Dict, Keys)}}.

format_dict_entries(Dict, Keys) ->
    [{ok, #{<<"key">> => Key, <<"value">> => Value}}
     || {Key, Value} <- filter_keys(Dict, Keys)].

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
format_dict2(Dict) ->
    ?LOG_ERROR(#{what => unknown_metric_type, dict => Dict}),
    {error, unknown_metric_type}.

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

%% Host types are normalized binaries; metric/module segments become existing atoms.
%% A segment that is neither cannot be a part of any metric name.
-spec prepare_name(name()) -> {ok, [binary() | atom()]} | error.
prepare_name(Segments) ->
    prepare_name(Segments, []).

prepare_name([], Acc) ->
    {ok, lists:reverse(Acc)};
prepare_name([Segment | Rest], Acc) ->
    case prepare_name_segment(Segment) of
        {ok, PreparedSegment} ->
            prepare_name(Rest, [PreparedSegment | Acc]);
        error ->
            error
    end.

prepare_name_segment(S) ->
    case lists:member(S, ?ALL_HOST_TYPES) of
        true ->
            {ok, binary:replace(S, <<" ">>, <<"_">>, [global])};
        false ->
            try {ok, binary_to_existing_atom(S)}
            catch error:badarg -> error
            end
    end.
