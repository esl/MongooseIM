-module(mongoose_metrics_api).
-export([get_metrics/1,
         get_metrics_as_dicts/2,
         get_cluster_metrics_as_dicts/3]).

-include("mongoose_logger.hrl").
-include("mongoose.hrl").

-type name() :: [atom() | integer()].
-type key() :: atom().
-type metric_result() ::
    {ok, #{binary() => binary() | non_neg_integer()}}
    | {error, atom()}.
-type dict_result() :: #{binary() => binary() | non_neg_integer()}.
-type metric_dict_result() ::
    {ok, #{binary() => binary() | [dict_result()]}}.
-type metric_node_dict_result() ::
    {ok, #{binary() => binary() | [metric_dict_result()]}}
    | {error, binary()}.

-spec get_metrics(Name :: name()) -> {ok, [metric_result()]}.
get_metrics(Name) ->
    PrepName = prepare_host_types(Name),
    Values = mongoose_instrument_exometer:get_metric_values(PrepName),
    {ok, lists:map(fun make_metric_result/1, Values)}.

-spec get_metrics_as_dicts(Name :: name(), Keys :: [key()]) ->
    {ok, [metric_dict_result()]}.
get_metrics_as_dicts(Name, Keys) ->
    PrepName = prepare_host_types(Name),
    Values = mongoose_instrument_exometer:get_metric_values(PrepName),
    {ok, [make_metric_dict_result(V, Keys) || V <- Values]}.

-spec get_cluster_metrics_as_dicts(Name :: name(), Keys :: [key()],
                                   Nodes :: [node()]) ->
    {ok, [metric_node_dict_result()]}.
get_cluster_metrics_as_dicts(Name, Keys, Nodes) ->
    PrepName = prepare_host_types(Name),
    Nodes2 = prepare_nodes_arg(Nodes),
    F = fun(Node) ->
            case rpc:call(Node, mongoose_instrument_exometer, get_metric_values, [PrepName]) of
            {badrpc, Reason} ->
                [{[error, Reason], []}];
            Result ->
                Result
            end
        end,
    Results = mongoose_lib:pmap(F, Nodes2),
    {ok, [make_node_result(Node, Result, Keys)
          || {Node, Result} <- lists:zip(Nodes2, Results)]}.

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
    [KV || KV = {Key, _} <- Dict, lists:member(Key, Keys)].

prepare_nodes_arg([]) ->
    [node()|nodes()];
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

prepare_host_types(Name) ->
    lists:map(
        fun(Ele) ->
            case lists:member(atom_to_binary(Ele), ?ALL_HOST_TYPES) of
                true ->
                    binary:replace(atom_to_binary(Ele), <<" ">>, <<"_">>);
                false ->
                    Ele
            end
        end,
    Name).
