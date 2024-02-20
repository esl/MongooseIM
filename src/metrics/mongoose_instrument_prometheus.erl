-module(mongoose_instrument_prometheus).

-behaviour(mongoose_instrument).

-export([set_up/3, handle_event/4]).

-spec set_up(mongoose_instrument:event_name(), mongoose_instrument:labels(),
            mongoose_instrument:config()) -> boolean().
set_up(EventName, Labels, #{metrics := Metrics}) ->
    LabelKeys = labels_to_keys(Labels),
    maps:foreach(fun(MetricName, MetricType) ->
                         set_up_metric(EventName, LabelKeys, MetricName, MetricType)
                 end, Metrics),
    true;
set_up(_EventName, _Labels, #{}) ->
    false.

-spec handle_event(mongoose_instrument:event_name(), mongoose_instrument:labels(),
                   mongoose_instrument:config(), mongoose_instrument:measurements()) -> ok.
handle_event(EventName, Labels, #{metrics := Metrics}, Measurements) ->
    LabelValues = labels_to_values(Labels),
    maps:foreach(fun(MetricName, MetricType) ->
                         handle_metric_event(EventName, LabelValues, MetricName, MetricType, Measurements)
                 end, Metrics).

set_up_metric(EventName, LabelKeys, MetricName, MetricType) ->
    MetricSpec = metric_spec(EventName, LabelKeys, MetricName),
    declare_metric(MetricSpec, MetricType).

declare_metric(MetricSpec, spiral) ->
    prometheus_counter:declare(MetricSpec);
declare_metric(MetricSpec, histogram) ->
    prometheus_histogram:declare([{buckets, histogram_buckets()} | MetricSpec]).

metric_spec(EventName, LabelKeys, MetricName) ->
    [{name, full_metric_name(EventName, MetricName)},
     {help, metric_help(EventName, MetricName)},
     {labels, LabelKeys}].

histogram_buckets() ->
    histogram_buckets([], 1 bsl 30). % ~1.07 * 10^9

histogram_buckets(AccBuckets, Val) when Val > 0 ->
    histogram_buckets([Val | AccBuckets], Val bsr 1);
histogram_buckets(AccBuckets, _Val) ->
    AccBuckets.

handle_metric_event(EventName, LabelValues, MetricName, MetricType, Measurements) ->
    case Measurements of
        #{MetricName := MetricValue} ->
            FullName = full_metric_name(EventName, MetricName),
            update_metric(FullName, LabelValues, MetricType, MetricValue);
        #{} ->
            ok
    end.

metric_help(EventName, MetricName) ->
    lists:flatten(io_lib:format("Event: ~p, Metric: ~p", [EventName, MetricName])).

full_metric_name(EventName, MetricName) ->
    list_to_atom(atom_to_list(EventName) ++ "_" ++ atom_to_list(MetricName)).

labels_to_keys(Labels) ->
    lists:sort(maps:keys(Labels)).

labels_to_values(Labels) ->
    [V || {_K, V} <- lists:keysort(1, maps:to_list(Labels))].

update_metric(Name, Labels, spiral, Value) when is_integer(Value), Value >= 0 ->
    prometheus_counter:inc(Name, Labels, Value);
update_metric(Name, Labels, histogram, Value) when is_integer(Value) ->
    prometheus_histogram:observe(Name, Labels, Value).
