-module(mongoose_instrument_prometheus).

-behaviour(mongoose_instrument).

-export([start/1, set_up/3, handle_event/4]).

%% Define Prometheus metric-related types, because the library has no type specs
-type spec() :: proplists:proplist().
-type name() :: string().
-type help() :: string().

-spec start(#{}) -> ok.
start(#{}) ->
    Apps = [prometheus, prometheus_httpd, prometheus_cowboy],
    {ok, _} = application:ensure_all_started(Apps, permanent),
    ok.

-spec set_up(mongoose_instrument:event_name(), mongoose_instrument:labels(),
             mongoose_instrument:config()) -> boolean().
set_up(EventName, Labels, #{metrics := Metrics}) ->
    maps:foreach(fun(MetricName, MetricType) ->
                         set_up_metric(EventName, Labels, MetricName, MetricType)
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

-spec set_up_metric(mongoose_instrument:event_name(), mongoose_instrument:labels(),
                    mongoose_instrument:metric_name(), mongoose_instrument:metric_type()) ->
          ok.
set_up_metric(EventName, Labels, MetricName, MetricType) ->
    LabelKeys = labels_to_keys(Labels),
    LabelValues = labels_to_values(Labels),
    MetricSpec = metric_spec(EventName, LabelKeys, MetricName),
    FullName = proplists:get_value(name, MetricSpec),
    case declare_metric(MetricSpec, MetricType) of
        true ->
            ok; %% Metric does not exist - no need to reset
        false ->
            reset_metric(FullName, LabelValues, MetricType)
    end,
    initialize_metric(FullName, LabelValues, MetricType).

-spec declare_metric(proplists:proplist(), mongoose_instrument:metric_type()) -> boolean().
declare_metric(MetricSpec, gauge) ->
    prometheus_gauge:declare(MetricSpec);
declare_metric(MetricSpec, counter) ->
    prometheus_gauge:declare(MetricSpec);
declare_metric(MetricSpec, spiral) ->
    prometheus_counter:declare(MetricSpec);
declare_metric(MetricSpec, histogram) ->
    prometheus_histogram:declare([{buckets, histogram_buckets()} | MetricSpec]).

-spec reset_metric(name(), [mongoose_instrument:label_value()],
                   mongoose_instrument:metric_type()) -> boolean().
reset_metric(Name, LabelValues, gauge) ->
    prometheus_gauge:remove(Name, LabelValues);
reset_metric(Name, LabelValues, counter) ->
    prometheus_gauge:remove(Name, LabelValues);
reset_metric(Name, LabelValues, spiral) ->
    prometheus_counter:remove(Name, LabelValues);
reset_metric(Name, LabelValues, histogram) ->
    prometheus_histogram:remove(Name, LabelValues).

-spec initialize_metric(name(), [mongoose_instrument:label_value()],
                        mongoose_instrument:metric_type()) -> ok.
initialize_metric(Name, LabelValues, spiral) ->
    %% Initialize counter, because it has a meaningful initial value of zero
    %% Additionally, leaving it undefined would delay rate calculation in Prometheus
    prometheus_counter:inc(Name, LabelValues, 0);
initialize_metric(Name, LabelValues, counter) ->
    %% Initialize the gauge, because as a counter it has a meaningful initial value of zero
    prometheus_gauge:inc(Name, LabelValues, 0);
initialize_metric(_Name, _LabelValues, _) ->
    %% Don't initialize, because no meaningful value could be provided
    ok.

-spec metric_spec(mongoose_instrument:event_name(), [mongoose_instrument:label_key()],
                  mongoose_instrument:metric_name()) -> spec().
metric_spec(EventName, LabelKeys, MetricName) ->
    [{name, full_metric_name(EventName, MetricName)},
     {help, metric_help(EventName, MetricName)},
     {labels, LabelKeys},
     {duration_unit, false} % prevent unwanted implicit conversions, e.g. seconds -> microseconds
    ].

-spec histogram_buckets() -> [integer()].
histogram_buckets() ->
    histogram_buckets([], 1 bsl 30). % ~1.07 * 10^9

histogram_buckets(AccBuckets, Val) when Val > 0 ->
    histogram_buckets([Val | AccBuckets], Val bsr 1);
histogram_buckets(AccBuckets, _Val) ->
    AccBuckets.

-spec handle_metric_event(mongoose_instrument:event_name(), [mongoose_instrument:label_value()],
                          mongoose_instrument:metric_name(), mongoose_instrument:metric_type(),
                          mongoose_instrument:measurements()) -> ok.
handle_metric_event(EventName, LabelValues, MetricName, MetricType, Measurements) ->
    case Measurements of
        #{MetricName := MetricValue} ->
            FullName = full_metric_name(EventName, MetricName),
            update_metric(FullName, LabelValues, MetricType, MetricValue);
        #{} ->
            ok
    end.

-spec metric_help(mongoose_instrument:event_name(), mongoose_instrument:metric_name()) -> help().
metric_help(EventName, MetricName) ->
    lists:flatten(io_lib:format("Event: ~p, Metric: ~p", [EventName, MetricName])).

-spec full_metric_name(mongoose_instrument:event_name(), mongoose_instrument:metric_name()) ->
          name().
full_metric_name(EventName, MetricName) ->
    atom_to_list(EventName) ++ "_" ++ atom_to_list(MetricName).

-spec labels_to_keys(mongoose_instrument:labels()) -> [mongoose_instrument:label_key()].
labels_to_keys(Labels) ->
    lists:sort(maps:keys(Labels)).

-spec labels_to_values(mongoose_instrument:labels()) -> [mongoose_instrument:label_value()].
labels_to_values(Labels) ->
    [V || {_K, V} <- lists:keysort(1, maps:to_list(Labels))].

-spec update_metric(name(), [mongoose_instrument:label_value()],
                    mongoose_instrument:metric_type(), integer()) -> ok.
update_metric(Name, Labels, gauge, Value) when is_integer(Value) ->
    ok = prometheus_gauge:set(Name, Labels, Value);
update_metric(Name, Labels, counter, Value) when is_integer(Value) ->
    ok = prometheus_gauge:inc(Name, Labels, Value);
update_metric(Name, Labels, spiral, Value) when is_integer(Value), Value >= 0 ->
    ok = prometheus_counter:inc(Name, Labels, Value);
update_metric(Name, Labels, histogram, Value) when is_integer(Value) ->
    ok = prometheus_histogram:observe(Name, Labels, Value).
