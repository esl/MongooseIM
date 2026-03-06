-module(mongoose_prometheus_sliding_window_collector).

-behaviour(prometheus_collector).

-export([deregister_cleanup/1,
         collect_mf/2]).

%% Collector behavior callbacks

deregister_cleanup(_) ->
    ok.

collect_mf(_Registry, Callback) ->
    Metrics = mongoose_prometheus_sliding_window:get_all_metrics(),
    lists:foreach(fun(Metric) ->
                      collect_metric_family(Metric, Callback)
                  end, Metrics),
    ok.

%% Internal functions

collect_metric_family({_Name, []}, _Callback) ->
    ok;
collect_metric_family({Name, Values}, Callback) ->
    MetricSpec = mongoose_prometheus_sliding_window:get_metric_spec(Name),
    Help = get_spec_value(MetricSpec, help, ""),
    LabelKeys = get_spec_value(MetricSpec, labels, []),
    %% Convert to Prometheus format
    Metrics = [create_summary_metric(LabelKeys, LabelValues, Count, Sum, Quantiles)
               || {LabelValues, {Count, Sum, Quantiles}} <- Values],
    %% Create and send the metric family
    MF = prometheus_model_helpers:create_mf(Name, Help, summary, Metrics),
    Callback(MF).

get_spec_value(undefined, _Key, Default) -> Default;
get_spec_value(Spec, Key, Default) -> proplists:get_value(Key, Spec, Default).

create_summary_metric(LabelKeys, LabelValues, Count, Sum, Quantiles) ->
    %% Convert label keys and values to label pairs format
    LabelPairs = lists:zip(LabelKeys, LabelValues),
    QuantilePairs = [{Q, V} || {Q, V} <- Quantiles],
    prometheus_model_helpers:summary_metric(LabelPairs, Count, Sum, QuantilePairs).
