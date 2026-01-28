-module(mongoose_prometheus_sliding_window_collector).

-behaviour(prometheus_collector).

-export([deregister_cleanup/1,
         collect_mf/2]).

%% Collector behavior callbacks

deregister_cleanup(_) ->
    ok.

collect_mf(_Registry, Callback) ->
    %% Get all metric names from the sliding window manager
    MetricNames = mongoose_prometheus_sliding_window:get_all_metric_names(),
    lists:foreach(fun(Name) ->
                      collect_metric_family(Name, Callback)
                  end, MetricNames),
    ok.

%% Internal functions

collect_metric_family(Name, Callback) ->
    Values = mongoose_prometheus_sliding_window:values(Name),
    case Values of
        [] ->
            ok;
        _ ->
            SWName = sliding_window_metric_name(Name),
            MetricSpec = mongoose_prometheus_sliding_window:get_metric_spec(Name),
            Help = case MetricSpec of
                       undefined -> "";
                       Spec -> proplists:get_value(help, Spec, "")
                   end,
            LabelKeys = case MetricSpec of
                            undefined -> [];
                            Spec2 -> proplists:get_value(labels, Spec2, [])
                        end,
            %% Convert to Prometheus format
            Metrics = [create_summary_metric(LabelKeys, LabelValues, Count, Sum, Quantiles)
                       || {LabelValues, {Count, Sum, Quantiles}} <- Values],
            %% Create and send the metric family
            MF = prometheus_model_helpers:create_mf(SWName, Help, summary, Metrics),
            Callback(MF)
    end.

sliding_window_metric_name(Name) -> Name.

create_summary_metric(LabelKeys, LabelValues, Count, Sum, Quantiles) ->
    %% Convert label keys and values to label pairs format
    LabelPairs = lists:zip(LabelKeys, LabelValues),
    QuantilePairs = [{Q, V} || {Q, V} <- Quantiles],
    prometheus_model_helpers:summary_metric(LabelPairs, Count, Sum, QuantilePairs).
