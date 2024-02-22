-module(mongoose_instrument_exometer).

-behaviour(mongoose_instrument).

-export([set_up/3, handle_event/4]).

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
    maps:foreach(fun(MetricName, MetricType) ->
                         handle_metric_event(EventName, Labels, MetricName, MetricType, Measurements)
                 end, Metrics).

set_up_metric(EventName, Labels, MetricName, MetricType) ->
    %% TODO improve handling of already existing metrics
    Name = exometer_metric_name(EventName, Labels, MetricName),
    catch exometer:new(Name, MetricType).

handle_metric_event(EventName, Labels, MetricName, MetricType, Measurements) ->
    case Measurements of
        #{MetricName := MetricValue} ->
            Name = exometer_metric_name(EventName, Labels, MetricName),
            update_metric(Name, MetricType, MetricValue);
        #{} ->
            ok
    end.

update_metric(Name, spiral, Value) when is_integer(Value), Value >= 0 ->
    exometer:update(Name, Value);
update_metric(Name, histogram, Value) when is_integer(Value) ->
    exometer:update(Name, Value).

%% This logic will need extending if we add more labels
exometer_metric_name(EventName, #{host_type := HostType}, MetricName) ->
    [mongoose_metrics:get_host_type_prefix(HostType), EventName, MetricName].
