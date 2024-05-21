-module(mongoose_instrument_exometer).

-behaviour(mongoose_instrument).

-define(PREFIXES, {?MODULE, prefixes}).

-export([config_spec/0, start/0, stop/0, set_up/3, handle_event/4]).

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{<<"all_metrics_are_global">> => #option{type = boolean}},
             defaults = #{<<"all_metrics_are_global">> => false}}.

-spec start() -> ok.
start() ->
    AllGlobal = mongoose_config:get_opt([instrumentation, exometer, all_metrics_are_global]),
    Prefixes = [{HostType, make_host_type_prefix(HostType, AllGlobal)}
                || HostType <- ?ALL_HOST_TYPES],
    persistent_term:put(?PREFIXES, maps:from_list(Prefixes)).

-spec stop() -> ok.
stop() ->
    persistent_term:erase(?PREFIXES),
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
    maps:foreach(fun(MetricName, MetricType) ->
                         handle_metric_event(EventName, Labels, MetricName, MetricType, Measurements)
                 end, Metrics).

-spec set_up_metric(mongoose_instrument:event_name(), mongoose_instrument:labels(),
                    mongoose_instrument:metric_name(), mongoose_instrument:metric_type()) ->
          ok.
set_up_metric(EventName, Labels, MetricName, MetricType) ->
    Name = exometer_metric_name(EventName, Labels, MetricName),
    try exometer:new(Name, MetricType)
    catch
        error:exists -> ok = exometer:reset(Name)
    end.

-spec handle_metric_event(mongoose_instrument:event_name(), mongoose_instrument:labels(),
                          mongoose_instrument:metric_name(), mongoose_instrument:metric_type(),
                          mongoose_instrument:measurements()) -> ok.
handle_metric_event(EventName, Labels, MetricName, MetricType, Measurements) ->
    case Measurements of
        #{MetricName := MetricValue} ->
            Name = exometer_metric_name(EventName, Labels, MetricName),
            update_metric(Name, MetricType, MetricValue);
        #{} ->
            ok
    end.

-spec update_metric(exometer:name(), spiral | histogram, integer()) -> ok.
update_metric(Name, gauge, Value) when is_integer(Value) ->
    ok = exometer:update(Name, Value);
update_metric(Name, spiral, Value) when is_integer(Value), Value >= 0 ->
    ok = exometer:update(Name, Value);
update_metric(Name, histogram, Value) when is_integer(Value) ->
    ok = exometer:update(Name, Value).

-spec exometer_metric_name(mongoose_instrument:event_name(), mongoose_instrument:labels(),
                           mongoose_instrument:metric_name()) -> exometer:name().
exometer_metric_name(EventName, Labels, MetricName) ->
    [get_host_type_prefix(Labels), EventName] ++ exometer_labels(Labels) ++ [MetricName].

%% This logic will need extending if we add more labels
exometer_labels(#{function := Function}) ->
    [Function];
exometer_labels(#{}) ->
    [].

-spec get_host_type_prefix(mongoose_instrument:labels()) -> mongooseim:host_type_or_global().
get_host_type_prefix(#{host_type := HostType}) ->
    #{HostType := Prefix} = persistent_term:get(?PREFIXES),
    Prefix;
get_host_type_prefix(#{}) ->
    global.

-spec make_host_type_prefix(mongooseim:host_type(), boolean()) -> mongooseim:host_type_or_global().
make_host_type_prefix(_HostType, true) ->
    global;
make_host_type_prefix(HostType, false) ->
    binary:replace(HostType, <<" ">>, <<"_">>, [global]).
