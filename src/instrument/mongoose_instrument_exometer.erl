-module(mongoose_instrument_exometer).

-behaviour(mongoose_instrument).

-define(PREFIXES, {?MODULE, prefixes}).

-export([config_spec/0, start/1, stop/1, set_up/3, handle_event/4]).

%% Config spec callbacks
-export([process_graphite_reporter/2]).

%% Used by metrics API
-export([get_metric_values/1, get_metric_value/2, get_aggregated_values/1,
         get_host_type_metric_names/1, get_global_metric_names/0]).

-ifdef(TEST).
-export([exometer_metric_name/3]).
-endif.

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

-type opts() :: #{all_metrics_are_global := boolean(),
                  report := #{reporter_name() => reporter_opts()}}.
-type reporter_name() :: atom(). %% exometer_report:reporter_name() is not exported
-type reporter_opts() :: #{module := module(),
                           host := string(),
                           port := inet:port_number(),
                           connect_timeout := pos_integer(),
                           prefix => string(),
                           env_prefix => string(),
                           api_key := string(),
                           interval := pos_integer()}.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{<<"all_metrics_are_global">> => #option{type = boolean},
                       <<"report">> => report_config_spec()},
             defaults = #{<<"all_metrics_are_global">> => false}
            }.

-spec report_config_spec() -> mongoose_config_spec:config_section().
report_config_spec() ->
    Reporters = [<<"graphite">>],
    Common = common_reporter_config_spec(),
    ReporterSpecs = [{Reporter, #list{items = reporter_config_spec(Common, Reporter), wrap = none}}
                     || Reporter <- Reporters],
    #section{items = maps:from_list(ReporterSpecs),
             include = always}.

-spec reporter_config_spec(mongoose_config_spec:config_section(), binary()) ->
          mongoose_config_spec:config_section().
reporter_config_spec(BaseSpec, Reporter) ->
    mongoose_config_utils:merge_sections(BaseSpec, reporter_config_spec(Reporter)).

-spec reporter_config_spec(binary()) -> mongoose_config_spec:config_section().
reporter_config_spec(<<"graphite">>) ->
    #section{items = #{<<"host">> => #option{type = string, validate = network_address},
                       <<"port">> => #option{type = integer, validate = port},
                       <<"connect_timeout">> => #option{type = integer, validate = positive},
                       <<"prefix">> => #option{type = string},
                       <<"env_prefix">> => #option{type = string},
                       <<"api_key">> => #option{type = string}},
             defaults = #{<<"port">> => 2003,
                          <<"connect_timeout">> => 5000, % milliseconds
                          <<"api_key">> => ""},
             required = [<<"host">>],
             process = fun ?MODULE:process_graphite_reporter/2}.

-spec common_reporter_config_spec() -> mongoose_config_spec:config_section().
common_reporter_config_spec() ->
    #section{items = #{<<"interval">> => #option{type = integer, validate = positive}},
             defaults = #{<<"interval">> => 60000} % milliseconds
            }.

-spec process_graphite_reporter(mongoose_config_parser_toml:path(), map()) ->
          {reporter_name(), reporter_opts()}.
process_graphite_reporter(_Path, #{host := Host, port := Port} = Opts) ->
    Name = list_to_atom(lists:flatten(io_lib:format("graphite:~s:~p", [Host, Port]))),
    {Name, Opts#{module => exometer_report_graphite}}.

-spec start(opts()) -> ok.
start(#{all_metrics_are_global := AllGlobal, report := Reporters}) ->
    Apps = [exometer_core, exometer_report_graphite, exometer_report_statsd],
    {ok, _} = application:ensure_all_started(Apps, permanent),
    Prefixes = [{HostType, make_host_type_prefix(HostType, AllGlobal)}
                || HostType <- ?ALL_HOST_TYPES],
    persistent_term:put(?PREFIXES, maps:from_list(Prefixes)),
    maps:foreach(fun add_reporter/2, Reporters).

-spec stop(opts()) -> ok.
stop(#{report := Reporters}) ->
    maps:foreach(fun remove_reporter/2, Reporters),
    persistent_term:erase(?PREFIXES),
    ok.

-spec add_reporter(reporter_name(), reporter_opts()) -> ok.
add_reporter(Name, Opts) ->
    ok = exometer_report:add_reporter(Name, maps:to_list(maps:remove(interval, Opts))).

-spec remove_reporter(reporter_name(), reporter_opts()) -> ok.
remove_reporter(Name, _Opts) ->
    ok = exometer_report:remove_reporter(Name).

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
    end,
    subscribe_reporters(Name, MetricType).

-spec subscribe_reporters(exometer:name(), exometer:type()) -> ok.
subscribe_reporters(Name, Type) ->
    maps:foreach(
      fun(ReporterName, #{interval := Interval}) ->
              ok = exometer_report:subscribe(ReporterName, Name, datapoints(Type), Interval)
      end, mongoose_config:get_opt([instrumentation, exometer, report])).

datapoints(counter) -> [value];
datapoints(histogram) -> [min, mean, max, median, 95, 99, 999];
datapoints(_) -> default.

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
update_metric(Name, counter, Value) when is_integer(Value) ->
    ok = exometer:update(Name, Value);
update_metric(Name, spiral, Value) when is_integer(Value), Value >= 0 ->
    ok = exometer:update(Name, Value);
update_metric(Name, histogram, Value) when is_integer(Value) ->
    ok = exometer:update(Name, Value).

-spec exometer_metric_name(mongoose_instrument:event_name(), mongoose_instrument:labels(),
                           mongoose_instrument:metric_name()) -> exometer:name().
exometer_metric_name(EventName, Labels, MetricName) ->
    HostPrefix = get_host_type_prefix(Labels),
    RemainingLabels = maps:without([host_type], Labels),
    ExometerLabels = exometer_labels(RemainingLabels),
    [HostPrefix, EventName] ++ ExometerLabels  ++ [MetricName].

%% This logic will need extending if we add more labels
exometer_labels(#{function := Function} = Labels) when map_size(Labels) =:= 1 ->
    [Function];
exometer_labels(#{pool_tag := PoolTag} = Labels) when map_size(Labels) =:= 1 ->
    [PoolTag];
exometer_labels(#{pool_id := PoolId} = Labels) when map_size(Labels) =:= 1 ->
    [PoolId];
exometer_labels(#{connection_type := ConnectionType} = Labels) when map_size(Labels) =:= 1 ->
    [ConnectionType];
exometer_labels(#{cache_name := CacheName} = Labels) when map_size(Labels) =:= 1 ->
    [CacheName];
exometer_labels(#{} = Labels) when map_size(Labels) =:= 0 ->
    [];
exometer_labels(#{} = Labels) ->
    ?LOG_WARNING(#{what => default_exometer_labels_conversion, labels => Labels}),
    [V || {_K, V} <- lists:keysort(1, maps:to_list(Labels))].

-spec get_host_type_prefix(mongoose_instrument:labels()) -> mongooseim:host_type_or_global().
get_host_type_prefix(#{host_type := HostType}) when HostType =/= <<>> ->
    case persistent_term:get(?PREFIXES, #{}) of
        #{HostType := Prefix} -> Prefix;
        #{} -> make_host_type_prefix(HostType, all_metrics_are_global())
    end;
get_host_type_prefix(#{}) ->
    global.

-spec make_host_type_prefix(mongooseim:host_type(), boolean()) -> mongooseim:host_type_or_global().
make_host_type_prefix(_HostType, true) ->
    global;
make_host_type_prefix(HostType, false) ->
    binary:replace(HostType, <<" ">>, <<"_">>, [global]).

get_metric_value(global, Name) when is_list(Name) ->
    get_metric_value([get_host_type_prefix(#{}) | Name]);
get_metric_value(HostType, Name) when is_list(Name) ->
    get_metric_value([get_host_type_prefix(#{host_type => HostType}) | Name]);
get_metric_value(HostType, Name) ->
    get_metric_value(HostType, [Name]).

get_metric_value(Metric) ->
    exometer:get_value(Metric).

get_metric_values(Metric) when is_list(Metric) ->
    exometer:get_values(Metric);
get_metric_values(HostType) ->
    exometer:get_values([HostType]).

get_aggregated_values(Metric) when is_list(Metric) ->
    exometer:aggregate([{{['_' | Metric], '_', '_'}, [], [true]}], [one, count, value]);
get_aggregated_values(Metric) when is_atom(Metric) ->
    get_aggregated_values([Metric]).

get_host_type_metric_names(HostType) ->
    case all_metrics_are_global() of
        false ->
            HostTypeName = get_host_type_prefix(#{host_type => HostType}),
            get_metric_names(HostTypeName);
        true ->
            []
    end.

get_global_metric_names() ->
    get_metric_names(global).

get_metric_names(HostTypeName) ->
    [MetricName || {[_HostTypeName | MetricName], Type, _} <- exometer:find_entries([HostTypeName]),
        Type =:= gauge orelse Type =:= counter orelse Type =:= spiral].

-spec all_metrics_are_global() -> boolean().
all_metrics_are_global() ->
    mongoose_config:get_opt([instrumentation, exometer, all_metrics_are_global]).
