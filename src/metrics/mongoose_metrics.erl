%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(mongoose_metrics).

-include("mongoose.hrl").
-include("mongoose_metrics_definitions.hrl").

%% API
-export([init/0,
         init_mongooseim_metrics/0,
         update/3,
         ensure_metric/3,
         get_metric_value/1,
         get_metric_values/1,
         get_metric_value/2,
         sample_metric/1,
         get_host_type_metric_names/1,
         get_global_metric_names/0,
         get_aggregated_values/1,
         remove_host_type_metrics/1,
         remove_all_metrics/0,
         get_report_interval/0
        ]).

-ignore_xref([remove_host_type_metrics/1, get_report_interval/0,
              sample_metric/1, get_metric_value/1]).

-define(PREFIXES, mongoose_metrics_prefixes).
-define(DEFAULT_REPORT_INTERVAL, 60000). %%60s

-type metric_name() :: atom() | list(atom() | binary()).
-type short_metric_type() :: spiral | histogram | counter | gauge.
-type metric_type() :: tuple() | short_metric_type().

%% ---------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------

-spec init() -> ok.
init() ->
    prepare_prefixes(),
    create_data_metrics().

-spec init_mongooseim_metrics() -> ok.
init_mongooseim_metrics() ->
    init_subscriptions().

init_subscriptions() ->
    Reporters = exometer_report:list_reporters(),
    lists:foreach(
        fun({Name, _ReporterPid}) ->
                Interval = get_report_interval(),
                subscribe_to_all(Name, Interval)
        end, Reporters).

-spec update(HostType :: mongooseim:host_type_or_global(), Name :: term() | list(),
             Change :: term()) -> any().
update(HostType, Name, Change) when is_list(Name) ->
    exometer:update(name_by_all_metrics_are_global(HostType, Name), Change);
update(HostType, Name, Change) ->
    update(HostType, [Name], Change).

-spec ensure_metric(mongooseim:host_type_or_global(), metric_name(), metric_type()) ->
    ok | {ok, already_present} | {error, any()}.
ensure_metric(HostType, Metric, Type) when is_tuple(Type) ->
    ensure_metric(HostType, Metric, Type, element(1, Type));
ensure_metric(HostType, Metric, Type) ->
    ensure_metric(HostType, Metric, Type, Type).

get_metric_value(HostType, Name) when is_list(Name) ->
    get_metric_value(name_by_all_metrics_are_global(HostType, Name));
get_metric_value(HostType, Name) ->
    get_metric_value(HostType, [Name]).

get_metric_value(Metric) ->
    exometer:get_value(Metric).

get_metric_values(Metric) when is_list(Metric) ->
    exometer:get_values(Metric);
get_metric_values(HostType) ->
    exometer:get_values([HostType]).

%% Force update a probe metric
sample_metric(Metric) ->
    exometer:sample(Metric).

%% Return metrics that have simple values, i.e. that can be used with get_aggregated_values/1
get_host_type_metric_names(HostType) ->
    HostTypeName = get_host_type_prefix(HostType),
    [MetricName || {[_HostTypeName | MetricName], Type, _} <- exometer:find_entries([HostTypeName]),
                   Type =:= gauge orelse Type =:= counter orelse Type =:= spiral].

get_global_metric_names() ->
    get_host_type_metric_names(global).

get_aggregated_values(Metric) when is_list(Metric) ->
    exometer:aggregate([{{['_' | Metric], '_', '_'}, [], [true]}], [one, count, value]);
get_aggregated_values(Metric) when is_atom(Metric) ->
    get_aggregated_values([Metric]).

remove_host_type_metrics(HostType) ->
    HostTypeName = get_host_type_prefix(HostType),
    lists:foreach(fun remove_metric/1, exometer:find_entries([HostTypeName])).

remove_all_metrics() ->
    persistent_term:erase(?PREFIXES),
    lists:foreach(fun remove_metric/1, exometer:find_entries([])).

%% ---------------------------------------------------------------------
%% Internal functions
%% ---------------------------------------------------------------------

prepare_prefixes() ->
    Prebuilt = maps:from_list([begin
                                   Prefix = make_host_type_prefix(HT),
                                   {Prefix, Prefix}
                               end || HT <- ?ALL_HOST_TYPES ]),
    Prefixes = maps:from_list([ {HT, make_host_type_prefix(HT)}
                                || HT <- ?ALL_HOST_TYPES ]),
    persistent_term:put(?PREFIXES, maps:merge(Prebuilt, Prefixes)).

-spec all_metrics_are_global() -> boolean().
all_metrics_are_global() ->
    mongoose_config:get_opt(all_metrics_are_global).

get_host_type_prefix(global) ->
    global;
get_host_type_prefix(HostType) when is_binary(HostType) ->
    case persistent_term:get(?PREFIXES, #{}) of
        #{HostType := HostTypePrefix} -> HostTypePrefix;
        #{} -> make_host_type_prefix(HostType)
    end.

make_host_type_prefix(HT) when is_binary(HT) ->
    binary:replace(HT, <<" ">>, <<"_">>, [global]).

pick_prefix_by_all_metrics_are_global(HostType) ->
    case all_metrics_are_global() of
        true -> global;
        false -> get_host_type_prefix(HostType)
    end.

pick_by_all_metrics_are_global(WhenGlobal, WhenNot) ->
    case all_metrics_are_global() of
        true -> WhenGlobal;
        false -> WhenNot
    end.

-spec name_by_all_metrics_are_global(HostType :: mongooseim:host_type_or_global(),
                                     Name :: list()) -> FinalName :: list().
name_by_all_metrics_are_global(HostType, Name) ->
    [pick_prefix_by_all_metrics_are_global(HostType) | Name].

get_report_interval() ->
    application:get_env(exometer_core, mongooseim_report_interval,
                        ?DEFAULT_REPORT_INTERVAL).

remove_metric({Name, _, _}) ->
    exometer_admin:delete_entry(Name).

ensure_metric(HostType, Metric, Type, ShortType) when is_atom(Metric) ->
    ensure_metric(HostType, [Metric], Type, ShortType);

ensure_metric(HostType, Metric, Type, ShortType) when is_list(Metric) ->
    %% the split into ShortType and Type is needed because function metrics are
    %% defined as tuples (that is Type), while exometer:info returns only 'function'
    PrefixedMetric = name_by_all_metrics_are_global(HostType, Metric),
    case exometer:info(PrefixedMetric, type) of
        undefined ->
            do_create_metric(PrefixedMetric, Type, []);
        ShortType -> {ok, already_present}
    end.

do_create_metric(PrefixedMetric, ExometerType, ExometerOpts) ->
    case catch exometer:new(PrefixedMetric, ExometerType, ExometerOpts) of
        {'EXIT', {exists, _}} -> {ok, already_present};
        ok -> ok;
        {'EXIT', Error} -> {error, Error}
    end.

create_data_metrics() ->
    lists:foreach(fun(Metric) -> ensure_metric(global, Metric, spiral) end,
        ?GLOBAL_SPIRALS).

start_metrics_subscriptions(Reporter, MetricPrefix, Interval) ->
    [subscribe_metric(Reporter, Metric, Interval)
     || Metric <- exometer:find_entries(MetricPrefix)].

subscribe_metric(Reporter, {Name, counter, _}, Interval) ->
    subscribe_verbose(Reporter, Name, [value], Interval);
subscribe_metric(Reporter, {Name, histogram, _}, Interval) ->
    subscribe_verbose(Reporter, Name, [min, mean, max, median, 95, 99, 999], Interval);
subscribe_metric(Reporter, {Name, _, _}, Interval) ->
    subscribe_verbose(Reporter, Name, default, Interval).

subscribe_verbose(Reporter, Name, Types, Interval) ->
    case exometer_report:subscribe(Reporter, Name, Types, Interval) of
        ok -> ok;
        Other ->
            ?LOG_ERROR(#{what => metrics_subscribe_failed,
                         reporter => Reporter, metric_name => Name,
                         reason => Other}),
            Other
    end.

subscribe_to_all(Reporter, Interval) ->
    HostTypePrefixes = pick_by_all_metrics_are_global([], ?ALL_HOST_TYPES),
    lists:foreach(
      fun(Prefix) ->
              UnspacedPrefix = get_host_type_prefix(Prefix),
              start_metrics_subscriptions(Reporter, [UnspacedPrefix], Interval)
      end, [global | HostTypePrefixes]).
