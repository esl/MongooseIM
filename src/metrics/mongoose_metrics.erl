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

%% API
-export([init/0,
         get_metric_value/1,
         get_metric_values/1,
         get_metric_value/2,
         sample_metric/1,
         get_host_type_metric_names/1,
         get_global_metric_names/0,
         get_aggregated_values/1,
         remove_host_type_metrics/1,
         remove_all_metrics/0
        ]).

-ignore_xref([remove_host_type_metrics/1, sample_metric/1, get_metric_value/1]).

-define(PREFIXES, mongoose_metrics_prefixes).

%% ---------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------

-spec init() -> ok.
init() ->
    prepare_prefixes().

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

-spec name_by_all_metrics_are_global(HostType :: mongooseim:host_type_or_global(),
                                     Name :: list()) -> FinalName :: list().
name_by_all_metrics_are_global(HostType, Name) ->
    [pick_prefix_by_all_metrics_are_global(HostType) | Name].

remove_metric({Name, _, _}) ->
    exometer_admin:delete_entry(Name).
