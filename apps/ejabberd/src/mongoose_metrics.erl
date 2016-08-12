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

-include("ejabberd.hrl").
-include("mongoose_metrics_definitions.hrl").

%% API
-export([init/0,
         create_global_metrics/0,
         init_predefined_host_metrics/1,
         init_subscriptions/0,
         create_generic_hook_metric/2,
         update/3,
         ensure_metric/3,
         start_host_metrics_subscriptions/3,
         start_vm_metrics_subscriptions/2,
         start_global_metrics_subscriptions/2,
         start_data_metrics_subscriptions/2,
         start_backend_metrics_subscriptions/2,
         get_metric_value/1,
         get_metric_values/1,
         get_host_metric_names/1,
         get_global_metric_names/0,
         get_aggregated_values/1,
         increment_generic_hook_metric/2,
         get_odbc_data_stats/0,
         get_odbc_mam_async_stats/0,
         get_dist_data_stats/0,
         get_up_time/0,
         remove_host_metrics/1,
         remove_all_metrics/0]).

-define(DEFAULT_REPORT_INTERVAL, 60000). %%60s

-type proper_counter_name() :: [ejabberd:server() | atom(), ...].

%% ---------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------

-spec init() -> ok.
init() ->
    create_global_metrics(),
    lists:foreach(
        fun(Host) ->
            mongoose_metrics:init_predefined_host_metrics(Host)
        end, ?MYHOSTS),
    init_subscriptions().

create_global_metrics() ->
    lists:foreach(fun({Metric, FunSpec, DataPoints}) ->
        FunSpecTuple = list_to_tuple(FunSpec ++ [DataPoints]),
        catch exometer:new(Metric, FunSpecTuple)
    end, ?VM_STATS),
    lists:foreach(fun({Metric, Spec}) -> ensure_metric(undefined, Metric, Spec) end,
                  ?GLOBAL_COUNTERS),
    create_data_metrics().

-spec init_predefined_host_metrics(ejabberd:lserver()) -> ok.
init_predefined_host_metrics(Host) ->
    create_metrics(Host),
    metrics_hooks(add, Host),
    ok.

init_subscriptions() ->
    Reporters = exometer_report:list_reporters(),
    lists:foreach(
        fun({Name, _ReporterPid}) ->
                Interval = get_report_interval(),
                subscribe_to_all(Name, Interval)
        end, Reporters).

-spec create_generic_hook_metric(ejabberd:lserver(), atom()) -> ok | {ok, already_present}.
create_generic_hook_metric(Host, Hook) ->
    FilteredHookName = filter_hook(Hook),
    do_create_generic_hook_metric([Host, FilteredHookName], [global, FilteredHookName]).

-spec update({term(), term()} | list(), {term(), term()} | list(), term()) -> any().
update(NamePerHost, NameGlobal, Change) when is_tuple(NamePerHost)->
    update(tuple_to_list(NamePerHost), NameGlobal, Change);
update(NamePerHost, NameGlobal, Change) when is_tuple(NameGlobal)->
    update(NamePerHost, tuple_to_list(NameGlobal), Change);
update(NamePerHost, NameGlobal, Change) ->
    exometer:update(name_by_all_metrics_global(NamePerHost, NameGlobal), Change).

-spec ensure_metric(list() | undefined, list(), term()) -> ok | {error, term()}.
ensure_metric(MetricPerHost, MetricGlobal, Type) when is_tuple(Type) ->
    ensure_metric(MetricPerHost, MetricGlobal, Type, element(1, Type));
ensure_metric(MetricPerHost, MetricGlobal, Type) ->
    ensure_metric(MetricPerHost, MetricGlobal, Type, Type).

start_host_metrics_subscriptions(Reporter, Host, Interval) ->
    Prefix = case all_metrics_are_global() of
                 true -> [global];
                 _ -> [Host]
             end,
    do_start_metrics_subscriptions(Reporter, Interval, Prefix).

start_vm_metrics_subscriptions(Reporter, Interval) ->
    do_start_vm_metrics_subscriptions(Reporter, Interval).

start_global_metrics_subscriptions(Reporter, Interval) ->
    do_start_global_metrics_subscriptions(Reporter, Interval).

start_data_metrics_subscriptions(Reporter, Interval) ->
    do_start_metrics_subscriptions(Reporter, Interval, [data, xmpp]).

start_backend_metrics_subscriptions(Reporter, Interval) ->
    do_start_metrics_subscriptions(Reporter, Interval, [backends]).

get_metric_value({Host, Name}) ->
    get_metric_value([Host, Name]);
get_metric_value(Metric) ->
    exometer:get_value(Metric).

get_metric_values(Metric) when is_list(Metric) ->
    exometer:get_values(Metric);
get_metric_values(Host) ->
    exometer:get_values([Host]).

get_host_metric_names(Host) ->
    [MetricName || {[_Host | MetricName], _, _} <- exometer:find_entries([Host])].

get_global_metric_names() ->
    get_host_metric_names(global).

get_aggregated_values(Metric) ->
    exometer:aggregate([{{['_',Metric],'_','_'},[],[true]}], [one, count, value]).

-spec increment_generic_hook_metric(ejabberd:lserver(), atom()) -> ok | {error, any()}.
increment_generic_hook_metric(Host, Hook) ->
    FilteredHook = filter_hook(Hook),
    do_increment_generic_hook_metric([Host, FilteredHook], [global, FilteredHook]).

get_odbc_data_stats() ->
    RegularODBCWorkers = [catch ejabberd_odbc_sup:get_pids(Host) || Host <- ?MYHOSTS],
    get_odbc_stats(lists:flatten(RegularODBCWorkers)).

get_odbc_mam_async_stats() ->
    %% MAM async ODBC workers are organized differently...
    MamAsynODBCWorkers = [catch element(2, gen_server:call(Pid, get_connection, 1000)) || {_, Pid, worker, _} <- supervisor:which_children(mod_mam_sup)],
    get_odbc_stats(MamAsynODBCWorkers).

get_dist_data_stats() ->
    DistStats = [inet_stats(Port) || {_, Port} <- erlang:system_info(dist_ctrl)],
    [{connections, length(DistStats)} | merge_stats(DistStats)].

-spec get_up_time() -> {value, integer()}.
get_up_time() ->
    {value, erlang:round(element(1, erlang:statistics(wall_clock))/1000)}.

remove_host_metrics(Host) ->
    lists:foreach(fun remove_metric/1,
                  exometer:find_entries([Host])).

remove_all_metrics() ->
    lists:foreach(fun remove_metric/1,
                  exometer:find_entries([])).

%% ---------------------------------------------------------------------
%% Internal functions
%% ---------------------------------------------------------------------

-spec all_metrics_are_global() -> boolean() | undefined.
all_metrics_are_global() ->
    ejabberd_config:get_local_option(all_metrics_are_global).

-spec name_by_all_metrics_global(NamePerHost :: list() | undefined, NameGlobal :: list()) -> list().
name_by_all_metrics_global(undefined, NameGlobal) ->
    NameGlobal;
name_by_all_metrics_global(NamePerHost, NameGlobal) ->
    case all_metrics_are_global() of
        true -> NameGlobal;
        _ -> NamePerHost
    end.

get_report_interval() ->
    application:get_env(exometer, mongooseim_report_interval,
                        ?DEFAULT_REPORT_INTERVAL).

-spec do_create_generic_hook_metric(PerHost :: list() | undefined, Global :: list()) ->
    ok | {ok, already_present}.
do_create_generic_hook_metric([_, skip], _) ->
    ok;
do_create_generic_hook_metric(PerHostMetricName, GlobalMetricName) ->
    ensure_metric(PerHostMetricName, GlobalMetricName, spiral).

-spec do_increment_generic_hook_metric(PerHost :: list(), Global :: list()) -> ok | {error, any()}.
do_increment_generic_hook_metric([_, skip], _) ->
    ok;
do_increment_generic_hook_metric(PerHostMetricName, GlobalMetricName) ->
    update(PerHostMetricName, GlobalMetricName, 1).

get_odbc_stats(ODBCWorkers) ->
    ODBCConnections = [catch ejabberd_odbc:get_db_info(Pid) || Pid <- ODBCWorkers],
    Ports = [get_port_from_odbc_connection(Conn) || Conn <- ODBCConnections],
    PortStats = [inet_stats(Port) || Port <- lists:flatten(Ports)],
    [{workers, length(ODBCConnections)} | merge_stats(PortStats)].

get_port_from_odbc_connection({ok, DbType, Pid}) when DbType =:= mysql; DbType =:= pgsql ->
    %% Pid of mysql_conn process
    {links, [MySQLRecv]} = erlang:process_info(Pid, links),
    %% Port is hold by mysql_recv process which is linked to the mysql_conn
    {links, Links} = erlang:process_info(MySQLRecv, links),
    [Port || Port <- Links, is_port(Port)];
get_port_from_odbc_connection({ok, odbc, Pid}) ->
    {links, Links} = erlang:process_info(Pid, links),
    [Port || Port <- Links, is_port(Port), {name, "tcp_inet"} == erlang:port_info(Port, name)];
get_port_from_odbc_connection(_) ->
    undefined.

merge_stats(Stats) ->
    OrdDict = lists:foldl(fun(Stat, Acc) ->
        StatDict = orddict:from_list(Stat),
        orddict:merge(fun merge_stats_fun/3, Acc, StatDict)
    end, orddict:from_list(?EMPTY_INET_STATS), Stats),

    orddict:to_list(OrdDict).

merge_stats_fun(recv_max, V1, V2) ->
    erlang:max(V1, V2);
merge_stats_fun(send_max, V1, V2) ->
    erlang:max(V1, V2);
merge_stats_fun(_, V1, V2) ->
    V1 + V2.

inet_stats(Port) when is_port(Port) ->
    {ok, Stats} = inet:getstat(Port, ?INET_STATS),
    Stats;
inet_stats(_) ->
    ?EMPTY_INET_STATS.

remove_metric({Name, _, _}) ->
    exometer_admin:delete_entry(Name).

%% decided whether to use a metric for given hook or not
filter_hook(sm_register_connection_hook) -> skip;
filter_hook(sm_remove_connection_hook) -> skip;
filter_hook(auth_failed) -> skip;
filter_hook(user_send_packet) -> skip;
filter_hook(user_receive_packet) -> skip;
filter_hook(xmpp_bounce_message) -> skip;
filter_hook(xmpp_stanza_dropped) -> skip;
filter_hook(xmpp_send_element) -> skip;
filter_hook(roster_get) -> skip;
filter_hook(roster_set) -> skip;
filter_hook(roster_push) -> skip;
filter_hook(register_user) -> skip;
filter_hook(remove_user) -> skip;
filter_hook(privacy_iq_get) -> skip;
filter_hook(privacy_iq_set) -> skip;
filter_hook(privacy_check_packet) -> skip;
filter_hook(mam_get_prefs) -> skip;
filter_hook(mam_set_prefs) -> skip;
filter_hook(mam_remove_archive) -> skip;
filter_hook(mam_archive_message) -> skip;
filter_hook(mam_flush_messages) -> skip;
filter_hook(mam_drop_message) -> skip;
filter_hook(mam_drop_iq) -> skip;
filter_hook(mam_drop_messages) -> skip;
filter_hook(mam_purge_single_message) -> skip;
filter_hook(mam_purge_multiple_messages) -> skip;
filter_hook(mam_muc_get_prefs) -> skip;
filter_hook(mam_muc_set_prefs) -> skip;
filter_hook(mam_muc_remove_archive) -> skip;
filter_hook(mam_muc_lookup_messages) -> skip;
filter_hook(mam_muc_archive_message) -> skip;
filter_hook(mam_muc_flush_messages) -> skip;
filter_hook(mam_muc_drop_message) -> skip;
filter_hook(mam_muc_drop_iq) -> skip;
filter_hook(mam_muc_drop_messages) -> skip;
filter_hook(mam_muc_purge_single_message) -> skip;
filter_hook(mam_muc_purge_multiple_messages) -> skip;

filter_hook(Hook) -> Hook.

-spec create_metrics(ejabberd:server()) -> 'ok'.
create_metrics(Host) ->
    lists:foreach(fun({PerHostName, GlobalName}) ->
                          ensure_metric(PerHostName, GlobalName, spiral) end,
                  get_general_counters(Host)),

    lists:foreach(fun({PerHostName, GlobalName}) ->
                          ensure_metric(PerHostName, GlobalName, counter) end,
                  get_total_counters(Host)).

ensure_metric(MetricPerHost, MetricGlobal, Type, ShortType) when is_list(MetricGlobal) ->
    %% the split into ShortType and Type is needed because function metrics are
    %% defined as tuples (that is Type), while exometer:info returns only 'function'
    Metric = name_by_all_metrics_global(MetricPerHost, MetricGlobal),
    case exometer:info(Metric, type) of
        ShortType -> {ok, already_present};
        undefined -> exometer:new(Metric, Type)
    end.

-spec metrics_hooks('add' | 'delete', ejabberd:server()) -> 'ok'.
metrics_hooks(Op, Host) ->
    lists:foreach(fun(Hook) ->
        apply(ejabberd_hooks, Op, Hook)
    end, mongoose_metrics_hooks:get_hooks(Host)).

-spec get_general_counters(ejabberd:server()) ->
    [{PerHost :: proper_counter_name(), Global :: proper_counter_name()}].
get_general_counters(Host) ->
    get_counters(Host, ?GENERAL_COUNTERS).

-spec get_total_counters(ejabberd:server()) ->
    [{PerHost :: proper_counter_name(), Global :: proper_counter_name()}].
get_total_counters(Host) ->
    get_counters(Host, ?TOTAL_COUNTERS).

create_data_metrics() ->
    lists:foreach(fun(Metric) -> exometer:new(Metric, histogram) end,
        ?GLOBAL_HISTOGRAMS),
    lists:foreach(fun({Metric, Spec}) -> exometer:new(Metric, Spec) end,
        ?DATA_FUN_METRICS).

-spec get_counters(Host :: ejabberd:lserver(), Counters :: [atom()]) ->
    [{PerHost :: proper_counter_name(), Global :: proper_counter_name()}].
get_counters(Host, Counters) ->
    [{[Host, Counter], [global, Counter]} || Counter <- Counters].

do_start_vm_metrics_subscriptions(Reporter, Interval) ->
    [exometer_report:subscribe(Reporter, Metric, DataPoints, Interval)
     || {Metric, _, DataPoints} <- ?VM_STATS].

do_start_global_metrics_subscriptions(Reporter, Interval) ->
    [exometer_report:subscribe(Reporter, Metric, default, Interval)
     || {Metric, _} <- ?GLOBAL_COUNTERS].

do_start_metrics_subscriptions(Reporter, Interval, MetricPrefix) ->
    [subscribe_metric(Reporter, Metric, Interval)
     || Metric <- exometer:find_entries(MetricPrefix)].

subscribe_metric(Reporter, {Name, counter, _}, Interval) ->
    exometer_report:subscribe(Reporter, Name, [value], Interval);
subscribe_metric(Reporter, {Name, histogram, _}, Interval) ->
    exometer_report:subscribe(Reporter, Name, [min, mean, max, median, 95, 99, 999], Interval);
subscribe_metric(Reporter, {Name, _, _}, Interval) ->
    exometer_report:subscribe(Reporter, Name, default, Interval).

subscribe_to_all(Reporter, Interval) ->
    start_global_metrics_subscriptions(Reporter, Interval),
    start_backend_metrics_subscriptions(Reporter, Interval),
    start_data_metrics_subscriptions(Reporter, Interval),
    SubscriptionHosts = case all_metrics_are_global() of
                            true -> [global];
                            _ -> ?MYHOSTS
                        end,
    lists:foreach(
      fun(Host) ->
              start_host_metrics_subscriptions(Reporter, Host, Interval)
      end, SubscriptionHosts).

