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
         create_global_metrics/0,
         init_predefined_host_metrics/1,
         init_subscriptions/0,
         create_generic_hook_metric/2,
         ensure_db_pool_metric/1,
         update/3,
         ensure_metric/3,
         get_metric_value/1,
         get_metric_values/1,
         get_metric_value/2,
         get_host_metric_names/1,
         get_global_metric_names/0,
         get_aggregated_values/1,
         increment_generic_hook_metric/2,
         get_rdbms_data_stats/0,
         get_rdbms_data_stats/1,
         get_dist_data_stats/0,
         get_up_time/0,
         get_mnesia_running_db_nodes_count/0,
         remove_host_metrics/1,
         remove_all_metrics/0,
         get_report_interval/0,
         subscribe_metric/3
        ]).

-define(DEFAULT_REPORT_INTERVAL, 60000). %%60s

-type use_or_skip() :: use | skip.
-type hook_name() :: atom().

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
        catch ensure_metric(global, Metric, FunSpecTuple)
    end, ?VM_STATS),
    lists:foreach(fun({Metric, Spec}) -> ensure_metric(global, Metric, Spec) end,
                  ?GLOBAL_COUNTERS),
    create_data_metrics().

-spec init_predefined_host_metrics(jid:lserver()) -> ok.
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

-spec create_generic_hook_metric(jid:lserver(), atom()) ->
    ok | {ok, already_present} | {error, any()}.
create_generic_hook_metric(Host, Hook) ->
    UseOrSkip = filter_hook(Hook),
    do_create_generic_hook_metric(Host, Hook, UseOrSkip).

ensure_db_pool_metric({rdbms, Host, Tag} = Name) ->
    ensure_metric(Host,
                  [data, rdbms, Tag],
                  {function, mongoose_metrics, get_rdbms_data_stats, [[Name]], proplist,
                   [workers | ?INET_STATS]}).

-spec update(Host :: jid:lserver() | global, Name :: term() | list(),
             Change :: term()) -> any().
update(Host, Name, Change) when is_list(Name) ->
    exometer:update(name_by_all_metrics_are_global(Host, Name), Change);
update(Host, Name, Change) ->
    update(Host, [Name], Change).

-spec ensure_metric(jid:lserver() | global, atom() | list(), term()) ->
    ok | {ok, already_present} | {error, any()}.
ensure_metric(Host, Metric, Type) when is_tuple(Type) ->
    ensure_metric(Host, Metric, Type, element(1, Type));
ensure_metric(Host, Metric, Type) ->
    ensure_metric(Host, Metric, Type, Type).

get_metric_value(Host, Name) when is_list(Name) ->
    get_metric_value(name_by_all_metrics_are_global(Host, Name));
get_metric_value(Host, Name) ->
    get_metric_value(Host, [Name]).

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
    exometer:aggregate([{{['_', Metric], '_', '_'}, [], [true]}], [one, count, value]).

-spec increment_generic_hook_metric(jid:lserver(), atom()) -> ok | {error, any()}.
increment_generic_hook_metric(Host, Hook) ->
    UseOrSkip = filter_hook(Hook),
    do_increment_generic_hook_metric(Host, Hook, UseOrSkip).

get_rdbms_data_stats() ->
    Pools = lists:filter(fun({Type, _Host, _Tag}) -> Type == rdbms end, mongoose_wpool:get_pools()),
    get_rdbms_data_stats(Pools).

get_rdbms_data_stats(Pools) ->
    RDBMSWorkers =
        lists:flatmap(
          fun({Type, Host, Tag}) ->
                  PoolName = mongoose_wpool:make_pool_name(Type, Host, Tag),
                  Wpool = wpool_pool:find_wpool(PoolName),
                  PoolSize = wpool_pool:wpool_get(size, Wpool),
                  [whereis(wpool_pool:worker_name(PoolName, I)) || I <- lists:seq(1, PoolSize)]
          end,
          Pools),

    get_rdbms_stats(RDBMSWorkers).

get_dist_data_stats() ->
    DistStats = [inet_stats(Port) || {_, Port} <- erlang:system_info(dist_ctrl)],
    [{connections, length(DistStats)} | merge_stats(DistStats)].

-spec get_up_time() -> {value, integer()}.
get_up_time() ->
    {value, erlang:round(element(1, erlang:statistics(wall_clock))/1000)}.

-spec get_mnesia_running_db_nodes_count() -> {value, non_neg_integer()}.
get_mnesia_running_db_nodes_count() ->
    {value, length(mnesia:system_info(running_db_nodes))}.

remove_host_metrics(Host) ->
    lists:foreach(fun remove_metric/1, exometer:find_entries([Host])).

remove_all_metrics() ->
    lists:foreach(fun remove_metric/1, exometer:find_entries([])).

%% ---------------------------------------------------------------------
%% Internal functions
%% ---------------------------------------------------------------------

-spec all_metrics_are_global() -> boolean() | undefined.
all_metrics_are_global() ->
    ejabberd_config:get_local_option(all_metrics_are_global).

pick_by_all_metrics_are_global(WhenGlobal, WhenNot) ->
    case all_metrics_are_global() of
        true -> WhenGlobal;
        _ -> WhenNot
    end.

-spec name_by_all_metrics_are_global(Host :: jid:lserver() | global,
                                     Name :: list()) -> FinalName :: list().
name_by_all_metrics_are_global(Host, Name) ->
    pick_by_all_metrics_are_global([global | Name], [Host | Name]).

get_report_interval() ->
    application:get_env(exometer_core, mongooseim_report_interval,
                        ?DEFAULT_REPORT_INTERVAL).

-spec do_create_generic_hook_metric(Host :: jid:lserver() | global,
                                    Hook :: hook_name(),
                                    UseOrSkip :: use_or_skip()) ->
    ok | {ok, already_present} | {error, any()}.
do_create_generic_hook_metric(_, _, skip) ->
    ok;
do_create_generic_hook_metric(Host, Hook, use) ->
    ensure_metric(Host, Hook, spiral).

-spec do_increment_generic_hook_metric(Host :: jid:lserver() | global,
                                       Hook :: hook_name(),
                                       UseOrSkip :: use_or_skip()) ->
    ok | {error, any()}.
do_increment_generic_hook_metric(_, _, skip) ->
    ok;
do_increment_generic_hook_metric(Host, Hook, use) ->
    update(Host, Hook, 1).

get_rdbms_stats(RDBMSWorkers) ->
    RDBMSConnections = [{catch mongoose_rdbms:get_db_info(Pid), Pid} || Pid <- RDBMSWorkers],
    Ports = [get_port_from_rdbms_connection(Conn) || Conn <- RDBMSConnections],
    PortStats = [inet_stats(Port) || Port <- lists:flatten(Ports)],
    [{workers, length(RDBMSConnections)} | merge_stats(PortStats)].

get_port_from_rdbms_connection({{ok, DB, Pid}, _WorkerPid}) when DB =:= mysql;
                                                                 DB =:= pgsql ->
    ProcState = sys:get_state(Pid),
    get_port_from_proc_state(DB, ProcState);
get_port_from_rdbms_connection({{ok, odbc, Pid}, WorkerPid}) ->
    Links = element(2, erlang:process_info(Pid, links)) -- [WorkerPid],
    [Port || Port <- Links, is_port(Port), {name, "tcp_inet"} == erlang:port_info(Port, name)];
get_port_from_rdbms_connection(_) ->
    undefined.

%% @doc Gets a socket from mysql/epgsql library Gen_server state
get_port_from_proc_state(mysql, State) ->
    %% -record(state, {server_version, connection_id, socket, sockmod, ssl_opts,
    %%                 host, port, user, password, log_warnings,
    %%                 ping_timeout,
    %%                 query_timeout, query_cache_time,
    %%                 affected_rows = 0, status = 0, warning_count = 0, insert_id = 0,
    %%                 transaction_level = 0, ping_ref = undefined,
    %%                 stmts = dict:new(), query_cache = empty, cap_found_rows = false}).
    SockInfo = element(4, State),
    get_port_from_sock(SockInfo);
get_port_from_proc_state(pgsql, State) ->
    %% -record(state, {mod,
    %%                 sock,
    %%                 data = <<>>,
    %%                 backend,
    %%                 handler,
    %%                 codec,
    %%                 queue = queue:new(),
    %%                 async,
    %%                 parameters = [],
    %%                 types = [],
    %%                 columns = [],
    %%                 rows = [],
    %%                 results = [],
    %%                 batch = [],
    %%                 sync_required,
    %%                 txstatus,
    %%                 complete_status :: undefined | atom() | {atom(), integer()},
    %%                 repl_last_received_lsn,
    %%                 repl_last_flushed_lsn,
    %%                 repl_last_applied_lsn,
    %%                 repl_feedback_required,
    %%                 repl_cbmodule,
    %%                 repl_cbstate,
    %%                 repl_receiver}).
    SockInfo = element(3, State),
    get_port_from_sock(SockInfo).

get_port_from_sock({sslsocket, {_, Port, _, _}, _}) ->
    Port;
get_port_from_sock(Port) ->
    Port.

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
%% TLS dist is operated by PID, not PORT directly, so we need to find the relevant port
inet_stats(Pid) when is_pid(Pid) ->
    {links, Links} = erlang:process_info(Pid, links),
    %% In case of TLS, controlling process of a TCP port is one of the linked proceses
    %% so we should check all links of all processes linked to givan one
    RelatedPidsAndPorts =
        lists:map(fun(LinkedPid) ->
            {links, SubLinks} = erlang:process_info(LinkedPid, links),
            SubLinks
        end, [Pid | Links]),

    PortsTCP = lists:filter(
        fun(Link) ->
            case Link of
                Port when is_port(Port) ->
                    {name, "tcp_inet"} == erlang:port_info(Port, name);
                _ ->
                    false
            end
        end, lists:flatten(RelatedPidsAndPorts)),

    case PortsTCP of
        [Port | _] ->
            inet_stats(Port);
        _ ->
            ?EMPTY_INET_STATS
    end;
inet_stats(_) ->
    ?EMPTY_INET_STATS.

remove_metric({Name, _, _}) ->
    exometer_admin:delete_entry(Name).

%% decided whether to use a metric for given hook or not
-spec filter_hook(hook_name()) -> use_or_skip().
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
filter_hook(mam_muc_get_prefs) -> skip;
filter_hook(mam_muc_set_prefs) -> skip;
filter_hook(mam_muc_remove_archive) -> skip;
filter_hook(mam_muc_lookup_messages) -> skip;
filter_hook(mam_muc_archive_message) -> skip;
filter_hook(mam_muc_flush_messages) -> skip;
filter_hook(mam_muc_drop_message) -> skip;
filter_hook(mam_muc_drop_iq) -> skip;
filter_hook(mam_muc_drop_messages) -> skip;

filter_hook(_) -> use.

-spec create_metrics(jid:server()) -> 'ok'.
create_metrics(Host) ->
    lists:foreach(fun(Name) -> ensure_metric(Host, Name, spiral) end, ?GENERAL_SPIRALS),
    lists:foreach(fun(Name) -> ensure_metric(Host, Name, counter) end, ?TOTAL_COUNTERS).

ensure_metric(Host, Metric, Type, ShortType) when is_atom(Metric) ->
    ensure_metric(Host, [Metric], Type, ShortType);

ensure_metric(Host, Metric, Type, probe = ShortType) ->
    PrefixedMetric = name_by_all_metrics_are_global(Host, Metric),
    {ShortType, Opts} = Type,
    case exometer:info(PrefixedMetric, type) of
        undefined ->
            ExometerOpts = [{module, mongoose_metrics_probe}, {type, ShortType}] ++ Opts,
            do_create_metric(PrefixedMetric, ad_hoc, ExometerOpts);
        _ ->
        {ok, already_present}
    end;
ensure_metric(Host, Metric, Type, ShortType) when is_list(Metric) ->
    %% the split into ShortType and Type is needed because function metrics are
    %% defined as tuples (that is Type), while exometer:info returns only 'function'
    PrefixedMetric = name_by_all_metrics_are_global(Host, Metric),
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

-spec metrics_hooks('add' | 'delete', jid:server()) -> 'ok'.
metrics_hooks(Op, Host) ->
    lists:foreach(fun(Hook) ->
        apply(ejabberd_hooks, Op, Hook)
    end, mongoose_metrics_hooks:get_hooks(Host)).

create_data_metrics() ->
    lists:foreach(fun(Metric) -> ensure_metric(global, Metric, histogram) end,
        ?GLOBAL_HISTOGRAMS),
    lists:foreach(fun({Metric, Spec}) -> ensure_metric(global, Metric, Spec) end,
        ?DATA_FUN_METRICS).

start_metrics_subscriptions(Reporter, MetricPrefix, Interval) ->
    [subscribe_metric(Reporter, Metric, Interval)
     || Metric <- exometer:find_entries(MetricPrefix)].

subscribe_metric(Reporter, {Name, counter, _}, Interval) ->
    exometer_report:subscribe(Reporter, Name, [value], Interval);
subscribe_metric(Reporter, {Name, histogram, _}, Interval) ->
    exometer_report:subscribe(Reporter, Name, [min, mean, max, median, 95, 99, 999], Interval);
subscribe_metric(Reporter, {Name, _, _}, Interval) ->
    exometer_report:subscribe(Reporter, Name, default, Interval).

subscribe_to_all(Reporter, Interval) ->
    HostPrefixes = pick_by_all_metrics_are_global([], ?MYHOSTS),
    lists:foreach(
      fun(Prefix) ->
              start_metrics_subscriptions(Reporter, [Prefix], Interval)
      end, [global | HostPrefixes]).
