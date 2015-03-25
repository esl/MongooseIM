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

%% API
-export([update/2,
         start_graphite_reporter/1,
         start_host_metrics_subscriptions/3,
         start_vm_metrics_subscriptions/2,
         start_global_metrics_subscriptions/2,
         start_data_metrics_subscriptions/2,
         get_metric_value/1,
         get_metric_values/1,
         get_host_metric_names/1,
         get_global_metric_names/0,
         get_aggregated_values/1,
         init_predefined_host_metrics/1,
         create_global_metrics/0,
         create_generic_hook_metric/2,
         increment_generic_hook_metric/2,
         get_odbc_data_stats/0,
         remove_host_metrics/1,
         remove_all_metrics/0]).

-spec update({term(), term()}, term()) -> no_return().
update(Name, Change) when is_tuple(Name)->
    update(tuple_to_list(Name), Change);
update(Name, Change) ->
    exometer:update(Name, Change).

start_graphite_reporter(GraphiteHost) ->
    GraphiteOpts = [{prefix, "exometer." ++ atom_to_list(node())},
        {host, GraphiteHost},
        {connect_timeout, 5000},
        {port, 2003},
        {api_key, ""}],
    case exometer_report:add_reporter(exometer_report_graphite, GraphiteOpts) of
        ok ->
            {ok, exometer_report_graphite};
        Error ->
            Error
    end.

start_host_metrics_subscriptions(Reporter, Host, Interval) ->
    do_start_host_metrics_subscriptions(check_reporter(Reporter), Host, Interval).

start_vm_metrics_subscriptions(Reporter, Interval) ->
    do_start_vm_metrics_subscriptions(check_reporter(Reporter), Interval).

start_global_metrics_subscriptions(Reporter, Interval) ->
    do_start_global_metrics_subscriptions(check_reporter(Reporter), Interval).

start_data_metrics_subscriptions(Reporter, Interval) ->
    do_start_data_metrics_subscriptions(check_reporter(Reporter), Interval).

get_host_metric_names(Host) ->
    [MetricName || {[_Host, MetricName | _], _, _} <- exometer:find_entries([Host])].

get_global_metric_names() ->
    get_host_metric_names(global).

get_metric_value({Host, Name}) ->
    get_metric_value([Host, Name]);
get_metric_value(Metric) ->
    exometer:get_value(Metric).

get_metric_values(Host) ->
    exometer:get_values([Host]).

get_aggregated_values(Metric) ->
    exometer:aggregate([{{['_',Metric],'_','_'},[],[true]}], [one, count, value]).

-spec init_predefined_host_metrics(ejabberd:lserver()) -> no_return().
init_predefined_host_metrics(Host) ->
    create_metrics(Host),
    metrics_hooks(add, Host),
    ok.

-spec create_generic_hook_metric(ejabberd:lserver(), atom()) -> no_return().
create_generic_hook_metric(Host, Hook) ->
    do_create_generic_hook_metric({Host, filter_hook(Hook)}).

-spec increment_generic_hook_metric(ejabberd:lserver(), atom()) -> no_return().
increment_generic_hook_metric(Host, Hook) ->
    do_increment_generic_hook_metric({Host, filter_hook(Hook)}).

do_create_generic_hook_metric({_, skip}) ->
    ok;
do_create_generic_hook_metric(MetricName) ->
    ensure_metric(MetricName, spiral).

do_increment_generic_hook_metric({_, skip}) ->
    ok;
do_increment_generic_hook_metric(MetricName) ->
    update(MetricName, 1).


get_odbc_data_stats() ->
    RegularODBCWorkers = ejabberd_odbc_sup:get_pids(<<"localhost">>),
    MamAsynODBCWorkers = [Pid || {_, Pid, worker, _} <- supervisor:which_children(mod_mam_sup)],

    get_odbc_stats(RegularODBCWorkers ++ MamAsynODBCWorkers).

get_odbc_stats(ODBCWorkers) ->
    ODBCConnections = [catch ejabberd_odbc:get_db_info(Pid) || Pid <- ODBCWorkers],
    Ports = [get_port_from_odbc_connection(Conn) || Conn <- ODBCConnections],
    PortStats = [inet_stats(Port) || Port <- Ports],
    merge_stats(PortStats).
%%

get_port_from_odbc_connection({ok, mysql, Pid}) ->
    %% Pid of mysql_conn process
    {links, [MySQLRecv]} = erlang:process_info(Pid, links),
    %% Port is hold by mysql_recv process which is linked to the mysql_conn
    {links, Links} = erlang:process_info(MySQLRecv, links),
    hd([Port || Port <- Links, is_port(Port)]);
get_port_from_odbc_connection(_) ->
    undefined.

-define(EMPTY_INET_STATS, [{recv_oct,0},
                           {recv_cnt,0},
                           {recv_max,0},
                           {recv_avg,0},
                           {recv_dvi,0},
                           {send_oct,0},
                           {send_cnt,0},
                           {send_max,0},
                           {send_avg,0},
                           {send_pend,0}
                          ]).

merge_stats(Stats) ->
    MergeFun = fun(_, V1, V2) -> V1 + V2 end,
    lists:foldl(fun(Stat, Acc) ->
        StatDict = orddict:from_list(Stat),
        orddict:merge(MergeFun, Acc, StatDict)
    end, orddict:from_list(?EMPTY_INET_STATS), Stats).

inet_stats(Port) when is_port(Port) ->
    {ok, Stats} = inet:getstat(Port),
    Stats;
inet_stats(_) ->
    ?EMPTY_INET_STATS.

remove_host_metrics(Host) ->
    lists:foreach(fun remove_metric/1,
                  exometer:find_entries([Host])).

remove_all_metrics() ->
    lists:foreach(fun remove_metric/1,
                  exometer:find_entries([])).

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
    lists:foreach(fun(Name) -> ensure_metric(Name, spiral) end,
                  get_general_counters(Host)),

    lists:foreach(fun(Name) -> ensure_metric(Name, counter) end,
                  get_total_counters(Host)),

    lists:foreach(fun(Name) -> ensure_metric(Name, histogram) end,
                  get_histograms(Host)).

ensure_metric({Host, Metric}, Type) ->
    case exometer:info([Host, Metric], type) of
        Type -> {ok, already_present};
        undefined -> exometer:new([Host, Metric], Type)
    end.

-spec metrics_hooks('add' | 'delete', ejabberd:server()) -> 'ok'.
metrics_hooks(Op, Host) ->
    lists:foreach(fun(Hook) ->
        apply(ejabberd_hooks, Op, Hook)
    end, mongoose_metrics_hooks:get_hooks(Host)).

-define (GENERAL_COUNTERS, [
    sessionSuccessfulLogins,
    sessionAuthAnonymous,
    sessionAuthFails,
    sessionLogouts,
    xmppMessageSent,
    xmppMessageReceived,
    xmppMessageBounced,
    xmppPresenceSent,
    xmppPresenceReceived,
    xmppIqSent,
    xmppIqReceived,
    xmppStanzaSent,
    xmppStanzaReceived,
    xmppStanzaDropped,
    xmppStanzaCount,
    xmppErrorTotal,
    xmppErrorIq,
    xmppErrorMessage,
    xmppErrorPresence,
    modRosterSets,
    modRosterGets,
    modPresenceSubscriptions,
    modPresenceUnsubscriptions,
    modRosterPush,
    modRegisterCount,
    modUnregisterCount,
    modPrivacySets,
    modPrivacySetsActive,
    modPrivacySetsDefault,
    modPrivacyPush,
    modPrivacyGets,
    modPrivacyStanzaBlocked,
    modPrivacyStanzaAll,
    modMamPrefsSets,
    modMamPrefsGets,
    modMamArchiveRemoved,
    modMamLookups,
    modMamForwarded,
    modMamArchived,
    modMamFlushed,
    modMamDropped,
    modMamDropped2,
    modMamDroppedIQ,
    modMamSinglePurges,
    modMamMultiplePurges,
    modMucMamPrefsSets,
    modMucMamPrefsGets,
    modMucMamArchiveRemoved,
    modMucMamLookups,
    modMucMamForwarded,
    modMucMamArchived,
    modMucMamSinglePurges,
    modMucMamMultiplePurges
]).


-spec get_general_counters(ejabberd:server()) -> [{ejabberd:server(), atom()}].
get_general_counters(Host) ->
    get_counters(Host, ?GENERAL_COUNTERS).

-define (TOTAL_COUNTERS, [
    sessionCount
]).


-spec get_total_counters(ejabberd:server()) ->
    [{ejabberd:server(),'sessionCount'}].
get_total_counters(Host) ->
    get_counters(Host, ?TOTAL_COUNTERS).

-define (HISTOGRAMS, [
    mam_archive_time,
    mam_lookup_time

]).

get_histograms(Host) ->
    get_counters(Host, ?HISTOGRAMS).

-define(EX_EVAL_SINGLE_VALUE, {[{l, [{t, [value, {v, 'Value'}]}]}],[value]}).
-define(GLOBAL_COUNTERS,
        [{[global, totalSessionCount],
          {function, ejabberd_sm, get_total_sessions_number, [],
           eval, ?EX_EVAL_SINGLE_VALUE}},
         {[global, uniqueSessionCount],
          {function, ejabberd_sm, get_unique_sessions_number, [],
           eval, ?EX_EVAL_SINGLE_VALUE}},
         {[global, nodeSessionCount],
          {function, ejabberd_sm, get_node_sessions_number, [],
           eval, ?EX_EVAL_SINGLE_VALUE}}
        ]
).

-define(GLOBAL_HISTOGRAMS, [[data, received, encrypted_data_size],
                            [data, received, compressed_data_size],
                            [data, received, xml_stanza_size],
                            [data, sent, encrypted_data_size],
                            [data, sent, compressed_data_size],
                            [data, sent, xml_stanza_size]]).

create_global_metrics() ->
    lists:foreach(fun({Metric, FunSpec, DataPoints}) ->
        FunSpecTuple = list_to_tuple(FunSpec ++ [DataPoints]),
        exometer:new(Metric, FunSpecTuple)
    end, get_vm_stats()),
    lists:foreach(fun({Metric, Spec}) -> exometer:new(Metric, Spec) end,
                  ?GLOBAL_COUNTERS),
    lists:foreach(fun(Metric) -> exometer:new(Metric, histogram) end,
                  ?GLOBAL_HISTOGRAMS).

get_vm_stats() ->
    [{[erlang, system_info], [function, erlang, system_info, ['$dp'], value],
        [port_count, port_limit, process_count, process_limit, ets_limit]},
     {[erlang, memory], [function, erlang, memory, ['$dp'], value],
      [total, processes_used, atom_used, binary, ets, system]}].

get_counters(Host, Counters) ->
    [{Host, Counter} || Counter <- Counters].

check_reporter(Reporter) ->
    Reporters = exometer_report:list_reporters(),
    case lists:keyfind(Reporter, 1, Reporters) of
        {Reporter, _} ->
            {ok, Reporter};
        _ ->
            {error, {no_such_reporter}}
    end.

do_start_host_metrics_subscriptions({ok, Reporter}, Host, Interval) ->
    [subscribe_metric(Reporter, Metric, Interval)
     || Metric <- exometer:find_entries([Host])];
do_start_host_metrics_subscriptions(Error, _, _) ->
    Error.

do_start_vm_metrics_subscriptions({ok, Reporter}, Interval) ->
    [exometer_report:subscribe(Reporter, Metric, DataPoints, Interval)
     || {Metric, _, DataPoints} <- get_vm_stats()];
do_start_vm_metrics_subscriptions(Error, _) ->
    Error.


do_start_global_metrics_subscriptions({ok, Reporter}, Interval) ->
    [exometer_report:subscribe(Reporter, Metric, default, Interval)
     || {Metric, _} <- ?GLOBAL_COUNTERS];
do_start_global_metrics_subscriptions(Error, _) ->
    Error.

do_start_data_metrics_subscriptions({ok, Reporter}, Interval) ->
    [subscribe_metric(Reporter, Metric, Interval)
     || Metric <- exometer:find_entries([data])];
do_start_data_metrics_subscriptions(Error, _) ->
    Error.

subscribe_metric(Reporter, {Name, counter, _}, Interval) ->
    exometer_report:subscribe(Reporter, Name, [value], Interval);
subscribe_metric(Reporter, {Name, histogram, _}, Interval) ->
    exometer_report:subscribe(Reporter, Name, [min, mean, max, median, 95, 99, 999], Interval);
subscribe_metric(Reporter, {Name, _, _}, Interval) ->
    exometer_report:subscribe(Reporter, Name, default, Interval).