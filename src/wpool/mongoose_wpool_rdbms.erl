-module(mongoose_wpool_rdbms).
-behaviour(mongoose_wpool).
-behaviour(mongoose_instrument_probe).

-include("mongoose.hrl").

-export([init/0, start/4, stop/2]).
-export([probe/2, instrumentation/2]).

%% --------------------------------------------------------------
%% mongoose_wpool callbacks
-spec init() -> ok.
init() ->
    {ok, _} = application:ensure_all_started([mysql, epgsql], permanent),
    ejabberd_sup:create_ets_table(
      prepared_statements, [named_table, public, {read_concurrency, true}]).

-spec start(mongooseim:host_type_or_global(), mongoose_wpool:tag(),
            mongoose_wpool:pool_opts(), mongoose_wpool:conn_opts()) -> {ok, pid()} | {error, any()}.
start(HostType, Tag, WpoolOpts, RdbmsOpts) ->
    try do_start(HostType, Tag, WpoolOpts, RdbmsOpts)
    catch
        Err -> {error, Err}
    end.

-spec stop(mongooseim:host_type_or_global(), mongoose_wpool:tag()) -> ok.
stop(_, _) ->
    ok.

%% --------------------------------------------------------------
%% Helper functions
do_start(HostType, Tag, WpoolOpts0, RdbmsOpts) when is_list(WpoolOpts0), is_map(RdbmsOpts) ->
    #{driver := BackendName} = RdbmsOpts,
    mongoose_rdbms_backend:init(BackendName),
    WpoolOpts = make_wpool_opts(WpoolOpts0, RdbmsOpts),
    ProcName = mongoose_wpool:make_pool_name(rdbms, HostType, Tag),
    mongoose_wpool:start_sup_pool(rdbms, ProcName, WpoolOpts).

make_wpool_opts(WpoolOpts0, RdbmsOpts) ->
    Worker = {mongoose_rdbms, RdbmsOpts},
    [{worker, Worker}, {pool_sup_shutdown, infinity} | WpoolOpts0].

-spec instrumentation(mongooseim:host_type_or_global(), mongoose_wpool:tag()) ->
    [mongoose_instrument:spec()].
instrumentation(global, Tag) ->
    % Services use global pools. Since the same number of labels for a metric is expected, for an
    % event, global pool has to emit an event under a different name.
    [{wpool_global_rdbms_stats, #{pool_tag => Tag},
      #{probe => #{module => ?MODULE}, metrics => gauges([workers | inet_stats()])}}];
instrumentation(HostType, Tag) ->
    [{wpool_rdbms_stats, #{host_type => HostType, pool_tag => Tag},
      #{probe => #{module => ?MODULE}, metrics => gauges([workers | inet_stats()])}}].

-spec probe(mongoose_instrument:event_name(), mongoose_instrument:labels()) ->
    mongoose_instrument:measurements().
probe(wpool_global_rdbms_stats, #{pool_tag := Tag} = _Labels) ->
    get_rdbms_data_stats(global, Tag);
probe(wpool_rdbms_stats, #{host_type := HostType, pool_tag := Tag} = _Labels) ->
    get_rdbms_data_stats(HostType, Tag).

get_rdbms_data_stats(HostType, Tag) ->
    PoolName = mongoose_wpool:make_pool_name(rdbms, HostType, Tag),
    Wpool = wpool_pool:find_wpool(PoolName),
    PoolSize = wpool_pool:wpool_get(size, Wpool),
    RDBMSWorkers = [whereis(wpool_pool:worker_name(PoolName, I)) || I <- lists:seq(1, PoolSize)],
    RDBMSConnections = [{catch mongoose_rdbms:get_db_info(Pid), Pid} || Pid <- RDBMSWorkers],
    Ports = [get_port_from_rdbms_connection(Conn) || Conn <- RDBMSConnections],
    PortStats = [inet_stats(Port) || Port <- lists:flatten(Ports)],

    Stats = merge_stats(PortStats),
    Stats#{workers => length(RDBMSConnections)}.

get_port_from_rdbms_connection({{ok, DB, Pid}, _WorkerPid}) when DB =:= mysql;
                                                                 DB =:= pgsql;
                                                                 DB =:= cockroachdb ->
    ProcState = sys:get_state(Pid),
    get_port_from_proc_state(DB, ProcState);
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
get_port_from_proc_state(Driver, State) when Driver =:= pgsql; Driver =:= cockroachdb ->
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
    lists:foldl(fun(Stat, Acc) ->
                    StatMap = maps:from_list(Stat),
                    maps:merge_with(fun merge_stats_fun/3, Acc, StatMap)
                end,
                empty_inet_stats_measurements(),
                Stats).

merge_stats_fun(recv_max, V1, V2) ->
    max(V1, V2);
merge_stats_fun(send_max, V1, V2) ->
    max(V1, V2);
merge_stats_fun(_, V1, V2) ->
    V1 + V2.

-spec inet_stats(inet:port_number() | ssl:sslsocket() | undefined) ->
        [{inet:stat_option(), integer()}].
inet_stats(undefined) ->
    [];
inet_stats(SslSock) when is_tuple(SslSock),
                         element(1, SslSock) =:= sslsocket ->
    case ssl:getstat(SslSock, inet_stats()) of
        {ok, Stats} ->
            Stats;
        {error, Reason} ->
            ?LOG_INFO(#{what => inet_stats_failed, transport => ssl, reason => Reason}),
            []
    end;
inet_stats(Port) when is_port(Port) ->
    case inet:getstat(Port, inet_stats()) of
        {ok, Stats} ->
            Stats;
        {error, Reason} ->
            ?LOG_INFO(#{what => inet_stats_failed, transport => tcp, reason => Reason}),
            []
    end.

inet_stats() ->
    [recv_oct,
     recv_cnt,
     recv_max,
     send_oct,
     send_max,
     send_cnt,
     send_pend].

empty_inet_stats_measurements() ->
    #{recv_oct => 0,
      recv_cnt => 0,
      recv_max => 0,
      send_oct => 0,
      send_max => 0,
      send_cnt => 0,
      send_pend => 0}.

-spec gauges([mongoose_instrument:metric_name()]) -> mongoose_instrument:metrics().
gauges(Keys) ->
    maps:from_keys(Keys, gauge).
