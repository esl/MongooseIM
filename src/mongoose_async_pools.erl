-module(mongoose_async_pools).

-include("mongoose_config_spec.hrl").
-include("mongoose_logger.hrl").

-behaviour(supervisor).
-export([start_link/3, init/1]).
-ignore_xref([start_link/3]).

% API
-export([start_pool/3, stop_pool/2, pool_name/2, config_spec/0]).
-export([sync/2]).

-type pool_id() :: atom().
-type pool_name() :: atom().
-type pool_opts() :: map().
-type pool_extra() :: #{host_type := mongooseim:host_type(),
                        queue_length := non_neg_integer(),
                        _ => _}.

-export_type([pool_id/0, pool_opts/0, pool_extra/0]).

%%% API functions
-spec start_pool(mongooseim:host_type(), pool_id(), pool_opts()) ->
    supervisor:startchild_ret().
start_pool(HostType, PoolId, Opts) ->
    ?LOG_INFO(#{what => async_pool_starting, host_type => HostType, pool_id => PoolId}),
    Supervisor = sup_name(HostType, PoolId),
    ChildSpec = #{id => Supervisor,
                  start => {?MODULE, start_link, [HostType, PoolId, Opts]},
                  restart => transient,
                  type => supervisor},
    ejabberd_sup:start_child(ChildSpec).

-spec stop_pool(mongooseim:host_type(), pool_id()) -> ok.
stop_pool(HostType, PoolId) ->
    ?LOG_INFO(#{what => async_pool_stopping, host_type => HostType, pool_id => PoolId}),
    ejabberd_sup:stop_child(sup_name(HostType, PoolId)).

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"enabled">> => #option{type = boolean},
                 <<"flush_interval">> => #option{type = integer, validate = non_negative},
                 <<"batch_size">> => #option{type = integer, validate = non_negative},
                 <<"pool_size">> => #option{type = integer, validate = non_negative}}
      }.

-spec pool_name(mongooseim:host_type(), pool_id()) -> pool_name().
pool_name(HostType, PoolId) ->
    persistent_term:get({?MODULE, HostType, PoolId}).

-spec sync(mongooseim:host_type(), pool_id()) -> term().
sync(HostType, PoolId) ->
    Pids = get_workers(HostType, PoolId),
    Context = #{what => sync_failed, host_type => HostType, pool_id => PoolId},
    F = fun(Pid) ->
                safely:apply_and_log(gen_server, call, [Pid, sync], Context)
        end,
    Results = mongoose_lib:pmap(F, Pids),
    check_results(Results).

check_results(Results) ->
    [check_result(Result) || Result <- Results].

check_result({ok, ok}) -> ok;
check_result({ok, skipped}) -> ok;
check_result(Other) -> ?LOG_ERROR(#{what => sync_failed, reason => Other}).

-spec get_workers(mongooseim:host_type(), pool_id()) -> [atom()].
get_workers(HostType, PoolId) ->
    Pool = pool_name(HostType, PoolId),
    wpool:get_workers(Pool).

%%% Supervisor callbacks
-spec start_link(mongooseim:host_type(), pool_id(), pool_opts()) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(HostType, PoolId, Opts) ->
    Supervisor = sup_name(HostType, PoolId),
    supervisor:start_link({local, Supervisor}, ?MODULE, [HostType, PoolId, Opts]).

-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([HostType, PoolId, Opts]) ->
    PoolName = gen_pool_name(HostType, PoolId),
    mongoose_metrics:ensure_metric(HostType, [?MODULE, PoolId, timed_flushes], counter),
    mongoose_metrics:ensure_metric(HostType, [?MODULE, PoolId, batch_flushes], counter),
    store_pool_name(HostType, PoolId, PoolName),
    WPoolOpts = make_wpool_opts(HostType, PoolId, Opts),
    WorkerSpec = #{id => PoolName,
                   start => {wpool, start_pool, [PoolName, WPoolOpts]},
                   restart => permanent,
                   type => supervisor},
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    {ok, {SupFlags, [WorkerSpec]}}.

%%% internal callbacks
-spec sup_name(mongooseim:host_type(), pool_id()) -> atom().
sup_name(HostType, PoolId) ->
    list_to_atom(
      atom_to_list(PoolId) ++ "_sup_async_pool_" ++ binary_to_list(HostType)).

-spec store_pool_name(mongooseim:host_type(), pool_id(), pool_name()) -> ok.
store_pool_name(HostType, PoolId, PoolName) ->
    persistent_term:put({?MODULE, HostType, PoolId}, PoolName).

-spec gen_pool_name(mongooseim:host_type(), pool_id()) -> pool_name().
gen_pool_name(HostType, PoolId) ->
    list_to_atom(
      atom_to_list(PoolId) ++ "_async_pool_" ++ binary_to_list(HostType)).

-spec make_wpool_opts(mongooseim:host_type(), pool_id(), pool_opts()) -> any().
make_wpool_opts(HostType, PoolId, Opts) ->
    Interval = maps:get(flush_interval, Opts, 1000),
    MaxSize = maps:get(batch_size, Opts, 100),
    NumWorkers = maps:get(pool_size, Opts, 4 * erlang:system_info(schedulers_online)),
    FlushCallback = maps:get(flush_callback, Opts),
    FlushExtra = make_extra(HostType, PoolId, Opts),
    ProcessOpts = [{message_queue_data, off_heap}],
    WorkerOpts = {HostType, PoolId, Interval, MaxSize, FlushCallback, FlushExtra},
    Worker = {mongoose_batch_worker, WorkerOpts},
    [{worker, Worker},
     {workers, NumWorkers},
     {worker_opt, ProcessOpts},
     {worker_shutdown, 10000}].

-spec make_extra(mongooseim:host_type(), pool_id(), pool_opts()) -> pool_extra().
make_extra(HostType, PoolId, Opts) ->
    DefExtra = #{host_type => HostType, queue_length => 0},
    Extra = maps:merge(maps:get(flush_extra, Opts, #{}), DefExtra),
    case maps:find(init_callback, Opts) of
        {ok, InitFun} ->
            Extra#{init_data => InitFun(HostType, PoolId, Opts)};
        error ->
            Extra
    end.
