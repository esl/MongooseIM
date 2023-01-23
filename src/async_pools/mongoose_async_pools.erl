-module(mongoose_async_pools).

-include("mongoose_logger.hrl").

-behaviour(supervisor).
-export([start_link/3, init/1]).
-ignore_xref([start_link/3]).

% API
-export([start_pool/3, stop_pool/2]).
-export([put_task/3, put_task/4, broadcast/3, broadcast_task/4]).
-ignore_xref([put_task/3, broadcast/3, broadcast_task/4]).
-export([sync/2]).

-type task() :: term().
-type pool_id() :: atom(). % The subsystem, like 'pm_mam', or 'inbox'
-type pool_name() :: atom(). % The pool name, like 'inbox_sup_async_pool_localhost'
-type pool_type() :: batch | aggregate.
-type pool_opts() :: #{pool_type := pool_type(),
                      _ => _}.
-type pool_extra() :: #{host_type := mongooseim:host_type(),
                        queue_length => non_neg_integer(),
                        _ => _}.

-type flush_callback() ::
    fun(([task()], pool_extra()) ->
        ok | {error, term()}).
-type prep_callback() ::
    fun((task(), pool_extra()) ->
        {ok, task()} | {error, term()}).
-type aggregate_callback() ::
    fun((task(), task(), pool_extra()) ->
        {ok, task()} | {error, term()}).
-type request_callback() ::
    fun((task() | [task()], pool_extra()) ->
        term()).
-type verify_callback() ::
    fun((term(), task(), pool_extra()) -> term()).

-export_type([flush_callback/0,
              prep_callback/0,
              aggregate_callback/0,
              request_callback/0,
              verify_callback/0]).

-export_type([task/0, pool_id/0, pool_opts/0, pool_extra/0]).

-spec put_task(mongooseim:host_type(), pool_id(), term()) -> ok.
put_task(HostType, PoolId, Task) ->
    PoolName = pool_name(HostType, PoolId),
    wpool:cast(PoolName, {task, Task}, best_worker).

-spec put_task(mongooseim:host_type(), pool_id(), term(), term()) -> ok.
put_task(HostType, PoolId, Key, Task) ->
    PoolName = pool_name(HostType, PoolId),
    wpool:cast(PoolName, {task, Key, Task}, {hash_worker, Key}).

-spec broadcast(mongooseim:host_type(), pool_id(), term()) -> ok.
broadcast(HostType, PoolId, Task) ->
    PoolName = pool_name(HostType, PoolId),
    wpool:broadcast(PoolName, {broadcast, Task}).

-spec broadcast_task(mongooseim:host_type(), pool_id(), term(), term()) -> ok.
broadcast_task(HostType, PoolId, Key, Task) ->
    PoolName = pool_name(HostType, PoolId),
    wpool:broadcast(PoolName, {task, Key, Task}).

%%% API functions
-spec start_pool(mongooseim:host_type(), pool_id(), pool_opts()) ->
    supervisor:startchild_ret().
start_pool(HostType, PoolId, PoolOpts) ->
    ?LOG_INFO(#{what => async_pool_starting, host_type => HostType, pool_id => PoolId}),
    Supervisor = sup_name(HostType, PoolId),
    ChildSpec = #{id => Supervisor,
                  start => {?MODULE, start_link, [HostType, PoolId, PoolOpts]},
                  restart => transient,
                  type => supervisor},
    ejabberd_sup:start_child(ChildSpec).

-spec stop_pool(mongooseim:host_type(), pool_id()) -> ok.
stop_pool(HostType, PoolId) ->
    ?LOG_INFO(#{what => async_pool_stopping, host_type => HostType, pool_id => PoolId}),
    ejabberd_sup:stop_child(sup_name(HostType, PoolId)).

-spec pool_name(mongooseim:host_type(), pool_id()) -> pool_name().
pool_name(HostType, PoolId) ->
    persistent_term:get({?MODULE, HostType, PoolId}).

-spec sync(mongooseim:host_type(), pool_id()) -> term().
sync(HostType, PoolId) ->
    Pool = pool_name(HostType, PoolId),
    WorkerNames = wpool:get_workers(Pool),
    Context = #{what => sync_failed, host_type => HostType, pool_id => PoolId},
    F = fun(Pid) -> safely:apply_and_log(gen_server, call, [Pid, sync], Context) end,
    Results = mongoose_lib:pmap(F, WorkerNames),
    [check_result(Result) || Result <- Results].

check_result({ok, ok}) -> ok;
check_result({ok, skipped}) -> ok;
check_result(Other) -> ?LOG_ERROR(#{what => sync_failed, reason => Other}).

%%% Supervisor callbacks
-spec start_link(mongooseim:host_type(), pool_id(), pool_opts()) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(HostType, PoolId, PoolOpts) ->
    Supervisor = sup_name(HostType, PoolId),
    supervisor:start_link({local, Supervisor}, ?MODULE, {HostType, PoolId, PoolOpts}).

-spec init({mongooseim:host_type(), pool_id(), pool_opts()}) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init({HostType, PoolId, PoolOpts}) ->
    WPoolOpts = process_pool_opts(HostType, PoolId, PoolOpts),
    PoolName = gen_pool_name(HostType, PoolId),
    store_pool_name(HostType, PoolId, PoolName),
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

-spec process_pool_opts(mongooseim:host_type(), pool_id(), pool_opts()) -> [wpool:option()].
process_pool_opts(HostType, PoolId, #{pool_size := NumWorkers} = Opts) ->
    WorkerModule = select_worker_module(HostType, PoolId, Opts),
    WorkerOpts = make_worker_opts(HostType, PoolId, Opts),
    Worker = {WorkerModule, WorkerOpts},
    [{worker, Worker},
     {workers, NumWorkers},
     {worker_opt, [{spawn_opt, [{message_queue_data, off_heap}]}]},
     {worker_shutdown, 10000}].

select_worker_module(HostType, PoolId, #{pool_type := batch}) ->
    mongoose_metrics:ensure_metric(HostType, [?MODULE, PoolId, timed_flushes], counter),
    mongoose_metrics:ensure_metric(HostType, [?MODULE, PoolId, batch_flushes], counter),
    mongoose_batch_worker;
select_worker_module(HostType, PoolId, #{pool_type := aggregate}) ->
    mongoose_metrics:ensure_metric(HostType, [?MODULE, PoolId, async_request], counter),
    mongoose_aggregator_worker.

-spec make_worker_opts(mongooseim:host_type(), pool_id(), pool_opts()) -> map().
make_worker_opts(HostType, PoolId, Opts) ->
    Opts#{host_type => HostType, pool_id => PoolId,
          flush_extra => make_extra(HostType, PoolId, Opts)}.

-spec make_extra(mongooseim:host_type(), pool_id(), pool_opts()) -> pool_extra().
make_extra(HostType, PoolId, Opts) ->
    DefExtra = case maps:get(pool_type, Opts) of
                   batch -> #{host_type => HostType, queue_length => 0};
                   aggregate -> #{host_type => HostType}
               end,
    Extra = maps:merge(maps:get(flush_extra, Opts, #{}), DefExtra),
    maybe_init_handler(HostType, PoolId, Opts, Extra).

maybe_init_handler(HostType, PoolId, Opts = #{init_callback := InitFun}, Extra)
  when is_function(InitFun, 3) ->
    Extra#{init_data => InitFun(HostType, PoolId, Opts)};
maybe_init_handler(_, _, _, Extra) ->
    Extra.
