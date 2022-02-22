-module(mongoose_async_pools).

-include("mongoose_config_spec.hrl").
-include("mongoose_logger.hrl").

-behaviour(supervisor).
-export([start_link/3, init/1]).
-ignore_xref([start_link/3]).

% API
-export([start_pool/3, stop_pool/2, pool_name/2, config_spec/0]).
-export([put_task/3, put_task/4]).
-ignore_xref([put_task/3]).
-export([sync/2]).

-type pool_id() :: atom(). % The subsystem, like 'pm_mam', or 'inbox'
-type pool_name() :: atom(). % The pool name, like 'inbox_sup_async_pool_localhost'
-type pool_opts() :: map().
-type pool_extra() :: #{host_type := mongooseim:host_type(),
                        queue_length := non_neg_integer(),
                        _ => _}.

-export_type([pool_id/0, pool_opts/0, pool_extra/0]).

-spec put_task(mongooseim:host_type(), pool_id(), term()) -> ok.
put_task(HostType, PoolId, Task) ->
    PoolName = pool_name(HostType, PoolId),
    wpool:cast(PoolName, {task, Task}, best_worker).

-spec put_task(mongooseim:host_type(), pool_id(), term(), term()) -> ok.
put_task(HostType, PoolId, Key, Task) ->
    PoolName = pool_name(HostType, PoolId),
    wpool:cast(PoolName, {task, Key, Task}, {hash_worker, Key}).

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
                 <<"pool_size">> => #option{type = integer, validate = non_negative}},
       defaults = #{<<"enabled">> => true,
                    <<"flush_interval">> => 2000,
                    <<"batch_size">> => 30,
                    <<"pool_size">> => 4 * erlang:system_info(schedulers_online)},
       format_items = map
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
-spec start_link(mongooseim:host_type(), pool_id(), gen_mod:module_opts()) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(HostType, PoolId, Opts) ->
    Supervisor = sup_name(HostType, PoolId),
    supervisor:start_link({local, Supervisor}, ?MODULE, {HostType, PoolId, Opts}).

-spec init({mongooseim:host_type(), pool_id(), gen_mod:module_opts()}) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init({HostType, PoolId, Opts}) ->
    WPoolOpts = process_pool_opts(HostType, PoolId, Opts),
    PoolName = gen_pool_name(HostType, PoolId),
    mongoose_metrics:ensure_metric(HostType, [?MODULE, PoolId, timed_flushes], counter),
    mongoose_metrics:ensure_metric(HostType, [?MODULE, PoolId, batch_flushes], counter),
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
    WorkerOpts = make_worker_opts(HostType, PoolId, Opts),
    Worker = {mongoose_batch_worker, WorkerOpts},
    [{worker, Worker},
     {workers, NumWorkers},
     {worker_opt, [{message_queue_data, off_heap}]},
     {worker_shutdown, 10000}].

-spec make_worker_opts(mongooseim:host_type(), pool_id(), pool_opts()) -> map().
make_worker_opts(HostType, PoolId, Opts) ->
    Opts#{host_type => HostType, pool_id => PoolId,
          flush_extra => make_extra(HostType, PoolId, Opts)}.

-spec make_extra(mongooseim:host_type(), pool_id(), pool_opts()) -> pool_extra().
make_extra(HostType, PoolId, Opts) ->
    DefExtra = #{host_type => HostType, queue_length => 0},
    Extra = maps:merge(maps:get(flush_extra, Opts, #{}), DefExtra),
    maybe_init_handler(HostType, PoolId, Opts, Extra).

maybe_init_handler(HostType, PoolId, Opts = #{init_callback := InitFun}, Extra)
  when is_function(InitFun, 3) ->
    Extra#{init_data => InitFun(HostType, PoolId, Opts)};
maybe_init_handler(_, _, _, Extra) ->
    Extra.
