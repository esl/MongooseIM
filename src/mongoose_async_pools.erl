-module(mongoose_async_pools).

-include("mongoose_config_spec.hrl").
-include("mongoose_logger.hrl").

-behaviour(supervisor).
-export([start_link/3, init/1]).
-ignore_xref([start_link/3]).

% API
-export([start_pool/3, stop_pool/2, pool_name/2, config_spec/0]).

-type pool_id() :: atom().
-type pool_name() :: atom().
-type pool_opts() :: gen_mod:module_opts().

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

%%% Supervisor callbacks
-spec start_link(mongooseim:host_type(), pool_id(), pool_opts()) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(HostType, PoolId, Opts) ->
    Supervisor = sup_name(HostType, PoolId),
    supervisor:start_link({local, Supervisor}, ?MODULE, [HostType, PoolId, Opts]).

-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([HostType, PoolId, Opts]) ->
    PoolName = gen_pool_name(HostType, PoolId),
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
    Interval = gen_mod:get_opt(flush_interval, Opts, 1000),
    MaxSize = gen_mod:get_opt(batch_size, Opts, 100),
    NumWorkers = gen_mod:get_opt(pool_size, Opts, 2 * erlang:system_info(schedulers_online)),
    FlushCallback = gen_mod:get_opt(flush_callback, Opts),
    FlushExtra = make_extra(HostType, PoolId, Opts),
    ProcessOpts = [{message_queue_data, off_heap}],
    WorkerOpts = {HostType, Interval, MaxSize, FlushCallback, FlushExtra},
    Worker = {mongoose_batch_worker, WorkerOpts},
    [{worker, Worker},
     {workers, NumWorkers},
     {worker_opt, ProcessOpts},
     {worker_shutdown, 10000}].

-spec make_extra(mongooseim:host_type(), pool_id(), pool_opts()) -> any().
make_extra(HostType, PoolId, Opts) ->
    case {gen_mod:get_opt(init_callback, Opts, undefined),
          gen_mod:get_opt(flush_extra, Opts,
                          fun(Val) -> Val#{host_type => HostType} end,
                          #{host_type => HostType})} of
        {undefined, Extra} ->
            Extra;
        {InitFun, Extra} when is_function(InitFun, 3) ->
            Extra#{init_data => InitFun(HostType, PoolId, Opts)}
    end.
