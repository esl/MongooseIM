%%%-------------------------------------------------------------------
%%% @doc
%%% This is here because there are pool options which have to be given when calling
%%% the pool (selection strategy, timeout), while we want to set it once for the pool and not
%%% worry about them later, hence additional storage.
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_wpool).
-author("bartlomiej.gorny@erlang-solutions.com").
-include("mongoose.hrl").

-record(mongoose_wpool, {
          name :: pool_name(),
          atom_name :: wpool:name(),
          strategy :: wpool:strategy(),
          call_timeout :: pos_integer()
         }).

%% API
-export([ensure_started/0,
         start/2, start/3, start/4, start/5,
         stop/0, stop/1, stop/2, stop/3,
         get_worker/2, get_worker/3,
         call/2, call/3, call/4, call/5,
         send_request/2, send_request/3, send_request/4, send_request/5,
         cast/2, cast/3, cast/4, cast/5,
         get_pool_settings/3, get_pools/0, stats/3]).

-export([start_sup_pool/3]).
-export([start_configured_pools/0, start_configured_pools/1,
         start_configured_pools/2, start_configured_pools/3]).
-export([is_configured/1]).
-export([make_pool_name/3]).
-export([call_start_callback/2]).

%% Mostly for tests
-export([expand_pools/3]).

-ignore_xref([call/2, cast/2, cast/3, expand_pools/3, get_worker/2,
              send_request/2, send_request/3, send_request/4, send_request/5,
              is_configured/2, is_configured/1, is_configured/1, start/2, start/3,
              start/5, start_configured_pools/1, start_configured_pools/2, start_configured_pools/3,
              get_pools/0, stats/3, stop/1, stop/2]).

-type pool_type() :: redis | http | rdbms | cassandra | elastic | generic | rabbit | ldap.

%% Config scope
-type scope() :: global | host_type | mongooseim:host_type().
-type host_type_or_global() :: mongooseim:host_type_or_global().
-type tag() :: atom().

%% Name of a process
-type proc_name() :: atom().

%% ID of a pool. Used as a key for an ETS table
-type pool_name() :: {PoolType :: pool_type(),
                      HostType :: host_type_or_global(),
                      Tag :: tag()}.

-type pool_opts_in() :: map().
-type pool_opts() :: [wpool:option()].
-type conn_opts() :: map().

-type pool_map_in() :: #{type := pool_type(),
                         scope := scope(),
                         tag := tag(),
                         opts := pool_opts_in(),
                         conn_opts := conn_opts()}.
%% Pool map with expanded HostType argument instead of scope
-type pool_map() :: #{type := pool_type(),
                      host_type := host_type_or_global(),
                      tag := tag(),
                      opts := pool_opts(),
                      conn_opts := conn_opts()}.
-type pool_error() :: {pool_not_started, term()}.
-type worker_result() :: {ok, pid()} | {error, pool_error()}.
-type pool_record_result() :: {ok, #mongoose_wpool{}} | {error, pool_error()}.
-type start_result() :: {ok, pid()} | {error, term()}.
-type stop_result() :: ok | term().

-export_type([pool_type/0, tag/0, scope/0, proc_name/0, pool_name/0, pool_opts/0, conn_opts/0]).

-type callback_fun() :: init | start | is_supported_strategy | stop.

-callback init() -> ok | {error, term()}.
-callback start(host_type_or_global(), tag(), WPoolOpts :: pool_opts(), ConnOpts :: conn_opts()) ->
    {ok, {pid(), proplists:proplist()}} | {ok, pid()} | {error, Reason :: term()}.
-callback is_supported_strategy(Strategy :: wpool:strategy()) -> boolean().
-callback instrumentation(mongooseim:host_type_or_global(), mongoose_wpool:tag()) -> [mongoose_instrument:spec()].
-callback stop(host_type_or_global(), tag()) -> ok.

-optional_callbacks([is_supported_strategy/1, instrumentation/2]).

ensure_started() ->
    wpool:start(),
    case whereis(mongoose_wpool_sup) of
        undefined ->
            mongoose_wpool_sup:start_link();
        _ ->
            ok
    end,
    ejabberd_sup:create_ets_table(
      ?MODULE,
      [named_table, public,
       {read_concurrency, true},
       {keypos, #mongoose_wpool.name},
       {heir, whereis(mongoose_wpool_sup), undefined}]).

start_configured_pools() ->
    Pools = mongoose_config:get_opt(outgoing_pools),
    start_configured_pools(Pools).

start_configured_pools(PoolsIn) ->
    start_configured_pools(PoolsIn, ?ALL_HOST_TYPES).

start_configured_pools(PoolsIn, HostTypes) ->
    HostTypeSpecific = get_host_type_specific_pools(HostTypes),
    start_configured_pools(PoolsIn, HostTypeSpecific, HostTypes).

start_configured_pools(PoolsIn, HostTypeSpecific, HostTypes) ->
    Pools = expand_pools(PoolsIn, HostTypeSpecific, HostTypes),
    [call_callback(init, PoolType, []) || PoolType <- get_unique_types(PoolsIn, HostTypeSpecific)],
    [start(Pool) || Pool <- Pools].

-spec start(pool_map()) -> start_result().
start(#{type := PoolType, host_type := HostType, tag := Tag,
        opts := PoolOpts, conn_opts := ConnOpts}) ->
    start(PoolType, HostType, Tag, PoolOpts, ConnOpts).

-spec start(pool_type(), pool_opts()) -> start_result().
start(PoolType, PoolOpts) ->
    start(PoolType, global, PoolOpts).

-spec start(pool_type(), host_type_or_global(), pool_opts()) -> start_result().
start(PoolType, HostType, PoolOpts) ->
    start(PoolType, HostType, default, PoolOpts).

-spec start(pool_type(), host_type_or_global(), tag(),
            pool_opts()) -> start_result().
start(PoolType, HostType, Tag, PoolOpts) ->
    start(PoolType, HostType, Tag, PoolOpts, #{}).

-spec start(pool_type(), host_type_or_global(), tag(),
            pool_opts(), conn_opts()) -> start_result().
start(PoolType, HostType, Tag, PoolOpts, ConnOpts) ->
    {Opts0, WpoolOptsIn} = proplists:split(PoolOpts, [strategy, call_timeout]),
    Opts = lists:append(Opts0),
    Strategy = proplists:get_value(strategy, Opts, best_worker),
    CallTimeout = proplists:get_value(call_timeout, Opts, 5000),
    %% If a callback doesn't explicitly blacklist a strategy, let's proceed.
    CallbackModule = make_callback_module_name(PoolType),
    case catch CallbackModule:is_supported_strategy(Strategy) of
        false ->
            error({strategy_not_supported, PoolType, HostType, Tag, Strategy});
        _ ->
            mongoose_instrument:set_up(instrumentation(PoolType, HostType, Tag)),
            start(PoolType, HostType, Tag, WpoolOptsIn, ConnOpts, Strategy, CallTimeout)
    end.

-spec start(pool_type(), host_type_or_global(), tag(),
            pool_opts(), conn_opts(), wpool:strategy(), pos_integer()) ->
    start_result().
start(PoolType, HostType, Tag, WpoolOptsIn, ConnOpts, Strategy, CallTimeout) ->
    case mongoose_wpool_mgr:start(PoolType, HostType, Tag, WpoolOptsIn, ConnOpts) of
        {ok, Pid} ->
            ets:insert(?MODULE, #mongoose_wpool{name = {PoolType, HostType, Tag},
                                                atom_name = make_pool_name(PoolType, HostType, Tag),
                                                strategy = Strategy,
                                                call_timeout = CallTimeout}),
            {ok, Pid};
        Error ->
            mongoose_instrument:tear_down(instrumentation(PoolType, HostType, Tag)),
            Error
    end.

%% @doc this function starts the worker_pool's pool under a specific supervisor
%% in MongooseIM application.
%% It's needed for 2 reasons:
%% 1. We want to have a full control of all the pools and its restarts
%% 2. When a pool is started via wpool:start_pool it's supposed be called by a supervisor,
%%    if not, there is no way to stop the pool.
-spec start_sup_pool(pool_type(), proc_name(), [wpool:option()]) ->
    {ok, pid()} | {error, term()}.
start_sup_pool(PoolType, ProcName, WpoolOpts) ->
    SupName = mongoose_wpool_type_sup:name(PoolType),
    ChildSpec = #{id => ProcName,
                  start => {wpool, start_pool, [ProcName, WpoolOpts]},
                  restart => temporary,
                  type => supervisor,
                  modules => [wpool]},
    supervisor:start_child(SupName, ChildSpec).

-spec stop() -> term().
stop() ->
    [stop_pool(PoolName) || PoolName <- get_pools()].

-spec stop_pool(pool_name()) -> stop_result().
stop_pool({PoolType, HostType, Tag}) ->
    stop(PoolType, HostType, Tag).

-spec stop(pool_type()) -> stop_result().
stop(PoolType) ->
    stop(PoolType, global).

-spec stop(pool_type(), host_type_or_global()) -> stop_result().
stop(PoolType, HostType) ->
    stop(PoolType, HostType, default).

-spec stop(pool_type(), host_type_or_global(), tag()) -> stop_result().
stop(PoolType, HostType, Tag) ->
    try
        ets:delete(?MODULE, {PoolType, HostType, Tag}),
        call_callback(stop, PoolType, [HostType, Tag]),
        mongoose_wpool_mgr:stop(PoolType, HostType, Tag),
        mongoose_instrument:tear_down(instrumentation(PoolType, HostType, Tag))
    catch
        C:R:S ->
            ?LOG_ERROR(#{what => pool_stop_failed,
                         pool_type => PoolType, server => HostType, pool_tag => Tag,
                         pool_key => {PoolType, HostType, Tag},
                         class => C, reason => R, stacktrace => S})
    end.

-spec is_configured(pool_type()) -> boolean().
is_configured(PoolType) ->
    Pools = mongoose_config:get_opt(outgoing_pools),
    lists:any(fun(#{type := Type}) -> Type =:= PoolType end, Pools).

-spec get_worker(pool_type(), host_type_or_global()) -> worker_result().
get_worker(PoolType, HostType) ->
    get_worker(PoolType, HostType, default).

-spec get_worker(pool_type(), host_type_or_global(), tag()) -> worker_result().
get_worker(PoolType, HostType, Tag) ->
    case get_pool(PoolType, HostType, Tag) of
        {ok, #mongoose_wpool{strategy = Strategy} = Pool} ->
            Worker = get_wpool_worker(make_pool_name(Pool), Strategy),
            {ok, whereis(Worker)};
        Err ->
            Err
    end.

call(PoolType, Request) ->
    call(PoolType, global, Request).

call(PoolType, HostType, Request) ->
    call(PoolType, HostType, default, Request).

call(PoolType, HostType, Tag, Request) ->
    case get_pool(PoolType, HostType, Tag) of
        {ok, #mongoose_wpool{strategy = Strategy, call_timeout = CallTimeout} = Pool} ->
            wpool:call(make_pool_name(Pool), Request, Strategy, CallTimeout);
        Err ->
            Err
    end.

call(PoolType, HostType, Tag, HashKey, Request) ->
    case get_pool(PoolType, HostType, Tag) of
        {ok, #mongoose_wpool{call_timeout = CallTimeout} = Pool} ->
            wpool:call(make_pool_name(Pool), Request, {hash_worker, HashKey}, CallTimeout);
        Err ->
            Err
    end.

send_request(PoolType, Request) ->
    send_request(PoolType, global, Request).

send_request(PoolType, HostType, Request) ->
    send_request(PoolType, HostType, default, Request).

send_request(PoolType, HostType, Tag, Request) ->
    case get_pool(PoolType, HostType, Tag) of
        {ok, #mongoose_wpool{strategy = Strategy, call_timeout = CallTimeout} = Pool} ->
            wpool_send_request(make_pool_name(Pool), Request, Strategy, CallTimeout);
        Err ->
            Err
    end.

send_request(PoolType, HostType, Tag, HashKey, Request) ->
    case get_pool(PoolType, HostType, Tag) of
        {ok, #mongoose_wpool{call_timeout = CallTimeout} = Pool} ->
            wpool_send_request(make_pool_name(Pool), Request, {hash_worker, HashKey}, CallTimeout);
        Err ->
            Err
    end.

wpool_send_request(PoolName, Request, Strategy, Timeout) ->
    wpool:send_request(PoolName, Request, Strategy, Timeout).

cast(PoolType, Request) ->
    cast(PoolType, global, Request).

cast(PoolType, HostType, Request) ->
    cast(PoolType, HostType, default, Request).

cast(PoolType, HostType, Tag, Request) ->
    case get_pool(PoolType, HostType, Tag) of
        {ok, #mongoose_wpool{strategy = Strategy} = Pool} ->
            wpool:cast(make_pool_name(Pool), Request, Strategy);
        Err ->
            Err
    end.

cast(PoolType, HostType, Tag, HashKey, Request) ->
    case get_pool(PoolType, HostType, Tag) of
        {ok, #mongoose_wpool{} = Pool} ->
            wpool:cast(make_pool_name(Pool), Request, {hash_worker, HashKey});
        Err ->
            Err
    end.

-spec get_pool_settings(pool_type(), host_type_or_global(), tag()) ->
    #mongoose_wpool{} | undefined.
get_pool_settings(PoolType, HostType, Tag) ->
    case get_pool(PoolType, HostType, Tag) of
        {ok, PoolRec} -> PoolRec;
        {error, {pool_not_started, _}} -> undefined
    end.

-spec get_pools() -> [pool_name()].
get_pools() ->
    lists:map(fun(#mongoose_wpool{name = Name}) -> Name end, ets:tab2list(?MODULE)).

stats(PoolType, HostType, Tag) ->
    wpool:stats(make_pool_name(PoolType, HostType, Tag)).

-spec make_pool_name(pool_type(), scope(), tag()) -> proc_name().
make_pool_name(PoolType, HostType, Tag) when is_atom(HostType) ->
    make_pool_name(PoolType, atom_to_binary(HostType, utf8), Tag);
make_pool_name(PoolType, HostType, Tag) when is_binary(HostType) ->
    binary_to_atom(<<"mongoose_wpool$", (atom_to_binary(PoolType, utf8))/binary, $$,
                     HostType/binary, $$, (atom_to_binary(Tag, utf8))/binary>>, utf8).

make_pool_name(#mongoose_wpool{atom_name = undefined, name = {PoolType, HostType, Tag}}) ->
    make_pool_name(PoolType, HostType, Tag);
make_pool_name(#mongoose_wpool{atom_name = AtomName}) ->
    AtomName.

-spec call_start_callback(pool_type(), list()) -> term().
call_start_callback(PoolType, Args) ->
    call_callback(start, PoolType, Args).

-spec call_callback(callback_fun(), pool_type(), list()) -> term().
call_callback(CallbackFun, PoolType, Args) ->
    try
        CallbackModule = make_callback_module_name(PoolType),
        erlang:apply(CallbackModule, CallbackFun, Args)
    catch E:R:ST ->
          ?LOG_ERROR(#{what => pool_callback_failed,
                       pool_type => PoolType, callback_function => CallbackFun,
                       error => E, reason => R, stacktrace => ST}),
          {error, {callback_crashed, CallbackFun, E, R, ST}}
    end.

-spec make_callback_module_name(pool_type()) -> module().
make_callback_module_name(PoolType) ->
    Name = "mongoose_wpool_" ++ atom_to_list(PoolType),
    list_to_atom(Name).

-spec get_host_type_specific_pools([mongooseim:host_type()]) -> [pool_map_in()].
get_host_type_specific_pools(HostTypes) ->
    lists:append([ mongoose_config:get_opt({outgoing_pools, HostType}, [])
                   || HostType <- HostTypes ]).

-spec expand_pools([pool_map_in()], [pool_map_in()], [mongooseim:host_type()]) -> [pool_map()].
expand_pools(Pools, PerHostType, HostTypes) ->
    %% First we select only vhost/host_type specific pools
    HostSpecific = [ {Type, HT, Tag}
                     || #{type := Type, scope := HT, tag := Tag} <- PerHostType ],
    %% Then we expand all pools with `host_type` as scope
    %% but using host_type specific configs if they were provided
    F = fun(M = #{type := PoolType, scope := host_type, tag := Tag}) ->
                [M#{scope => HostType} || HostType <- HostTypes,
                                          not lists:member({PoolType, HostType, Tag}, HostSpecific)];
           (Other) -> [Other]
        end,
    Pools1 = lists:flatmap(F, Pools),
    lists:map(fun prepare_pool_map/1, PerHostType ++ Pools1).

-spec prepare_pool_map(pool_map_in()) -> pool_map().
prepare_pool_map(Pool = #{scope := HT, opts := Opts}) ->
    %% Rename "scope" field to "host_type" and change wpool opts to a KV list
    Pool1 = maps:remove(scope, Pool),
    Pool1#{host_type => HT, opts => maps:to_list(prepare_pool_opts(Opts))}.

-spec prepare_pool_opts(pool_map_in()) -> pool_map_in().
prepare_pool_opts(Opts = #{strategy := best_worker, max_worker_queue_len := MaxQueueLen}) ->
    Opts1 = maps:remove(max_worker_queue_len, Opts),
    Opts1#{strategy := fun(Name) -> best_worker_with_max_queue_len(Name, MaxQueueLen) end};
prepare_pool_opts(Opts) ->
    Opts.

-spec get_unique_types([pool_map_in()], [pool_map_in()]) -> [pool_type()].
get_unique_types(Pools, HostTypeSpecific) ->
    lists:usort([maps:get(type, Pool) || Pool <- Pools ++ HostTypeSpecific]).

-spec get_pool(pool_type(), host_type_or_global(), tag()) -> pool_record_result().
get_pool(PoolType, HostType, Tag) ->
    case ets:lookup(?MODULE, {PoolType, HostType, Tag}) of
        [] when is_binary(HostType) -> get_pool(PoolType, global, Tag);
        [] -> {error, {pool_not_started, {PoolType, HostType, Tag}}};
        [Pool] -> {ok, Pool}
    end.

-spec get_wpool_worker(wpool:name(), wpool:strategy()) -> proc_name().
get_wpool_worker(PoolName, Strategy) when is_atom(Strategy) ->
    wpool_pool:Strategy(PoolName);
get_wpool_worker(PoolName, StrategyFun) when is_function(StrategyFun, 1) ->
    StrategyFun(PoolName).

-spec instrumentation(pool_type(), host_type_or_global(), tag()) -> [mongoose_instrument:spec()].
instrumentation(PoolType, HostType, Tag) ->
    CallbackModule = make_callback_module_name(PoolType),
    case mongoose_lib:is_exported(CallbackModule, instrumentation, 2) of
        true ->
            CallbackModule:instrumentation(HostType, Tag);
        false ->
            []
    end.

-spec best_worker_with_max_queue_len(wpool:name(), pos_integer()) -> atom().
best_worker_with_max_queue_len(Name, MaxQueueLen) ->
    Worker = wpool_pool:best_worker(Name),
    case process_info(whereis(Worker), message_queue_len) of
        {_, QueueLen} when QueueLen >= MaxQueueLen ->
            exit(no_available_workers);
        _ ->
            Worker
    end.
