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
-include("mongoose_wpool.hrl").

-record(mongoose_wpool, {
          name :: term(),
          strategy :: atom(),
          call_timeout :: pos_integer() | undefined
         }).
-dialyzer({no_match, start/4}).

%% API
-export([ensure_started/0,
         start/2, start/3, start/4, start/5,
         stop/0, stop/1, stop/2, stop/3,
         get_worker/1, get_worker/2, get_worker/3,
         call/2, call/3, call/4, call/5,
         cast/2, cast/3, cast/4, cast/5,
         get_pool_settings/3, get_pools/0, stats/3]).

-export([start_sup_pool/3]).
-export([start_configured_pools/0]).
-export([start_configured_pools/1]).
-export([start_configured_pools/2]).
-export([is_configured/1]).
-export([make_pool_name/3]).
-export([call_start_callback/2]).

%% Mostly for tests
-export([expand_pools/2]).

-type pool_type() :: redis | riak | http | rdbms | cassandra | elastic | generic
                | rabbit | ldap.

%% Config scope
-type scope() :: global | host | mongooseim:host_type().
-type host_type_or_global() :: mongooseim:host_type() | global.

-type tag() :: atom().
-type name() :: atom().

-export_type([pool_type/0]).
-export_type([tag/0]).
-export_type([scope/0]).
-export_type([name/0]).

-callback init() -> ok | {error, term()}.
-callback start(scope(), tag(), WPoolOpts :: [wpool:option()], ConnOpts :: [{atom(), any()}]) ->
    {ok, {pid(), proplists:proplist()}} | {ok, pid()} |
    {external, pid()} | {error, Reason :: term()}.
-callback default_opts() -> proplist:proplists().
-callback is_supported_strategy(Strategy :: wpool:strategy()) -> boolean().
-callback stop(scope(), tag()) -> ok.

-optional_callbacks([default_opts/0, is_supported_strategy/1]).

ensure_started() ->
    wpool:start(),
    case whereis(mongoose_wpool_sup) of
        undefined ->
            mongoose_wpool_sup:start_link();
        _ ->
            ok
    end,

    case ets:info(?MODULE) of
        undefined ->
            % we set heir here because the whole thing may be started by an ephemeral process
            ets:new(?MODULE, [named_table, public,
                {read_concurrency, true},
                {keypos, 2},
                {heir, whereis(mongoose_wpool_sup), undefined}]);
        _ ->
            ok
    end.

start_configured_pools() ->
    Pools = ejabberd_config:get_local_option_or_default(outgoing_pools, []),
    start_configured_pools(Pools).

start_configured_pools(PoolsIn) ->
    start_configured_pools(PoolsIn, ?MYHOSTS).

start_configured_pools(PoolsIn, Hosts) ->
    [call_callback(init, PoolType, []) || PoolType <- get_unique_types(PoolsIn)],
    Pools = expand_pools(PoolsIn, Hosts),
    [start(Pool) || Pool <- Pools].

start({PoolType, HostType, Tag, PoolOpts, ConnOpts}) ->
    start(PoolType, HostType, Tag, PoolOpts, ConnOpts).


start(PoolType, PoolOpts) ->
    start(PoolType, global, PoolOpts).

start(PoolType, HostType, PoolOpts) ->
    start(PoolType, HostType, default, PoolOpts).

start(PoolType, HostType, Tag, PoolOpts) ->
    start(PoolType, HostType, Tag, PoolOpts, []).

start(PoolType, HostType, Tag, PoolOpts, ConnOpts) ->
    {Opts0, WpoolOptsIn} = proplists:split(PoolOpts, [strategy, call_timeout]),
    Opts = lists:append(Opts0) ++ default_opts(PoolType),
    Strategy = proplists:get_value(strategy, Opts, best_worker),
    CallTimeout = proplists:get_value(call_timeout, Opts, 5000),

    %% If a callback doesn't explicitly blacklist a strategy, let's proceed.
    CallbackModule = make_callback_module_name(PoolType),
    case catch CallbackModule:is_supported_strategy(Strategy) of
        false ->
            error({strategy_not_supported, PoolType, HostType, Tag, Strategy});
        _ ->
            start(PoolType, HostType, Tag, WpoolOptsIn, ConnOpts, Strategy, CallTimeout)
    end.

start(PoolType, HostType, Tag, WpoolOptsIn, ConnOpts, Strategy, CallTimeout) ->
    case mongoose_wpool_mgr:start(PoolType, HostType, Tag, WpoolOptsIn, ConnOpts) of
        {ok, Pid} ->
            ets:insert(?MODULE, #mongoose_wpool{name = {PoolType, HostType, Tag},
                                                strategy = Strategy,
                                                call_timeout = CallTimeout}),
            {ok, Pid};
        {external, Pid} ->
            ets:insert(?MODULE, #mongoose_wpool{name = {PoolType, HostType, Tag}}),
            {ok, Pid};
        Error ->
            Error
    end.

%% @doc this function starts the worker_pool's pool under a specific supervisor
%% in MongooseIM application.
%% It's needed for 2 reasons:
%% 1. We want to have a full control of all the pools and its restarts
%% 2. When a pool is started via wpool:start_pool it's supposed be called by a supervisor,
%%    if not, there is no way to stop the pool.
-spec start_sup_pool(pool_type(), name(), [wpool:option()]) ->
    {ok, pid()} | {error, term()}.
start_sup_pool(PoolType, Name, WpoolOpts) ->
    SupName = mongoose_wpool_type_sup:name(PoolType),
    ChildSpec = #{id => Name,
                  start => {wpool, start_pool, [Name, WpoolOpts]},
                  restart => temporary,
                  type => supervisor,
                  modules => [wpool]},
    supervisor:start_child(SupName, ChildSpec).

stop() ->
    [ stop(PoolType, HostType, Tag) || {PoolType, HostType, Tag} <- get_pools() ].

stop(PoolType) ->
    stop(PoolType, global).

stop(PoolType, HostType) ->
    stop(PoolType, HostType, default).

stop(PoolType, HostType, Tag) ->
    try
        ets:delete(?MODULE, {PoolType, HostType, Tag}),
        call_callback(stop, PoolType, [HostType, Tag]),
        mongoose_wpool_mgr:stop(PoolType, HostType, Tag)
    catch
        C:R:S ->
            ?LOG_ERROR(#{what => pool_stop_failed,
                         pool_type => PoolType, server => HostType, pool_tag => Tag,
                         pool_key => {PoolType, HostType, Tag},
                         class => C, reason => R, stacktrace => S})
    end.

-spec is_configured(pool_type()) -> boolean().
is_configured(PoolType) ->
    Pools = ejabberd_config:get_local_option_or_default(outgoing_pools, []),
    lists:keymember(PoolType, 1, Pools).

get_worker(PoolType) ->
    get_worker(PoolType, global).

get_worker(PoolType, HostType) ->
    get_worker(PoolType, HostType, default).

get_worker(PoolType, HostType, Tag) ->
    case get_pool(PoolType, HostType, Tag) of
        {ok, #mongoose_wpool{strategy = Strategy} = Pool} ->
            Worker = wpool_pool:Strategy(make_pool_name(Pool)),
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

get_pool_settings(PoolType, HostType, Tag) ->
    case get_pool(PoolType, HostType, Tag) of
        {ok, PoolOpts} -> PoolOpts;
        {error, pool_not_started} -> undefined
    end.

get_pools() ->
    lists:map(fun(#mongoose_wpool{name = Name}) -> Name end, ets:tab2list(?MODULE)).

stats(PoolType, HostType, Tag) ->
    wpool:stats(make_pool_name(PoolType, HostType, Tag)).

-spec make_pool_name(pool_type(), scope(), tag()) -> atom().
make_pool_name(PoolType, HostType, Tag) when is_atom(HostType) ->
    make_pool_name(PoolType, atom_to_binary(HostType, utf8), Tag);
make_pool_name(PoolType, HostType, Tag) when is_binary(HostType) ->
    binary_to_atom(<<"mongoose_wpool$", (atom_to_binary(PoolType, utf8))/binary, $$,
                     HostType/binary, $$, (atom_to_binary(Tag, utf8))/binary>>, utf8).

make_pool_name(#mongoose_wpool{name = {PoolType, HostType, Tag}}) ->
    make_pool_name(PoolType, HostType, Tag).

call_start_callback(PoolType, Args) ->
    call_callback(start, PoolType, Args).

call_callback(Name, PoolType, Args) ->
    try
        CallbackModule = make_callback_module_name(PoolType),
        erlang:apply(CallbackModule, Name, Args)
    catch E:R:ST ->
          ?LOG_ERROR(#{what => pool_callback_failed,
                       pool_type => PoolType, callback_function => Name,
                       error => E, reason => R, stacktrace => ST}),
          {error, {callback_crashed, Name, E, R, ST}}
    end.

-spec make_callback_module_name(pool_type()) -> module().
make_callback_module_name(PoolType) ->
    Name = "mongoose_wpool_" ++ atom_to_list(PoolType),
    list_to_atom(Name).

default_opts(PoolType) ->
    Mod = make_callback_module_name(PoolType),
    case erlang:function_exported(Mod, default_opts, 0) of
        true -> Mod:default_opts();
        false -> []
    end.

expand_pools(Pools, AllHosts) ->
    %% First we select only pools for a specific vhost
    HostSpecific = [{PoolType, HostType, Tag} ||
                     {PoolType, HostType, Tag, _, _} <- Pools, is_binary(HostType)],
    %% Then we expand all pools with `host` as HostType parameter but using host specific configs
    %% if they were provided
    F = fun({PoolType, host, Tag, WpoolOpts, ConnOpts}) ->
                [{PoolType, HostType, Tag, WpoolOpts, ConnOpts} || HostType <- AllHosts,
                                                           not lists:member({PoolType, HostType, Tag}, HostSpecific)];
           (Other) -> [Other]
        end,
    lists:flatmap(F, Pools).

get_unique_types(Pools) ->
    ordsets:to_list(ordsets:from_list([PoolType || {PoolType, _, _, _, _} <- Pools])).

get_pool(PoolType, HostType, Tag) ->
    case ets:lookup(?MODULE, {PoolType, HostType, Tag}) of
        [] when is_binary(HostType) -> get_pool(PoolType, global, Tag);
        [] -> {error, pool_not_started};
        [Pool] -> {ok, Pool}
    end.
