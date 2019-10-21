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

-type type() :: redis | riak | http | rdbms | cassandra | elastic | generic
              | rabbit | ldap.
-type host() :: global | host | jid:lserver().
-type tag() :: atom().
-type name() :: atom().

-export_type([type/0]).
-export_type([tag/0]).
-export_type([host/0]).
-export_type([name/0]).

-callback init() -> ok | {error, term()}.
-callback start(host(), tag(), WPoolOpts :: [wpool:option()], ConnOpts :: [{atom(), any()}]) ->
    {ok, {pid(), proplists:proplist()}} | {ok, pid()} |
    {external, pid()} | {error, Reason :: term()}.
-callback default_opts() -> proplist:proplists().
-callback is_supported_strategy(Strategy :: wpool:strategy()) -> boolean().
-callback stop(host(), tag()) -> ok.

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
    [call_callback(init, Type, []) || Type <- get_unique_types(PoolsIn)],
    Pools = expand_pools(PoolsIn, Hosts),
    [start(Pool) || Pool <- Pools].

start({Type, Host, Tag, PoolOpts, ConnOpts}) ->
    start(Type, Host, Tag, PoolOpts, ConnOpts).


start(Type, PoolOpts) ->
    start(Type, global, PoolOpts).

start(Type, Host, PoolOpts) ->
    start(Type, Host, default, PoolOpts).

start(Type, Host, Tag, PoolOpts) ->
    start(Type, Host, Tag, PoolOpts, []).

start(Type, Host, Tag, PoolOpts, ConnOpts) ->
    {Opts0, WpoolOptsIn} = proplists:split(PoolOpts, [strategy, call_timeout]),
    Opts = lists:append(Opts0) ++ default_opts(Type),
    Strategy = proplists:get_value(strategy, Opts, best_worker),
    CallTimeout = proplists:get_value(call_timeout, Opts, 5000),

    %% If a callback doesn't explicitly blacklist a strategy, let's proceed.
    CallbackModule = make_callback_module_name(Type),
    case catch CallbackModule:is_supported_strategy(Strategy) of
        false ->
            error({strategy_not_supported, Type, Host, Tag, Strategy});
        _ ->
            start(Type, Host, Tag, WpoolOptsIn, ConnOpts, Strategy, CallTimeout)
    end.

start(Type, Host, Tag, WpoolOptsIn, ConnOpts, Strategy, CallTimeout) ->
    case mongoose_wpool_mgr:start(Type, Host, Tag, WpoolOptsIn, ConnOpts) of
        {ok, Pid} ->
            ets:insert(?MODULE, #mongoose_wpool{name = {Type, Host, Tag},
                                                strategy = Strategy,
                                                call_timeout = CallTimeout}),
            {ok, Pid};
        {external, Pid} ->
            ets:insert(?MODULE, #mongoose_wpool{name = {Type, Host, Tag}}),
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
-spec start_sup_pool(type(), name(), [wpool:option()]) ->
    {ok, pid()} | {error, term()}.
start_sup_pool(Type, Name, WpoolOpts) ->
    SupName = mongoose_wpool_type_sup:name(Type),
    ChildSpec = #{id => Name,
                  start => {wpool, start_pool, [Name, WpoolOpts]},
                  restart => temporary,
                  type => supervisor,
                  modules => [wpool]},
    supervisor:start_child(SupName, ChildSpec).

stop() ->
    [ stop(Type, Host, Tag) || {Type, Host, Tag} <- get_pools() ].

stop(Type) ->
    stop(Type, global).

stop(Type, Host) ->
    stop(Type, Host, default).

stop(Type, Host, Tag) ->
    try
        ets:delete(?MODULE, {Type, Host, Tag}),
        call_callback(stop, Type, [Host, Tag]),
        mongoose_wpool_mgr:stop(Type, Host, Tag)
    catch
        C:R:S ->
            ?ERROR_MSG("event=cannot_stop_pool,type=~p,host=~p,tag=~p,"
                       "class=~p,reason=~p,stack_trace=~p",
                       [Type, Host, Tag, C, R, S])
    end.

-spec is_configured(type()) -> boolean().
is_configured(Type) ->
    Pools = ejabberd_config:get_local_option_or_default(outgoing_pools, []),
    lists:keymember(Type, 1, Pools).

get_worker(Type) ->
    get_worker(Type, global).

get_worker(Type, Host) ->
    get_worker(Type, Host, default).

get_worker(Type, Host, Tag) ->
    case get_pool(Type, Host, Tag) of
        {ok, #mongoose_wpool{strategy = Strategy} = Pool} ->
            Worker = wpool_pool:Strategy(make_pool_name(Pool)),
            {ok, whereis(Worker)};
        Err ->
            Err
    end.

call(Type, Request) ->
    call(Type, global, Request).

call(Type, Host, Request) ->
    call(Type, Host, default, Request).

call(Type, Host, Tag, Request) ->
    case get_pool(Type, Host, Tag) of
        {ok, #mongoose_wpool{strategy = Strategy, call_timeout = CallTimeout} = Pool} ->
            wpool:call(make_pool_name(Pool), Request, Strategy, CallTimeout);
        Err ->
            Err
    end.

call(Type, Host, Tag, HashKey, Request) ->
    case get_pool(Type, Host, Tag) of
        {ok, #mongoose_wpool{call_timeout = CallTimeout} = Pool} ->
            wpool:call(make_pool_name(Pool), Request, {hash_worker, HashKey}, CallTimeout);
        Err ->
            Err
    end.

cast(Type, Request) ->
    cast(Type, global, Request).

cast(Type, Host, Request) ->
    cast(Type, Host, default, Request).

cast(Type, Host, Tag, Request) ->
    case get_pool(Type, Host, Tag) of
        {ok, #mongoose_wpool{strategy = Strategy} = Pool} ->
            wpool:cast(make_pool_name(Pool), Request, Strategy);
        Err ->
            Err
    end.

cast(Type, Host, Tag, HashKey, Request) ->
    case get_pool(Type, Host, Tag) of
        {ok, #mongoose_wpool{} = Pool} ->
            wpool:cast(make_pool_name(Pool), Request, {hash_worker, HashKey});
        Err ->
            Err
    end.

get_pool_settings(Type, Host, Tag) ->
    case get_pool(Type, Host, Tag) of
        {ok, PoolOpts} -> PoolOpts;
        {error, pool_not_started} -> undefined
    end.

get_pools() ->
    lists:map(fun(#mongoose_wpool{name = Name}) -> Name end, ets:tab2list(?MODULE)).

stats(Type, Host, Tag) ->
    wpool:stats(make_pool_name(Type, Host, Tag)).

-spec make_pool_name(type(), host(), tag()) -> atom().
make_pool_name(Type, Host, Tag) when is_atom(Host) ->
    make_pool_name(Type, atom_to_binary(Host, utf8), Tag);
make_pool_name(Type, Host, Tag) when is_binary(Host) ->
    binary_to_atom(<<"mongoose_wpool$", (atom_to_binary(Type, utf8))/binary, $$,
                     Host/binary, $$, (atom_to_binary(Tag, utf8))/binary>>, utf8).

make_pool_name(#mongoose_wpool{name = {Type, Host, Tag}}) ->
    make_pool_name(Type, Host, Tag).

call_start_callback(Type, Args) ->
    call_callback(start, Type, Args).

call_callback(Name, Type, Args) ->
    try
        CallbackModule = make_callback_module_name(Type),
        erlang:apply(CallbackModule, Name, Args)
    catch E:R:ST ->
          ?ERROR_MSG("event=wpool_callback_error, name=~p, error=~p, reason=~p, stacktrace=~p",
                     [Name, E, R, ST]),
          {error, {callback_crashed, Name, E, R, ST}}
    end.

-spec make_callback_module_name(type()) -> module().
make_callback_module_name(Type) ->
    Name = "mongoose_wpool_" ++ atom_to_list(Type),
    list_to_atom(Name).

default_opts(Type) ->
    Mod = make_callback_module_name(Type),
    case erlang:function_exported(Mod, default_opts, 0) of
        true -> Mod:default_opts();
        false -> []
    end.

expand_pools(Pools, AllHosts) ->
    %% First we select only pools for a specific vhost
    HostSpecific = [{Type, Host, Tag} ||
                     {Type, Host, Tag, _, _} <- Pools, is_binary(Host)],
    %% Then we expand all pools with `host` as Host parameter but using host specific configs
    %% if they were provided
    F = fun({Type, host, Tag, WpoolOpts, ConnOpts}) ->
                [{Type, Host, Tag, WpoolOpts, ConnOpts} || Host <- AllHosts,
                                                           not lists:member({Type, Host, Tag}, HostSpecific)];
           (Other) -> [Other]
        end,
    lists:flatmap(F, Pools).

get_unique_types(Pools) ->
    ordsets:to_list(ordsets:from_list([Type || {Type, _, _, _, _} <- Pools])).

get_pool(Type, Host, Tag) ->
    case ets:lookup(?MODULE, {Type, Host, Tag}) of
        [] when is_binary(Host) -> get_pool(Type, global, Tag);
        [] -> {error, pool_not_started};
        [Pool] -> {ok, Pool}
    end.
