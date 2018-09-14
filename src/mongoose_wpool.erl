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

-record(mongoose_wpool, {name :: term(), strategy :: atom(), call_timeout :: pos_integer()}).
-dialyzer({no_match, start/4}).

%% API
-export([ensure_started/0,
         start/2, start/3, start/4, start/5,
         stop/1, stop/2, stop/3,
         get_worker/1, get_worker/2, get_worker/3,
         call/2, call/3, call/4, call/5,
         cast/2, cast/3, cast/4, cast/5,
         get_pool_settings/3, get_pools/0, stats/3]).

-export([start_configured_pools/0]).
-export([start_configured_pools/1]).
-export([is_configured/1]).

%% Mostly for tests
-export([make_pool_name/3]).

-type type() :: redis | riak | http | rdbms | cassandra | elastic | generic.
-type host() :: global | host | jid:lserver().
-type tag() :: atom().

%-callback wpool_spec(WPoolOpts :: [wpool:option()], ConnOpts :: [{atom(), any()}]) -> [wpool:option()].
-callback start(host(), tag(), WPoolOpts :: [wpool:option()], ConnOpts :: [{atom(), any()}]) ->
    {ok, pid()} | {error, Reason :: term()}.

ensure_started() ->
    wpool:start(),
    case ets:info(?MODULE) of
        undefined ->
            % we set heir here because the whole thing may be started by an ephemeral process
            ets:new(?MODULE, [named_table, public,
                {read_concurrency, true},
                {keypos, 2},
                {heir, whereis(wpool_sup), undefined}]);
        _ ->
            ok
    end.

start_configured_pools() ->
    Pools = ejabberd_config:get_local_option_or_default(outgoing_pools, []),
    start_configured_pools(Pools).

start_configured_pools(Pools) ->
    [start(Pool) || Pool <- Pools].

start({Type, Host, Tag, PoolOpts, ConnOpts}) ->
    ?INFO_MSG("event=starting_pool, pool=~p, host=~p, tag=~p, pool_opts=~p, conn_opts=~p",
              [Type, Host, Tag, PoolOpts, ConnOpts]),
    start(Type, Host, Tag, PoolOpts, ConnOpts).


start(Type, PoolOpts) ->
    start(Type, global, PoolOpts).

start(Type, Host, PoolOpts) ->
    start(Type, Host, default, PoolOpts).

start(Type, Host, Tag, PoolOpts) ->
    start(Type, Host, Tag, PoolOpts, []).

start(Type, Host, Tag, PoolOpts, ConnOpts) ->
    {Opts0, WpoolOptsIn} = proplists:split(PoolOpts, [strategy, call_timeout]),
    Opts = lists:append(Opts0),
    %WpoolOpts = maybe_get_wpool_opts_from_callback(Type, WpoolOptsIn, ConnOpts),
    %case wpool:start_sup_pool(make_pool_name(Type, Host, Tag), WpoolOpts) of
    case start_by_callback(Type, Host, Tag, WpoolOptsIn,ConnOpts) of
        {ok, Pid} ->
            Strategy = proplists:get_value(strategy, Opts, best_worker),
            CallTimeout = proplists:get_value(call_timeout, Opts, 5000),
            ets:insert(?MODULE, #mongoose_wpool{name = {Type, Host, Tag},
                                                strategy = Strategy,
                                                call_timeout = CallTimeout}),
            {ok, Pid};
        Error ->
            Error
    end.

stop(Type) ->
    stop(Type, global).

stop(Type, Host) ->
    stop(Type, Host, default).

stop(Type, Host, Tag) ->
    ets:delete(?MODULE, {Type, Host, Tag}),
    wpool:stop_sup_pool(make_pool_name(Type, Host, Tag)).

-spec is_configured(type()) -> boolean().
is_configured(Type) ->
    Pools = ejabberd_config:get_local_option_or_default(outgoing_pools, []),
    lists:keymember(Type, 1, Pools).

get_worker(Type) ->
    get_worker(Type, global).

get_worker(Type, Host) ->
    get_worker(Type, Host, default).

get_worker(Type, Host, Tag) ->
    case ets:lookup(?MODULE, {Type, Host, Tag}) of
        [#mongoose_wpool{strategy = Strategy}] ->
            Worker = wpool_pool:Strategy(make_pool_name(Type, Host, Tag)),
            {ok, whereis(Worker)};
        [] ->
            {error, pool_not_started}
    end.

call(Type, Request) ->
    call(Type, global, Request).

call(Type, Host, Request) ->
    call(Type, Host, default, Request).

call(Type, Host, Tag, Request) ->
    case ets:lookup(?MODULE, {Type, Host, Tag}) of
        [#mongoose_wpool{strategy = Strategy, call_timeout = CallTimeout}] ->
            wpool:call(make_pool_name(Type, Host, Tag), Request, Strategy, CallTimeout);
        [] ->
            {error, pool_not_started}
    end.

call(Type, Host, Tag, HashKey, Request) ->
    case ets:lookup(?MODULE, {Type, Host, Tag}) of
        [#mongoose_wpool{call_timeout = CallTimeout}] ->
            wpool:call(make_pool_name(Type, Host, Tag), Request, {hash_worker, HashKey}, CallTimeout);
        [] ->
            {error, pool_not_started}
    end.

cast(Type, Request) ->
    cast(Type, global, Request).

cast(Type, Host, Request) ->
    cast(Type, Host, default, Request).

cast(Type, Host, Tag, Request) ->
    case ets:lookup(?MODULE, {Type, Host, Tag}) of
        [#mongoose_wpool{strategy = Strategy}] ->
            wpool:cast(make_pool_name(Type, Host, Tag), Request, Strategy);
        [] ->
            {error, pool_not_started}
    end.

cast(Type, Host, Tag, HashKey, Request) ->
    case ets:lookup(?MODULE, {Type, Host, Tag}) of
        [#mongoose_wpool{}] ->
            wpool:cast(make_pool_name(Type, Host, Tag), Request, {hash_worker, HashKey});
        [] ->
            {error, pool_not_started}
    end.

get_pool_settings(Type, Host, Tag) ->
    case ets:lookup(?MODULE, {Type, Host, Tag}) of
        [PoolOpts] -> PoolOpts;
        [] -> undefined
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

start_by_callback(Type, Host, Tag, WpoolOpts, ConnOpts) ->
    try
        CallbackModule = make_callback_module_name(Type),
        CallbackModule:start(Host, Tag, WpoolOpts, ConnOpts)
    catch E:R ->
          ?ERROR_MSG("event=wpool_callback_error, error=~p, reason=~p, stacktrace=~p",
                    [E, R, erlang:get_stacktrace()]),
          {error, pool_not_started}
    end.

-spec make_callback_module_name(type()) -> module().
make_callback_module_name(Type) ->
    Name = "mongoose_wpool_" ++ atom_to_list(Type),
    list_to_existing_atom(Name).
