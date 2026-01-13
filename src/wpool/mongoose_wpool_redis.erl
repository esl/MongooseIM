-module(mongoose_wpool_redis).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4]).
-export([stop/2]).
-export([is_supported_strategy/1]).

%% --------------------------------------------------------------
%% mongoose_wpool callbacks
-spec init() -> ok.
init() ->
    {ok, _} = application:ensure_all_started([eredis], permanent),
    ok.

-spec start(mongooseim:host_type_or_global(), mongoose_wpool:tag(),
            mongoose_wpool:pool_opts(), mongoose_wpool:conn_opts()) -> {ok, pid()} | {error, any()}.
start(HostType, Tag, WpoolOptsIn, ConnOpts) ->
    ProcName = mongoose_wpool:make_pool_name(redis, HostType, Tag),
    WpoolOpts = wpool_spec(WpoolOptsIn, ConnOpts),
    mongoose_wpool:start_sup_pool(redis, ProcName, WpoolOpts).

-spec stop(mongooseim:host_type_or_global(), mongoose_wpool:tag()) -> ok.
stop(_, _) ->
    ok.

is_supported_strategy(available_worker) -> false;
is_supported_strategy(_) -> true.

%% --------------------------------------------------------------
%%% Internal functions
wpool_spec(WpoolOptsIn, ConnOpts) ->
    Worker = {eredis_client, makeargs(ConnOpts)},
    [{worker, Worker} | WpoolOptsIn].

makeargs(Opts = #{host := _Host, port := _Port, database := _Database, password := _Password}) ->
    BaseOpts = maps:with([host, port, database, password], Opts),
    TlsOpts = tls_opts(Opts),
    proplists:from_map(BaseOpts) ++ TlsOpts ++ [{reconnect_sleep, 100}, {connect_timeout, 5000}].

tls_opts(#{tls := TLSOpts}) ->
    [{tls, just_tls:make_client_opts(TLSOpts)}];
tls_opts(#{}) ->
    [].
