-module(mongoose_wpool_riak).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4]).
-export([stop/2]).
-export([is_supported_strategy/1]).

%% --------------------------------------------------------------
%% mongoose_wpool callbacks
-spec init() -> ok.
init() ->
    ok.

-spec start(mongooseim:host_type_or_global(), mongoose_wpool:tag(),
            mongoose_wpool:pool_opts(), mongoose_wpool:conn_opts()) -> {ok, pid()} | {error, any()}.
start(HostType, Tag, WpoolOptsIn, ConnOpts) ->
    ProcName = mongoose_wpool:make_pool_name(riak, HostType, Tag),
    WpoolOpts = wpool_spec(WpoolOptsIn, ConnOpts),
    mongoose_wpool:start_sup_pool(riak, ProcName, WpoolOpts).

-spec stop(mongooseim:host_type_or_global(), mongoose_wpool:tag()) -> ok.
stop(_, _) ->
    ok.

is_supported_strategy(available_worker) -> false;
is_supported_strategy(_) -> true.

%% --------------------------------------------------------------
%% Other functions

wpool_spec(WpoolOptsIn, ConnOpts = #{address := RiakAddr, port := RiakPort}) ->
    SecurityOptsKV = prepare_sec_opts(ConnOpts),
    RiakPBOpts = [auto_reconnect, keepalive],
    WorkerArgs = RiakPBOpts ++ SecurityOptsKV,
    Worker = {riakc_pb_socket, [RiakAddr, RiakPort, WorkerArgs]},
    [{worker, Worker} | WpoolOptsIn].

prepare_sec_opts(ConnOpts) ->
    lists:flatmap(fun(Opt) -> sec_opts(Opt, ConnOpts) end, [credentials, tls]).

sec_opts(credentials, #{credentials := #{user := User, password := Password}}) ->
    [{credentials, User, Password}];
sec_opts(tls, #{tls := TLSOpts}) ->
    TLSOpts;
sec_opts(_Opt, #{}) ->
    [].
