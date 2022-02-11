-module(mongoose_wpool_riak).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4]).
-export([stop/2]).
-export([is_supported_strategy/1]).

%% --------------------------------------------------------------
%% mongoose_wpool callbacks
init() ->
    ok.

start(HostType, Tag, WpoolOptsIn, ConnOpts) ->
    ProcName = mongoose_wpool:make_pool_name(riak, HostType, Tag),
    WpoolOpts = wpool_spec(WpoolOptsIn, ConnOpts),
    mongoose_wpool:start_sup_pool(riak, ProcName, WpoolOpts).

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
    SecurityOptsKeys = [credentials, cacertfile, ssl_opts],
    SecurityOpts = maps:with(SecurityOptsKeys, ConnOpts),
    ListOpts = maps:to_list(SecurityOpts),
    lists:map(fun prepare_credentials/1, ListOpts).

prepare_credentials({credentials, {User, Password}}) ->
    {credentials, User, Password};
prepare_credentials(Opt) ->
    Opt.
