-module(mongoose_wpool_redis).
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
    ProcName = mongoose_wpool:make_pool_name(redis, HostType, Tag),
    WpoolOpts = wpool_spec(WpoolOptsIn, ConnOpts),
    mongoose_wpool:start_sup_pool(redis, ProcName, WpoolOpts).

stop(_, _) ->
    ok.

is_supported_strategy(available_worker) -> false;
is_supported_strategy(_) -> true.

%% --------------------------------------------------------------
%%% Internal functions
wpool_spec(WpoolOptsIn, ConnOpts) ->
    Worker = {eredis_client, makeargs(ConnOpts)},
    [{worker, Worker} | WpoolOptsIn].

makeargs(RedisOpts) ->
    Host = proplists:get_value(host, RedisOpts, "127.0.0.1"),
    Port = proplists:get_value(port, RedisOpts, 6379),
    Database = proplists:get_value(database, RedisOpts, 0),
    Password = proplists:get_value(password, RedisOpts, ""),
    [Host, Port, Database, Password, 100, 5000].
