-module(mongoose_cassandra).
-export([start/0, stop/0]).

start() ->
    case ejabberd_config:get_local_option(cassandra_servers) of
        undefined ->
            ignore;
        Pools ->
            [start_pool(Pool) || Pool <- Pools]
    end.

start_pool({PoolName, PoolConfig}) ->
    cassandra_sup:start(PoolName, extend_config(PoolConfig)).

extend_config(PoolConfig) ->
    PoolConfig
    ++ [{servers, [{"localhost", 9042, 1}]},
        {socket_options, [{connect_timeout, 4000}]},
        {keyspace, "mongooseim"},
        {credentials, undefined}].

-spec stop() -> _.
stop() ->
    cassandra_sup:stop().
