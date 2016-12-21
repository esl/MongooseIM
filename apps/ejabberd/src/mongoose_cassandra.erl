-module(mongoose_cassandra).
-export([start/0, stop/0]).
-export([now_timestamp/0]).

-callback prepared_queries() -> list({term(), string()}).

start() ->
    case ejabberd_config:get_local_option(cassandra_servers) of
        undefined ->
            ignore;
        Pools ->
            [start_pool(Pool) || Pool <- Pools]
    end.

start_pool({PoolName, PoolConfig}) ->
    mongoose_cassandra_sup:start(PoolName, extend_config(PoolConfig)).

extend_config(PoolConfig) ->
    PoolConfig
        ++ [{servers, [{"localhost", 9042, 1}]},
            {socket_options, [{connect_timeout, 4000}]},
            {keyspace, "mongooseim"},
            {credentials, undefined}].

-spec stop() -> _.
stop() ->
    mongoose_cassandra_sup:stop().

%% @doc Return timestamp in nanoseconds
now_timestamp() ->
    now_to_usec(os:timestamp()).

-spec now_to_usec(erlang:timestamp()) -> non_neg_integer().
now_to_usec({MSec, Sec, USec}) ->
    (MSec*1000000 + Sec)*1000000 + USec.
