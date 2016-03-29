%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright 2016 Erlang Solutions, Ltd.
%%% @doc Cassandra functions
%%% @end
%%%-------------------------------------------------------------------
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

%% @doc Return timestamp in microseconds
now_timestamp() ->
    usec:from_now(os:timestamp()).
