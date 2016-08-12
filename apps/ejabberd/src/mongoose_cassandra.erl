%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright 2016 Erlang Solutions, Ltd.
%%% @doc Cassandra functions
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_cassandra).
-export([start/0, stop/0]).
-export([now_timestamp/0]).
-export([status/0, get_pools/0]).

-include_lib("ejabberd/include/ejabberd.hrl").

-callback prepared_queries() -> list({term(), string()}).

start() ->
    [start_pool(Pool) || Pool <- get_pools()].

start_pool({PoolName, PoolConfig}) ->
    PoolConfig1 = deduplicate_proplist(expand_config(PoolConfig)),
    mongoose_cassandra_sup:start(PoolName, PoolConfig1).

expand_config(PoolConfig) ->
    expand_servers(PoolConfig)
    ++ [{servers, [{"localhost", 9042, 1}]},
        {socket_options, [{connect_timeout, 4000}]},
        {keyspace, "mongooseim"},
        {credentials, undefined}].

expand_servers(PoolConfig) ->
    Address     = proplists:get_value(address, PoolConfig),
    Servers     = proplists:get_value(servers, PoolConfig, []),
    expand_servers(Address, Servers, PoolConfig).

expand_servers(undefined, undefined, PoolConfig) ->
    [{servers, [{"localhost", 9042, 1}]}];
expand_servers(undefined, Servers, PoolConfig) ->
    PoolConfig;
expand_servers(Address, undefined, PoolConfig) ->
    DefaultPort = proplists:get_value(port, PoolConfig, 9042),
    PoolSize    = proplists:get_value(pool_size, PoolConfig, 5),
    Servers = expand_address(Address, DefaultPort, PoolSize),
    [{servers, Servers} | PoolConfig];
expand_servers(_Address, _Servers, PoolConfig) ->
    ?ERROR_MSG("issue=config_error, details=\"Both servers and address options "
               "can't be specified at the same time\", pool_config=~1000p",
               [PoolConfig]),
    erlang:error(config_error).

%% "localhost:9042|otherhost" => [{"localhost", 9042, 50}, {"otherhost", 9042, 50}]
expand_address(Address, DefaultPort, PoolSize) when is_integer(DefaultPort),
                                                    is_integer(PoolSize) ->
    %% Make PoolSize connections to each server
    %% TODO better logic is possible here
    [make_server(Server, DefaultPort, PoolSize)
     || Server <- string:tokens(Address, "|")].

make_server(Server, DefaultPort, PoolSize) ->
    case string:tokens(Server, ":") of
        [Host, Port] ->
            {Host, list_to_integer(Port), PoolSize};
        [Host] ->
            {Host, DefaultPort, PoolSize}
    end.

-spec stop() -> _.
stop() ->
    mongoose_cassandra_sup:stop().

%% @doc Return timestamp in microseconds
now_timestamp() ->
    usec:from_now(os:timestamp()).


%% Check that Cassandra connections are fine.
%%
%% disabled - cassandra is not configured
%% ok - each pool has alive connections
%% failure - one or more pools have dead connections
-spec status() -> disabled | ok | failure.
status() ->
    case get_pools() of
        [] ->
            disabled;
        Pools ->
            all_ok([pool_status(PoolName) || {PoolName, _} <- Pools])
    end.

pool_status(PoolName) ->
    case mongoose_cassandra_worker:test_query(PoolName) of
        [] ->
            %% empty pool
            failure;
        [_|_] = Results ->
            all_ok([Status || {_Worker, Status} <- Results])
    end.

all_ok(List) ->
    case lists:all(fun(X) -> X =:= ok end, List) of
        true -> ok;
        false -> failure
    end.

deduplicate_proplist(List) ->
    [{Key, proplists:get_value(Key, List)} || Key <- proplists:get_keys(List)].

get_pools() ->
    [{PoolName, PoolConfig}
     || {local_config, {cassandra_server, PoolName, global}, PoolConfig}
        <- ejabberd_config:get_local_config()].
