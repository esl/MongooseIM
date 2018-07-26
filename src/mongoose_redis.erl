%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 27. Jun 2018 17:16
%%%-------------------------------------------------------------------
-module(mongoose_redis).
-author("bartlomiej.gorny@erlang-solutions.com").
-include("mongoose.hrl").
-dialyzer({no_match, start_pool/1}).
-define(POOL_NAME, redis_pool).

%% API
-export([start_pool/1, cmd/1, cmd/2, cmds/1, cmds/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_pool(list()) -> {ok, pid()} | {error, {already_started, pid()}}.
start_pool(Opts) ->
    mongoose_wpool:ensure_started(),
    PoolSize = proplists:get_value(pool_size, Opts, 10),
    RedisOpts = proplists:get_value(worker_config, Opts, []),
    PoolOpts = proplists:get_value(pool_opts, Opts, []),
    PoolOptions = [{workers, PoolSize},
                   {worker, {eredis_client, makeargs(RedisOpts)}}
                  ] ++ PoolOpts,
    Res = wpool:start_sup_pool(?POOL_NAME, PoolOptions),
    case Res of
        {ok, _Pid} -> Res;
        Error ->
            ?ERROR_MSG("Failed to start worker pool, reason: ~p~n", [Error]),
            Error
    end.

-spec cmd(iolist()) -> undefined
                       | binary()
                       | [binary() | [binary() | integer()] | integer() | {'error', _}]
                       | integer()
                       | {'error', _}.
cmd(Cmd) ->
    cmd(Cmd, 5000).

-spec cmds([iolist()]) -> undefined
                          | binary()
                          | [binary() | [binary() | integer()] | integer() | {'error', _}]
                          | integer()
                          | {'error', _}.
cmds(Cmd) ->
    cmds(Cmd, 5000).

-spec cmd(iolist(), integer()) -> eredis:return_value()
                                  | {'error', _}.
cmd(Cmd, Timeout) ->
    case eredis:q(wpool_pool:random_worker(?POOL_NAME), Cmd, Timeout) of
        {ok, Value} -> Value;
        V -> V
    end.

-spec cmds([iolist()], integer()) -> [eredis:return_value()]
                                     | {'error', _}.
cmds(Cmd, Timeout) ->
    eredis:qp(wpool_pool:random_worker(?POOL_NAME), Cmd, Timeout).

%%%===================================================================
%%% Internal functions
%%%===================================================================

makeargs(RedisOpts) ->
    Host = proplists:get_value(host, RedisOpts, "localhost"),
    Port = proplists:get_value(port, RedisOpts, 6379),
    Database = proplists:get_value(database, RedisOpts, 0),
    Password = proplists:get_value(password, RedisOpts, ""),
    [Host, Port, Database, Password, 100, 5000].

