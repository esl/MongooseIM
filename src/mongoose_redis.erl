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

%% API
-export([start_pool/1, cmd/1, cmd/2, cmds/1, cmds/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_pool(list()) -> {ok, pid()} | {error, {already_started, pid()}}.
start_pool(Opts) ->
    PoolSize = proplists:get_value(pool_size, Opts, 10),
    RedisOpts = proplists:get_value(worker_config, Opts, []),
    PoolOpts = proplists:get_value(pool_opts, Opts, []),
    PoolOptions = [{strategy, random_worker},
                   {workers, PoolSize}
                   | PoolOpts],
    case mongoose_wpool:start(redis, global, default, PoolOptions, RedisOpts) of
        {ok, Pid} -> {ok, Pid};
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
    {ok, Worker} = mongoose_wpool:get_worker(redis, global, default),
    case eredis:q(Worker, Cmd, Timeout) of
        {ok, Value} -> Value;
        V -> V
    end.

-spec cmds([iolist()], integer()) -> [eredis:return_value()]
                                     | {'error', _}.
cmds(Cmd, Timeout) ->
    {ok, Worker} = mongoose_wpool:get_worker(redis, global, default),
    eredis:qp(Worker, Cmd, Timeout).

