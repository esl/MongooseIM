-module(mongoose_redis).
-author("bartlomiej.gorny@erlang-solutions.com").

%% API
-export([cmd/1, cmd/2, cmds/1, cmds/2]).

-ignore_xref([cmd/2, cmds/2]).

-include_lib("eredis/include/eredis.hrl").
-export_type([return_value/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec cmd(iolist()) -> return_value() | {error, _}.
cmd(Cmd) ->
    cmd(Cmd, 5000).

-spec cmds([iolist()]) ->
    [{ok, return_value()} | {error, Reason :: binary()}] | {error, no_connection}.
cmds(Cmd) ->
    cmds(Cmd, 5000).

-spec cmd(iolist(), integer()) -> return_value() | {error, _}.
cmd(Cmd, Timeout) ->
    {ok, Worker} = mongoose_wpool:get_worker(redis, global, default),
    case eredis:q(Worker, Cmd, Timeout) of
        {ok, Value} -> Value;
        V -> V
    end.

-spec cmds([iolist()], integer()) ->
    [{ok, return_value()} | {error, Reason :: binary()}] | {error, no_connection}.
cmds(Cmd, Timeout) ->
    {ok, Worker} = mongoose_wpool:get_worker(redis, global, default),
    eredis:qp(Worker, Cmd, Timeout).
