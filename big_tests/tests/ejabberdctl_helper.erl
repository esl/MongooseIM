%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2015, Erlang Solutions
%%% @doc
%%%
%%% @end
%%% Created : 12. Nov 2015 14:39
%%%-------------------------------------------------------------------
-module(ejabberdctl_helper).
-author("ludwikbukowski").

-compile(export_all).

-include_lib("escalus/include/escalus.hrl").

-import(distributed_helper, [mim/0,
                             rpc/4]).

-spec ejabberdctl(Cmd :: string(), Args :: [binary() | string()], Config :: list()) ->
    {Data :: iolist(), ExitStatus :: integer()} | no_return().
ejabberdctl(Cmd, Args, Config) ->
    #{node := Node} = mim(),
    ejabberdctl(Node, Cmd, Args, Config).

ejabberdctl(Node, Cmd, Args, Config) ->
    CtlCmd = distributed_helper:ctl_path(Node, Config),
    run(CtlCmd, [Cmd | Args]).

rpc_call(M, F, Args) ->
    case rpc(mim(), M, F, Args) of
        {badrpc, Reason} ->
            ct:fail("~p:~p/~p with arguments ~w fails with reason ~p.",
                    [M, F, length(Args), Args, Reason]);
        Result ->
            Result
    end.

run(Cmd, Args) ->
    run(Cmd, Args, [], 60000).

run(Cmd, Args, Opts) ->
    run(Cmd, Args, Opts, 60000).

run(Cmd, Args, Opts, Timeout) ->
    Port = erlang:open_port({spawn_executable, Cmd},
                            [exit_status, {args, Args} | Opts]),
    loop(Cmd, Port, [], Timeout).

loop(Cmd, Port, Data, Timeout) ->
    receive
        {Port, {data, NewData}} -> loop(Cmd, Port, Data++NewData, Timeout);
        {Port, {exit_status, ExitStatus}} -> {Data, ExitStatus}
    after Timeout ->
        erlang:error(#{reason => timeout, command => Cmd})
    end.
