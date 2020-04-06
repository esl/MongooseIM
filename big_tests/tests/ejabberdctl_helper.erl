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
    run(string:join([CtlCmd, Cmd | normalize_args(Args)], " ")).

rpc_call(M, F, Args) ->
    case rpc(mim(), M, F, Args) of
        {badrpc, Reason} ->
            ct:fail("~p:~p/~p with arguments ~w fails with reason ~p.",
                    [M, F, length(Args), Args, Reason]);
        Result ->
            Result
    end.

normalize_args(Args) ->
    lists:map(fun
                  (Arg) when is_binary(Arg) ->
                      binary_to_list(Arg);
                  (Arg) when is_list(Arg) ->
                      Arg
              end, Args).

run(Cmd) ->
    run(Cmd, 60000).

run(Cmd, Timeout) ->
    Port = erlang:open_port({spawn, Cmd}, [exit_status]),
    loop(Cmd, Port, [], Timeout).

loop(Cmd, Port, Data, Timeout) ->
    receive
        {Port, {data, NewData}} -> loop(Cmd, Port, Data++NewData, Timeout);
        {Port, {exit_status, ExitStatus}} -> {Data, ExitStatus}
    after Timeout ->
        erlang:error(#{reason => timeout, command => Cmd})
    end.
