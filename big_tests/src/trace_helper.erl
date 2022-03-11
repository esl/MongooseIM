-module(trace_helper).
-export([run/2]).
-export([flush/0]).
-export([stop/1]).

run(Parent, Patterns) when is_pid(Parent) ->
    Starter = self(),
    Pid = spawn(fun() ->
            erlang:monitor(process, Parent),
            erlang:trace(all, true, [call]),
            [erlang:trace_pattern(Pattern, true, []) || Pattern <- Patterns],
            Starter ! {started, self()},
            cycle(Parent)
        end),
    %% Do not return until tracing is on
    receive
        {started, Pid} -> Pid
    after 5000 -> {error, timeout}
    end.

stop(Pid) ->
    Pid ! stop,
    Mon = erlang:monitor(process, Pid),
    receive
        {'DOWN', Mon, _, _, _} -> ok
    after 5000 -> erlang:error(timeout)
    end.

cycle(Parent) ->
    receive
        stop ->
            ok;
        {'DOWN', _, _, _, _} ->
            ok;
        M ->
            Parent ! M,
            cycle(Parent)
    end.

flush() ->
    receive M -> [M | flush()] after 0 -> [] end.
