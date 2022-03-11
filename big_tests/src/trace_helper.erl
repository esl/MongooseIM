-module(trace_helper).
-export([run/2]).
-export([flush/0]).
-export([stop/1]).

run(Parent, Patterns) when is_pid(Parent) ->
    spawn(fun() ->
            erlang:monitor(process, Parent),
            erlang:trace(all, true, [call]),
            [erlang:trace_pattern(Pattern, true, []) || Pattern <- Patterns],
            cycle(Parent)
        end).

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
