-module(par).
-export([map/2]).

map(F, L) when is_function(F, 1), is_list(L) ->
    TaskId = {make_ref(), self()},
    [spawn(worker(TaskId, F, El)) || El <- tag(L)],
    collect(TaskId, length(L), []).

tag(L) -> lists:zip(lists:seq(1, length(L)), L).
untag(L) -> [ Val || {_Ord,Val} <- lists:sort(L) ].

reply(Ord, {Ref,Pid}, Val) -> Pid ! {Ref, {Ord, Val}}.

worker(TaskId, Fun, {Ord, Item}) -> fun() -> reply(Ord, TaskId, Fun(Item)) end.

collect(_TaskId, 0, Acc) -> untag(Acc);
collect({Ref, _}=TaskId, N, Acc) ->
    receive {Ref, Val} -> collect(TaskId, N-1, [Val|Acc])
    after 5000 -> error({partial_results, Acc})
    end.

