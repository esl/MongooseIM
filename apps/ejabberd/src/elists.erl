-module(elists).

-export([insert/2]).

%% @doc Insert Elem into a List sorted in ascending order
%% maintaining that order.
-spec insert(Elem, List1) -> List2
    when Elem :: any(),
         List1 :: list(),
         List2 :: list().
insert(Elem, List1) ->
    insert({elem,Elem}, List1, []).

insert(done, L, Acc) ->
    lists:reverse(Acc) ++ L;
insert({elem, E}, [], Acc) ->
    lists:reverse([E | Acc]);
insert({elem, E}, [H|T], Acc) ->
    if
        E > H ->
            insert({elem, E}, T, [H|Acc]);
        E =< H ->
            insert(done, T, [H,E|Acc])
    end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

insert_test_() ->
    [?_assertEqual([1], insert(1,[])),
     ?_assertEqual([1,5], insert(5,[1])),
     ?_assertEqual([0,1,5], insert(0,[1,5])),
     ?_assertEqual([0,1,2,5], insert(2,[0,1,5])),
     ?_assertEqual([1,3,4,8,9,34,43,56,99],
                   insert(43,[1,3,4,8,9,34,56,99])),
     ?_assertEqual([0,1,2,2,5], insert(2,[0,1,2,5]))].

lists_merge_equivalence_test_() ->
    [?_assertEqual(insert(1, []), lists:merge([1], [])),
     ?_assertEqual(insert(5, [1]), lists:merge([5], [1])),
     ?_assertEqual(insert(0, [1,5]), lists:merge([0], [1,5])),
     ?_assertEqual(insert(2, [0,1,5]), lists:merge([2], [0,1,5])),
     ?_assertEqual(insert(43, [1,3,4,8,9,34,56,99]),
                   lists:merge([43],[1,3,4,8,9,34,56,99])),
     ?_assertEqual(insert(2, [0,1,2,5]), lists:merge([2], [0,1,2,5]))].

-endif.
