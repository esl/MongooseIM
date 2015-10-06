-module(mochi2jsx).

-export([m2j/1, j2m/1]).

% String:  {"a" : [ {"b": 10} , 2] } 
% JSX:     [{ <<"a">> , [ [{ <<"b">> ,10 }],2] }]
% JSX:     {struct, [ <<"a">> , [[{ <<"b">> ,10 }],2] ] }
% Mochi:   { struct, [{ <<"a">>, [{struct,[{<<"b">>,10}]},2]}]}

j2m([]) -> [];
j2m(Obj) -> do_j2m(Obj).

do_j2m([{}]) -> {struct, []};
do_j2m([{_, _} | _R] = L) -> {struct, [{K, do_j2m(V)} || {K, V} <- L]};
do_j2m([H|T]) -> [ do_j2m(H) | do_j2m(T)];
do_j2m(X) -> X.

m2j([]) -> [];
m2j(Obj) -> do_m2j(Obj).

do_m2j({struct, []}) -> [{}];
do_m2j({struct, [{_, _} | _R] = L}) -> [{K, do_m2j(V)} || {K,V} <- L];
do_m2j([H|T]) -> [do_m2j(H) | do_m2j(T)];
do_m2j(X) -> X.




