-module(prop_helper).
%% @doc property helpers for ejabberd tests

-export([prop/2]).
-export([readable_bitstring/0, alnum_bitstring/0]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

prop(Name, Prop) ->
    Props = proper:conjunction([{Name, Prop}]),
    ?assert(proper:quickcheck(Props, [verbose, long_result, {numtests, 50}])).

readable_bitstring() ->
    ?LET(String, non_empty(list(choose($!, $z))),
         list_to_binary(String)).

alnum_bitstring() ->
    ?LET(String, non_empty(list(oneof([choose($a, $z), choose($0, $9)]))),
         list_to_binary(String)).

