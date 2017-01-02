%%%-------------------------------------------------------------------
%%%
%%% This module encapsulates a data type which will initially be passed to
%%% hookhandlers as accumulator, and later will be passed all the way along
%%% processing chain.
%%%
%%%-------------------------------------------------------------------
-module(mongoose_stanza).
-author("bartek").

%% API
-export([new/0, new/1, put/3, get/2, get/3, append/3, to_map/1]).


new() ->
    #{}.

new(A) when is_map(A) ->
    A.

%% @doc convert to map so that we can pattern-match on it
to_map(P) ->
    P.

put(Key, Val, P) ->
    maps:put(Key, Val, P).

get(Key, P) ->
    maps:get(Key, P).

get(Key, P, Default) ->
    maps:get(Key, P, Default).

append(Key, Val, P) ->
    L = get(Key, P, []),
    maps:put(Key, append(Val, L), P).

append(Val, L) when is_list(L), is_list(Val) ->
    L ++ Val;
append(Val, L) when is_list(L) ->
    [Val | L].
