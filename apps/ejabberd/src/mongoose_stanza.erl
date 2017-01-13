%%%-------------------------------------------------------------------
%%%
%%% This module encapsulates a data type which will initially be passed to
%%% hookhandlers as accumulator, and later will be passed all the way along
%%% processing chain.
%%%
%%%-------------------------------------------------------------------
-module(mongoose_stanza).
-author("bartek").

-include("jlib.hrl").

%% API
-export([new/0, from_kv/2, put/3, get/2, get/3, append/3, to_map/1]).
-export([from_element/1]).
-export_type([t/0]).

%% if it is defined as -opaque then dialyzer fails
-type t() :: map().

-spec new() -> t().
new() ->
    #{}.

-spec from_kv(atom(), any()) -> t().
from_kv(K, V) ->
    maps:put(K, V, #{}).

-spec from_element(xmlel()) -> t().
from_element(El) ->
    #{element => El}.

%% @doc convert to map so that we can pattern-match on it
-spec to_map(t()) -> map()|{error, cant_convert_to_map}.
to_map(P) when is_map(P) ->
    P;
to_map(_) ->
    {error, cant_convert_to_map}.

-spec put(atom(), any(), t()) -> t().
put(Key, Val, P) ->
    maps:put(Key, Val, P).

-spec get(atom()|[atom()], t()) -> any().
get([], _) ->
    undefined;
get([Key|Keys], P) ->
    case maps:is_key(Key, P) of
        true ->
            maps:get(Key, P);
        _ ->
            get(Keys, P)
    end;
get(Key, P) ->
    maps:get(Key, P).

-spec get(atom(), t(), any()) -> any().
get(Key, P, Default) ->
    maps:get(Key, P, Default).

-spec append(atom(), any(), t()) -> t().
append(Key, Val, P) ->
    L = get(Key, P, []),
    maps:put(Key, append(Val, L), P).

append(Val, L) when is_list(L), is_list(Val) ->
    L ++ Val;
append(Val, L) when is_list(L) ->
    [Val | L].
