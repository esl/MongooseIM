%%%-------------------------------------------------------------------
%%%
%%% This module encapsulates a data type which will initially be passed to
%%% hookhandlers as accumulator, and later will be passed all the way along
%%% processing chain.
%%%
%%%-------------------------------------------------------------------
-module(mongoose_stanza).
-author("bartek").

-include("ejabberd.hrl").
-include("jlib.hrl").

%% API
-export([new/0, from_kv/2, put/3, get/2, get/3, append/3, to_map/1, update/2]).
-export([from_element/1, from_map/1, is_stanza/1]).
-export([initialise/3, terminate/3, dump/1]).
-export_type([t/0]).

%% if it is defined as -opaque then dialyzer fails
-type t() :: map().

%% development only
initialise(Map, F, L) ->
%%    ?ERROR_MSG("AAA Initialize stanza ~p ~p", [F, L]),
    % we call it at the entry
    maps:put(mongoose_stanza, true, Map).
terminate(Stanza, F, L) ->
%%    ?ERROR_MSG("AAA Terminate stanza ~p ~p", [F, L]),
    % here we stop using stanza and revert to original xmlel
    maps:get(element, Stanza).
dump(Stanza) ->
    Keys = lists:sort(maps:keys(Stanza)),
    ?ERROR_MSG("------", []),
    lists:map(fun(K) -> ?ERROR_MSG("~p = ~p", [K, maps:get(K, Stanza)]) end, Keys).

-spec is_stanza(any()) -> boolean().
is_stanza(M) when not is_map(M) ->
    false;
is_stanza(M) ->
    maps:get(mongoose_stanza, M, false).

-spec new() -> t().
new() ->
    #{mongoose_stanza=>true}.

-spec update(t(), t()) -> t().
update(Stanza, M) ->
    maps:merge(Stanza, M).

-spec from_map(map()) -> t().
from_map(M) ->
    maps:put(mongoose_stanza, true, M).

-spec from_kv(atom(), any()) -> t().
from_kv(K, V) ->
    M = maps:put(K, V, #{}),
    maps:put(mongoose_stanza, true, M).

-spec from_element(xmlel()) -> t().
from_element(El) ->
    #{element => El, mongoose_stanza=>true}.

%% @doc convert to map so that we can pattern-match on it
-spec to_map(t()) -> map()|{error, cant_convert_to_map}.
to_map(P) when is_map(P) ->
    P;
to_map(_) ->
    {error, cant_convert_to_map}.

-spec put(atom(), any(), t()) -> t().
put(Key, Val, P) ->
    maps:put(Key, Val, P).

%% @doc a version of get specyfying multiple keys, we check all of them until we find something
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
