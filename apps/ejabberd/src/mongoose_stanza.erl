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

%% API
-export([new/0, from_kv/2, put/3, get/2, get/3, append/3, to_map/1, update/2]).
-export([from_element/1, from_map/1, is_stanza/1]).
-export([initialise/3, terminate/3, dump/1]).


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

is_stanza(M) when not is_map(M) ->
    false;
is_stanza(M) ->
    maps:get(mongoose_stanza, M, false).

new() ->
    #{mongoose_stanza=>true}.

update(Stanza, M) ->
    maps:merge(Stanza, M).

from_map(M) ->
    maps:put(mongoose_stanza, true, M).

from_kv(K, V) ->
    M = maps:put(K, V, #{}),
    maps:put(mongoose_stanza, true, M).

from_element(El) ->
    #{element => El, mongoose_stanza=>true}.

%% @doc convert to map so that we can pattern-match on it
to_map(P) ->
    P.

put(Key, Val, P) ->
    maps:put(Key, Val, P).

%% @doc a version of get specyfying multiple keys, we check all of them until we find something
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

get(Key, P, Default) ->
    maps:get(Key, P, Default).

append(Key, Val, P) ->
    L = get(Key, P, []),
    maps:put(Key, append(Val, L), P).

append(Val, L) when is_list(L), is_list(Val) ->
    L ++ Val;
append(Val, L) when is_list(L) ->
    [Val | L].
