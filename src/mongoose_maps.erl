-module(mongoose_maps).
-export([maps_append/3]).
-export([maps_foreach/2]).
-export([pairs_foreach/2]).
-export([maps_or_pairs_foreach/2]).

%% Appends a new Value to the current list of values associated with Key.
maps_append(Key, Value, Map) ->
    Values = maps:get(Key, Map, []),
    maps:put(Key, Values ++ [Value], Map).

-spec maps_foreach(fun(), map()) -> ok.
maps_foreach(Fun, Map) when is_function(Fun, 1) ->
    maps:fold(fun(Key, Value, Acc) ->
                      Fun({Key, Value}), Acc
              end, ok, Map);
maps_foreach(Fun, Map) when is_function(Fun, 2) ->
    maps:fold(fun(Key, Value, Acc) ->
                      Fun(Key, Value), Acc
              end, ok, Map).

%% Disable dialyzer for the function. See dialyzer attribute.
-spec pairs_foreach(Fun, [{Key, Value}]) -> ok
    when
      Fun :: fun((Key, Value) -> term())
           | fun(({Key, Value}) -> term()),
      Key :: term(),
      Value :: term().
pairs_foreach(Fun, List) when is_function(Fun, 1) ->
    lists:foreach(Fun, List);
pairs_foreach(Fun, List) when is_function(Fun, 2) ->
    lists:foreach(fun({K,V}) -> Fun(K,V) end, List).

maps_or_pairs_foreach(Fun, Map) when is_map(Map) ->
    maps_foreach(Fun, Map);
maps_or_pairs_foreach(Fun, List) when is_list(List) ->
    pairs_foreach(Fun, List).
