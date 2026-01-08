-module(mongoose_pagination_utils).
-export([slice/3]).

-spec slice(list(), integer() | undefined, integer()) -> list().
slice(List, undefined, 0) ->
    List;
slice(List, undefined, Offset) ->
    try lists:nthtail(Offset, List)
    catch _:_ -> [] end;
slice(List, Limit, Offset) ->
    lists:sublist(List, Offset + 1, Limit).
