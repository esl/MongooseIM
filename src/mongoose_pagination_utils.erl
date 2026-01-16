-module(mongoose_pagination_utils).
-export([slice/3]).

-spec slice(list(), integer() | undefined, integer()) -> list().
slice(List, undefined, 0) ->
    List;
slice(List, undefined, Offset) when is_list(List), is_integer(Offset), Offset > 0 ->
    nthtail_or_empty(Offset, List);
slice(List, Limit, Offset) ->
    lists:sublist(List, Offset + 1, Limit).

-spec nthtail_or_empty(non_neg_integer(), list()) -> list().
nthtail_or_empty(0, List) ->
    List;
nthtail_or_empty(N, [_ | Tail]) when N > 0 ->
    nthtail_or_empty(N - 1, Tail);
nthtail_or_empty(_N, []) ->
    [].
