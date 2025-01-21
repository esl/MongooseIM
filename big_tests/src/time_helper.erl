-module(time_helper).
-export([validate_datetime/1]).

%% @doc Validates that string is in ISO 8601 format
-spec validate_datetime(string()) -> boolean().
validate_datetime(TimeStr) ->
    [Date, Time] = string:tokens(TimeStr, "T"),
    validate_date(Date) and validate_time(Time).

validate_date(Date) ->
    [Y, M, D] = string:tokens(Date, "-"),
    Date1 = {list_to_integer(Y), list_to_integer(M), list_to_integer(D)},
    calendar:valid_date(Date1).

validate_time(Time) ->
  [T | _] = string:tokens(Time, "Z"),
  validate_time1(T).

validate_time1(Time) ->
    [H, M, S] = string:tokens(Time, ":"),
    check_list([{H, 24}, {M, 60}, {S, 60}]).

check_list(List) ->
    lists:all(fun({V, L}) -> I = list_to_integer(V), I >= 0 andalso I < L end, List).
