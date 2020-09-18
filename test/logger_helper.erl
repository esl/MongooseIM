%%%-------------------------------------------------------------------
%%% @copyright 2020, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(logger_helper).

%% API
-export(['contains?'/2,
         filter_out_non_matching/2,
         get_at_least_n_log_lines/3,
         get_at_least_n_log_lines/4,
         get_log/1,
         levels/0,
         levels_less_than_or_equal_to/1]).

levels() ->
    [
        {-1, none},
        {0, emergency},
        {1, alert},
        {2, critical},
        {3, error},
        {4, warning},
        {5, notice},
        {6, info},
        {7, debug}
    ].

levels_less_than_or_equal_to(LName) ->
    {LNumber, LName} = lists:keyfind(LName, 2, levels()),
    ListOfNamesLessThanOrEqual = lists:filtermap(
        fun({LNum, Name}) when LNum =< LNumber-> {true, Name};
            (_) -> false end, levels()),
    ListsAtoms = lists:delete(none, ListOfNamesLessThanOrEqual),
    lists:map(fun(El) -> atom_to_binary(El, utf8) end, ListsAtoms).

'contains?'(String, Pattern) ->
    binary:match(String, [Pattern]) /= nomatch.

filter_out_non_matching(Lines, Pattern) ->
    lists:filter(fun (L) -> 'contains?'(L, Pattern) end, Lines).

get_log(LogFile) ->
    case file:read_file(LogFile) of
        {error, enoent} -> [];
        {ok, Contents} -> binary:split(Contents, <<"\n">>, [global, trim])
    end.

get_at_least_n_log_lines(LogFile, NLines, Timeout) ->
    TRef = erlang:start_timer(Timeout, self(), get_at_least_n_log_lines),
    get_at_least_n_log_lines(LogFile, NLines, TRef, get_log(LogFile)).

get_at_least_n_log_lines(_LogFile, NLines, TRef, Lines)
    when length(Lines) >= NLines ->
    erlang:cancel_timer(TRef),
    Lines;
get_at_least_n_log_lines(LogFile, NLines, TRef, _Lines) ->
    receive
        {timeout, TRef, get_at_least_n_log_lines} -> timeout
    after 100 ->
        get_at_least_n_log_lines(LogFile, NLines, TRef, get_log(LogFile))
    end.
