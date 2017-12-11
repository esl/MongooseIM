-module(safely_SUITE).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

all() ->
    [ handles_errors_like_catch,
      handles_exits_like_catch,
      handles_throws_unlike_catch,
      handles_success_like_catch
    ].


handles_errors_like_catch(_) ->
    {SafeRes, CatchRes} =
        %% These two must be on the same line for the stacktraces to be equal.
        {safely:apply(lists, min, [[]]),(catch apply(lists, min, [[]]))},

    {'EXIT', {function_clause, SafeST}} = SafeRes,
    {'EXIT', {function_clause, CatchST}} = CatchRes,

    true = (hd(CatchST) == hd(SafeST)),

    {safely,apply,3,[{file,_},{line,_}]} = hd(tl(SafeST)), % this is extra in SafeStackTrace

    true = (tl(CatchST) == tl(tl(SafeST))).

handles_exits_like_catch(_) ->
    ExitF = fun() -> exit(i_quit) end,
    {'EXIT', i_quit} = safely:apply(ExitF,[]),
    {'EXIT', i_quit} = (catch apply(ExitF,[])),
    ok.

handles_throws_unlike_catch(_) ->
    ThrowF = fun() -> throw(up) end,
    {'EXIT', up} = safely:apply(ThrowF,[]),
    up = (catch apply(ThrowF,[])),
    ok.

handles_success_like_catch(_) ->
    1 = safely:apply(lists, min, [[1,2,3]]),
    1 = (catch apply(lists, min, [[1,2,3]])).
