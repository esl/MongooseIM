-module(safely).
%% This module preserves the return types of a `catch` statement,
%% but doesn't silently convert `throw`s to normal values.

-include("mongoose.hrl").

-export([apply/2,
         apply/3]).

-type catch_result(A) :: A | {'EXIT', term()}.

-spec apply(fun((...) -> A), [term()]) -> catch_result(A).
apply(Function, Args) when is_function(Function), is_list(Args) ->
    try erlang:apply(Function, Args)
    catch ?EXCEPTION(error, R, Stacktrace) -> {'EXIT', {R, ?GET_STACK(Stacktrace)}};
          _:R -> {'EXIT', R}
    end.

-spec apply(atom(), atom(), [term()]) -> catch_result(any()).
apply(Module, Function, Args) when is_atom(Function), is_list(Args) ->
    try erlang:apply(Module, Function, Args)
    catch ?EXCEPTION(error, R, Stacktrace) -> {'EXIT', {R, ?GET_STACK(Stacktrace)}};
          _:R -> {'EXIT', R}
    end.
