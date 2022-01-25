-module(safely).
%% This module preserves the return types of a `catch' statement,
%% but doesn't silently convert `throw's to normal values.
%% The issue is that throws have different semantics than errors:
%% https://erlang.org/doc/reference_manual/expressions.html#catch-and-throw,
%% so a throw is actually a control flow escape, while an error is an actual crash.
%% If it is an error, we want the stacktrace, if not, we don't.
%% For more information, see usage in test/safely_SUITE.erl

-include_lib("kernel/include/logger.hrl").

-export([apply/2, apply/3]).
-export([apply_and_log/3, apply_and_log/4]).
-compile({no_auto_import, [apply/2, apply/3]}).
-ignore_xref([apply/3, apply_and_log/4]).

-type error_class() :: error | exit | throw.
-type catch_result(A) :: A | {error_class(), term()}.

-define(MATCH_EXCEPTIONS_DO_LOG(F, Context),
        try F catch
            error:R:S ->
                ?LOG_ERROR(Context#{class => error, reason => R, stacktrace => S}),
                {error, {R, S}};
            throw:R ->
                ?LOG_ERROR(Context#{class => throw, reason => R}),
                {throw, R};
            exit:R:S ->
                ?LOG_ERROR(Context#{class => exit, reason => R, stacktrace => S}),
                {exit, {R, S}}
        end).

-define(MATCH_EXCEPTIONS(F),
        try F catch
            error:R:S -> {error, {R, S}};
            throw:R -> {throw, R};
            exit:R:S -> {exit, {R, S}}
        end).

-spec apply(fun((...) -> A), [term()]) -> catch_result(A).
apply(Function, Args) when is_function(Function), is_list(Args) ->
    ?MATCH_EXCEPTIONS(erlang:apply(Function, Args)).

-spec apply(atom(), atom(), [term()]) -> catch_result(any()).
apply(Module, Function, Args) when is_atom(Function), is_list(Args) ->
    ?MATCH_EXCEPTIONS(erlang:apply(Module, Function, Args)).

-spec apply_and_log(fun((...) -> A), [term()], map()) -> catch_result(A).
apply_and_log(Function, Args, Context)
  when is_function(Function), is_list(Args), is_map(Context) ->
    ?MATCH_EXCEPTIONS_DO_LOG(erlang:apply(Function, Args), Context).

-spec apply_and_log(atom(), atom(), [term()], map()) -> catch_result(any()).
apply_and_log(Module, Function, Args, Context)
  when is_atom(Function), is_list(Args), is_map(Context) ->
    ?MATCH_EXCEPTIONS_DO_LOG(erlang:apply(Module, Function, Args), Context).
