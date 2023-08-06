-module(safely).
%% This module preserves the return types of a `catch' statement,
%% but doesn't silently convert `throw's to normal values.
%% The issue is that throws have different semantics than errors:
%% https://erlang.org/doc/reference_manual/expressions.html#catch-and-throw,
%% so a throw is actually a control flow escape, while an error is an actual crash.
%% If it is an error, we want the stacktrace, if not, we don't.
%% For more information, see usage in test/safely_SUITE.erl

-include("safely.hrl").
-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-export([apply/2, apply/3]).
-endif.
-export([apply_and_log/3, apply_and_log/4]).
-ignore_xref([apply_and_log/4]).

-type error_class() :: error | exit | throw.
-type error_info() :: #{class => error_class(), reason => term(), stacktrace => [term()]}.
-type exception() :: {exception, error_info()}.
-export_type([exception/0]).

-define(MATCH_EXCEPTIONS_DO_LOG(F, Context),
        try F catch
            error:R:S ->
                Info = #{class => error, reason => R, stacktrace => S},
                ?LOG_ERROR(maps:merge(Context, Info)),
                {exception, Info};
            throw:R ->
                Info = #{class => throw, reason => R},
                ?LOG_ERROR(maps:merge(Context, Info)),
                {exception, Info};
            exit:R:S ->
                Info = #{class => exit, reason => R, stacktrace => S},
                ?LOG_ERROR(maps:merge(Context, Info)),
                {exception, Info}
        end).

-ifdef(TEST).
-spec apply(fun((...) -> A), [term()]) -> A | exception().
apply(Function, Args) when is_function(Function), is_list(Args) ->
    ?SAFELY(erlang:apply(Function, Args)).

-spec apply(atom(), atom(), [term()]) -> term() | exception().
apply(Module, Function, Args) when is_atom(Function), is_list(Args) ->
    ?SAFELY(erlang:apply(Module, Function, Args)).
-endif.

-spec apply_and_log(fun((...) -> A), [term()], map()) -> A | exception().
apply_and_log(Function, Args, Context)
  when is_function(Function), is_list(Args), is_map(Context) ->
    ?MATCH_EXCEPTIONS_DO_LOG(erlang:apply(Function, Args), Context).

-spec apply_and_log(atom(), atom(), [term()], map()) -> term() | exception().
apply_and_log(Module, Function, Args, Context)
  when is_atom(Function), is_list(Args), is_map(Context) ->
    ?MATCH_EXCEPTIONS_DO_LOG(erlang:apply(Module, Function, Args), Context).
