-module(mongoose_graphql_helper).

-export([null_to_default/2, null_to_undefined/1, undefined_to_null/1]).

-export([format_result/2, make_error/2, make_error/3]).

-include("mongoose_graphql_types.hrl").

-spec format_result(InResult, Context) -> OutResult when
      InResult :: {atom(), iodata() | integer()},
      Context :: map(),
      OutResult :: {ok, binary() | integer()} | {error, resolver_error()}.
format_result(Result, Context) ->
    case Result of
        {ok, Val} when is_integer(Val) orelse is_map(Val) -> {ok, Val};
        {ok, Msg} -> {ok, iolist_to_binary(Msg)};
        {ErrCode, Msg} -> make_error(ErrCode, Msg, Context)
    end.

-spec make_error({atom(), iodata()}, map()) -> {error, resolver_error()}.
make_error({Reason, Msg}, Context) ->
    make_error(Reason, Msg, Context).

-spec make_error(atom(), iodata(), map()) -> {error, resolver_error()}.
make_error(Reason, Msg, Context) ->
    {error, #resolver_error{reason = Reason, msg = iolist_to_binary(Msg), context = Context}}.

null_to_default(null, Default) ->
    Default;
null_to_default(Value, _Default) ->
    Value.

null_to_undefined(null) -> undefined;
null_to_undefined(V) -> V.

undefined_to_null(undefined) -> null;
undefined_to_null(V) -> V.
