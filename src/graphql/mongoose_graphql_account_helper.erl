-module(mongoose_graphql_account_helper).

-export([format_result/2, make_error/2, make_error/3]).

-include("mongoose_graphql_types.hrl").

-spec format_result(InResult, Context) -> OutResult when
      InResult :: {atom(), iodata() | integer()},
      Context :: map(),
      OutResult :: {ok, binary() | integer()} | {error, resolver_error()}.
format_result(Result, Context) ->
    case Result of
        {ok, Val} when is_integer(Val) -> {ok, Val};
        {ok, Msg} -> {ok, iolist_to_binary(Msg)};
        {ErrCode, Msg} -> make_error(ErrCode, Msg, Context)
    end.

-spec make_error({atom(), iodata()}, map()) -> {error, resolver_error()}.
make_error({Reason, Msg}, Context) ->
    make_error(Reason, Msg, Context).

-spec make_error(atom(), iodata(), map()) -> {error, resolver_error()}.
make_error(Reason, Msg, Context) ->
    {error, #resolver_error{reason = Reason, msg = iolist_to_binary(Msg), context = Context}}.
