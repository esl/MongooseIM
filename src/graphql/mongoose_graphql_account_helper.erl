-module(mongoose_graphql_account_helper).

-export([format_result/2, make_error/2, make_error/3]).

-include("mongoose_graphql_types.hrl").

-spec format_result(InResult, Context) -> OutResult when
      InResult :: {atom(), string() | binary()},
      Context :: map(),
      OutResult :: {ok, binary()} | {error, resolver_error()}.
format_result(Result, Context) ->
    case Result of
        {ok, Msg} when is_list(Msg) -> {ok, iolist_to_binary(Msg)};
        {ok, Msg} -> {ok, Msg};
        {ErrCode, Msg} -> make_error(ErrCode, Msg, Context)
    end.

-spec make_error({atom(), string() | binary()}, map()) -> {error, resolver_error()}.
make_error({Reason, Msg}, Context) ->
    make_error(Reason, Msg, Context).

-spec make_error(atom(), string() | binary(), map()) -> {error, resolver_error()}.
make_error(Reason, Msg, Context) when is_list(Msg) ->
    make_error(Reason, iolist_to_binary(Msg), Context);
make_error(Reason, Msg, Context) ->
    {error, #resolver_error{reason = Reason, msg = Msg, context = Context}}.
