-module(mongoose_graphql_operations).

-export([verify_operations/2]).

-include_lib("graphql/src/graphql_schema.hrl").
-include_lib("graphql/src/graphql_internal.hrl").

verify_operations(Ctx, #document{definitions = Definitions}) ->
    Method = maps:get(method, Ctx, undefined),
    [verify_op_type(Method, op_type(Ty)) || #op{id = Id, ty = Ty} <- Definitions,
                                            is_requested_op(Ctx, Id)],
    ok.

verify_op_type(Method, OpType) ->
    case is_supported(Method, OpType) of
        true ->
            ok;
        false ->
            Error = {unsupported_operation, Method, OpType},
            graphql_err:abort([], verify, Error)
    end.

is_supported(Method, subscription) -> Method =:= sse;
is_supported(Method, _) -> Method =/= sse.

is_requested_op(#{operation_name := undefined}, _) -> true;
is_requested_op(#{operation_name := OpName}, {name, _, Name}) -> OpName =:= Name.

op_type(undefined) -> query;
op_type({OpType, _}) -> OpType.
