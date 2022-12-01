-module(mongoose_graphql_last_user_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [null_to_default/2]).

-type last_info() :: map().
-type args() :: mongoose_graphql:args().
-type ctx() :: mongoose_graphql:context().

execute(Ctx, last, <<"setLast">>, Args) ->
   set_last(Ctx, Args).

-spec set_last(ctx(), args()) -> {ok, last_info()} | {error, resolver_error()}.
set_last(#{user := JID}, #{<<"timestamp">> := Timestamp, <<"status">> := Status}) ->
    mongoose_graphql_last_helper:set_last(JID, Timestamp, Status).
