-module(mongoose_graphql_last_user_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2, format_result/2, null_to_default/2]).

-type last_info() :: mongoose_graphql_last_helper:last_info().
-type args() :: mongoose_graphql:args().
-type ctx() :: mongoose_graphql:context().

execute(Ctx, last, <<"getLast">>, Args) ->
    get_last(Ctx, Args).

-spec get_last(ctx(), args()) -> {ok, last_info()} | {error, resolver_error()}.
get_last(#{user := UserJID}, #{<<"user">> := JID}) ->
    mongoose_graphql_last_helper:get_last(null_to_default(JID, UserJID)).
