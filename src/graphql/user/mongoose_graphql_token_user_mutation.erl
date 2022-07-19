-module(mongoose_graphql_token_user_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [null_to_default/2]).

-type token_info() :: map().
-type args() :: mongoose_graphql:args().
-type ctx() :: mongoose_graphql:ctx().

execute(Ctx, token, <<"requestToken">>, Args) ->
   request_token(Ctx, Args);
execute(Ctx, token, <<"revokeToken">>, Args) ->
   revoke_token(Ctx, Args).

-spec request_token(ctx(), args()) -> {ok, token_info()} | {error, resolver_error()}.
request_token(#{user := JID}, #{}) ->
    {ok, #{<<"access">> => <<"123">>, <<"revoke">> => <<"1234">>}}.

-spec revoke_token(ctx(), args()) -> {ok, binary()} | {error, resolver_error()}.
revoke_token(#{user := JID}, #{}) ->
    {ok, <<"ok">>}.
