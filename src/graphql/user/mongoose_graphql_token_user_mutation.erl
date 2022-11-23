-module(mongoose_graphql_token_user_mutation).
 -behaviour(mongoose_graphql).

 -export([execute/4]).

 -ignore_xref([execute/4]).

 -include("../mongoose_graphql_types.hrl").

 -import(mongoose_graphql_helper, [make_error/2]).

 -type token_info() :: map().
 -type args() :: mongoose_graphql:args().
 -type ctx() :: mongoose_graphql:context().

 execute(Ctx, token, <<"requestToken">>, Args) ->
    request_token(Ctx, Args);
 execute(Ctx, token, <<"revokeToken">>, Args) ->
    revoke_token(Ctx, Args).

 -spec request_token(ctx(), args()) -> {ok, token_info()} | {error, resolver_error()}.
 request_token(#{user := JID}, #{}) ->
    case mod_auth_token_api:create_token(JID) of
        {ok, _} = Result -> Result;
        Error -> make_error(Error, #{user => JID})
    end.

 -spec revoke_token(ctx(), args()) -> {ok, string()} | {error, resolver_error()}.
 revoke_token(#{user := JID}, #{}) ->
    case mod_auth_token_api:revoke_token_command(JID) of
        {ok, _} = Result -> Result;
        Error -> make_error(Error, #{user => JID})
    end.
