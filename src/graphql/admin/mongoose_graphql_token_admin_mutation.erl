-module(mongoose_graphql_token_admin_mutation).
 -behaviour(mongoose_graphql).

 -export([execute/4]).

 -ignore_xref([execute/4]).

 -include("../mongoose_graphql_types.hrl").

 -import(mongoose_graphql_helper, [make_error/2]).

 -type token_info() :: map().

 execute(_Ctx, token, <<"requestToken">>, #{<<"user">> := JID}) ->
     request_token(JID);
 execute(_Ctx, token, <<"revokeToken">>, #{<<"user">> := JID}) ->
     revoke_token(JID).

 -spec request_token(jid:jid()) -> {ok, token_info()} | {error, resolver_error()}.
 request_token(JID) ->
    case mod_auth_token_api:create_token(JID) of
        {ok, _} = Result -> Result;
        Error -> make_error(Error, #{user => JID})
    end.

 -spec revoke_token(jid:jid()) -> {ok, string()} | {error, resolver_error()}.
 revoke_token(JID) ->
    case mod_auth_token_api:revoke_token_command(JID) of
        {ok, _} = Result -> Result;
        Error -> make_error(Error, #{user => JID})
    end.
