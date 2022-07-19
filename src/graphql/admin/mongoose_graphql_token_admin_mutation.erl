-module(mongoose_graphql_token_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2, format_result/2]).

-type token_info() :: map().
-type args() :: mongoose_graphql:args().
-type ctx() :: mongoose_graphql:ctx().

execute(_Ctx, token, <<"requestToken">>, #{<<"user">> := JID}) ->
    request_token(JID).

-spec request_token(jid:jid()) -> {ok, token_info()} | {error, resolver_error()}.
request_token(JID) ->
    {ok, #{<<"access">> => <<"123">>, <<"revoke">> => <<"1234">>}}.
