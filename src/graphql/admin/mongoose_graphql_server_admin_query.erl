-module(mongoose_graphql_server_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(_Ctx, server, <<"status">>, _) ->
    case mongoose_server_api:status() of
        {ok, String} ->
            {ok, #{<<"statusCode">> => <<"RUNNING">>, <<"message">> => String}};
        {_, String} ->
            {ok, #{<<"statusCode">> => <<"NOT_RUNNING">>, <<"message">> => String}}
    end;
execute(_Ctx, server, <<"getLoglevel">>, _) ->
    mongoose_server_api:graphql_get_loglevel();
execute(_Ctx, server, <<"getCookie">>, _) ->
    {ok, mongoose_server_api:get_cookie()}.
