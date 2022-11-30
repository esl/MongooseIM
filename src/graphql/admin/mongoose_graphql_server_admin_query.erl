-module(mongoose_graphql_server_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(_Ctx, server, <<"status">>, _) ->
    {ok, {Status, String}} = mongoose_server_api:status(),
    {ok, #{<<"statusCode">> => status_code(Status), <<"message">> => String}};
execute(_Ctx, server, <<"getLoglevel">>, _) ->
    mongoose_server_api:get_loglevel();
execute(_Ctx, server, <<"getCookie">>, _) ->
    mongoose_server_api:get_cookie().

status_code(true) -> <<"RUNNING">>;
status_code(false) -> <<"NOT_RUNNING">>.
