-module(mongoose_graphql_session_user_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

execute(Ctx, _Obj, <<"listResources">>, Args) ->
    list_resources(Ctx, Args);
execute(Ctx, _Obj, <<"countResources">>, Args) ->
    count_resources(Ctx, Args);
execute(Ctx, _Obj, <<"listSessions">>, Args) ->
    list_sessions_info(Ctx, Args).

-spec list_resources(map(), map()) -> {ok, [jid:lresource()]}.
list_resources(#{user := JID}, _Args) ->
    Resources = mongoose_session_api:list_user_resources(JID),
    Result = lists:map(fun(R) -> {ok, R} end, Resources),
    {ok, Result}.

-spec count_resources(map(), map()) -> {ok, non_neg_integer()}.
count_resources(#{user := JID}, _Args) ->
    Number = length(mongoose_session_api:list_user_resources(JID)),
    {ok, Number}.

-spec list_sessions_info(map(), map()) -> {ok, mongoose_graphql_session_helper:session_list()}.
list_sessions_info(#{user := JID}, _Args) ->
    Sessions = mongoose_session_api:list_user_sessions(JID),
    Result = mongoose_graphql_session_helper:format_sessions(Sessions),
    {ok, Result}.
