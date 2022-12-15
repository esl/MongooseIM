-module(mongoose_graphql_session_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_session_helper, [format_sessions/1, format_status_users/1]).
-import(mongoose_graphql_helper, [format_result/2, make_error/2]).

execute(_Ctx, _Obj, <<"listSessions">>, Args) ->
    list_sessions(Args);
execute(_Ctx, _Obj, <<"countSessions">>, Args) ->
    count_sessions(Args);
execute(_Ctx, _Obj, <<"listUserSessions">>, Args) ->
    list_user_sessions(Args);
execute(_Ctx, _Obj, <<"countUserResources">>, Args) ->
    count_user_resources(Args);
execute(_Ctx, _Obj, <<"getUserResource">>, Args) ->
    get_user_resource(Args);
execute(_Ctx, _Obj, <<"listUsersWithStatus">>, Args) ->
    list_users_with_status(Args);
execute(_Ctx, _Obj, <<"countUsersWithStatus">>, Args) ->
    count_users_with_status(Args).

-spec list_sessions(map()) -> {ok, mongoose_graphql_session_helper:session_list()} | {error, resolver_error()}.
list_sessions(#{<<"domain">> := null}) ->
    {ok, Sessions} = mongoose_session_api:list_sessions(),
    {ok, format_sessions(Sessions)};
list_sessions(#{<<"domain">> := Domain}) ->
    case mongoose_session_api:list_sessions(Domain) of
        {ok, Sessions} ->
            {ok, format_sessions(Sessions)};
        Error ->
            make_error(Error, #{domain => Domain})
    end.

-spec count_sessions(map()) -> {ok, non_neg_integer()} | {error, resolver_error()}.
count_sessions(#{<<"domain">> := null}) ->
    mongoose_session_api:count_sessions();
count_sessions(#{<<"domain">> := Domain}) ->
    format_result(mongoose_session_api:count_sessions(Domain), #{domain => Domain}).

-spec list_user_sessions(map()) -> {ok, mongoose_graphql_session_helper:session_list()}.
list_user_sessions(#{<<"user">> := JID}) ->
    case mongoose_session_api:list_user_sessions(JID) of
        {ok, Sessions} ->
            {ok, format_sessions(Sessions)};
        Error ->
            make_error(Error, #{user => jid:to_binary(JID)})
    end.

-spec count_user_resources(map()) -> {ok, non_neg_integer()} | {error, resolver_error()}.
count_user_resources(#{<<"user">> := JID}) ->
    format_result(mongoose_session_api:num_resources(JID), #{jid => jid:to_binary(JID)}).

-spec get_user_resource(map()) -> {ok, jid:lresource()} | {error, resolver_error()}.
get_user_resource(#{<<"user">> := JID, <<"number">> := ResNumber}) ->
    Result = mongoose_session_api:get_user_resource(JID, ResNumber),
    format_result(Result, #{user => jid:to_binary(JID), number => ResNumber}).

-spec list_users_with_status(map()) -> {ok, mongoose_graphql_session_helper:status_user_list()}
                                     | {error, resolver_error()}.
list_users_with_status(#{<<"domain">> := null, <<"status">> := Status}) ->
    {ok, StatusUsers} = mongoose_session_api:list_status_users(Status),
    {ok, format_status_users(StatusUsers)};
list_users_with_status(#{<<"domain">> := Domain, <<"status">> := Status}) ->
    case mongoose_session_api:list_status_users(Domain, Status) of
        {ok, StatusUsers} ->
            {ok, format_status_users(StatusUsers)};
        Error ->
            make_error(Error, #{domain => Domain, status => Status})
    end.

-spec count_users_with_status(map()) -> {ok, non_neg_integer()} | {error, resolver_error()}.
count_users_with_status(#{<<"domain">> := null, <<"status">> := Status}) ->
    format_result(mongoose_session_api:num_status_users(Status), #{status => Status});
count_users_with_status(#{<<"domain">> := Domain, <<"status">> := Status}) ->
    format_result(mongoose_session_api:num_status_users(Domain, Status), #{domain => Domain, status => Status}).
