-module(mongoose_graphql_session_admin_query).

-export([execute/4]).

-ignore_xref([execute/4]).

-import(mongoose_graphql_session_helper, [format_sessions/1, format_status_users/1]).
-import(mongoose_graphql_helper, [format_result/2]).

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
execute(_Ctx, _Obj, <<"listStatusUsers">>, Args) ->
    list_status_users(Args);
execute(_Ctx, _Obj, <<"countStatusUsers">>, Args) ->
    count_status_users(Args).

-spec list_sessions(map()) -> {ok, mongoose_graphql_session_helper:session_list()}.
list_sessions(#{<<"domain">> := null}) ->
    Sessions = mongoose_session_api:list_sessions(),
    {ok, format_sessions(Sessions)};
list_sessions(#{<<"domain">> := Domain}) ->
    Sessions = mongoose_session_api:list_sessions(Domain),
    {ok, format_sessions(Sessions)}.

-spec count_sessions(map()) -> {ok, non_neg_integer()}.
count_sessions(#{<<"domain">> := null}) ->
    {ok, mongoose_session_api:count_sessions()};
count_sessions(#{<<"domain">> := Domain}) ->
    {ok, mongoose_session_api:count_sessions(Domain)}.

-spec list_user_sessions(map()) -> {ok, mongoose_graphql_session_helper:session_list()}.
list_user_sessions(#{<<"user">> := JID}) ->
    Sessions = mongoose_session_api:list_user_sessions(JID),
    {ok, format_sessions(Sessions)}.

-spec count_user_resources(map()) -> {ok, non_neg_integer()}.
count_user_resources(#{<<"user">> := JID}) ->
    Number = mongoose_session_api:num_resources(JID),
    {ok, Number}.

-spec get_user_resource(map()) -> {ok, jid:lresource()}.
get_user_resource(#{<<"user">> := JID, <<"number">> := ResNumber}) ->
    Result = mongoose_session_api:get_user_resource(JID, ResNumber),
    format_result(Result, #{number => ResNumber}).

-spec list_status_users(map()) -> {ok, mongoose_graphql_session_helper:status_user_list()}.
list_status_users(#{<<"domain">> := null, <<"status">> := Status}) ->
    StatusUsers = mongoose_session_api:list_status_users(Status),
    {ok, format_status_users(StatusUsers)};
list_status_users(#{<<"domain">> := Domain, <<"status">> := Status}) ->
    StatusUsers = mongoose_session_api:list_status_users(Domain, Status),
    {ok, format_status_users(StatusUsers)}.

-spec count_status_users(map()) -> {ok, non_neg_integer()}.
count_status_users(#{<<"domain">> := null, <<"status">> := Status}) ->
    Number = mongoose_session_api:num_status_users(Status),
    {ok, Number};
count_status_users(#{<<"domain">> := Domain, <<"status">> := Status}) ->
    Number = mongoose_session_api:num_status_users(Domain, Status),
    {ok, Number}.
