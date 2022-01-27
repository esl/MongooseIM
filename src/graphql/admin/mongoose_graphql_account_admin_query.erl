-module(mongoose_graphql_account_admin_query).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_account_helper, [format_result/2, make_error/3]).

execute(_Ctx, _Obj, <<"listUsers">>, Args) ->
    list_users(Args);
execute(_Ctx, _Obj, <<"listOldUsers">>, Args) ->
    list_old_users(Args);
execute(_Ctx, _Obj, <<"countUsers">>, Args) ->
    count_users(Args);
execute(_Ctx, _Obj, <<"countActiveUsers">>, Args) ->
    get_active_users_number(Args);
execute(_Ctx, _Obj, <<"checkPassword">>, Args) ->
    check_password(Args);
execute(_Ctx, _Obj, <<"checkPasswordHash">>, Args) ->
    check_password_hash(Args);
execute(_Ctx, _Obj, <<"checkUser">>, Args) ->
    check_user(Args).

% Internal

-spec list_users(map()) -> {ok, [{ok, binary()}]}.
list_users(#{<<"domain">> := Domain}) ->
    Users = mongoose_account_api:list_users(Domain),
    Users2 = lists:map(fun(U) -> {ok, U} end, Users),
    {ok, Users2}.

-spec list_old_users(map()) -> {ok, [{ok, binary()}]}.
list_old_users(#{<<"domain">> := null, <<"days">> := Days}) ->
    {ok, Users} = mongoose_account_api:list_old_users(Days),
    Users2 = lists:map(fun(U) -> {ok, U} end, Users),
    {ok, Users2};
list_old_users(#{<<"domain">> := Domain, <<"days">> := Days}) ->
    {ok, Users} = mongoose_account_api:list_old_users_for_domain(Domain, Days),
    Users2 = lists:map(fun(U) -> {ok, U} end, Users),
    {ok, Users2}.

-spec count_users(map()) -> {ok, non_neg_integer()}.
count_users(#{<<"domain">> := Domain}) ->
    {ok, mongoose_account_api:count_users(Domain)}.

-spec get_active_users_number(map()) -> {ok, non_neg_integer()} | {error, resolver_error()}.
get_active_users_number(#{<<"domain">> := Domain, <<"days">> := Days}) ->
    Result = mongoose_account_api:num_active_users(Domain, Days),
    format_result(Result, #{domain => Domain}).

-spec check_password(map()) -> {ok, map()} | {error, resolver_error()}.
check_password(#{<<"user">> := JID, <<"password">> := Password}) ->
    case mongoose_account_api:check_password(JID, Password) of
        {user_does_not_exist, Msg} ->
            make_error(user_does_not_exist, Msg, #{jid => JID});
        {incorrect, Msg} ->
            {ok, #{<<"correct">> => false, <<"message">> => Msg}};
        {ok, Msg} ->
            {ok, #{<<"correct">> => true, <<"message">> => Msg}}
    end.

-spec check_password_hash(map()) -> {ok, map()} | {error, resolver_error()}.
check_password_hash(#{<<"user">> := JID, <<"passwordHash">> := Hash,
                      <<"hashMethod">> := HashMethod}) ->
    Val = binary_to_list(Hash),
    Method = binary_to_list(HashMethod),
    case mongoose_account_api:check_password_hash(JID, Val, Method) of
        {incorrect, Msg} ->
            {ok, #{<<"correct">> => false, <<"message">> => Msg}};
        {ok, Msg} ->
            {ok, #{<<"correct">> => true, <<"message">> => Msg}};
        {ErrCode, Msg} ->
            make_error(ErrCode, Msg, #{jid => JID})
    end.

-spec check_user(map()) -> {ok, map()}.
check_user(#{<<"user">> := JID}) ->
    case mongoose_account_api:check_account(JID) of
        {ok, Msg} ->
            {ok, #{<<"exist">> => true, <<"message">> => Msg}};
        {user_does_not_exist, Msg} ->
            {ok, #{<<"exist">> => false, <<"message">> => Msg}}
    end.
