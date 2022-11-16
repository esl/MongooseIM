-module(mongoose_graphql_account_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [format_result/2, make_error/3]).

execute(_Ctx, _Obj, <<"listUsers">>, Args) ->
    list_users(Args);
execute(_Ctx, _Obj, <<"countUsers">>, Args) ->
    count_users(Args);
execute(_Ctx, _Obj, <<"checkPassword">>, Args) ->
    check_password(Args);
execute(_Ctx, _Obj, <<"checkPasswordHash">>, Args) ->
    check_password_hash(Args);
execute(_Ctx, _Obj, <<"checkUser">>, Args) ->
    check_user(Args).

% Internal

-spec list_users(map()) -> {ok, [{ok, binary()}]} | {error, resolver_error()}.
list_users(#{<<"domain">> := Domain}) ->
    case mongoose_account_api:list_users(jid:nameprep(Domain)) of
        {domain_not_found, Msg} ->
            make_error(domain_not_found, Msg, #{domain => Domain});
        {ok, Users} ->
            Users2 = lists:map(fun(U) -> {ok, U} end, Users),
            {ok, Users2}
    end.

-spec count_users(map()) -> {ok, non_neg_integer()}.
count_users(#{<<"domain">> := Domain}) ->
    case mongoose_account_api:count_users(Domain) of
        {domain_not_found, Msg} ->
            make_error(domain_not_found, Msg, #{domain => Domain});
        {ok, UserCount} ->
            {ok, UserCount}
    end.

-spec check_password(map()) -> {ok, map()} | {error, resolver_error()}.
check_password(#{<<"user">> := JID, <<"password">> := Password}) ->
    case mongoose_account_api:check_password(JID, Password) of
        {user_does_not_exist, Msg} ->
            make_error(user_does_not_exist, Msg, #{jid => jid:to_binary(JID)});
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
            make_error(ErrCode, Msg, #{jid => jid:to_binary(JID)})
    end.

-spec check_user(map()) -> {ok, map()}.
check_user(#{<<"user">> := JID}) ->
    case mongoose_account_api:check_account(JID) of
        {ok, Msg} ->
            {ok, #{<<"exist">> => true, <<"message">> => Msg}};
        {user_does_not_exist, Msg} ->
            {ok, #{<<"exist">> => false, <<"message">> => Msg}}
    end.
