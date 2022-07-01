-module(mongoose_graphql_account_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4, command/2]).

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

command(<<"listUsers">>, [Domain]) ->
    Doc = <<"query ($domain: String!) { account { listUsers(domain: $domain) } }">>,
    Vars = #{<<"domain">> => list_to_binary(Domain)},
    #{document => Doc, vars => Vars};
command(<<"countUsers">>, [Domain]) ->
    Doc = <<"query ($domain: String!) { account { countUsers(domain: $domain) } }">>,
    Vars = #{<<"domain">> => list_to_binary(Domain)},
    #{document => Doc, vars => Vars};
command(<<"checkPassword">>, [UserJID, Password]) ->
    Doc = <<"query ($user: JID!, $password: String!) "
            "{ account { checkPassword(user: $user, password: $password) {correct message} } }">>,
    Vars = #{<<"user">> => list_to_binary(UserJID),
             <<"password">> => list_to_binary(Password)},
    #{document => Doc, vars => Vars};
command(<<"checkPasswordHash">>, [UserJID, Hash, Method]) ->
    Doc = <<"query ($user: JID!, $hash: String!, $method: String!) "
            "{ account { checkPasswordHash(user: $user, passwordHash: $hash, hashMethod: $method) "
            "{correct message} } }">>,
    Vars = #{<<"user">> => list_to_binary(UserJID),
             <<"hash">> => list_to_binary(Hash),
             <<"method">> => list_to_binary(Method)},
    #{document => Doc, vars => Vars};
command(<<"checkUser">>, [UserJID]) ->
    Doc = <<"query ($user: JID!) { account { checkUser(user: $user) {exist message} } }">>,
    Vars = #{<<"user">> => list_to_binary(UserJID)},
    #{document => Doc, vars => Vars}.

% Internal

-spec list_users(map()) -> {ok, [{ok, binary()}]}.
list_users(#{<<"domain">> := Domain}) ->
    Users = mongoose_account_api:list_users(Domain),
    Users2 = lists:map(fun(U) -> {ok, U} end, Users),
    {ok, Users2}.

-spec count_users(map()) -> {ok, non_neg_integer()}.
count_users(#{<<"domain">> := Domain}) ->
    {ok, mongoose_account_api:count_users(Domain)}.

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
