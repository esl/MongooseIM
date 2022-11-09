-module(mongoose_graphql_account_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [format_result/2, make_error/2]).

execute(_Ctx, _Obj, <<"registerUser">>, Args) ->
    register_user(Args);
execute(_Ctx, _Obj, <<"removeUser">>, Args) ->
    remove_user(Args);
execute(_Ctx, _Obj, <<"banUser">>, Args) ->
    ban_user(Args);
execute(_Ctx, _Obj, <<"changeUserPassword">>, Args) ->
    change_user_password(Args).

% Internal

-spec register_user(map()) -> {ok, map()} | {error, resolver_error()}.
register_user(#{<<"domain">> := Domain, <<"username">> := null,
                <<"password">> := Password}) ->
    case mongoose_account_api:register_generated_user(Domain, Password) of
        {{ok, Msg}, JID} ->
            {ok, make_user_payload(Msg, JID)};
        {Result, JID} ->
            format_result(Result, #{jid => JID})
    end;
register_user(#{<<"domain">> := Domain, <<"username">> := Username,
                <<"password">> := Password}) ->
    Result = mongoose_account_api:register_user(Username, Domain, Password),
    format_user_payload(Result, jid:make(Username, Domain, <<>>)).

-spec remove_user(map()) -> {ok, map()} | {error, resolver_error()}.
remove_user(#{<<"user">> := JID}) ->
    Result = mongoose_account_api:unregister_user(JID),
    format_user_payload(Result, JID).

-spec ban_user(map()) -> {ok, map()} | {error, resolver_error()}.
ban_user(#{<<"user">> := JID, <<"reason">> := Reason}) ->
    Result = mongoose_account_api:ban_account(JID, Reason),
    format_user_payload(Result, JID).

-spec change_user_password(map()) -> {ok, map()} | {error, resolver_error()}.
change_user_password(#{<<"user">> := JID, <<"newPassword">> := Password}) ->
    Result = mongoose_account_api:change_password(JID, Password),
    format_user_payload(Result, JID).

-spec format_user_payload({atom(), string()}, jid:jid()) -> {ok, map()} | {error, resolver_error()}.
format_user_payload(InResult, JID) ->
    case InResult of
        {ok, Msg} ->
            {ok, make_user_payload(Msg, jid:to_binary(JID))};
        Result ->
            make_error(Result, #{jid => jid:to_binary(JID)})
    end.

-spec make_user_payload(string(), jid:literal_jid()) -> map().
make_user_payload(Msg, JID) ->
    #{<<"message">> => iolist_to_binary(Msg), <<"jid">> => JID}.
