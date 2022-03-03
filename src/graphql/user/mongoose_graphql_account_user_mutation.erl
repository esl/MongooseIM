-module(mongoose_graphql_account_user_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [format_result/2]).

execute(Ctx, _Obj, <<"unregister">>, _Args) ->
    unregister_user(Ctx);
execute(Ctx, _Obj, <<"changePassword">>, Args) ->
    change_user_password(Ctx, Args).

%% Internal

-spec unregister_user(map()) -> {ok, binary()} | {error, resolver_error()}.
unregister_user(#{user := JID}) ->
    Result = mongoose_account_api:unregister_user(JID),
    format_result(Result, #{}).

-spec change_user_password(map(), map()) -> {ok, binary()} | {error, resolver_error()}.
change_user_password(#{user := JID}, #{<<"newPassword">> := Password}) ->
    Result = mongoose_account_api:change_password(JID, Password),
    format_result(Result, #{}).
