-module(mongoose_graphql_user_auth_info).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

execute(#{authorized := Authorized}, user, <<"authStatus">>, _Args) ->
    case Authorized of
        true ->
            {ok, 'AUTHORIZED'};
        false ->
            {ok, 'UNAUTHORIZED'}
    end;
execute(Ctx, user, <<"username">>, _Args) ->
    case maps:get(user, Ctx, null) of
        null -> {ok, null};
        User -> {ok, jid:to_binary(User)}
    end.
