-module(mongoose_graphql_user_auth_info).

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
    Username = maps:get(username, Ctx, null),
    {ok, Username}.
