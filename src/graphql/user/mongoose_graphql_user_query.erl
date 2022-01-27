-module(mongoose_graphql_user_query).

-export([execute/4]).

-ignore_xref([execute/4]).

execute(_Ctx, _Obj, <<"account">>, _Args) ->
    {ok, account};
execute(_Ctx, _Obj, <<"checkAuth">>, _Args) ->
    {ok, user}.
