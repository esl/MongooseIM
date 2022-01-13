-module(mongoose_graphql_user_mutation).

-export([execute/4]).

-ignore_xref([execute/4]).

execute(_Ctx, _Obj, <<"account">>, _Args) ->
    {ok, account}.
