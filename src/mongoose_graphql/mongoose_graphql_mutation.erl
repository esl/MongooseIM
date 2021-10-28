-module(mongoose_graphql_mutation).

-export([execute/4]).

-ignore_xref([execute/4]).

-import(mongoose_graphql_permission, [if_permitted/3]).

execute(Ctx, _Obj, <<"admin">>, #{}) ->
    if_permitted(Ctx, admin,
        fun() ->
            {ok, admin}
        end);
execute(Ctx, _Obj, <<"user">>, #{}) ->
    if_permitted(Ctx, user,
        fun() ->
            {ok, user}
        end).
