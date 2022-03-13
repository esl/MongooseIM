-module(mongoose_graphql_user_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

execute(_Ctx, _Obj, <<"account">>, _Args) ->
    {ok, account};
execute(_Ctx, _Obj, <<"muc_light">>, _Args) ->
    {ok, muc_light};
execute(_Ctx, _Obj, <<"stanza">>, _Args) ->
    {ok, stanza};
execute(_Ctx, _Obj, <<"roster">>, _Args) ->
    {ok, roster}.
