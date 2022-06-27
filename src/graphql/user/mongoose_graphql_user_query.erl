-module(mongoose_graphql_user_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

execute(_Ctx, _Obj, <<"checkAuth">>, _Args) ->
    {ok, user};
execute(_Ctx, _Obj, <<"account">>, _Args) ->
    {ok, account};
execute(_Ctx, _Obj, <<"last">>, _Args) ->
    {ok, last};
execute(_Ctx, _Obj, <<"muc">>, _Args) ->
    {ok, muc};
execute(_Ctx, _Obj, <<"muc_light">>, _Args) ->
    {ok, muc_light};
execute(_Ctx, _Obj, <<"private">>, _Args) ->
    {ok, private};
execute(_Ctx, _Obj, <<"roster">>, _Args) ->
    {ok, roster};
execute(_Ctx, _Obj, <<"session">>, _Args) ->
    {ok, session};
execute(_Ctx, _Obj, <<"stanza">>, _Args) ->
    {ok, stanza};
execute(_Ctx, _Obj, <<"vcard">>, _Args) ->
    {ok, vcard}.
