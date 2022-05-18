-module(mongoose_graphql_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(_Ctx, _Obj, <<"domains">>, _Args) ->
    {ok, admin};
execute(_Ctx, _Obj, <<"account">>, _Args) ->
    {ok, account};
execute(_Ctx, _Obj, <<"muc">>, _Args) ->
    {ok, muc};
execute(_Ctx, _Obj, <<"muc_light">>, _Args) ->
    {ok, muc_light};
execute(_Ctx, _Obj, <<"session">>, _Args) ->
    {ok, session};
execute(_Ctx, _Obj, <<"stanza">>, _Args) ->
    {ok, #{}};
execute(_Ctx, _Obj, <<"roster">>, _Args) ->
    {ok, roster};
execute(_Ctx, _Obj, <<"checkAuth">>, _Args) ->
    {ok, admin};
execute(_Ctx, _Obj, <<"vcard">>, _Args) ->
    {ok, vcard}.
