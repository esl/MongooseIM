-module(mongoose_graphql_admin_mutation).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(_Ctx, _Obj, <<"domains">>, _Args) ->
    {ok, admin};
execute(_Ctx, _Obj, <<"account">>, _Args) ->
    {ok, account};
execute(_Ctx, _Obj, <<"session">>, _Opts) ->
    {ok, session};
execute(_Ctx, _Obj, <<"stanza">>, _) ->
    {ok, #{}}.
