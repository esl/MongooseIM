-module(mongoose_graphql_admin_query).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(_Ctx, _Obj, <<"domains">>, _Args) ->
    {ok, admin};

execute(#{authorized := Authorized}, _Obj, <<"checkAuth">>, _Args) ->
    case Authorized of
        true ->
            {ok, 'AUTHORIZED'};
        false ->
            {ok, 'UNAUTHORIZED'}
    end;
execute(_Ctx, _Obj, <<"stanza">>, _Opts) ->
    {ok, #{}}.
