-module(mongoose_graphql_admin_mutation).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(_Ctx, _Obj, <<"domains">>, _Args) ->
    {ok, admin}.
