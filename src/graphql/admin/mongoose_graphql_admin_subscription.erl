-module(mongoose_graphql_admin_subscription).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(_Ctx, _Obj, <<"stanza">>, _Args) ->
    {ok, stanza}.
