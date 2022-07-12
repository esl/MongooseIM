-module(mongoose_graphql_stats_domain).
-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

execute(_Ctx, Domain, <<"registeredUsers">>, _Args) ->
    {ok, stats_api:stats(<<"registeredusers">>, Domain)};
execute(_Ctx, Domain, <<"onlineUsers">>, _Args) ->
    {ok, stats_api:stats(<<"onlineusers">>, Domain)}.
