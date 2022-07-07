-module(mongoose_graphql_stats).
-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

execute(_Ctx, stats, <<"uptimeSeconds">>, _Args) ->
    {ok, stats_api:stats(<<"uptimeseconds">>)};
execute(_Ctx, stats, <<"registeredUsers">>, _Args) ->
    {ok, stats_api:stats(<<"registeredusers">>)};
execute(_Ctx, stats, <<"onlineUsersNode">>, _Args) ->
    {ok, stats_api:stats(<<"onlineusersnode">>)};
execute(_Ctx, stats, <<"onlineUsers">>, _Args) ->
    {ok, stats_api:stats(<<"onlineusers">>)};
execute(_Ctx, stats, <<"incomingS2S">>, _Args) ->
    {ok, stats_api:incoming_s2s_number()};
execute(_Ctx, stats, <<"outgoingS2S">>, _Args) ->
    {ok, stats_api:outgoing_s2s_number()}.
