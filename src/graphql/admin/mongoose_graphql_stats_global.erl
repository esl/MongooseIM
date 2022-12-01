-module(mongoose_graphql_stats_global).
-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

execute(_Ctx, globalStats, <<"uptimeSeconds">>, _Args) ->
    globalStats(<<"uptimeseconds">>);
execute(_Ctx, globalStats, <<"registeredUsers">>, _Args) ->
    globalStats(<<"registeredusers">>);
execute(_Ctx, globalStats, <<"onlineUsersNode">>, _Args) ->
    globalStats(<<"onlineusersnode">>);
execute(_Ctx, globalStats, <<"onlineUsers">>, _Args) ->
    globalStats(<<"onlineusers">>);
execute(_Ctx, globalStats, <<"incomingS2S">>, _Args) ->
    stats_api:incoming_s2s_number();
execute(_Ctx, globalStats, <<"outgoingS2S">>, _Args) ->
    stats_api:outgoing_s2s_number().

globalStats(Name) ->
    case stats_api:stats(Name) of
        {ok, _} = Result -> Result;
        Error -> make_error(Error, #{})
    end.
