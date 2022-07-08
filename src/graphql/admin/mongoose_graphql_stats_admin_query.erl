-module(mongoose_graphql_stats_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

execute(_Ctx, stats, <<"globalStats">>, _Args) ->
    {ok, globalStats};
execute(_Ctx, stats, <<"domainStats">>, #{<<"domain">> := Domain}) ->
    {ok, Domain}.
