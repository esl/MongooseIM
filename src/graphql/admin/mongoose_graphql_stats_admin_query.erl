-module(mongoose_graphql_stats_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

execute(_Ctx, stats, <<"getIncomingS2SNumber">>, #{}) ->
    {ok, stats_api:incoming_s2s_number()};
execute(_Ctx, stats, <<"getOutgoingS2SNumber">>, #{}) ->
    {ok, stats_api:outgoing_s2s_number()};
execute(_Ctx, stats, <<"stats">>, #{<<"domain">> := null, <<"statName">> := StatName}) ->
    case stats_api:stats(StatName) of
        {error, String} ->
            make_error({no_command_error, String}, #{statName => StatName});
        Result ->
            {ok, Result}
    end;
execute(_Ctx, stats, <<"stats">>, #{<<"domain">> := Domain, <<"statName">> := StatName}) ->
    case stats_api:stats(StatName, Domain) of
        {error, String} ->
            make_error({no_command_error, String}, #{statName => StatName});
        Result ->
            {ok, Result}
    end.
