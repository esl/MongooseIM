-module(mongoose_graphql_last_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2, format_result/2, null_to_default/2]).

-type last_info() :: mongoose_graphql_last_helper:last_info().
-type args() :: mongoose_graphql:args().

execute(_Ctx, last, <<"getLast">>, Args) ->
    get_last(Args);
execute(_Ctx, last, <<"countActiveUsers">>, Args) ->
    count_active_users(Args).

-spec get_last(args()) -> {ok, last_info()} | {error, resolver_error()}.
get_last(#{<<"user">> := JID}) ->
    mongoose_graphql_last_helper:get_last(JID).

-spec count_active_users(args()) -> {ok, pos_integer()} | {error, resolver_error()}.
count_active_users(#{<<"domain">> := Domain, <<"timestamp">> := Timestamp}) ->
    DefTimestamp = null_to_default(Timestamp, os:system_time(microsecond)),
    TimestampSec = mongoose_graphql_last_helper:microseconds_to_seconds(DefTimestamp),
    Res = mod_last_api:count_active_users(Domain, TimestampSec),
    format_result(Res, #{domain => Domain}).
