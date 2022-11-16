-module(mongoose_graphql_last_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2, format_result/2, null_to_default/2]).
-import(mongoose_graphql_last_helper, [format_old_users/1]).

-type old_users() :: mongoose_graphql_last_helper:old_users().
-type last_info() :: mongoose_graphql_last_helper:last_info().
-type args() :: mongoose_graphql:args().

execute(_Ctx, last, <<"getLast">>, Args) ->
    get_last(Args);
execute(_Ctx, last, <<"countActiveUsers">>, Args) ->
    count_active_users(Args);
execute(_Ctx, last, <<"listOldUsers">>, Args) ->
    list_old_users(Args).

-spec get_last(args()) -> {ok, last_info()} | {error, resolver_error()}.
get_last(#{<<"user">> := JID}) ->
    mongoose_graphql_last_helper:get_last(JID).

-spec count_active_users(args()) -> {ok, pos_integer()} | {error, resolver_error()}.
count_active_users(#{<<"domain">> := Domain, <<"timestamp">> := Timestamp}) ->
    DefTimestamp = null_to_default(Timestamp, os:system_time(microsecond)),
    TimestampSec = mongoose_graphql_last_helper:microseconds_to_seconds(DefTimestamp),
    Res = mod_last_api:count_active_users(Domain, TimestampSec),
    format_result(Res, #{domain => Domain}).

-spec list_old_users(args()) -> {ok, old_users()} | {error, resolver_error()}.
list_old_users(#{<<"domain">> := null, <<"timestamp">> := Timestamp}) ->
    Timestamp2 = mongoose_graphql_last_helper:microseconds_to_seconds(Timestamp),
    {ok, OldUsers} = mod_last_api:list_old_users(Timestamp2),
    {ok, format_old_users(OldUsers)};
list_old_users(#{<<"domain">> := Domain, <<"timestamp">> := Timestamp}) ->
    Timestamp2 = mongoose_graphql_last_helper:microseconds_to_seconds(Timestamp),
    case mod_last_api:list_old_users(Domain, Timestamp2) of
        {ok, OldUsers} ->
            {ok, format_old_users(OldUsers)};
        Error ->
            make_error(Error, #{domain => Domain})
    end.
