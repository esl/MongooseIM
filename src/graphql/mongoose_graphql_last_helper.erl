-module(mongoose_graphql_last_helper).

-export([get_last/1, set_last/3]).

-export([microseconds_to_seconds/1, seconds_to_microseconds/1, format_old_users/1]).

-ignore_xref([microseconds_to_seconds/1, seconds_to_microseconds/1]).

-include("mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2, null_to_default/2]).

-type old_users() :: [{ok, map()}].
-type last_info() :: map().
-type timestamp() :: mod_last:timestamp() | null.
-type status() :: mod_last:status().

-export_type([last_info/0, old_users/0]).

-spec get_last(jid:jid()) -> {ok, last_info()} | {error, resolver_error()}.
get_last(JID) ->
    case mod_last_api:get_last(JID) of
        {ok, #{timestamp := T, status := S}} ->
            {ok, #{<<"user">> => JID, <<"timestamp">> => seconds_to_microseconds(T),
                   <<"status">> => S}};
        Error ->
            make_error(Error, #{user => jid:to_binary(JID)})
    end.

-spec set_last(jid:jid(), timestamp(), status()) -> {ok, last_info()} | {error, resolver_error()}.
set_last(JID, Timestamp, Status) ->
    DefTimestamp = microseconds_to_seconds(null_to_default(Timestamp, os:system_time(microsecond))),
    case mod_last_api:set_last(JID, DefTimestamp, Status) of
        {ok, #{timestamp := DefTimestamp, status := Status}} ->
            {ok, #{<<"user">> => JID, <<"timestamp">> => seconds_to_microseconds(DefTimestamp),
                   <<"status">> => Status}};
        Error ->
            make_error(Error, #{user => jid:to_binary(JID)})
    end.

format_old_users(OldUsers) ->
    [{ok, make_old_user(JID, Timestamp)} || {JID, Timestamp} <- OldUsers].

microseconds_to_seconds(Timestamp) ->
    Timestamp div 1000000.

seconds_to_microseconds(Timestamp) ->
    Timestamp * 1000000.

%% Internal

make_old_user(JID, null) ->
    #{<<"jid">> => JID, <<"timestamp">> => null};
make_old_user(JID, Timestamp) ->
    #{<<"jid">> => JID, <<"timestamp">> => seconds_to_microseconds(Timestamp)}.
