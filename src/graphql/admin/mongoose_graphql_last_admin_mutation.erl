-module(mongoose_graphql_last_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2]).
-import(mongoose_graphql_last_helper, [format_old_users/1]).

-type old_users() :: mongoose_graphql_last_helper:old_users().
-type last_info() :: mongoose_graphql_last_helper:last_info().
-type args() :: mongoose_graphql:args().

execute(_Ctx, last, <<"setLast">>, Args) ->
   set_last(Args);
execute(_Ctx, last, <<"removeOldUsers">>, Args) ->
   remove_old_users(Args).

-spec set_last(args()) -> {ok, last_info()} | {error, resolver_error()}.
set_last(#{<<"user">> := JID, <<"timestamp">> := Timestamp, <<"status">> := Status}) ->
    mongoose_graphql_last_helper:set_last(JID, Timestamp, Status).

-spec remove_old_users(args()) -> {ok, old_users()} | {error, resolver_error()}.
remove_old_users(#{<<"domain">> := null, <<"timestamp">> := Timestamp}) ->
    Timestamp2 = mongoose_graphql_last_helper:microseconds_to_seconds(Timestamp),
    {ok, OldUsers} = mod_last_api:remove_old_users(Timestamp2),
    {ok, format_old_users(OldUsers)};
remove_old_users(#{<<"domain">> := Domain, <<"timestamp">> := Timestamp}) ->
    Timestamp2 = mongoose_graphql_last_helper:microseconds_to_seconds(Timestamp),
    case mod_last_api:remove_old_users(Domain, Timestamp2) of
        {ok, OldUsers} ->
            {ok, format_old_users(OldUsers)};
        Error ->
            make_error(Error, #{domain => Domain})
    end.
