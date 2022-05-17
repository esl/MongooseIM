-module(mongoose_graphql_last_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2]).

-type last_info() :: mongoose_graphql_last_helper:last_info().
-type args() :: mongoose_graphql:args().

execute(_Ctx, last, <<"setLast">>, Args) ->
   set_last(Args).

-spec set_last(args()) -> {ok, last_info()} | {error, resolver_error()}.
set_last(#{<<"user">> := JID, <<"timestamp">> := Timestamp, <<"status">> := Status}) ->
    mongoose_graphql_last_helper:set_last(JID, Timestamp, Status).
