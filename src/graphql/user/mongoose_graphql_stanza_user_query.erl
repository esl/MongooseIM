-module(mongoose_graphql_stanza_user_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose_logger.hrl").

execute(#{user := User}, _Obj, <<"getLastMessages">>, Args) ->
    get_last_messages(Args, User).

get_last_messages(#{<<"limit">> := Limit,
                    <<"with">> := With, <<"before">> := Before}, Caller)
        when is_integer(Limit) ->
    mongoose_stanza_helper:get_last_messages(Caller, Limit, With, Before).
