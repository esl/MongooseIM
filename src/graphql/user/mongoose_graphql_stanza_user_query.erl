-module(mongoose_graphql_stanza_user_query).
-behaviour(mongoose_graphql).

-import(mongoose_graphql_helper, [format_result/2]).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(#{user := User}, _Obj, <<"getLastMessages">>, Args) ->
    get_last_messages(Args, User).

get_last_messages(#{<<"limit">> := Limit,
                    <<"with">> := With, <<"before">> := Before}, Caller)
        when is_integer(Limit) ->
    Res = mongoose_graphql_stanza_helper:get_last_messages(Caller, Limit, With, Before, false),
    format_result(Res, #{user => jid:to_binary(Caller)}).
