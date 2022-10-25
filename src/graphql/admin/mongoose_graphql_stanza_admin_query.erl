-module(mongoose_graphql_stanza_admin_query).
-behaviour(mongoose_graphql).

-import(mongoose_graphql_helper, [format_result/2]).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(_Ctx, _Obj, <<"getLastMessages">>, Args) ->
    get_last_messages(Args).

get_last_messages(#{<<"caller">> := Caller, <<"limit">> := Limit,
                    <<"with">> := With, <<"before">> := Before})
        when is_integer(Limit) ->
    Res = mongoose_graphql_stanza_helper:get_last_messages(Caller, Limit, With, Before, true),
    format_result(Res, #{user => jid:to_binary(Caller)}).
