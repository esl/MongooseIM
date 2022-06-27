-module(mongoose_graphql_stanza_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose_logger.hrl").

execute(_Ctx, _Obj, <<"getLastMessages">>, Args) ->
    get_last_messages(Args).

get_last_messages(#{<<"caller">> := Caller, <<"limit">> := Limit,
                    <<"with">> := With, <<"before">> := Before})
        when is_integer(Limit) ->
    case mongoose_graphql_helper:check_user(Caller, true) of
        {ok, _HostType} ->
            mongoose_stanza_helper:get_last_messages(Caller, Limit, With, Before);
        Error ->
            Error
    end.
