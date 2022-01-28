-module(mongoose_graphql_session_admin_mutation).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2]).

execute(_Ctx, _Obj, <<"kickUser">>, Args) ->
    kick_user(Args);
execute(_Ctx, _Obj, <<"setPresence">>, Args) ->
    set_presence(Args).

-spec kick_user(map()) -> {ok, map()} | {error, resolver_error()}.
kick_user(#{<<"user">> := JID, <<"reason">> := KickReason}) ->
    Result = mongoose_session_api:kick_session(JID, KickReason),
    format_session_payload(Result, JID).

-spec set_presence(map()) -> {ok, map()}.
set_presence(#{<<"user">> := JID, <<"type">> := Type,
               <<"show">> := Show, <<"status">> := Status,
               <<"priority">> := Priority}) ->
    PriorityBin = integer_to_binary(Priority),
    Result = mongoose_session_api:set_presence(JID, Type, Show, Status, PriorityBin),
    format_session_payload(Result, JID).

%% Internal

-spec format_session_payload({atom(), binary()}, jid:jid()) ->
    {ok, map()} | {error, resolver_error()}.
format_session_payload(InResult, JID) ->
    case InResult of
        {ok, Msg} ->
            {ok, make_session_payload(Msg, JID)};
        Result ->
            make_error(Result, #{jid => JID})
    end.

-spec make_session_payload(binary(), jid:jid()) -> map().
make_session_payload(Msg, JID) ->
    #{<<"message">> => Msg, <<"jid">> => JID}.
