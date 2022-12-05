-module(mongoose_graphql_session_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2]).

execute(_Ctx, _Obj, <<"kickUserSession">>, Args) ->
    kick_user_session(Args);
execute(_Ctx, _Obj, <<"kickUser">>, Args) ->
    kick_user(Args);
execute(_Ctx, _Obj, <<"setPresence">>, Args) ->
    set_presence(Args).

-spec kick_user_session(map()) -> {ok, mongoose_session_api:kick_user_result()} | {error, resolver_error()}.
kick_user_session(#{<<"user">> := JID, <<"reason">> := KickReason}) ->
    case mongoose_session_api:kick_session(JID, KickReason) of
        {ok, Result} -> {ok, Result};
        Error -> make_error(Error, #{jid => jid:to_binary(JID), reason => KickReason})
    end.

-spec kick_user(map()) -> {ok, [mongoose_session_api:kick_user_result()]} | {error, resolver_error()}.
kick_user(#{<<"user">> := JID, <<"reason">> := KickReason}) ->
    case mongoose_session_api:kick_sessions(JID, KickReason) of
        {ok, Result} -> {ok, Result};
        Error -> make_error(Error, #{jid => jid:to_binary(JID), reason => KickReason})
    end.

-spec set_presence(map()) -> {ok, map()} | {error, resolver_error()}.
set_presence(#{<<"user">> := JID, <<"type">> := Type,
               <<"show">> := Show, <<"status">> := Status,
               <<"priority">> := Priority}) ->
    Show2 = null_to_empty(Show),
    Status2 = null_to_empty(Status),
    Priority2 = null_to_empty(Priority),
    Result = mongoose_session_api:set_presence(JID, Type, Show2, Status2, Priority2),
    format_session_payload(Result, JID).

%% Internal

-spec null_to_empty(null | integer() | binary()) -> binary().
null_to_empty(null) -> <<>>;
null_to_empty(Int) when is_integer(Int) -> integer_to_binary(Int);
null_to_empty(Bin) -> Bin.

-spec format_session_payload({atom(), binary()}, jid:jid()) ->
    {ok, map()} | {error, resolver_error()}.
format_session_payload(InResult, JID) ->
    case InResult of
        {ok, Msg} ->
            {ok, make_session_payload(Msg, JID)};
        Result ->
            make_error(Result, #{jid => jid:to_binary(JID)})
    end.

-spec make_session_payload(binary(), jid:jid()) -> map().
make_session_payload(Msg, JID) ->
    #{<<"message">> => Msg, <<"jid">> => JID}.
