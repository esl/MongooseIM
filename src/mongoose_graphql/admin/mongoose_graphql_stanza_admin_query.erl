-module(mongoose_graphql_stanza_admin_query).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose_logger.hrl").

execute(_Ctx, _Obj, <<"getLastMessages">>, Opts) ->
    get_last_messages(Opts).

get_last_messages(#{<<"caller">> := Caller, <<"limit">> := Limit,
                    <<"with">> := With, <<"before">> := Before})
        when is_integer(Limit) ->
    With2 = null_as_undefined(With),
    BeforeSeconds = maybe_datetime_to_seconds(Before),
    Limit2 = min(500, Limit),
    Rows = mongoose_stanza_api:lookup_recent_messages(Caller, With2, BeforeSeconds, Limit2),
    Maps = lists:map(fun row_to_map/1, Rows),
    {ok, #{<<"stanzas">> => Maps, <<"limit">> => Limit2}}.

maybe_datetime_to_seconds(null) -> undefined;
maybe_datetime_to_seconds(Microseconds) -> Microseconds / 1000000. %% Could be float

null_as_undefined(null) -> undefined;
null_as_undefined(Value) -> Value.

-spec row_to_map(mod_mam:message_row()) -> {ok, map()}.
row_to_map(#{id := Id, jid := From, packet := Msg}) ->
    {Microseconds, _} = mod_mam_utils:decode_compact_uuid(Id),
    StanzaID = mod_mam_utils:mess_id_to_external_binary(Id),
    Map = #{<<"sender">> => From, <<"timestamp">> => Microseconds,
            <<"stanza_id">> => StanzaID, <<"stanza">> => Msg},
    {ok, Map}.
