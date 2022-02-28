-module(mongoose_graphql_mam_helper).
-export([get_last_messages/4]).

get_last_messages(Caller, Limit, With, Before) ->
    With2 = null_as_undefined(With),
    Before2 = null_as_undefined(Before), %% Before is in microseconds
    Limit2 = min(500, Limit),
    Rows = mongoose_stanza_api:lookup_recent_messages(Caller, With2, Before2, Limit2),
    Maps = lists:map(fun row_to_map/1, Rows),
    {ok, #{<<"stanzas">> => Maps, <<"limit">> => Limit2}}.

null_as_undefined(null) -> undefined;
null_as_undefined(Value) -> Value.

-spec row_to_map(mod_mam:message_row()) -> {ok, map()}.
row_to_map(#{id := Id, jid := From, packet := Msg}) ->
    {Microseconds, _} = mod_mam_utils:decode_compact_uuid(Id),
    StanzaID = mod_mam_utils:mess_id_to_external_binary(Id),
    Map = #{<<"sender">> => From, <<"timestamp">> => Microseconds,
            <<"stanza_id">> => StanzaID, <<"stanza">> => Msg},
    {ok, Map}.
