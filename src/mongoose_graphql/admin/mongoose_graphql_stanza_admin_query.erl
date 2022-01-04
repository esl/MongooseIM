-module(mongoose_graphql_stanza_admin_query).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose_logger.hrl").

execute(_Ctx, _Obj, <<"getLastMessages">>, Opts) ->
    get_last_messages(Opts).

get_last_messages(Opts = #{<<"caller">> := Caller, <<"limit">> := Limit}) ->
    With = maybe_null(maps:get(<<"with">>, Opts, undefined), undefined),
    BeforeSeconds = maybe_datetime_to_seconds(maps:get(<<"before">>, Opts, undefined)),
    Limit2 = maybe_null(Limit, 50),
    Rows = mod_commands:lookup_recent_messages(Caller, With, BeforeSeconds, Limit2),
    Maps = lists:map(fun row_to_map/1, Rows),
    {ok, #{<<"stanzas">> => Maps}}.

maybe_datetime_to_seconds(null) -> undefined;
maybe_datetime_to_seconds(undefined) -> undefined;
maybe_datetime_to_seconds(Microseconds) -> Microseconds / 1000000. %% Could be float

maybe_null(null, Default) -> Default;
maybe_null(Value, _Default) -> Value.

-spec row_to_map(mod_mam:message_row()) -> {ok, map()}.
row_to_map(#{id := Id, jid := From, packet := Msg}) ->
    {Microseconds, _} = mod_mam_utils:decode_compact_uuid(Id),
    StanzaID = mod_mam_utils:mess_id_to_external_binary(Id),
    Map = #{<<"sender">> => From, <<"timestamp">> => Microseconds,
            <<"stanza_id">> => StanzaID, <<"stanza">> => Msg},
    {ok, Map}.
