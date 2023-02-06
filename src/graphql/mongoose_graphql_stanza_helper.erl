-module(mongoose_graphql_stanza_helper).

-import(mongoose_graphql_helper, [null_to_undefined/1, make_error/2]).

-export([get_last_messages/5, row_to_map/1, handle_event/1]).

-include("mongoose.hrl").
-include("jlib.hrl").

-spec get_last_messages(Caller :: jid:jid(),
                        Limit :: null | non_neg_integer(),
                        With :: null | jid:jid(),
                        Before :: null | mod_mam:unix_timestamp(), boolean()) ->
          {ok, map()} | {unknown_user, iodata()}.
get_last_messages(Caller, Limit, With, Before, CheckUser) ->
    case mongoose_stanza_api:lookup_recent_messages(
           Caller, null_to_undefined(With), null_to_undefined(Before),
           null_to_undefined(Limit), CheckUser) of
        {ok, {Rows, Limit2}} ->
            Maps = lists:map(fun row_to_map/1, Rows),
            {ok, #{<<"stanzas">> => Maps, <<"limit">> => Limit2}};
        Error ->
            Error
    end.

-spec handle_event(term()) -> {ok, map() | null}.
handle_event({route, Acc}) ->
    {From, _To, Packet} = mongoose_acc:packet(Acc),
    case Packet of
        Stanza = #xmlel{name = <<"message">>} ->
            StanzaID = exml_query:attr(Stanza, <<"id">>, <<>>),
            Timestamp = os:system_time(microsecond),
            stanza_result(From, Timestamp, StanzaID, Stanza);
        _ ->
            {ok, null} % Skip other stanza types
    end;
handle_event(Msg) ->
    ?UNEXPECTED_INFO(Msg),
    {ok, null}.

-spec row_to_map(mod_mam:message_row()) -> {ok, map()}.
row_to_map(#{id := Id, jid := From, packet := Stanza}) ->
    {Microseconds, _} = mod_mam_utils:decode_compact_uuid(Id),
    StanzaID = mod_mam_utils:mess_id_to_external_binary(Id),
    stanza_result(From, Microseconds, StanzaID, Stanza).

stanza_result(From, Timestamp, StanzaID, Stanza) ->
    Map = #{<<"sender">> => From, <<"timestamp">> => Timestamp,
            <<"stanza_id">> => StanzaID, <<"stanza">> => Stanza},
    {ok, Map}.
