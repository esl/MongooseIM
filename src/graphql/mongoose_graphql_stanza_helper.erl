-module(mongoose_graphql_stanza_helper).

-import(mongoose_graphql_helper, [null_to_undefined/1, make_error/2]).

-export([get_last_messages/5, row_to_map/1]).

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

-spec row_to_map(mod_mam:message_row()) -> {ok, map()}.
row_to_map(#{id := Id, jid := From, packet := Msg}) ->
    {Microseconds, _} = mod_mam_utils:decode_compact_uuid(Id),
    StanzaID = mod_mam_utils:mess_id_to_external_binary(Id),
    Map = #{<<"sender">> => From, <<"timestamp">> => Microseconds,
            <<"stanza_id">> => StanzaID, <<"stanza">> => Msg},
    {ok, Map}.
