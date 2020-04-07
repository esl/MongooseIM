%%%----------------------------------------------------------------------------
%%% @copyright (C) 2020, Erlang Solutions Ltd.
%%% @doc
%%%    RDBMS backend for mod_smart_markers
%%% @end
%%%----------------------------------------------------------------------------
-module(mod_smart_markers_rdbms).
-author("denysgonchar").
-behavior(mod_smart_markers).

-export([init/2, update_chat_marker/2, get_chat_markers/4]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-spec init(Host :: jid:lserver(), Opts :: proplists:proplist()) -> ok.
init(Host, _) ->
    KeyFields = [<<"from_jid">>, <<"to_jid">>, <<"thread">>, <<"type">>],
    UpdateFields = [<<"msg_id">>, <<"timestamp">>],
    InsertFields = KeyFields ++ UpdateFields,
    QueryName = smart_markers_upsert,
    rdbms_queries:prepare_upsert(Host, QueryName, smart_markers,
                                 InsertFields, UpdateFields, KeyFields),
    ok.

%%% @doc
%%% 'from', 'to', 'thread' and 'type' keys of the ChatMarker map serve
%%% as a composite database key. If key is not available in the database,
%%% then chat marker must be added. Otherwise this function must update
%%% chat marker record for that composite key.
%%% @end
-spec update_chat_marker(Host :: jid:lserver(),
                         ChatMarker :: mod_smart_markers:chat_marker()) -> ok.
update_chat_marker(Host, ChatMarker) ->
    do_update_chat_marker(Host, ChatMarker).

%%% @doc
%%% This function must return the latest chat markers sent to the
%%% user/room (with or w/o thread) later than provided timestamp.
%%% @end
-spec get_chat_markers(Host :: jid:lserver(), To :: jid:jid(),
                       Thread :: mod_smart_markers:maybe_thread(),
                       Timestamp :: integer()) -> [mod_smart_markers:chat_marker()].
get_chat_markers(Host, To, Thread, TS) ->
    do_get_chat_markers(Host, To, Thread, TS).

%%--------------------------------------------------------------------
%% local functions
%%--------------------------------------------------------------------
do_update_chat_marker(Host, #{from := From, to := To, thread := Thread,
                              type := Type, timestamp := TS, id := Id}) ->
    FromEncoded = encode_jid(From),
    ToEncoded = encode_jid(To),
    ThreadEncoded = encode_thread(Thread),
    TypeEncoded = encode_type(Type),
    KeyValues = [FromEncoded, ToEncoded, ThreadEncoded, TypeEncoded],
    UpdateValues = [Id, TS],
    InsertValues = KeyValues ++ UpdateValues,
    Res = rdbms_queries:execute_upsert(Host, smart_markers_upsert,
                                       InsertValues, UpdateValues, KeyValues),
    ok = check_upsert_result(Res).

do_get_chat_markers(Host, To, Thread, TS) ->
    ToEscaped = escape(encode_jid(To)),
    ThreadEscaped = escape(encode_thread(Thread)),
    TSEscaped = escape(TS),
    SelectQuery = [
        "select from_jid, to_jid, thread, type, msg_id, timestamp from smart_markers"
        " WHERE to_jid = ", ToEscaped, " AND thread = ", ThreadEscaped,
        " AND timestamp >= ", TSEscaped, " ;"],
    {selected, ChatMarkers} = mongoose_rdbms:sql_query(Host, SelectQuery),
    decode(ChatMarkers).

encode_jid(JID) -> jid:to_binary(jid:to_lus(JID)).

encode_thread(undefined) -> <<"">>;
encode_thread(Thread)    -> Thread.

encode_type(received)     -> <<"R">>;
encode_type(displayed)    -> <<"D">>;
encode_type(acknowledged) -> <<"A">>.

escape(String) when is_binary(String) -> escape_string(String);
escape(Int) when is_integer(Int)      -> escape_int(Int).

escape_string(String) ->
    mongoose_rdbms:use_escaped_string(mongoose_rdbms:escape_string(String)).

escape_int(Int) ->
    mongoose_rdbms:use_escaped_integer(mongoose_rdbms:escape_integer(Int)).

%% MySQL returns 1 when an upsert is an insert
%% and 2, when an upsert acts as update
check_upsert_result({updated, 1}) -> ok;
check_upsert_result({updated, 2}) -> ok;
check_upsert_result(Result) ->
    {error, {bad_result, Result}}.

decode(ChatMarkersList) ->
    [decode_record(ChatMarker) || ChatMarker <- ChatMarkersList].

decode_record({From, To, Thread, Type, Id, TS}) ->
    #{from => decode_jid(From),
      to => decode_jid(To),
      thread => decode_thread(Thread),
      type => decode_type(Type),
      timestamp => decode_timestamp(TS),
      id => Id}.

decode_jid(EncodedJID) -> jid:from_binary(EncodedJID).

decode_thread(<<"">>) -> undefined;
decode_thread(Thread) -> Thread.

decode_type(<<"R">>) -> received;
decode_type(<<"D">>) -> displayed;
decode_type(<<"A">>) -> acknowledged.

decode_timestamp(EncodedTS) ->
    mongoose_rdbms:result_to_integer(EncodedTS).

