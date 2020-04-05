%%%----------------------------------------------------------------------------
%%% @copyright (C) 2020, Erlang Solutions Ltd.
%%% @doc
%%%   RDBMS backend for mod_offline_chatmarkers module.
%%% @end
%%%----------------------------------------------------------------------------

-module(mod_offline_chatmarkers_rdbms).
-behaviour(mod_offline_chatmarkers).

-export([init/2,
         get/1,
         maybe_store/4,
         remove_user/1]).

-include("mongoose.hrl").
-include("jlib.hrl").

-spec init(Host :: jid:lserver(), Opts :: list()) -> ok.
init(_Host, _Opts) ->
    ok.

-spec get(Jid :: jid:jid()) -> {ok, [{Thread :: undefined | binary(),
                                      Room :: undefined | jid:jid(),
                                      Timestamp :: integer()}]}.
get(#jid{lserver = Host} = Jid) ->
    JidEscaped = escape(encode_jid(Jid)),
    SelectQuery = ["SELECT thread, room, timestamp FROM offline_markers",
                   " WHERE jid = ", JidEscaped, ";"],
    {selected, Rows} = mongoose_rdbms:sql_query(Host, SelectQuery),
    decode(Rows).

%%% @doc
%%% Jid, Thread, and Room parameters serve as a composite database key. If
%%% key is not available in the database, then it must be added with the
%%% corresponding timestamp. Otherwise this function does nothing, the stored
%%% timestamp for the composite key MUST remain unchanged!
%%% @end
-spec maybe_store(Jid :: jid:jid(), Thread :: undefined | binary(),
                  Room :: undefined | jid:jid(), Timestamp :: integer()) -> ok.
maybe_store(#jid{lserver = Host} = Jid, Thread, Room, Timestamp) ->
    JidEscaped = escape(encode_jid(Jid)),
    ThreadEscaped = escape(encode_thread(Thread)),
    RoomEscaped = escape(encode_jid(Room)),
    TSEscaped = escape(Timestamp),
    InsertQuery = ["INSERT INTO offline_markers (jid, thread, room, timestamp) VALUES (",
                   JidEscaped, ",", ThreadEscaped, ",", RoomEscaped, ",", TSEscaped, ");"],
    Res = mongoose_rdbms:sql_query(Host, InsertQuery),
    ok = check_insert_result(Res).

-spec remove_user(Jid :: jid:jid()) -> ok.
remove_user(#jid{lserver = Host} = Jid) ->
    JidEscaped = escape(encode_jid(Jid)),
    DelQuery = ["DELETE FROM offline_markers WHERE jid = ", JidEscaped, ";"],
    {updated, _} = mongoose_rdbms:sql_query(Host, DelQuery),
    ok.

encode_jid(undefined) -> <<"">>;
encode_jid(JID)       -> jid:to_binary(jid:to_lus(JID)).

encode_thread(undefined) -> <<"">>;
encode_thread(Thread)    -> Thread.

escape(String) when is_binary(String) -> escape_string(String);
escape(Int) when is_integer(Int)      -> escape_int(Int).

escape_string(String) ->
    mongoose_rdbms:use_escaped_string(mongoose_rdbms:escape_string(String)).

escape_int(Int) ->
    mongoose_rdbms:use_escaped_integer(mongoose_rdbms:escape_integer(Int)).

%% add new record if key is not available, otherwise no changes to the table
check_insert_result({error,duplicate_key}) -> ok;
check_insert_result({updated, 1}) -> ok;
check_insert_result(Result) ->
    {error, {bad_result, Result}}.

decode(Rows) ->
    {ok, [decode_row(R) || R <- Rows]}.

decode_row({Thread, Room, TS}) ->
    {decode_thread(Thread), decode_jid(Room), decode_timestamp(TS)}.

decode_jid(<<"">>)     -> undefined;
decode_jid(EncodedJID) -> jid:from_binary(EncodedJID).

decode_thread(<<"">>)        -> undefined;
decode_thread(EncodedThread) -> EncodedThread.

decode_timestamp(EncodedTS) ->
    mongoose_rdbms:result_to_integer(EncodedTS).

