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
init(Host, _Opts) ->
    mongoose_rdbms:prepare(offline_chatmarkers_select, offline_markers, [jid],
        <<"SELECT thread, room, timestamp FROM offline_markers WHERE jid = ?;">>),
    mongoose_rdbms:prepare(offline_chatmarkers_delete, offline_markers, [jid],
        <<"DELETE FROM offline_markers WHERE jid = ?;">>),
    rdbms_queries:prepare_upsert(Host, offline_chatmarkers_upsert, offline_markers,
                                 [<<"jid">>, <<"thread">>, <<"room">>, <<"timestamp">>],
                                 [],
                                 [<<"jid">>, <<"thread">>, <<"room">>]),
    ok.

-spec get(Jid :: jid:jid()) -> {ok, [{Thread :: undefined | binary(),
                                      Room :: undefined | jid:jid(),
                                      Timestamp :: integer()}]}.
get(#jid{lserver = Host} = Jid) ->
    {selected, Rows} = execute_select(Host, encode_jid(Jid)),
    decode(Rows).

-spec execute_select(Host :: jid:lserver(), Jid :: binary()) -> mongoose_rdbms:query_result().
execute_select(Host, Jid) ->
    mongoose_rdbms:execute_successfully(Host, offline_chatmarkers_select, [Jid]).

-spec execute_delete_user(Host :: jid:lserver(), Jid :: binary()) -> mongoose_rdbms:query_result().
execute_delete_user(Host, Jid) ->
    mongoose_rdbms:execute_successfully(Host, offline_chatmarkers_delete, [Jid]).

-spec execute_maybe_store(Host :: jid:lserver(),
                          Jid :: binary(),
                          Thread :: binary(),
                          Room :: binary(),
                          Timestamp :: integer()) ->
                              mongoose_rdbms:query_result().
execute_maybe_store(Host, Jid, Thread, Room, Timestamp) ->
    rdbms_queries:execute_upsert(Host, offline_chatmarkers_upsert,
                                 [Jid, Thread, Room, Timestamp],
                                 [],
                                 [Jid, Thread, Room]).
%%% @doc
%%% Jid, Thread, and Room parameters serve as a composite database key. If
%%% key is not available in the database, then it must be added with the
%%% corresponding timestamp. Otherwise this function does nothing, the stored
%%% timestamp for the composite key MUST remain unchanged!
%%% @end
-spec maybe_store(Jid :: jid:jid(), Thread :: undefined | binary(),
                  Room :: undefined | jid:jid(), Timestamp :: integer()) -> ok.
maybe_store(#jid{lserver = Host} = Jid, Thread, Room, Timestamp) ->
    {updated, _} = execute_maybe_store(Host, encode_jid(Jid), encode_thread(Thread),
                                       encode_jid(Room), Timestamp),
    ok.

-spec remove_user(Jid :: jid:jid()) -> ok.
remove_user(#jid{lserver = Host} = Jid) ->
    {updated, _} = execute_delete_user(Host, encode_jid(Jid)),
    ok.

encode_jid(undefined) -> <<"">>;
encode_jid(JID)       -> jid:to_binary(jid:to_lus(JID)).

encode_thread(undefined) -> <<"">>;
encode_thread(Thread)    -> Thread.

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

