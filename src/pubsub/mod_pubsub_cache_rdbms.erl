-module(mod_pubsub_cache_rdbms).

-include("pubsub.hrl").
-include("jlib.hrl").

-export([start/0, stop/0]).

-export([create_table/0,
    delete_table/0,
    upsert_last_item/4,
    delete_last_item/1,
    get_last_item/1]).
%% ------------------------ Backend start/stop ------------------------

-spec start() -> ok.
start() -> ok.

-spec stop() -> ok.
stop() -> ok.

%% ------------------- Pubusub last item ------------------------------

-spec create_table() -> ok | {error, Reason :: term()}.
create_table() -> ok.

-spec delete_table() -> ok | {error, Reason :: term()}.
delete_table() -> ok.

-spec upsert_last_item(Nidx :: mod_pubsub:nodeIdx(),
    ItemID :: mod_pubsub:itemId(),
    Publisher ::jid:ljid(),
    Payload :: mod_pubsub:payload()) -> ok | {error, Reason :: term()}.
upsert_last_item(Nidx, ItemID, Publisher, Payload) ->
    Backend = {mongoose_rdbms:db_engine(global), mongoose_rdbms_type:get()},
    ReadQuerySQL = upsert_pubsub_last_item(Nidx, ItemID, Publisher, Payload, Backend),
    {updated, _} = mongoose_rdbms:sql_query(Publisher#jid.lserver, ReadQuerySQL),
    ok.

-spec delete_last_item(Nidx :: mod_pubsub:nodeIdx()) -> ok | {error, Reason :: term()}.
delete_last_item(Nidx) ->
    DeleteQuerySQL = delete_pubsub_last_item(Nidx),
    {updated, _} = mongoose_rdbms:sql_query(global, DeleteQuerySQL),
    ok.

-spec get_last_item(Nidx :: mod_pubsub:nodeIdx()) ->
    [mod_pubsub:pubsubLastItem()] | {error, Reason :: term()}.
get_last_item(Nidx) ->
    ReadQuerySQL = get_pubsub_last_item(Nidx),
    {selected, LastItemRows} = mongoose_rdbms:sql_query(global, ReadQuerySQL),
    LastItemRows.


-spec get_pubsub_last_item(mod_pubsub:nodeIdx()) -> iolist().
get_pubsub_last_item(Nidx) ->
    ["SELECT * FROM pubsub_last_item"
    " WHERE nidx = ", esc_int(Nidx), ";"].

 -spec delete_pubsub_last_item(mod_pubsub:nodeIdx()) -> iolist().
delete_pubsub_last_item(Nidx) ->
    ["DELETE FROM pubsub_last_item"
    " WHERE nidx = ", esc_int(Nidx), ";"].

 -spec upsert_pubsub_last_item(
    Nidx::mod_pubsub:nodeIdx(),
    ItemId::mod_pubsub:itemId(),
    Publisher::jid:ljid(),
    Payload::mod_pubsub:payload(),
    Backend::{atom(), atom()}) -> iolist().
upsert_pubsub_last_item(Nidx, ItemId, Publisher, Payload, {pgsql, _}) ->
    upsert_parametrized(Nidx, ItemId, Publisher, Payload, pgsql);
upsert_pubsub_last_item(Nidx, ItemId, Publisher, Payload, {mysql, _}) ->
    upsert_parametrized(Nidx, ItemId, Publisher, Payload, mysql);
upsert_pubsub_last_item(_Nidx, _ItemId, _Publisher, _Payload, {odbc, mssql}) ->
    "TODO IMPLEMENT ME".

%%====================================================================
%% Helpers
%%====================================================================

esc_string(String) ->
    mongoose_rdbms:use_escaped_string(mongoose_rdbms:escape_string(String)).

esc_int(Int) ->
    mongoose_rdbms:use_escaped_integer(mongoose_rdbms:escape_integer(Int)).

esc_query_parms(Nidx, ItemId, Publisher, Payload) ->
    EscNidx = esc_int(Nidx),
    EscItemId = esc_string(ItemId),
    EscCreatedAt = esc_int(usec:from_now(os:timestamp())),
    BinaryPayload = exml:to_binary(Payload),
    EscPayload = esc_string(BinaryPayload),
    EscModifiedLUser = esc_string(Publisher#jid.luser),
    EscModifiedLServer = esc_string(Publisher#jid.lserver),
    {
        EscNidx, EscItemId,
        EscModifiedLUser, EscModifiedLServer,
        EscCreatedAt, EscPayload
    }.

columns() -> "nidx, itemid, created_luser, created_lserver, created_at, payload".

upsert_parametrized(Nidx, ItemId, Publisher, Payload, OnConflictLine) ->
        {
        EscNidx, EscItemId,
        EscModifiedLUser, EscModifiedLServer,
        EscCreatedAt, EscPayload
    } = esc_query_parms(Nidx, ItemId, Publisher, Payload),
    [
        "INSERT INTO pubsub_last_item (",
        columns(),
        ") VALUES (",
            EscNidx,", ",
            EscItemId,", ",
            EscModifiedLUser,", ",
            EscModifiedLServer,", ",
            EscCreatedAt,", ",
            EscPayload,
            ") ",
        on_conflict_line(OnConflictLine),
            "nidx = ", EscNidx, ", ",
            "itemid = ", EscItemId, ", ",
            "created_luser = ", EscModifiedLUser, ", ",
            "created_lserver = ", EscModifiedLServer, ", ",
            "created_at = ", EscCreatedAt, ", ",
            "payload = ", EscPayload,
            ";"].

on_conflict_line(pgsql) -> "ON CONFLICT (nidx) DO UPDATE SET ";
on_conflict_line(mysql) -> "ON DUPLICATE KEY UPDATE ".
