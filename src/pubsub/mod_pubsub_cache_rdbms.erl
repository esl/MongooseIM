-module(mod_pubsub_cache_rdbms).

-include("pubsub.hrl").
-include("jlib.hrl").

-export([start/0, stop/0]).

-export([create_table/0,
    delete_table/0,
    insert_last_item/4,
    delete_last_item/1,
    get_last_item/1]).
%% ------------------------ Backend start/stop ------------------------

-spec start() -> ok.
start() -> ok.

-spec stop() -> ok.
stop() -> ok.

-spec create_table() -> ok | {error, Reason :: term()}.
create_table() -> ok.

-spec delete_table() -> ok | {error, Reason :: term()}.
delete_table() -> ok.

-spec insert_last_item(Nidx :: mod_pubsub:nodeIdx(),
    ItemID :: mod_pubsub:itemId(),
    Publisher::{erlang:timestamp(), jid:ljid()},
    Payload::mod_pubsub:payload()) -> ok | {error, Reason :: term()}.
insert_last_item(Nidx, ItemID, Publisher, Payload) ->
    ReadQuerySQL = insert_pubsub_last_item(Nidx, ItemID, Publisher, Payload),
    mongoose_rdbms:sql_query(Publisher#jid.lserver, ReadQuerySQL),
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

%% ------------------- Pubusub last item ------------------------------

-spec get_pubsub_last_item(mod_pubsub:nodeIdx()) -> iolist().
get_pubsub_last_item(Nidx) ->
    ["SELECT * FROM pubsub_last_item"
    " WHERE nidx = ", esc_int(Nidx), ";"].

 -spec delete_pubsub_last_item(mod_pubsub:nodeIdx()) -> iolist().
delete_pubsub_last_item(Nidx) ->
    ["DELETE FROM pubsub_last_item"
    " WHERE nidx = ", esc_int(Nidx), ";"].

 -spec insert_pubsub_last_item(
    Nidx::mod_pubsub:nodeIdx(),
    ItemId::mod_pubsub:itemId(),
    Publisher::jid:ljid(),
    Payload::mod_pubsub:payload()) -> iolist().
insert_pubsub_last_item(Nidx, ItemId, Jid, Payload) ->
   EscCreatedAt = esc_int(usec:from_now(os:timestamp())),
   BinaryPayload = exml:to_binary(Payload),
   EscPayload = esc_string(BinaryPayload),
   EscModifiedLUser = esc_string(Jid#jid.luser),
   EscModifiedLServer = esc_string(Jid#jid.lserver),
   [
       "INSERT INTO pubsub_last_item (",
       columns(),
       ") VALUES (",
           esc_int(Nidx),", ",
           esc_string(ItemId),", ",
           EscModifiedLUser,", ",
           EscModifiedLServer,", ",
           EscCreatedAt,", ",
           EscPayload,
   ");"].

columns() -> "nidx, itemid, created_luser, created_lserver, created_at, payload".

%%====================================================================
%% Helpers
%%====================================================================

esc_string(String) ->
    mongoose_rdbms:use_escaped_string(mongoose_rdbms:escape_string(String)).

esc_int(Int) ->
    mongoose_rdbms:use_escaped_integer(mongoose_rdbms:escape_integer(Int)).

