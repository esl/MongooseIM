-module(mod_pubsub_cache_rdbms).

-include("pubsub.hrl").
-include("jlib.hrl").

-export([start/0, stop/0]).

-export([
    create_table/0, 
    delete_table/0, 
    insert_last_item/4,
    get_last_item/1, 
    delete_last_item/1]).

%% ------------------------ Backend start/stop ------------------------

start() -> ok.

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
ReadQuerySQL = mod_pubsub_db_rdbms_sql:insert_pubsub_last_item(Nidx, ItemID, Publisher, Payload),
    {updated, _} = mongoose_rdbms:sql_query(global, ReadQuerySQL),
    ok.

-spec  delete_last_item(Nidx :: mod_pubsub:nodeIdx()) -> ok | {error, Reason :: term()}.
delete_last_item(Nidx) -> 
    DeleteQuerySQL = mod_pubsub_db_rdbms_sql:delete_pubsub_last_item(Nidx),
    {updated, _} = mongoose_rdbms:sql_query(global, DeleteQuerySQL),
    ok.

-spec get_last_item(Nidx :: mod_pubsub:nodeIdx()) -> 
    [mod_pubsub:pubsubLastItem()] | {error, Reason :: term()}.
get_last_item(Nidx) -> 
    ReadQuerySQL = mod_pubsub_db_rdbms_sql:get_pubsub_last_item(Nidx),
    {selected, LastItemRows} = mongoose_rdbms:sql_query(global, ReadQuerySQL),
    LastItemRows.
