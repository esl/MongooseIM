-module(pubsub_cache).
-include("pubsub.hrl").

%% Last item
-export([
    create_table/1,
    dirty_write/5,
    dirty_read/2,
    dirty_delete/2
]).

%% --------------------------------- Last item ---------------------------------

create_table(mnesia) ->
    mnesia:create_table(
        pubsub_last_item,
        [
            {ram_copies, [node()]},
            {attributes, record_info(fields, pubsub_last_item)}
        ]);
create_table(_) -> ok.

dirty_write(Nidx, ItemId, Publisher, Payload, mnesia) -> 
    mnesia:dirty_write(
        {pubsub_last_item, 
        Nidx, 
        ItemId, 
        {os:timestamp(), 
            jid:to_lower(jid:to_bare(Publisher))}, 
        Payload}
    );
dirty_write(Nidx, ItemId, Publisher, Payload, mnesia) -> 
    ReadQuerySQL = mod_pubsub_db_rdbms_sql:insert_pubsub_last_item(Nidx, ItemId, Publisher, Payload),
    {updated, _} = mongoose_rdbms:sql_query(global, ReadQuerySQL),
    ok.

dirty_read(Nidx, mnesia) -> 
    mnesia:dirty_read({pubsub_last_item, Nidx});
dirty_read(Nidx, _) ->  
    ReadQuerySQL = mod_pubsub_db_rdbms_sql:get_pubsub_last_item(Nidx),
    {selected, LastItemRows} = mongoose_rdbms:sql_query(global, ReadQuerySQL),
    LastItemRows.

dirty_delete(Nidx, mnesia) ->
    mnesia:dirty_delete({pubsub_last_item, Nidx});
dirty_delete(Nidx, _) ->
    DeleteQuerySQL = mod_pubsub_db_rdbms_sql:delete_pubsub_last_item(Nidx),
    {updated, _} = mongoose_rdbms:sql_query(global, DeleteQuerySQL),
    ok.
