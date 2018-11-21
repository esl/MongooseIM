-module(pubsub_last_item).
-include("pubsub.hrl").

-export([
    create_table/1,
    dirty_write/5,
    dirty_read/2,
    dirty_delete/2
]).

create_table(mnesia) ->
    mnesia:create_table(
        pubsub_last_item,
        [
            {ram_copies, [node()]},
            {attributes, record_info(fields, pubsub_last_item)}
        ]).

dirty_write(Nidx, ItemId, Publisher, Payload, mnesia) -> 
    mnesia:dirty_write(
        {pubsub_last_item, 
        Nidx, 
        ItemId, 
        {os:timestamp(), 
            jid:to_lower(jid:to_bare(Publisher))}, 
        Payload}
    ).

dirty_read(Nidx, mnesia) -> 
    mnesia:dirty_read({pubsub_last_item, Nidx}).

dirty_delete(Nidx, mnesia) ->
    mnesia:dirty_delete({pubsub_last_item, Nidx}).
