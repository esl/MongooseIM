-record(item, {node_key :: mod_pubsub:node_key(),
               id :: mod_pubsub:item_id(),
               publisher_jid :: jid:jid(),
               payload = [] :: mod_pubsub:item_payload()}).
