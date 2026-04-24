-record(pubsub_node, {node_key :: mod_pubsub:node_key(),
                      config :: mod_pubsub:node_config()}).

-record(subscription, {node_key :: mod_pubsub:node_key(),
                       jid :: jid:jid()}).

-record(item, {node_key :: mod_pubsub:node_key(),
               id :: mod_pubsub:item_id(),
               publisher_jid :: jid:jid(),
               payload :: mod_pubsub:item_payload()}).
