# Selected hooks description

## `user_send_packet`

```erlang
ejabberd_hooks:run(user_send_packet, Server,
                   [FromJID, ToJID, Stanza])
```

This hook is run in `ejabberd_c2s` after the user sends a packet.
Some rudimentary verification of the stanza is done once it is received
from the socket:

- if present, the `from` attribute of the stanza is checked against
  the identity of the user whose session the process in question serves;
  in case of the identity not matching the contents of the attribute an error
  is returned,

- the recipient JID (`to` attribute) format is verified.

The hook is not run for stanzas which do not pass these basic validity
checks. Neither are such stanzas further processed by the server.

This hook won't be called for stanzas which arrive from a user served
by a federated server (i.e. on a server-to-server connection handled
by `ejabberd_s2s`) and are intended for a user served by the relevant
ejabberd instance.

## `user_receive_packet`

```erlang
ejabberd_hooks:run(user_receive_packet, Server,
                   [Jid, From, To, FixedPacket])
```

The hook is run just after a packet has been sent to the user.

Prior to sending, the packet is verified against any relevant
privacy lists (the mechanism is described in [XEP-0016][privacy-lists]).
The privacy list mechanism itself is not mandatory and requires
`mod_privacy` to be configured; otherwise all stanzas are allowed
to pass.

[privacy-lists]: http://xmpp.org/extensions/xep-0016.html

This hook won't run for stanzas which are destined to users of a different
XMPP domain served by a federated server, connection to which is handled
by `ejabberd_s2s`.

## `filter_packet`

```erlang
ejabberd_hooks:run_fold(filter_packet,
                        {OrigFrom, OrigTo, OrigPacket}, [])
```

This hook is run by `ejabberd_router` as soon as the packet is routed
via `ejaberd_router:route/3`.
This is the most general function used to route stanzas across the
entire cluster and its calls are scattered all over ejabberd code.
`ejabberd_c2s` calls it after it receives a packet from
`ejabberd_receiver` (i.e. the socket) and multiple modules use it for
sending replies and errors.

As seen in the example the handlers take no arguments.
The accumulator is the packet which may or may not be filtered out
(in case the handler chain returns `drop`) or modified.

Note that this hook is run with `ejabberd_hooks:run_fold/3`,
not the usual and already mentioned `ejabberd_hooks:run_fold/4`.
The ternary variant doesn't take the XMPP domain argument
and hence it's not possible to register per-domain handlers for this hook.
Keep that in mind when registering the handlers and
appropriately use `ejabberd_hooks:add/4` instead of `ejabberd_hooks:add/5`.

## `offline_message_hook`

```erlang
ejabberd_hooks:run(offline_message_hook,
                   Server,
                   [From, To, Packet])
```

`ejabberd_sm` runs this hook once it determines that a routed stanza
is a message and that no resource (i.e. device or desktop client
application) of its recipient is available online for delivery.

The conservative approach is to use `mod_offline` to store that message in
a persistent way until the recipient becomes online and the message can be
successfully delivered.
The handler in `mod_offline` stores the message and returns `stop`.
In case some handler is registered with a sequence number greater than 50
it will not be called if `mod_offline` successfully stores the message.

## `remove_user`

```erlang
ejabberd_hooks:run(remove_user, Server, [User, Server])
```

`remove_user` is run by `ejabberd_auth` - the authentication module - when
a request is made to remove the user from the database of the server.
The hook is used to allow modules clean up the user data in any relevant
way - in a process state, persistent storage or by notifying some external
service.

## `node_cleanup`
```erlang
ejabberd_hooks:run(node_cleanup, [Node])
```

`node_cleanup` is run by mongooseim_cleaner process which subscribes to 
`nodedown` messages. Currently the hook is run inside global transaction
(via `global:trans/4`).

Number of retries for this transaction is set to 1 which means that in some
situations the hook may be run on more than one node in the cluster, especially 
when there is little garbage to clean after the dead node. Setting retries to 0
is not good decision as it was observed that in some setups it may abort the
transaction on all nodes.