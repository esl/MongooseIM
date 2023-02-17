# Route of a message through the system

Let's examine the flow of a message sent from Alice to Bob, both of whom are served by the same domain and connected to the server.

Note that hooks are called at various stages of routing - they perform many tasks, and many MongooseIM functionalities are implemented through hooks & handlers.
For a general introduction to hooks, see [Hooks and Handlers](Hooks-and-handlers.md); to get a closer look at a core few, see the [hooks description](hooks_description.md).

## 1. Sender's C2S process receives the message

Alice's C2S (client-to-server) process, which is a state machine implemented in the `mongoose_c2s` module, receives data from the TCP socket, and parses each incoming XML element with `exml` to an internal representation of the stanza, which is then processed by the C2S as a subsequent event.

## 2. Call to `user_send_*` hooks

Upon some minimal validation of the stanza, the hook `user_send_packet` is called.
Next, depending on the type of the stanza, one of the following hooks is called:

* `user_send_message` for messages,
* `user_send_presence` for presences,
* `user_send_iq` for IQ (info/query) stanzas,
* `user_send_xmlel` for other XML elements.

Each hook can be handled by multiple modules subscribed to it. Those modules do various complementary tasks, like storing the message in an archive, sending carbon copies, checking the stanza against privacy lists etc. It is possible for a handler to immediately stop routing at this point, preventing execution of any subsequent handlers or hooks. See [hooks description](../hooks_description/#hooks-called-for-session_established) for more information.

## 3. Message routing

The stanza is routed by `ejabberd_router:route/3`, which passes it through a chain of routing modules implementing the `xmpp_router` behaviour and applies the following functions for each of them:

1. `Mod:filter/3`, which either drops the stanza, stopping the routing chain, or returns it for further processing, modifying it if necessary.
2. `Mod:route/3`, which either handles the stanza, stopping the routing chain, or returns it for further processing, modifying it if necessary.

A list of routing modules can be set in the [`routing_modules`](../../configuration/general#generalrouting_modules) option.
The default behaviour is the following:

* `mongoose_router_global`: runs a global `filter_packet` hook.
* `mongoose_router_localdomain`: if there is a local route registered for the destination domain (i.e. there is an entry in the `mongoose_router` ETS table), routes the stanza to it. When the recipient's domain is checked for the first time, the corresponding route is not registered yet, because the routes are added lazily - see `mongoose_router_dynamic_domains`.
* `mongoose_router_external_localnode`: if there is an external component registered for the destination domain on the current node, routes the stanza to it. Such components are stored in the Mnesia table `external_component`, which is not replicated in the cluster.
* `mongoose_router_external`: if there is an external component registered for the destination domain on any node in the cluster, routes the stanza to it. Such components are stored in the Mnesia table `external_component_global`, which is replicated among all cluster nodes.
* `mongoose_router_dynamic_domains`: if the recipient's domain is hosted by the local server, a route is added for it, and the stanza is routed locally.
* `ejabberd_s2s`: tries to find or establish a connection to another server and send the stanza there.

![Routing chain](routing.png#only-light) <!-- https://docs.google.com/drawings/d/1V0n6mPN03TsDsdggXCymoaLnf3JIoPqTr_5lcEF8JrY -->
![Routing chain](routing-dark.png#only-dark) <!-- https://docs.google.com/drawings/d/14UHBjhNR1yvqj-C3YBm3VU0UokYFvu0TNLb3Qw_Ut_s -->

Assuming that the message from Alice to Bob is not the first stanza addressed to their domain, the routing chain will stop at `mongoose_router_localdomain`, which will deliver the message locally.

## 4. `mongoose_local_delivery`

When an external component or a local route is found, the packet is delivered locally by `mongoose_local_delivery:do_route/5`. Firstly, the `filter_local_packet` hook is run to check if the stanza should be delivered or dropped. This hook is also a place where modules can add their own functionality evaluated for each locally delivered stanza.

If the check passes, the next step is to call the handler associated with the component or the local route. Handlers are modules implementing the `mongoose_packet_handler` behaviour, and stanzas to local users (like Alice and Bob) are handled by the `ejabberd_local` module.

## 5. `ejabberd_local` to `ejabberd_sm`

`ejabberd_local:process_packet/5` checks if the stanza is addressed to a user or to the server itself. For local users like Bob, `ejabberd_sm:route/4` is called.

## 6. `ejabberd_sm`

`ejabberd_sm` determines the available resources of the recipient, takes into account their priorities and whether the message is addressed to a particular resource or a bare JID.
It appropriately replicates (or not) the message and sends it to the recipient's C2S process(es) by calling `mongoose_c2s:route/2`.
In case no resources are available for delivery (hence no C2S processes to pass the message to), `offline_message_hook` is run.

As Bob has one online session, the message is sent to the C2S process associated with that session.

## 7. Recipient's C2S process delivers the message

The `user_receive_packet` hook is run to notify the rest of the system about the stanza delivery.
Next, depending on the type of the stanza, one of the following hooks is called:

* `user_receive_message` for messages,
* `user_receive_presence` for presences,
* `user_receive_iq` for IQ (info/query),
* `user_receive_xmlel` for other XML elements.

Each hook can be handled by multiple modules subscribed to it. These hooks' handlers can stop the routing, e.g. when the stanza is blocked by `mod_privacy`. Finally, the `xmpp_presend_element` hook is called, which is used `mod_csi` and `mod_stream_management`. This is the last hook that can stop the routing - otherwise, the stanza is converted to binary data and sent to the recipient's TCP socket.
