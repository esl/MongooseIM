# Selected hooks description

This is a brief documentation for a few selected hooks.
Though hooks & handlers differ in what they are there to do, it is not necessary to describe them all, because the mechanism is general.
The following is meant to give you the idea of how the hooks work, what they are used for and the various purposes they can serve.

## `user_send_*`

```erlang
mongoose_c2s_hooks:user_send_packet(HostType, Acc, Params)
mongoose_c2s_hooks:user_send_message(HostType, Acc, Params)
mongoose_c2s_hooks:user_send_presence(HostType, Acc, Params)
mongoose_c2s_hooks:user_send_iq(HostType, Acc, Params)
mongoose_c2s_hooks:user_send_xmlel(HostType, Acc, Params)
```
These hooks are run in `mongoose_c2s` after the C2S process receives an XML element from the client.

The hooks won't be called for stanzas arriving from a user served by a federated server (i.e. on a server-to-server connection handled by `ejabberd_s2s`).

The logic depends on the C2S state, which changes during the connection, authentication and resource binding process:

### Hooks called for `session_established`

Some rudimentary verification of the stanza is done once it is received from the socket:

- if present, the `from` attribute of the stanza is checked against the identity of the user whose session the process in question serves;
  if the identity does not match the contents of the attribute, an error is returned,
- the recipient JID (`to` attribute) format is verified.

After successful checks, the following hooks are called. The first one is `user_send_packet`, which is called for all received XML elements. Next, depending on the type of the element, one of the following hooks is called:

* `user_send_message` for messages,
* `user_send_presence` for presences,
* `user_send_iq` for IQ (info/query) stanzas,
* `user_send_xmlel` for other XML elements.

These type-specific hooks should be used instead of `user_send_packet` when possible.

### Hooks called for other states

If the session is not established (e.g. the client hasn't authenticated or its resource is not bound yet), only the `user_send_xmlel` hook is called regardless of the XML element type. No other `user_send_*` hooks are called, and no stanza checks are performed.

### Handler examples

These hooks are handled by the following modules:

* [`mod_blocking`](../modules/mod_blocking.md) - handles IQ requests for blocking lists.
* [`mod_caps`](../modules/mod_caps.md) - detects and caches capability information sent with certain presences for later use.
* [`mod_carboncopy`](../modules/mod_carboncopy.md) - forwards messages to all the user's resources which have carbon copying enabled.
* [`mod_event_pusher`](../modules/mod_event_pusher.md) - sends selected messages to an external service.
* [`mod_inbox`](../modules/mod_inbox.md) - stores messages in the user's inbox.
* [`mod_mam`](../modules/mod_mam.md) - stores outgoing messages in an archive.
* [`mod_ping`](../modules/mod_ping.md) - upon reception of every message from the client, this module (re)starts a timer;
 if nothing more is received from the client within 60 seconds, it sends an IQ ping, to which the client should reply - which starts another timer.
* [`mod_presence`](../modules/mod_presence.md) - handles presence stanzas, updating the user presence state and broadcasting presence updates.
* [`mod_privacy`](../modules/mod_privacy.md) - filters sent stanzas according to privacy lists and handles privacy-related IQ requests.
* [`mod_register`](../modules/mod_register.md) - registers a new user when a registration IQ is received. `user_send_xmlel` is used because the stanza is received while the session is not established.
* [`mod_smart_markers`](../modules/mod_smart_markers.md) - checks if the stanza contains chat markers info and stores the update.
* [`mod_stream_management`](../modules/mod_stream_management.md) - counts stanzas sent by the client and handles special XML elements like `<a>` and `<enable>`.

## `filter_packet` and `filter_local_packet`

```erlang
mongoose_hooks:filter_packet({From, To, Acc, Packet})
mongoose_hooks:filter_local_packet({From, To, Acc, Packet})
```

These hooks are run when the packet is being [routed](../Stanza-routing/#3-message-routing) by `ejaberd_router:route/4`, which is the most general function used to route stanzas across the entire cluster. For example, `mongoose_c2s` calls it after calling the `user_send_message` or `user_send_iq` hook, and multiple modules use it for sending replies and errors.

* `filter_packet` is run by `mongoose_router_global` for all routed packets. It is called at the start of the routing procedure.
* `filter_local_packet` is run by `mongoose_local_delivery` when the packet is being routed to a domain hosted by the local server.

The handlers expect the `{From, To, Acc, Packet}` accumulator as their first argument.
The stanza can be filtered out (in case the handler returns `drop`), left unchanged or modified.

!!! note "`filter_packet` is a global hook"
    Note the hook code inside `mongoose_hooks`:
    ```erlang
    filter_packet(Acc) ->
        run_global_hook(filter_packet, Acc, #{}).
    ```
    This hook is run not for a host type, but globally across the whole cluster.
    Keep that in mind when registering the handlers and appropriately use the atom `global` instead of a host type as the second argument.

### Handler examples

These hooks are handled by the following modules:

* [`mod_domain_isolation`](../modules/mod_domain_isolation.md) - filters out cross-domain stanzas.
* [`mod_event_pusher`](../modules/mod_event_pusher.md) - sends out configured events (e.g. push notifications) for incoming stanzas.
* [`mod_inbox`](../modules/mod_inbox.md) - stores incoming messages in the recipient's inbox.
* [`mod_mam`](../modules/mod_mam.md) - stores incoming messages in the recipient's archive, and adds MAM-related elements to the message.
* [`mod_pubsub`](../modules/mod_pubsub.md) - for each subscription authorization form sent by a node owner, the subscription state is updated, and the stanza is dropped.
* [`mod_smart_markers`](../modules/mod_smart_markers.md) - filters out chat markers, because they are handled separately by `mod_offline_chatmarkers`.

## `user_receive_*`

```erlang
mongoose_c2s_hooks:user_receive_packet(HostType, Acc, Params)
mongoose_c2s_hooks:user_receive_message(HostType, Acc, Params)
mongoose_c2s_hooks:user_receive_presence(HostType, Acc, Params)
mongoose_c2s_hooks:user_receive_iq(HostType, Acc, Params)
mongoose_c2s_hooks:user_receive_xmlel(HostType, Acc, Params)
```

These hooks are run in `mongoose_c2s` after the recipient's  C2S process receives an XML element and before sending it to the user.

The hooks won't run for stanzas which are destined to users of a different XMPP domain served by a federated server, connection to which is handled by `ejabberd_s2s`.

The first hook is `user_receive_packet`, which is called for all received XML elements. Next, depending on the type of the stanza, one of the following hooks is called:

* `user_receive_message` for messages,
* `user_receive_presence` for presences,
* `user_receive_iq` for IQ (info/query) stanzas,
* `user_receive_xmlel` for other XML elements.

These type-specific hooks should be used instead of `user_receive_packet` when possible.

### Handler examples

These hooks are handled by the following modules:

* [`mod_caps`](../modules/mod_caps.md) - detects and caches capability information sent with certain messages for later use.
* [`mod_carboncopy`](../modules/mod_carboncopy.md) - forwards messages to all the user's resources which have carbon copying enabled.
* [`mod_last`](../modules/mod_last.md) - filters queries for user's last activity according to presence subscriptions.
* [`mod_presence`](../modules/mod_presence.md) - handles incoming presences from other users, updating the presence status, and responds to presence probes.
* [`mod_privacy`](../modules/mod_privacy.md) - filters received stanzas according to privacy lists.
* [`mod_stream_management`](../modules/mod_stream_management.md) - filters out stanzas with conflicting session ID's.

## `offline_message_hook`

```erlang
mongoose_hooks:offline_message_hook(Acc, From, To, Packet)
```

`ejabberd_sm` runs this hook for each message which cannot be delivered, because no resource (i.e. device or desktop client application) of its recipient is available online for delivery.

### Handler examples

This hook is handled by the following modules, listed in the order of execution:

* `mod_offline_chatmarkers` - for chat marker messages, the handler stores them and returns `{stop, Acc}`, preventing further handlers from being called.

* [`mod_offline`](../modules/mod_offline.md) - stores messages in a persistent way until the recipient comes online, and the message can be successfully delivered. The handler returns `{stop, Acc}` for all messages, preventing further handlers from being called.

* [`mod_offline_stub`](../modules/mod_offline_stub.md) - returns `{stop, Acc}` for all messages, preventing further handlers from being called.

* `ejabberd_sm` - calls `ejabberd_sm:bounce_offline_message`, which responds with the `<service-unavailable/>` stanza error. In the case of using `mod_mam` the message is actually stored, and no such error should be sent - that's why the module `mod_offline_stub` can be enabled.

## `remove_user`

```erlang
mongoose_hooks:remove_user(Acc, LServer, LUser)
```

`remove_user` is run by `ejabberd_auth` - the authentication module - when a request is made to remove the user from the database of the server.

### Handler examples

This hook is used by multiple modules, since removing a user requires many cleanup operations:

* [`mod_auth_token`](../modules/mod_auth_token.md) removes user's authentication tokens;
* [`mod_event_pusher`](../modules/mod_event_pusher.md) disables user's push notifications;
* [`mod_inbox`](../modules/mod_inbox.md) removes user's inbox;
* [`mod_last`](../modules/mod_smart_markers.md) removes last activity information ([XEP-0012: Last Activity][XEP-0012]);
* [`mod_mam`](../modules/mod_mam.md) removes the user's message archive;
* [`mod_muc_light`](../modules/mod_muc_light.md) quits multi-user chat rooms;
* [`mod_offline`](../modules/mod_offline.md) deletes the user's offline messages;
* [`mod_privacy`](../modules/mod_privacy.md) removes the user's privacy lists;
* [`mod_private`](../modules/mod_private.md) removes the user's private xml data storage;
* [`mod_pubsub`](../modules/mod_pubsub.md) unsubscribes from publish/subscribe channels;
* [`mod_roster`](../modules/mod_roster.md) removes the user's roster from the database;
* [`mod_smart_markers`](../modules/mod_smart_markers.md) removes chat markers stored for the user;
* [`mod_vcard`](../modules/mod_vcard.md) removes user's vCard information.

## `node_cleanup`

```erlang
mongoose_hooks:node_cleanup(Node)
```

`node_cleanup` is run by a `mongooseim_cleaner` process which subscribes to `nodedown` messages.
Currently, the hook is run inside a global transaction (via `global:trans/4`).

The job of this hook is to remove all processes registered in Mnesia.
MongooseIM uses Mnesia to store processes through which messages are then routed - like user sessions or server-to-server communication channels - or various handlers, e.g. IQ request handlers.
Those must obviously be removed when a node goes down, and to do this the modules `ejabberd_local`, `ejabberd_router`, `ejabberd_s2s`, `ejabberd_sm` and `mod_bosh` register their handlers with this hook.

Number of retries for this transaction is set to 1 which means that in some situations the hook may be run on more than one node in the cluster, especially when there is little garbage to clean after the dead node.
Setting retries to 0 is not good decision as it was observed that in some setups it may abort the transaction on all nodes.

## `session_opening_allowed_for_user`
```erlang
allow == mongoose_hooks:session_opening_allowed_for_user(HostType, JID)
```

This hook is run after authenticating when user sends the IQ opening a session.
Handler function are expected to return:

* `allow` if a given JID is allowed to open a new sessions (the default)
* `deny` if the JID is not allowed but other handlers should be run
* `{stop, deny}` if the JID is not allowed but other handlers should **not** be run

In the default implementation the hook is not used, built-in user control methods are supported elsewhere.
This is the perfect place to plug in custom security control.

## Other hooks

* acc_room_affiliations
* adhoc_local_commands
* adhoc_sm_commands
* amp_check_condition
* amp_determine_strategy
* amp_verify_support
* anonymous_purge_hook
* auth_failed
* c2s_stream_features
* can_access_identity
* can_access_room
* caps_recognised
* check_bl_c2s
* disco_info
* disco_local_features
* disco_local_identity
* disco_local_items
* disco_muc_features
* disco_sm_features
* disco_sm_identity
* disco_sm_items
* does_user_exist
* extend_inbox_result
* failed_to_store_message
* filter_local_packet
* filter_packet
* filter_pep_recipient
* filter_room_packet
* filter_unacknowledged_messages
* forbidden_session_hook
* foreign_event
* forget_room
* get_key
* get_mam_muc_gdpr_data
* get_mam_pm_gdpr_data
* get_pep_recipients
* get_personal_data
* inbox_unread_count
* invitation_sent
* is_muc_room_owner
* join_room
* leave_room
* mam_archive_id
* mam_archive_message
* mam_archive_size
* mam_archive_sync
* mam_flush_messages
* mam_get_behaviour
* mam_get_prefs
* mam_lookup_messages
* mam_muc_archive_id
* mam_muc_archive_message
* mam_muc_archive_size
* mam_muc_archive_sync
* mam_muc_flush_messages
* mam_muc_get_behaviour
* mam_muc_get_prefs
* mam_muc_lookup_messages
* mam_muc_remove_archive
* mam_muc_retraction
* mam_muc_set_prefs
* mam_remove_archive
* mam_retraction
* mam_set_prefs
* mod_global_distrib_known_recipient
* mod_global_distrib_unknown_recipient
* node_cleanup
* offline_groupchat_message_hook
* offline_message_hook
* packet_to_component
* presence_probe_hook
* privacy_check_packet
* privacy_get_user_list
* privacy_iq_get
* privacy_iq_set
* privacy_list_push
* privacy_updated_list
* push_notifications
* register_subhost
* register_user
* remove_domain
* remove_user
* reroute_unacked_messages
* resend_offline_messages_hook
* room_exists
* room_new_affiliations
* room_packet
* roster_get
* roster_get_jid_info
* roster_get_subscription_lists
* roster_get_versioning_feature
* roster_groups
* roster_in_subscription
* roster_out_subscription
* roster_process_item
* roster_push
* roster_set
* s2s_allow_host
* s2s_receive_packet
* s2s_send_packet
* s2s_stream_features
* session_cleanup
* session_opening_allowed_for_user
* set_presence_hook
* set_vcard
* sm_filter_offline_message
* sm_register_connection_hook
* sm_remove_connection_hook
* unacknowledged_message
* unregister_subhost
* unset_presence_hook
* update_inbox_for_muc
* user_available_hook
* user_open_session
* user_ping_response
* user_receive_iq
* user_receive_message
* user_receive_packet
* user_receive_presence
* user_receive_xmlel
* user_send_iq
* user_send_message
* user_send_packet
* user_send_presence
* user_send_xmlel
* user_socket_closed
* user_socket_error
* user_stop_request
* user_terminate
* vcard_set
* xmpp_bounce_message
* xmpp_presend_element
* xmpp_send_element
* xmpp_stanza_dropped

[privacy-lists]: http://xmpp.org/extensions/xep-0016.html
[XEP-0012]: https://xmpp.org/extensions/xep-0012.html
