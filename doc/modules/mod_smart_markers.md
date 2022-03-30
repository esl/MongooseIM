## Module Description

Smart markers are an experimental feature, described in detail as our [Open XMPP Extension for markers](../open-extensions/smart-markers.md).

## Options

### `modules.mod_smart_markers.iqdisc`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** `"no_queue"`

Strategy to handle incoming IQ requests. For details, please refer to
[IQ processing policies](../configuration/Modules.md#iq-processing-policies).

### `modules.mod_smart_markers.backend`
* **Syntax:** string, one of `"rdbms"`, `"rdbms_async"`
* **Default:** `"rdbms"`
* **Example:** `backend = "rdbms_async"`

Only RDBMS storage is supported, but `rdbms` means flushes to DB are synchronous with each message, while `rdbms_async` is instead asynchronous.

Regular `rdbms` has worse performance characteristics, but it has better consistency properties, as events aren't lost nor reordered. `rdbms_async` processes events asynchronously and potentially unloading a lot of aggregation from the DB. Like the case of the asynchronous workers for MAM, it is the preferred method, with the risk messages being lost on an ungraceful shutdown.

### `modules.mod_smart_markers.keep_private`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `keep_private = true`

This indicates if markers are meant to be private to the sender of the marker (setting `keep_private` as `true`), or if they can be public.

By default markers are public to the conversation where they are sent, so they'll be routed to all recipients, and anyone in the chat can see where its peers are at any time, i.e., the Facebook Messenger model; but they can be configured private, so markers won't be routed to anyone, and a user who fetches their status will only receive information for markers they have sent alone, i.e., the Slack model.

## Example configuration

```toml
[modules.mod_smart_markers]
  backend = "rdbms"
  iqdisc = "parallel"
```

## Implementation details
The current implementation has some limitations:

* It does not verify that markers only move forwards, hence a user can, intentionally or accidentally, send a marker to an older message, and this would override newer ones.
* It stores markers sent only for users served on a local domain. It does not store received markers, so if the peer is reached across federation, this module won't track markers for federated users. Therefore extensions that desire seeing not only the sender's markers but also the peer's markers, won't work with the current implementation across federated users.
