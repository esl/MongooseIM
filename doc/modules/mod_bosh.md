## Module Description
This module implements [XEP-0206: XMPP Over BOSH](http://xmpp.org/extensions/xep-0206.html) (using [XEP-0124: Bidirectional-streams Over Synchronous HTTP (BOSH)](http://xmpp.org/extensions/xep-0124.html)),
 allowing clients to connect to MongooseIM over regular HTTP long-lived connections.

If you want to use BOSH, you must enable it both in the `listen` section of 
`mongooseim.toml` ([Listener Modules](../configuration/listen.md))
 and as a module.

## Options

### `modules.mod_bosh.backend`
* **Syntax:** string: `"mnesia"` or `"cets"`
* **Default:** `"mnesia"`
* **Example:** `backend = "mnesia"`

Backend to use for storing BOSH connections.

!!! Warning
    The corresponding [internal database](../configuration/internal-databases.md) has to be enabled.

### `modules.mod_bosh.inactivity`
 * **Syntax:** positive integer or the string `"infinity"`
 * **Default:** `30`
 * **Example:** `inactivity = 30`
 
Maximum allowed inactivity time (in seconds) for a BOSH connection.
Please note that a long-polling request is not considered to be an inactivity.

### `modules.mod_bosh.max_wait`
 * **Syntax:** positive integer or the string `"infinity"`
 * **Default:** `"infinity"`
 * **Example:** `max_wait = 30`

 This is the longest time (in seconds) that the connection manager will wait before responding to any request during the session.

### `modules.mod_bosh.server_acks`
 * **Syntax:** boolean
 * **Default:** `false`
 * **Example:** `server_acks = true`
 
Enables/disables [acks](http://xmpp.org/extensions/xep-0124.html#ack-request) sent by server.

### `modules.mod_bosh.max_pause`
 * **Syntax:** positive integer
 * **Default:** `120`
 * **Example:** `max_pause = 30`

Maximum allowed pause in seconds (e.g. to switch between pages and then resume connection) to request by client-side.

## Example Configuration

In the listener section:
```toml
[[listen.http]]
  port = 5280
  transport.num_acceptors = 10
  transport.max_connections = 1024

  [[listen.http.handlers.mod_bosh]]
    host = "_"
    path = "/http-bind"
```

In the module section:

```toml
[modules.mod_bosh]
  inactivity = 20
  max_wait = "infinity"
  server_acks = true
  max_pause = 120
```
