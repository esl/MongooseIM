### Module Description
This module implements [XEP-0206: XMPP Over BOSH](http://xmpp.org/extensions/xep-0206.html) (using [XEP-0124: Bidirectional-streams Over Synchronous HTTP (BOSH)](http://xmpp.org/extensions/xep-0124.html)), allowing clients to connect to MongooseIM over regular HTTP long-lived connections.

If you want to use BOSH, you must enable it both in the `listen` section of `mongooseim.cfg` ([Listener Modules](../advanced-configuration/Listener-modules.md)) and as a module.

### Options

* `inactivity` (positive integer or `infinity`, default: 30): Maximum allowed inactivity time for a BOSH connection. Please note that a long-polling request is not considered to be an inactivity.
* `max_wait` (positive integer or `infinity`, default: `infinity`): This is the longest time (in seconds) that the connection manager will wait before responding to any request during the session.
* `server_acks` (boolean, default: `false`): Enables/disables [acks](http://xmpp.org/extensions/xep-0124.html#ack-request) sent by server.
* `backend` (atom, default: `mnesia`): Backend used for storing BOSH session data. `mnesia` is the only supported value.
* `maxpause` (positive integer, default: 120): Maximum allowed pause in seconds (e.g. to switch between pages and then resume connection) to request by client-side.

### Example Configuration

In the listener section:
```
{listen,
 [
  { 5280, ejabberd_cowboy, [
      {num_acceptors, 10},
      {max_connections, 1024},
      {modules, [
          {"_", "/http-bind", mod_bosh}
      ]}
  ]}
```

In the module section:  
```  {mod_bosh, []} ```
