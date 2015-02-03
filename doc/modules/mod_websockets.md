### Module Description

If you'd like clients to connect over Websocket connections, you must enable
this module both in the [`listen` section of `ejabberd.cfg`](../advanced-configuration/Listener-modules.md) and as a module. Websockets are enabled by default.

### Options
none

### Example Configuration

In the listener section:
```
{listen,
 [
  { 5285, ejabberd_cowboy, [
      {num_acceptors, 10},
      {max_connections, 1024},
      {modules, [
          {"_", "/ws-xmpp", mod_websockets}
      ]}
  ]}
```

In the module section:
```
  {mod_websockets, []}
```
