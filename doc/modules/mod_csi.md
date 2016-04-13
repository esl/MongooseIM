### Module Description
Enables [XEP-0352 (Client State Indication)](http://xmpp.org/extensions/xep-352.html)
functionality. It is implemented mostly in `ejabberd_c2s`, this module is
just a "starter", to advertise the `csi` stream feature.

The Client State Indication functionality will be possible to use even
without enabling this module, but the feature will not be present in the
stream features list.

### Options

* **buffer_max** (default: 20) - Buffer size for messages queued when session was `inactive`

### Example Configuration

```Erlang
  {mod_csi, [{buffer_max, 40}]},
```

