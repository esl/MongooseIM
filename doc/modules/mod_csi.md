### Module Description
Enables [XEP-0352: Client State Indication](http://xmpp.org/extensions/xep-0352.html)
functionality. It is implemented mostly in `ejabberd_c2s`, this module is
just a "starter", to advertise the `csi` stream feature.

The Client State Indication functionality will be possible to use even
without enabling this module, but the feature will not be present in the
stream features list.

The XEP doesn't **require** any specific server behaviour in response to CSI stanzas, there are only some suggestions.
The implementation in MongooseIM will simply buffer all packets (up to a configured limit) when session is "inactive" and will flush the buffer when the session becomes "active" again.

### Options

* `buffer_max` (default: 20): Buffer size for messages queued when session was `inactive`

### Example Configuration

```Erlang
  {mod_csi, [{buffer_max, 40}]},
```

