### Module Description
Enables [XEP-0198 (Stream Management)](http://xmpp.org/extensions/xep-0198.html) functionality. It is implemented mostly in `ejabberd_c2s`, this module is just a "starter", to supply configuration values to new client connections.

### Options

* **buffer_max** (default: 100) - Buffer size for messages yet to be acknowledged.
* **ack_freq** (default: 1) - Frequency of server-side acks, e.g. 1 means an ack after each stanza, 3 means an ack after each 3 stanzas.
* **resume_timeout** (default: 600) - Timeout for the session resumption. Sessions will be removed after the specified number of seconds.

### Example Configuration

```
  {mod_stream_management, [{buffer_max, 30},
                           {ack_freq, 1},
                           {resume_timeout, 600}
                          ]},
```

