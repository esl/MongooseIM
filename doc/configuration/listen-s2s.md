# Server to server (S2S): `[[listen.s2s]]`

Handles incoming server-to-server (S2S) connections (federation).
The recommended port number for an S2S listener is 5269 [as registered in the XMPP protocol](https://tools.ietf.org/html/rfc6120#section-14.7).

!!! Note
    Many S2S options are configured in the `s2s` section of the configuration file, and they apply to both incoming and outgoing connections.

## General s2s options

### `listen.s2s.shaper`
* **Syntax:** string, name of the shaper rule or `"none"`
* **Default:** `"none"` - no shaper
* **Example:** `shaper = "s2s_shaper"`

Name of the rule that determines what traffic shaper is used to limit the incoming XMPP traffic to prevent the server from being flooded with incoming data. The rule referenced here needs to be defined in the `access` config section and it should return the shaper name or the value `"none"`.

## TLS options for S2S

S2S connections do not use TLS encryption unless enabled with the `use_starttls` option in the `s2s` section.
You can specify additional options of the TLS encryption in the `tls` subsection of the listener configuration. Accepted options are: `verify_mode`, `certfile`, `cacertfile`, `dhfile`, `ciphers` and `protocol_options`. They have the same semantics as the corresponding [c2s options](listen-c2s.md#tls-options-for-c2s) for `fast_tls`.

## S2S listener configuration example

The following section configures an S2S listener with some basic settings set up.
The `s2s_shaper` access rule is used, which requires a definition in the `access` section.

```toml
[[listen.s2s]]
  port = 5269
  shaper = "s2s_shaper"
  max_stanza_size = 131072
  tls.dhfile = "dh_server.pem"
```
