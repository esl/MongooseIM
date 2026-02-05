## Module Description

Implements [XEP-0359: Unique and Stable Stanza ID](https://xmpp.org/extensions/xep-0359.html).


## Options

This module has no configuration options.

## Example Configuration

```toml
[modules.mod_stanzaid]
```

## Metrics

This module does not provide any additional metrics.

## Impact on other modules

Activating this module implicitly sets:

`modules.mod_mam.pm.same_mam_id_for_peers = true`

`modules.mod_mam.no_stanzaid_element = false`

overriding any custom MAM settings.
