## Module description

This module provides a presence-based mechanism for exchanging information about entity capabilities as defined in [XEP-0115: Entity Capabilities](https://xmpp.org/extensions/xep-0115.html). Additionally, it filters out PEP messages that the recipient declared (in announced caps) being not capable of handling.
It is not this module's responsibility to intercept and answer disco requests routed between clients.

## Options

This module expects two optional arguments that apply to [cache tab](https://github.com/processone/cache_tab):

### `modules.mod_caps.cache_size`
* **Syntax:** non-negative integer
* **Default:** `1000`
* **Example:** `cache_size = 2000`

The size of a cache_tab (the amount of entries) holding the information about capabilities of each user. 

### `modules.mod_caps.cache_life_time`
* **Syntax:** non-negative integer or the string `"infinity"`
* **Default:** `86`
* **Example:** `cache_life_time = 30`

Time (in seconds) after which entries will be removed.

## Example Configuration

```toml
[modules.mod_caps]
  cache_size = 2000
  cache_life_time = 86
```
