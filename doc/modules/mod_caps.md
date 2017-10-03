### Module description

This module provides a presence-based mechanism for exchanging information about entity capabilities as defined in [XEP-0115](https://xmpp.org/extensions/xep-0115.html). Additionally, it filters out PEP messages that the recipient declared (in announced caps) being not capable of handling.  
It is not this module's responsibility to intercept and answer disco requests routed between clients.

### Options

This module expects two optional arguments that apply to [cache tab](https://github.com/processone/cache_tab):
* `cache_size` (default: 1000) - the size of a cache_tab (the amount of entries) holding the information about capabilities of each user. 
* `cache_life_time` (default: 86) - time (in seconds) after which entries will be removed
