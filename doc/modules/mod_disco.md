### Module Description
Implements [XEP-0030 (Service Discovery)](http://xmpp.org/extensions/xep-0030.html). The module itself only provides the essential disco interface, the actual capabilities announced by Service Discovery are gathered via executing a fold-type hook.

### Options
* **iqdisc**
* **extra_domains** (list of binaries, default: `[]`) - Adds domains that are not registered with other means to a local item announcement
* **server_info** (list of tuples `{[Module] | all, Name, [URL]}`, default: `[]`) - Adds extra disco information to chosen or all modules. Example: `{server_info, [{all, "abuse-address", ["admin@example.com"]}, {[mod_muc, mod_disco], "friendly-spirits", ["spirit1@localhost", "spirit2@localhost"]}]}`

### Example Configuration
`    {mod_disco, []} `