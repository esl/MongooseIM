### Module Description
Implements [XEP-0030: Service Discovery](http://xmpp.org/extensions/xep-0030.html). The module itself provides only the essential disco interface, the actual capabilities announced by Service Discovery are gathered via executing a fold-type hook.

### Options
* **iqdisc** (default: `one_queue`)
* **extra_domains** (list of binaries, default: `[]`): Adds domains that are not registered with other means to a local item announcement (response to `http://jabber.org/protocol/disco#items` IQ get). 
 Please note that `mod_disco` doesn't verify these domains, so if no handlers are registered later for them, a client will receive a `service-unavailable` error for every stanza sent to one of these hosts.
* **server_info** (list of tuples `{[Module] | all, Name, [URL]}`, default: `[]`): Adds extra disco information to all or a chosen module. 
 Example: `{server_info, [{all, "abuse-address", ["admin@example.com"]}, {[mod_muc, mod_disco], "friendly-spirits", ["spirit1@localhost", "spirit2@localhost"]}]}`. 
 New fields will be added in a manner compliant with XEP-0157.

### Example Configuration
`    {mod_disco, []} `
