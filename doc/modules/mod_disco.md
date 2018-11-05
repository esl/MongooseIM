### Module Description
Implements [XEP-0030: Service Discovery](http://xmpp.org/extensions/xep-0030.html). The module itself provides only the essential disco interface, the actual capabilities announced by Service Discovery are gathered via executing a fold-type hook.

### Options
* **iqdisc** (default: `one_queue`)
* **extra_domains** (list of binaries, default: `[]`): Adds domains that are not registered with other means to a local item announcement (response to `http://jabber.org/protocol/disco#items` IQ get). 
 Please note that `mod_disco` doesn't verify these domains, so if no handlers are registered later for them, a client will receive a `service-unavailable` error for every stanza sent to one of these hosts.
* **server_info** (list of tuples `{[Module] | all, Name, [URL]}`, default: `[]`): Adds extra disco information to all or chosen modules. 
 Example: `{server_info, [{all, "abuse-address", ["admin@example.com"]}, {[mod_muc, mod_disco], "friendly-spirits", ["spirit1@localhost", "spirit2@localhost"]}]}`. 
 New fields will be added in a manner compliant with XEP-0157.
* **users_can_see_hidden_services** (boolean, default: `true`): MongooseIM node with this option set to `false` will exclude ["hidden services"](../advanced-configuration/Listener-modules.md#xmpp-components-ejabberd_service) from disco results sent to clients (identified by bare or full JID).
Other entities, with empty username part in their JIDs (e.g. `component.example.com`), will still receive full disco results.

### Example Configuration
`    {mod_disco, []} `
