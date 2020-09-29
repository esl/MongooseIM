### Module Description
Implements [XEP-0030: Service Discovery](http://xmpp.org/extensions/xep-0030.html). The module itself provides only the essential disco interface, the actual capabilities announced by Service Discovery are gathered via executing a fold-type hook.

### Options
#### `modules.mod_disco.iqdisc.type`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** `"no_queue"`

Strategy to handle incoming stanzas. For details, please refer to
[IQ processing policies](../../advanced-configuration/Modules/#iq-processing-policies).

#### `modules.mod_disco.extra_domains`
* **Syntax:** array of strings, valid domain names
* **Default:** no extra domains
* **Example:** `extra_domains = ["custom_domain"]`
 
Adds domains that are not registered with other means to a local item announcement (response to `http://jabber.org/protocol/disco#items` IQ get). 
Please note that `mod_disco` doesn't verify these domains, so if no handlers are registered later for them, a client will receive a `service-unavailable` error for every stanza sent to one of these hosts.

#### `modules.mod_disco.server_info`
* **Syntax:** array of tables described below
* **Default:** no additional server info
* **Example:** 
```
server_info = [
                {module = "all", name = "abuse-address", urls = ["admin@example.com"]}
              ]
```
Adds extra disco information to all or chosen modules. 
New fields will be added in a manner compliant with [XEP-0157](https://xmpp.org/extensions/xep-0157.html).

Required keys and their values for each entry:

* `module` - the string `"all"` or an array of module names for which the additional server information is to be returned
* `name` - a non-empty string with the name of the field
* `urls` - an array of valid addresses

#### `modules.mod_disco.users_can_see_hidden_services`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `users_can_see_hidden_services = false`

MongooseIM node with this option set to `false` will exclude ["hidden services"](../advanced-configuration/Listener-modules.md#xmpp-components-ejabberd_service)
from disco results sent to clients (identified by bare or full JID).
Other entities, with empty username part in their JIDs (e.g. `component.example.com`),
will still receive full disco results.

### Example Configuration
```
[modules.mod_disco]
  iqdisc.type = "one_queue"
  extra_domains = ["some_domain", "another_domain"]
  server_info = [
    {module = "all", name = "abuse-address", urls = ["admin@example.com"]},
    {module = ["mod_muc", "mod_disco"], name = "friendly-spirits", urls = ["spirit1@localhost", "spirit2@localhost"]}
  ]
  users_can_see_hidden_services = true
```
