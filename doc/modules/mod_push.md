### Module Description
This module implements [XEP-0357: Push Notifications](https://xmpp.org/extensions/xep-0357.html).
 It enables a service that notifies `PubSub` of user's choice about every message that he could 
 miss while being offline. There are two control stanzas that clinet may send to this module: 
 `enable` and `disable`. `enable` stanza enables push notifications and forwards them to 
 specified `PubSub` node. This stanza also may contain optional `Data Form` that will be added to
  each and every notification to `PubSub` node as `publish-options`. Please be sure to provide 
  all form fields that are required by specified `PubSub` node. Any publish error may result in 
  disabling push notifications to this node.  

### Options

* **backend** (atom, default: `mnesia`) - Backend to use for storing registrations. Currently 
only `mnesia` may be used.
* **wpool** (list, default: []) - List of options that will be passed to `worker_pool` library 
that handles all the requests. Please refer to [Project Site](https://github.com/inaka/worker_pool) for more details.
* **plugin_module** (atom, default: mod_push_plugin) - Plugin module that implements some dynamic
 configurations. Currently this module allow configuration of parsing message `sender id` and can
  filter messages that shall not be published to `PubSub` node.


[aws-virtual-host]: https://docs.aws.amazon.com/AmazonS3/latest/dev/VirtualHosting.html
[aws-region]: https://docs.aws.amazon.com/general/latest/gr/rande.html?shortFooter=true#s3_region
[aws-keys]: https://docs.aws.amazon.com/general/latest/gr/aws-sec-cred-types.html?shortFooter=true#access-keys-and-secret-access-keys

### Example configuration

```Erlang
{mod_push, [
    {backend, mnesia},
    {wpool, [{workers, 200}]},
    {plugin_module, mod_push_plugin}
]}.
```
