## Module Description

This module is a backend of [mod_event_pusher] that implements [XEP-0357: Push Notifications](https://xmpp.org/extensions/xep-0357.html).
It provides push notification data to the service that delivers actual notifications to a client device.

We've prepared [a detailed tutorial](../user-guide/Push-notifications.md) about a proper push notifications setup both on a client and a server side.

Please make sure that clients provide all form fields required by the specified `PubSub` node.
Some publish errors may result in disabling push notifications for the specific device until it attempts to enable them again.

## Options

* **backend** (atom, default: `mnesia`) - Backend to use for storing the registrations.
 Possible options are `mnesia` and `rdbms`.
* **wpool** (list, default: `[]`) - List of options that will be passed to the `worker_pool` library that handles all the requests.
 Please refer to the [Project Site](https://github.com/inaka/worker_pool) for more details.
* **plugin_module** (atom, default: `mod_event_pusher_push_plugin_defaults`) - module implementing `mod_event_pusher_push_plugin` behaviour,
  used for dynamic configuration of push notifications. Read more about it [here](#plugin-module)
* **virtual_pubsub_hosts** (list of strings, default: `[]`) - a list of "simulated" Publish-Subscribe domains. See [relevant section](#virtual-pubsub-hosts) for more details. You may use `@HOST@` pattern in the domain name. It will automatically be replaced by a respective XMPP domain (e.g. `localhost`)

## Virtual PubSub hosts

If a notification is published to one of the configured domains, the internal push notification hook is executed in MongooseIM instead of the XEP-0357 typical behaviour.
If an existing PubSub domain is added to this list, it will be shadowed in the push notifications context.
It enables easy migration from PubSub-ful deployments to PubSub-less variants.

### Advantages

* No need to use PubSub at all
* Slightly more efficient (PubSub has noticable impact on heavily loaded systems)
* Simpler client-side usage

### Drawbacks

* Not suitable for public deployments, which allow connections from 3rd party client applications. Notifications won't work at all in such case.
* It is only partially XEP-0357 compliant, so 3rd party clients most probably won't work even in private deployments.
* This option works out of the box only with the default plugin. If you create a custom one, it must take the `virtual_pubsub_hosts` option into account as well.

## Configuration examples

### XEP-0357 compliant with local PubSub

This is the second most versatile setup.
It allows your clients to enable push notifications via either a local PubSub or a remote one.

```Erlang
{mod_pubsub, [{plugins, [<<"push">>]}]}, % mandatory minimal config

{mod_event_pusher, [
    {backends, [
        {push, [
            {backend, mnesia}, % optional
            {wpool, [{workers, 200}]}, % optional
            {plugin_module, mod_event_pusher_push_plugin_defaults} % optional
        ]}
    ]}
]}
```

### XEP-0357 compliant with remote PubSub

In this case, clients must provide a domain of a PubSub service that exists on another XMPP cluster.
Notifications will be delivered over XMPP Federation to the remote server(s).

```Erlang
{mod_event_pusher, [
    {backends, [
        {push, [
            {backend, mnesia}, % optional
            {wpool, [{workers, 200}]}, % optional
            {plugin_module, mod_event_pusher_push_plugin_defaults} % optional
        ]}
    ]}
]}
```

### Private deployment only

A direct connection to a push service (e.g. MongoosePush) must be configured on the same MongooseIM node.
PubSub is completely bypassed and the clients don't need to create a push node.
They only have to provide the virtual PubSub address.

```Erlang
{mod_event_pusher, [
    {backends, [
        {push, [
            {backend, mnesia}, % optional
            {wpool, [{workers, 200}]}, % optional
            {plugin_module, mod_event_pusher_push_plugin_defaults}, % optional
            {virtual_pubsub_hosts, ["virtual.@HOST@"]}
        ]}
    ]}
]}
```

### XEP-0357 compliant + virtual hosts

This is the most versatile setup. Allows the clients to use all push notifications configurations:

* Push service handled by another cluster via XEP-0357
* Push service handled locally via XEP-0357
* PubSub-less

```Erlang
{mod_pubsub, [{plugins, [<<"push">>]}]}, % mandatory minimal config

{mod_event_pusher, [
    {backends, [
        {push, [
            {backend, mnesia}, % optional
            {wpool, [{workers, 200}]}, % optional
            {plugin_module, mod_event_pusher_push_plugin_defaults}, % optional
            {virtual_pubsub_hosts, ["virtual.@HOST@"]}
        ]}
    ]}
]}
```

### Migration from XEP-0357 to virtual hosts

This is an example of how you can migrate the existing setup to the new model.
PubSub service still exists (because you might want to reuse it e.g. for publishing E2E encryption keys).
However, its domain is overridden for the purpose of sending push notifications.
Please note the value of `virtual_pubsub_hosts` option.
`pubsub.@HOST@` is the default domain for `mod_pubsub`.

```Erlang
{mod_pubsub, [{plugins, [<<"push">>]}]}, % mandatory minimal config

{mod_event_pusher, [
    {backends, [
        {push, [
            {backend, mnesia}, % optional
            {wpool, [{workers, 200}]}, % optional
            {plugin_module, mod_event_pusher_push_plugin_defaults}, % optional
            {virtual_pubsub_hosts, ["pubsub.@HOST@"]}
        ]}
    ]}
]}
```

## Plugin module

A plugin module handles dynamic configuration of push notifications. It implements `mod_event_pusher_push_plugin` behaviour which
requires two callbacks:

* `should_publish/3` - callback used for filtering push notifications. A push notification is triggered for a given a message only if this
callback returns `true`.

```
-spec should_publish(From :: ejabberd:jid(), To :: ejabberd:jid(), Packet :: jlib:xmlel()) -> boolean().
```

* `publish_notification/5` - does the actual push.
  By default it pushes to the registered pubsub nodes (or executes the internal hook in case of a publish to a virtual domain).

```
-spec publish_notification(Acc :: mongooseim_acc:t(), From :: jid:jid(),
                           To :: jid:jid(), Packet :: exml:element(),
                           Services :: [mod_event_pusher_push:publish_service()]) -> mongooseim_acc:t().
```

[mod_event_pusher]: ./mod_event_pusher.md
