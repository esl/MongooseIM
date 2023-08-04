!!! warning
    Some [modules](../configuration/Modules.md#modules-incompatible-with-dynamic-domains) do not work with dynamic domains.
    This is also the case for [`s2s`](../configuration/s2s.md) and the XMPP components (XEP-0114) mechanism, as configured in the [`listen.service` section](../configuration/listen.md#xmpp-components-listenservice).

## MongooseIM core component

Implemented by `mongoose_domain_core` module.

It is based on gen\_server & ETS table w. public read access.
This module is local for the node, it does not implement any sync across the
nodes in a cluster.
This component is responsible for dynamic routing, it is always
started by MIM even if there is no support of dynamic domain names configured.

It provides the following interfaces:

- Init - accepts the list of initial domain/host\_type pairs provided in
  config file, and the list of host\_types that can be used for dynamic insertion.
  Any of these lists can be empty, initial list of domain/host\_type pairs can
  have some unique host\_types not mentioned in the host\_types list.
  The component is initialised by the main MIM supervisor.
  Implemented in `mongoose_domain_sup:start_link/0`.
- Insert - adding new domain/host\_type pair.
  This function is idempotent. It returns success on an attempt to insert the existing data,
  but fails if ETS already has the domain name associated with another host type.
  Implemented in `mongoose_domain_api:insert_domain(Domain, HostType)`.
- Remove - This function is idempotent. It deletes existing
  domain/host\_type pairs.
  It is impossible to delete domain/host\_type pairs specified on init
  of the component.
  Implemented in `mongoose_domain_api:delete_domain(Domain)`.
- Get host type by domain.
  Implemented in `mongoose_domain_api:get_host_type(Domain).`.
- Get all domains configured for the host\_type. 
  Implemented in `mongoose_domain_api:get_domains_by_host_type(HostType).`.
- Get the list of the host\_types provided during initialisation.
  Implemented in `mongoose_domain_api:get_all_static().`.

`mongoose_domain_core` implementation:

- Has `mongoose_domain_core` table.
- Default (initial) domains are **static**.
- Disabled or deleted domains are not in `mongoose_domain_core`.
- Static domains are non-mutable.
- Static domains are not replicated.
- Static domains has priority above DB domains.

## MongooseIM service

As described in [Services](../configuration/Services.md#service_domain_db).
Implements the service behaviour.
Implemented by `service_domain_db` module.

This service provides an interface for dynamic management of domain names.
It has persistent storage (RDBMS) where it stores information about domain names.
This service ensures synchronization of dynamically managed domain names
across different nodes in the cluster.

The minimal set of information associated with domain name is this:

- Host type
- Status (enabled/disabled)

This service provides the following interfaces:

- Init - on init all the “enabled” domain names from the persistent storage
  is added to the core MIM component described above.
- Add domain name (w/ host type) - This function is idempotent.
  An added domain is always “enabled” by default - it must be added in the core MIM
  component described in the previous section.
  If it’s successfully enabled than Information about the domain name is
  added into persistent storage and distributed across all the nodes in the cluster.
- Disabling/Enabling domain name - This function is idempotent. The status
  of the existing domain is always changed on successful call.
  If domain name is enabled, then it is added in the core MIM component.
  On disabling domain name is deleted from the core MIM component.
  Change of the status is distributed across all the nodes in the cluster.
- Remove the domain name - This function is idempotent.
  Domain name is deleted from the core MIM component (if required) and from the DB.
  This action is distributed across all the nodes in the cluster.

In case of any issues (domain name is already configured with another
host\_type or host\_type is not supported), errors are logged.

The database schema contains two tables:

- `domain_settings` - one record per domain. Maps `domain` name to `host_type` and `enabled` status.
- `domain_events` - the log of changes. The only reason it exists is that
  we can track updates in the `domain_settings` and get apply updates across different nodes.
  The old events are eventually deleted from the table.  Removal is triggered by
  all nodes of MongooseIM, that have the service configured.

`service_domain_db` module does two tasks:

- Initially downloads domains from `domain_settings` table, using sorting by id.
- Waits for `check_for_updates` message and updates core component, depending
  on records in the `domain_events` table.

We use `id` field to sort records when paginating.

### Domain removal

You cannot delete domains with unknown host-type.
Configure host-type first to delete such domains.

Modules which store data in RDBMS and support dynamic domains will remove **all** persistent data associated with a domain when its removal is requested.
This is not the case for NoSQL databases or Mnesia.
Because of that, we recommend using RDBMS with dynamic domains.
Please note, that [`mod_auth_token`](../modules/mod_auth_token.md) is the only exception for now and does not remove data from RDBMS when removing a domain.

## Service options

Described in the [`services` section](../configuration/Services.md#service_domain_db).

## Command Line Interface

You can manage the domains with the `mongooseimctl` command. Some examples are provided below:

### Add domain:

```
./mongooseimctl domain addDomain --domain example.com --hostType type1
```

### Delete domain:

```
./mongooseimctl domain removeDomain --domain example.com --hostType type1
```

### Disable domain:

```
./mongooseimctl domain disableDomain --domain example.com
```

### Enable domain:

```
./mongooseimctl domain enableDomain --domain example.com
```

Run `./mongooseimctl domain` to get more information about all supported operations.

## API

You can manage domains with one of our API's:

* The [GraphQL API](../../graphql-api/Admin-GraphQL) has the same funtionality as the command line interface. The <a href="../../graphql-api/admin-graphql-doc.html#query-domain">queries</a> and <a href="../../graphql-api/admin-graphql-doc.html#mutation-domain">mutations</a> for domains are grouped under the `domain` category.
* The [REST API](../../rest-api/Administration-backend) (deprecated) supports domain management as well. See <a href="../../swagger/index.html#/Dynamic_domains">Dynamic Domains</a> for details.
