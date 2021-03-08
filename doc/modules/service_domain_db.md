# MongooseIM core component

Implemented by `mongoose_domain_core` module.

It must be based on gen\_server & ETS table w. public read access.
This module is local for the node, it must not implement any sync across the
nodes in a cluster.
This component will be responsible for dynamic routing, it will always be
started by MIM even if there is no support of dynamic domain names configured.

It must provide the following interfaces:
- Init - should accept the list of initial domain/host\_type pairs provided in
  config file and the list of host\_types that can be used for dynamic insertion.
  Any of these lists can be empty, initial list of domain/host\_type pair can
  have some unique host\_types not mentioned in in the host\_types list.
  The component must be initialised by the main MIM supervisor
  (for initial implementation it can be initialised with empty lists)
  Implemented in `mongoose_domain_api:init(Pairs)`.
- Insert - adding new domain/host\_type pair.
  This function must be **idempotent**, it must return success on attempt to
  insert the existing data, but it must fail if ETS already has the domain
  name associated with another host type.
  Only host types provided during init are allowed! Domain names must be valid!
  Implemented in `mongoose_domain_api:insert_domain(Domain, HostType)`.
- Remove - This function must be idempotent. this function removes existing
  domain/host\_type pairs.
  It must be impossible to remove domain/host\_type pairs specified on init
  of the component!
  Implemented in `mongoose_domain_api:remove_domain(Domain)`.
- Get host type by domain.
  Implemented in `mongoose_domain_api:get_host_type(Domain).`.
- Get all domains configured for the host\_type.
- Get the list of the host\_types provided during initialisation.

`mongoose_domain_core` implementation:

- Has `mongoose_domain_core` table.
- Default (initial) domains are **locked**.
- Disabled or removed domains are not in `mongoose_domain_core`.
- Locked domains are non-mutable.
- Locked domains are not replicated.
- Locked domains has priority above DB domains.

# MongooseIM service

Implements the service behaviour.
Implemented by `service_domain_db` module.

This service must provide an interface for dynamic management of domain names.
It must have persistent storage (RDBMS) where it stores information about domain names.
This service must ensure synchronization of dynamically managed domain names
across different nodes in the cluster.

The minimal set of information associated with domain name is this:

- Host type
- Status (enabled/disabled)

This service must provide the following interfaces:

- Init - on init all the “enabled” domain names from the persistent storage
  must be added to the core MIM component described above.
- Add domain name (w/ host type) - This function must be idempotent.
  Added domain is always “enabled” by default it must be added in the core MIM
  component described in the previous section.
  If it’s successfully enabled than Information about the domain name must be
  added into persistent storage and distributed across all the nodes in the cluster.
- Disabling/Enabling domain name - This function must be idempotent. the status
  of the existing domain must be changed.
  If domain name is enabled, then it must be added in the core MIM component.
  On disabling domain name must be removed from the core MIM component.
  Change of the status must be distributed across all the nodes in the cluster.
- Removal the domain name - This function must be idempotent.
  Domain name must be removed from the core MIM component (if required) and from the DB.
  This action must be distributed across all the nodes in the cluster.

In case of any issues (domain name is already configured with another
host\_type or host\_type is not supported), they must be logged as errors.

The database schema contains two tables:

- `domain_settings` - one record per domain. Maps `domain` name to `host_type` and `enabled` status.
- `domain_events` - the log of changes. The only reason it exists is that
  we can track updates in the `domain_settings` and get apply updates acrooss different nodes.
  The old events are eventually removed from the table.

`service_domain_db` module do two tasks:

- Initially downloads domains from `domain_settings` table, using sorting by id.
- Waits for `check_for_updates` message and updates core component, depending
  on records in the `domain_events` table.

We use `id` field to sort records when paginating.

# REST API

We must provide REST API for Add/Remove and Enable/Disable interfaces of the
MongooseIM service described in the section above.

# Command Line Interfaces
 
We must provide CLI for Add/Remove and Enable/Disable interfaces of MongooseIM
service described in the section above.
