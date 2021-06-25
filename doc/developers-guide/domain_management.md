## MongooseIM core component

Implemented by `mongoose_domain_core` module.

It must be based on gen\_server & ETS table w. public read access.
This module is local for the node, it must not implement any sync across the
nodes in a cluster.
This component will be responsible for dynamic routing, it will always be
started by MIM even if there is no support of dynamic domain names configured.

It must provide the following interfaces:

- Init - should accept the list of initial domain/host\_type pairs provided in
  config file and the list of host\_types that can be used for dynamic insertion.
  Any of these lists can be empty, initial list of domain/host\_type pairs can
  have some unique host\_types not mentioned in the host\_types list.
  The component must be initialised by the main MIM supervisor.
  Implemented in `mongoose_domain_api:init()`.
- Insert - adding new domain/host\_type pair.
  This function must be **idempotent**, it must return success on attempt to
  insert the existing data, but it must fail if ETS already has the domain
  name associated with another host type.
  Implemented in `mongoose_domain_api:insert_domain(Domain, HostType)`.
- Remove - This function must be idempotent. this function deletes existing
  domain/host\_type pairs.
  It must be impossible to delete domain/host\_type pairs specified on init
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
  On disabling domain name must be deleted from the core MIM component.
  Change of the status must be distributed across all the nodes in the cluster.
- Remove the domain name - This function must be idempotent.
  Domain name must be deleted from the core MIM component (if required) and from the DB.
  This action must be distributed across all the nodes in the cluster.

In case of any issues (domain name is already configured with another
host\_type or host\_type is not supported), they must be logged as errors.

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

You are not allowed to delete domains with unknown host-type. 
Configure host-type first to delete such domains.

## Service options

### `event_cleaning_interval`
The number of seconds between cleaning attempts of the `domain_events` table.
* **Syntax:** positive integer
* **Default:** `1800` (30 minutes)
* **Example:** `event_cleaning_interval = 1800`

### `event_max_age`
The number of seconds after an event must be deleted from the `domain_events` table.
* **Syntax:** positive integer
* **Default:** `7200` (2 hours)
* **Example:** `event_max_age = 7200`

## REST API

Provides API for adding/removing and enabling/disabling domains over HTTP.
Implemented by `mongoose_domain_handler` module.

### `listen.http.handlers.mongoose_domain_handler.username`
* **Syntax:** string
* **Default:** not set
* **Example:** `username = "admin"`

When set, enables authentication to access this endpoint. Requires setting `password`.

### `listen.http.handlers.mongoose_domain_handler.password`
* **Syntax:** string
* **Default:** not set
* **Example:** `password = "secret"`

Required to enable authentication for this endpoint.

### Example configuration

```toml
[[listen.http]]
  ip_address = "127.0.0.1"
  port = 8088
  transport.num_acceptors = 10
  transport.max_connections = 1024

  [[listen.http.handlers.mongoose_domain_handler]]
    host = "localhost"
    path = "/api"
    username = "admin"
    password = "secret"
```

### Add domain

```bash
curl -v -X PUT "http://localhost:8088/api/domains/example.db" \
    --user admin:secret \
    -H 'content-type: application/json' \
    -d '{"host_type": "type1"}'
```

Result codes:

* 204 - inserted.
* 409 - domain already exists with a different host type.
* 403 - DB service disabled.
* 403 - unknown host type.
* 500 - other errors.

Example of the result body with a failure reason:

```
{"what":"unknown host type"}
```

Check the `src/domain/mongoose_domain_handler.erl` file for the exact values of the `what` field if needed.


### Delete domain

You must provide the domain's host type inside the body:

```bash
curl -v -X DELETE "http://localhost:8088/api/domains/example.db" \
    --user admin:secret \
    -H 'content-type: application/json' \
    -d '{"host_type": "type1"}'
```

Result codes:

* 204 - the domain is removed or not found.
* 403 - the domain is static.
* 403 - the DB service is disabled.
* 403 - the host type is wrong (does not match the host type in the database).
* 403 - the host type is unknown.
* 500 - other errors.


### Enable/disable domain

Provide `{"enabled": true}` as a body to enable a domain.
Provide `{"enabled": false}` as a body to disable a domain.

```bash
curl -v -X PATCH "http://localhost:8088/api/domains/example.db" \
    --user admin:secret \
    -H 'content-type: application/json' \
    -d '{"enabled": true}'
```

Result codes:

* 204 - updated.
* 404 - domain not found;
* 403 - domain is static;
* 403 - service disabled.


## Command Line Interface

Implemented by `service_admin_extra_domain` module.

### Configuration example:

```toml
[services.service_admin_extra]
  submods = ["node", "accounts", "sessions", "vcard", "gdpr", "upload",
             "roster", "last", "private", "stanza", "stats", "domain"]
```
 
### Add domain:

```
./mongooseimctl insert_domain domain host_type
```

### Delete domain:

```
./mongooseimctl delete_domain domain host_type
```

### Disable domain:

```
./mongooseimctl disable_domain domain
```

### Enable domain:

```
./mongooseimctl enable_domain domain
```
