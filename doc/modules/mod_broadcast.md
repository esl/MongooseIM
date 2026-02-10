## Module Description

`mod_broadcast` lets administrators send the same XMPP message to many users in a domain.
It is intended for operational announcements (maintenance windows, policy changes, emergency notices) and similar one-to-many communication.

Recipients receive a normal `<message/>` stanza routed by the server.
The module itself is not user-facing: broadcasts are typically created and managed through the **GraphQL Admin API**.

### Prerequisites

- **RDBMS authentication is required.** `mod_broadcast` currently works only with `ejabberd_auth_rdbms`, because it uses the RDBMS auth backend to iterate over a snapshot of registered users.
- **RDBMS outgoing pool must be configured.** Both `ejabberd_auth_rdbms` and the broadcast job storage use an outgoing connection pool of type `rdbms` (typically the `default` tag) defined in the `outgoing_pools` section. See [Outgoing connections](../configuration/outgoing-connections.md) and [RDBMS authentication](../authentication-methods/rdbms.md).
- **GraphQL Admin API is the recommended control plane.** Broadcast management is exposed via the admin GraphQL schema (`broadcast.*`). See [GraphQL API (Admin)](../graphql-api/Admin-GraphQL.md) for general GraphQL setup and authentication.

!!! Warning
    Broadcast content (subject/body) is stored in the database until the broadcast is deleted.
    Treat this as sensitive data: review your retention policy and access controls.

## Options

### `modules.mod_broadcast.backend`

* **Syntax:** string, currently only `"rdbms"`
* **Default:** `"rdbms"`
* **Example:** `backend = "rdbms"`

Backend used to store broadcast jobs and worker progress.

## Broadcast job parameters and limits

These are not configuration keys; they are **job parameters** provided when starting a broadcast (for example via GraphQL):

- **Domain**: broadcasts are scoped to a single XMPP domain.
- **Recipient group**: currently only `ALL_USERS_IN_DOMAIN` is supported.
- **Sender JID**: must be an existing account.
- **Message rate**: must be between 1 and 1000 messages/second.
- **Content limits**:
  - `name`: 1..250 characters
  - `messageSubject`: 0..1024 characters (may be empty)
  - `messageBody`: 1..16000 characters
- **Concurrency limit**: currently only **one running broadcast per domain** is allowed.

!!! Warning
    Be careful with high `messageRate` values.
    Broadcasts can put load on routing, offline storage, push notifications, MAM, and external integrations (depending on your deployment).

## Managing broadcasts (GraphQL Admin API)

The admin schema exposes the following operations under the `broadcast` category:

- Query: `getBroadcasts(domain, limit, index)`
- Query: `getBroadcast(domain, id)`
- Mutation: `startBroadcast(...)`
- Mutation: `abortBroadcast(domain, id)`
- Mutation: `deleteInactiveBroadcastsByIds(domain, ids)`
- Mutation: `deleteInactiveBroadcastsByDomain(domain)`

### Example: start a broadcast

```graphql
mutation {
  broadcast {
    startBroadcast(
      name: "maintenance-2026-02-10"
      domain: "example.com"
      messageSubject: "Maintenance notice"
      messageBody: "We will perform maintenance at 22:00 UTC."
      senderJid: "admin@example.com"
      messageRate: 50
      recipientGroup: ALL_USERS_IN_DOMAIN
    ) {
      message
      ids
    }
  }
}
```

### Example: monitor progress

```graphql
query {
  broadcast {
    getBroadcast(domain: "example.com", id: 123) {
      id
      executionState
      recipientCount
      recipientsProcessed
      createTimestamp
      startTimestamp
      stopTimestamp
      abortionReason
    }
  }
}
```

### Example: abort a running broadcast

```graphql
mutation {
  broadcast {
    abortBroadcast(domain: "example.com", id: 123) {
      message
      ids
    }
  }
}
```

!!! Note
    Aborting a running job is performed by contacting the Erlang node that owns the job.
    If that node is down or unreachable, aborting may fail until the node is back.

## Message format and delivery semantics

Each recipient gets an XMPP `<message/>` stanza with:

- `type="chat"`
- `from` set to the configured sender bare JID
- `to` set to the recipient bare JID
- `<subject/>` and `<body/>` containing the configured content
- An `origin-id` element (`urn:xmpp:sid:0`) for traceability

Delivery to offline users depends on your deployment (for example, whether offline storage is enabled, whether push is configured, etc.).

## Example configuration

```toml
[modules.mod_broadcast]
  backend = "rdbms"
```

## Metrics

If you'd like to learn more about metrics in MongooseIM, please visit the [MongooseIM metrics](../operation-and-maintenance/MongooseIM-metrics.md) page.

!!! Note
  All `mod_broadcast` metrics are local to a single Erlang node.
  In a cluster, sum (or otherwise aggregate) the metrics across all MongooseIM nodes to obtain a cluster-wide total.

Prometheus metrics have a `host_type` label associated with these metrics.
Since Exometer doesn't support labels, the host types, or word `global`, are part of the metric names, depending on the [`instrumentation.exometer.all_metrics_are_global`](../configuration/instrumentation.md#instrumentationexometerall_metrics_are_global) option.

=== "Prometheus"

    | Name | Type | Description |
    |------|------|-------------|
    | `mod_broadcast_live_jobs` | gauge | Number of currently running broadcast jobs on the local node. |
    | `mod_broadcast_jobs_started` | counter | Broadcast jobs started. |
    | `mod_broadcast_jobs_finished` | counter | Broadcast jobs finished successfully. |
    | `mod_broadcast_jobs_aborted_admin` | counter | Broadcast jobs aborted by an administrator. |
    | `mod_broadcast_jobs_aborted_error` | counter | Broadcast jobs aborted automatically due to an error. |
    | `mod_broadcast_recipients_processed` | counter | Recipients processed (attempted deliveries). |
    | `mod_broadcast_recipients_success` | counter | Successful per-recipient routes. |
    | `mod_broadcast_recipients_skipped` | counter | Per-recipient routes that failed and were skipped. |

=== "Exometer"

    | Name | Type | Description |
    |------|------|-------------|
    | `[HostType, mod_broadcast_live_jobs, count]` | gauge | Number of currently running broadcast jobs on the local node. |
    | `[HostType, mod_broadcast_jobs_started, count]` | spiral | Broadcast jobs started. |
    | `[HostType, mod_broadcast_jobs_finished, count]` | spiral | Broadcast jobs finished successfully. |
    | `[HostType, mod_broadcast_jobs_aborted_admin, count]` | spiral | Broadcast jobs aborted by an administrator. |
    | `[HostType, mod_broadcast_jobs_aborted_error, count]` | spiral | Broadcast jobs aborted automatically due to an error. |
    | `[HostType, mod_broadcast_recipients_processed, count]` | spiral | Recipients processed (attempted deliveries). |
    | `[HostType, mod_broadcast_recipients_success, count]` | spiral | Successful per-recipient routes. |
    | `[HostType, mod_broadcast_recipients_skipped, count]` | spiral | Per-recipient routes that failed and were skipped. |

## Architecture overview

A broadcast is represented as a **job** persisted in the database.
The job is started on the Erlang node that handled the start request, and it remains owned by that node for its whole lifecycle.
In a cluster, this means that different broadcast runs may be owned by different nodes.

Aborting a job is automatically routed to the correct owner node.
Retrieving broadcast information (listing jobs and reading job details) is independent of job ownership and can be served by any node.

A per-host-type manager process starts a worker that:

1. Reads the job metadata and the last persisted worker state.
2. Loads recipients in batches from a *snapshot* of registered users (so the recipient list is consistent for the duration of the job).
3. Routes one message per recipient, rate-limited to the configured message rate.
4. Persists progress after each batch.

If the node restarts, the manager resumes jobs that were owned by that node and were still marked as `RUNNING`.