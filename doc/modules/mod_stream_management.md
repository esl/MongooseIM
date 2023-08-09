## Module Description

Enables [XEP-0198: Stream Management](http://xmpp.org/extensions/xep-0198.html).
Implements logic regarding session resumption and acknowledgement as well as
the management of the session tables and configuration.

## Options

### `modules.mod_stream_management.backend`
* **Syntax:** string: `"mnesia"` or `"cets"`
* **Default:** `"mnesia"`
* **Example:** `backend = "mnesia"`

Backend for in-memory session data stored by this module.

!!! Warning
    The corresponding [internal database](../configuration/internal-databases.md) has to be enabled.

### `modules.mod_stream_management.buffer`
* **Syntax:** boolean
* **Default:** true
* **Example:** `buffer = false`

Enables buffer for messages to be acknowledged.

### `modules.mod_stream_management.buffer_max`
* **Syntax:** positive integer or string `"infinity"`
* **Default:** `100`
* **Example:** `buffer_max = 500`

Buffer size for messages yet to be acknowledged.

### `modules.mod_stream_management.ack`
* **Syntax:** boolean
* **Default:** true
* **Example:** `ack = false`

Enables ack requests to be sent from the server to the client.

### `modules.mod_stream_management.ack_freq`
* **Syntax:** positive integer
* **Default:** `1`
* **Example:** `ack_freq = 3`

Frequency of ack requests sent from the server to the client, e.g. 1 means a request after each stanza, 3 means a request after each 3 stanzas.

### `modules.mod_stream_management.resume_timeout`
* **Syntax:** positive integer, value given in seconds
* **Default:** `600`
* **Example:** `resume_timeout = 600`

Timeout for the session resumption. Sessions will be removed after the specified number of seconds.

### Stale_h options
Enables keeping old server's `<h>` values after the resumption timed out. Disabled by default. When enabled, parameters for the garbage collection of these tables should be provided.

#### `modules.mod_stream_management.stale_h.enabled`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `enabled = true`

Enables `stale_h` configuration

#### `modules.mod_stream_management.stale_h.repeat_after`
* **Syntax:** positive integer, value given in seconds
* **Default:** `1800` (half an hour)
* **Example:** `repeat_after = 1800`

How often the garbage collection will run in the background to clean this table.

#### `modules.mod_stream_management.stale_h.geriatric`
* **Syntax:** positive integer, value given in seconds
* **Default:** `3600` (one hour)
* **Example:** `geriatric = 3600`

The maximum lifespan of a record in memory. After this, they will be chased for cleanup.

## Example Configuration

```toml
[modules.mod_stream_management]
  buffer_max = 30
  ack_freq = 1
  resume_timeout = 600
  stale_h.enabled = true
  stale_h.repeat_after = 1800
  stale_h.geriatric = 3600
```

## Implementation details

Stream management state data is stored under the `mod_stream_management` key in the `#c2s_data.state_mod` map.
The state data record, `sm_state`, has the following fields:

* `buffer` - buffered stanzas not yet acked by the user
* `buffer_size` - number of stanzas buffered for the user
* `counter_in` - number of stanzas received by the server (server's `<h>`)
* `counter_out` - number of stanzas delivered to the user and acked by the user (user's `<h>`)
* `buffer_max` - server's capacity for buffering
* `ack_freq` - how often the server requests acks
* `peer` - in case of stream resumption, the `ejabberd_sm:sid()` identifiying the old session, or `gen_statem:from()` identifying the new session.

`mod_stream_management` introduces a new `resume_session` state to the C2S state machine,
that is used by a session being closed to allow stream resumption.

This module also has a Mnesia backend keeping a table defined as follows:

```erlang
-record(sm_session,
        {smid :: smid(),
         sid :: ejabberd_sm:sid()
        }).
```

where `smid` is a unique identifier — in this case a random binary, and `sid` is an opaque session
identifier from `ejabberd_sm`, which is needed to find the previous session we want to resume from.
This module implements hooks that run on connection removals and session cleanups, in order to clean
records from a dying session; and it also implements registration callbacks,
used when a session is registered for resumption.

XEP version 1.6 requires the server to attempt giving the user the value of the server's `<h>` when
a session timed out and cannot be resumed anymore. To be compliant with it, there's a second
optional table:

```erlang
-record(stream_mgmt_stale_h,
        {smid :: smid(),
         h :: non_neg_integer(),
         stamp :: non_neg_integer()
        }).
```

This table is created, together with a `gen_server` responsible for cleaning up the tables, when
`stale_h` is set to true with the proper garbage collection configuration. Then, when removing a
record from the `sm_session` table (which happens when the state of the previous session is also
dropped), a new record is added to this new table with the `smid` and `h` values of the dropped
session, together with a timestamp. Next, when a new session attempting resumption queries
`mod_stream_management` for the data behind a `smid`, `mod_stream_management` can answer with one of
the following:

```erlang
{sid, ejabberd_sm:sid()} | {stale_h, non_neg_integer()} | {error, smid_not_found}.
```

And `mod_stream_management` will pattern-match and act accordingly.
