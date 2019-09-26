### Module Description

Enables [XEP-0198: Stream Management](http://xmpp.org/extensions/xep-0198.html).
Most of the logic regarding session resumption and acknowledgement is implemented in `ejabberd_c2s`,
while the management of the session tables and configuration is implemented in
`mod_stream_management`.

### In `ejabberd_c2s`

The record `#state{}` in the `ejabberd_c2s` `gen_fsm` server keeps fields like:

```erlang
stream_mgmt = false, %% whether SM is enabled, used in pattern matching inside `ejabberd_c2s`
stream_mgmt_in = 0, %% amount of msgs on the server and not acked by the user (server's <h>)
stream_mgmt_id, %% the mod_stream_management:smid() unique identifier
stream_mgmt_out_acked = 0, %% messages delivered to the user, and acked by the user (user's <h>)
stream_mgmt_buffer = [], %% buffered stanzas not yet acked by the user
stream_mgmt_buffer_size = 0, %% amount of messages buffered for the user
stream_mgmt_buffer_max = ?STREAM_MGMT_CACHE_MAX, %% server's capacity for buffering
stream_mgmt_ack_freq = ?STREAM_MGMT_ACK_FREQ, %% how often the server requests acks
stream_mgmt_resume_timeout = ?STREAM_MGMT_RESUME_TIMEOUT, %% resumption timeout
stream_mgmt_resume_tref, %% a ref() to pattern-match a given timeout
stream_mgmt_resumed_from, %% a ejabberd_sm:sid() to keep identifiying the old session
stream_mgmt_constraint_check_tref, %% another ref() for a timeout, this time for buffer_full check
```

### In `mod_stream_management`

This module is just a "starter", to supply the configuration values to new client connections. It
also provides a basic session table API and adds a new stream feature.

At a bare minimum, this module keeps the config values in its `gen_mod` records, and it keeps a
mnesia table defined as follows:

```erlang
-record(sm_session,
        {smid :: smid(),
         sid :: ejabberd_sm:sid()
        }).
```

Where `smid` is a unique identifier — in this case a random binary, and `sid` is an opaque session
identifier from `ejabberd_sm`, which is needed to find the previous session we want to resume from.
This module implements hooks that run on connection removals and session cleanups, in order to clean
records from a dying session; and it also implements registration callbacks, used in `ejabberd_c2s`
when a session is registered for resumption.

In order to be compliant with the XEP version 1.6, where a server can do the best effort to give the
user the value of the server's `<h>` when a session timed out and cannot be resumed anymore, there's
a second optional table:

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
session, together with a timestamp. Then, when a new session attempting resumption queries
`mod_stream_management` for the data behind a `smid`, `mod_stream_management` can answer one of the
following:

```erlang
{sid, ejabberd_sm:sid()} | {stale_h, non_neg_integer()} | {error, smid_not_found}.
```

And `ejabberd_c2s` will pattern-match and act accordingly.

### Options

* `buffer_max` (default: 100): Buffer size for messages yet to be acknowledged.
* `ack_freq` (default: 1): Frequency of ack requests sent from the server to the client, e.g. 1
  means a request after each stanza, 3 means a request after each 3 stanzas.
* `resume_timeout` (default: 600): Timeout for the session resumption. Sessions will be removed
  after the specified number of seconds.
* `stale_h`: enable keeping old server's `<h>` values after the resumption timed out. Defaults to
  `{false, []}`. When enabled, parameters for the garbage collection of these tables have to be
  provided, for example as `{true, [{gc_repeat_after, 1800}, {gc_geriatric, 3600}]}` — 1800 for
  `gc_repeat_after` and 3600 for `gc_geriatric` are the defaults.
  - `gc_repeat_after`: How often the garbage collection will run in the background to clean this
    table. Defaults to 1800 seconds (half an hour).
  - `gc_geriatric`: The maximum lifespan of a record in memory. After this, they will be chased for
    cleanup. Defaults to 3600 seconds (one hour).

### Example Configuration

```
  {mod_stream_management, [{buffer_max, 30},
                           {ack_freq, 1},
                           {resume_timeout, 600}
                           {stale_h, {true, [{gc_repeat_after, 1800},
                                             {gc_geriatric, 3600}]}
                          ]},
```

