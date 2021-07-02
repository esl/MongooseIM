## Module Description

`mod_cache_users` is a module that caches whether a user exists. This is useful for example to decide if a message should be stored in [MAM] or [Inbox] — for example, the receiver might not exist, so no message should be stored in his archive nor his inbox.

This cache has a coarse-grained FIFO strategy, that is, it keeps a set of ETS tables that are periodically rotated, and on rotation, the last table is cleared. Records are simply inserted in the first table and aren't moved afterwards, only the table order is.


## Options

### `modules.mod_cache_users.time_to_live`
* **Syntax:** integer, in minutes, or the string `"infinity"`
* **Default:** `8 * 60` (8h)
* **Example:** `time_to_live = 480`

Time between rotations, that is, the time a single table will live. A record that is inserted in the first table will live as long as this ttl multiplied by the number of tables.

### `modules.mod_cache_users.number_of_segments`
* **Syntax:** integer
* **Default:** `3`
* **Example:** `number_of_segments = 3`

Number of segments the cache has. The more segments there are, the more fine-grained the cache can be, but the slower queries will be: query the cache checks the tables in order until a match is found.

## Example configuration

```toml
[modules.mod_cache_users]
  time_to_live = 60
  number_of_segments = 1
```

[MAM]: ../mod_mam
[Inbox]: ../mod_inbox
