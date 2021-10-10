## Module Description

`mod_cache_users` is a module that caches whether a user exists, and possibly stores metadata assigned to them. This is useful for example to decide if a message should be stored in [MAM] or [Inbox] — for example, the receiver might not exist, so no message should be stored in his archive nor his inbox.

This cache uses [segmented cache](https://github.com/esl/segmented_cache) under the hood, for more details, read the library documentation.

## Options

### `modules.mod_cache_users.strategy`
* **Syntax:** string, one of `fifo` or `lru`
* **Default:** `fifo`
* **Example:** `strategy = "lru"`

Eviction strategy for the cache. FIFO is simply a queue, that ensures records will eventually be evicted and require reloading; LRU ensures queried records keep moving to the front of the queue, possibly keeping them alive forever.

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
  strategy = "lru"
  time_to_live = 60
  number_of_segments = 1
```

[MAM]: mod_mam.md
[Inbox]: mod_inbox.md
