### Module Description
This module implements [XEP-0313 (Message Archive Management)](http://xmpp.org/extensions/attic/xep-0313.html). It enables a service to store all users messages for one-to-one chats as well as group chats (MUC, MultiUser Chat). It uses [XEP-0059: Result Set Management](http://xmpp.org/extensions/xep-0059.html) for paging. It is a highly customizable module, that requires some skill and knowledge to operate properly and efficiently.

Configure MAM with different storage backends:

* ODBC (RDBMS, like MySQL, PostgreSQL, MS SQL Server)
* Riak KV (NOSQL)
* Cassandra (NOSQL)

### Configure MAM with ODBC backend

#### Options
* `mod_mam_odbc_prefs`, `mod_mam_mnesia_prefs`:
Consider the process as a kind of recipe. For each step you can enable none ("optional"), one ("single") or more ("multi") modules, according to instructions. Provided there are any, please use the options described in a specific step. All config parameters are boolean, so you can enable them by adding an atom to the configuration list, e.g. `{mod_mam_odbc_arch, [pm, no_writer]}`.

##### Step 1 (multi)
* `*mod_mam` + `mod_mam_odbc_arch`: Enables support for one-to-one messages archive.
* `mod_mam_muc` + `mod_mam_muc_odbc_arch`: Enables support for group chat messages archive.

If you haven't chosen any of the above, skip the next part.

**Options:**

* `mod_mam_muc`:
    * `muc_module` (optional, default: `mod_muc`): MUC module the MAM service will attach to; **Warning:** If you are using MUC Light, make sure this option is set to `mod_muc_light`.
* `mod_mam_odbc_arch`:
    * `pm` (mandatory when `mod_mam` enabled): Enable archiving one-to-one messages
    * `muc` (optional): Enable group chat archive, mutually exclusive with `mod_mam_muc_odbc_arch`. **Not recommended**, `mod_mam_muc_odbc_arch` is more efficient.
    * `simple`: Same as `{simple, true}`
    * `{simple, true}`: Store messages in XML and full JIDs. Archive MUST be empty to change this option.
    * `{simple, false} (default)`: Store messages and JIDs in internal format.
* `mod_mam_odbc_arch`, `mod_mam_muc_odbc_arch`:
    * `no_writer`: Disables default synchronous, slow writer and uses async one (step 5 & 6) instead.
    * `{simple, true}`: Store messages in XML and full JIDs. Archive MUST be empty to change this option.
    * `{simple, false} (default)`: Store messages and JIDs in internal format.

##### Step 2 (mandatory)
* `mod_mam_odbc_user`: Maps archive ID to integer.

**Options**

* `pm`: Mandatory when `mod_mam` enabled.
* `muc`: Mandatory when `mod_mam_muc` enabled.

##### Step 3 (optional, recommended)
* `mod_mam_cache_user`: Enables Archive ID to integer mappings cache.

**Options**

* `pm`: Optional, enables cache for one-to-one messaging, works only with `mod_mam` enabled.
* `muc`: Optional, enables cache for group chat messaging, works only with `mod_mam_muc` enabled.

##### Step 4 (single, optional)
Skipping this step will make `mod_mam` archive all the messages and users will not be able to set their archiving preferences. It will also increase performance.

* `mod_mam_odbc_prefs`: User archiving preferences saved in ODBC. Slow and not recommended, but might be used to simplify things and keep everything in ODBC.
* `mod_mam_mnesia_prefs`: User archiving preferences saved in Mnesia and accessed without transactions. Recommended in most deployments, could be overloaded with lots of users updating their preferences at once. There's a small risk of inconsistent (in a rather harmless way) state of preferences table. Provides best performance.

**Options:** (common for all three modules)

* `pm`: Optional, enables MAM preferences for user-to-user messaging, works only with `mod_mam` enabled.
* `muc`: Optional, enables MAM preferences for group chat messaging, works only with `mod_mam_muc` enabled.

##### Step 5 (single, optional, recommended, requires `mod_mam` module enabled and `no_writer` option set in `mod_mam_odbc_arch`)

Enabling asynchronous writers will make debugging more difficult.

* `mod_mam_odbc_async_pool_writer`: Asynchronous writer, will insert batches of messages, grouped by archive ID.

**Options:** (common for both modules)

* `pm`: Optional, enables the chosen writer for one-to-one messaging, works only with `mod_mam` enabled.
* `muc`: Optional, enables the chosen writer for group chat messaging, use only when `mod_mam_odbc_arch` has `muc` enabled. **Not recommended**.

##### Step 6 (single, optional, recommended, requires `mod_mam_muc` module enabled and `no_writer` option set in `mod_mam_muc_odbc_arch`)

Enabling asynchronous writers will make debugging more difficult.

* `mod_mam_muc_odbc_async_pool_writer`: Asychronous writer, will insert batches of messages, grouped by archive ID.

### Configure MAM with Riak KV backend

In order to use Riak KV as the backend for one-to-one archives, the following configuration must be used:

```erlang
{mod_mam, []}.
{mod_mam_riak_timed_arch_yz, [pm]}.
```

To archive both one-to-one and group chat (MUC, Multi-User Chat) messages use this configuration instead:

```erlang
{mod_mam, []}.
{mod_mam_muc, []}.
{mod_mam_riak_timed_arch_yz, [pm, muc]}.
```

The Riak KV backend for MAM stores messages in weekly buckets so it's easier to remove old buckets.
Archive querying is done using Riak KV 2.0 [search mechanism](http://docs.basho.com/riak/2.1.1/dev/using/search/)
called Yokozuna. Your instance of Riak KV must be configured with Yokozuna enabled.

This backend works with Riak KV 2.0 and above, but we recommend version 2.1.1.

### Configure MAM with Cassandra backend

There are two Cassandra modules:
- `mod_mam_con_ca_arch`: for one-to-one messages
- `mod_mam_muc_ca_arch`: for group chat messages

They can be used together.

#### Module `mod_mam_con_ca_arch`

Module to store conversations (con in the module's name) in Cassandra.

This module uses keyspace "mam" in Cassadra (keyspace is simular to database in relational databases).
Has configuration parameter `servers`. It is a list of Cassandra servers.
If you have 4 Cassandra servers with IP adresses from 10.0.0.1 to 10.0.0.4, then pass:

```erlang
{cassandra_config, [
    {servers, [
      {"10.0.0.1", 9042, 1},
      {"10.0.0.2", 9042, 1},
      {"10.0.0.3", 9042, 1},
      {"10.0.0.4", 9042, 1}
      ]},
   {keyspace, "mam"},
   {credentials, undefined}
]}
```

Each line n `servers`like `{"10.0.0.1", 9042, 1}` means:

* Address is `10.0.0.1`;
* Port is `9042` (default for Cassandra);
* One connection between MongooseIM and Cassandra.

Default value is `[{"localhost", 9042, 1}]` (one connection to localhost).

It is different from `mod_mam_odbc_arch`:

* This module does not use archive integer IDs. It stores JIDs for each message instead
(it means, that for minimal configuration you do not need `mod_mam_odbc_user`);
* It stores not two copies, but one copy of an unique message (between two users);
* User is not allowed to purge (delete) messages.

Configuration example:

```erlang
{mod_mam_con_ca_arch, [
    pm,
    {cassandra_config, [
        {servers, [
          {"10.0.0.1", 9042, 1}
          ]}
    ]}
]},
{mod_mam, []}
```

#### Module `mod_mam_muc_ca_arch`

Module to store group chat history in Cassandra.

It has the same configuration parameter `servers` as module `mod_mam_con_ca_arch`.

It is different from `mod_mam_muc_odbc_arch`:

* User is not allowed to purge. Purging is a feature not described in
  XEP that allows user to delete messages.

Configuration example:

```erlang
{mod_mam_muc_ca_arch, [
    {host, "muc.@HOST"},
    {cassandra_config, [
        {servers, [
          {"10.0.0.1", 9042, 1}
          ]}
    ]}
]},
{mod_mam_odbc_user, [muc]},
{mod_mam_muc, []}
```

#### Example configuration

Configuration example with both modules enabled:

```erlang
{mod_mam_con_ca_arch, [
    pm,
    {cassandra_config, [
        {servers, [ {"10.0.0.1", 9042, 1} ]}
    ]}
]},
{mod_mam_muc_ca_arch, [
    {host, "muc.@HOST"},
    {cassandra_config, [
        {servers, [ {"10.0.0.1", 9042, 1} ]}
    ]}
]},
{mod_mam_odbc_user, [muc]},

{mod_mam, []},
{mod_mam_muc, []}
```

#### Extra `cassandra_config` options

Custom connect timeout in milliseconds:

```erlang
{connect_timeout, 5000} % five seconds
```

Default is infinity `{connect_timeout, infinity}`.

Custom credentials:

```erlang
{credentials, [{"username", "cassandra"}, {"password", "secret"}]}
```

### `mod_mam` options

- `add_archived_element`: add `<archived/>` element from MAM v0.2
- `is_complete_message`: module name implementing is_complete_message/3 callback.
  This callback returns true if message should be archived.

### Example configuration

Default configuration for `mod_mam`:

```erlang
{mod_mam, []}.
```

It's expanded to:

```erlang
{mod_mam, [{add_archived_element, false},
           {is_complete_message, mod_mam_utils}]}
```
