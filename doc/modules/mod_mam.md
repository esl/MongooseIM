# Module Description
This module implements revision 0.2 of [XEP-0313 (Message Archive Management)](http://xmpp.org/extensions/attic/xep-0313-0.2.html). It is a highly customizable module, that requires some skill and knowledge to operate properly and efficiently.

## Configuring MAM with ODBC backend

### Options
* **mod_mam_odbc_prefs, mod_mam_mnesia_prefs**
Consider the process as a kind of recipe. For each step you can enable none ("optional"), one ("single") or more ("multi") modules, according to instructions. Provided there are any, please use the options described in a specific step. All config parameters are boolean, so you can enable them by adding an atom to the configuration list, e.g. `{mod_mam_odbc_arch, [pm, no_writer]}`

##### Step 1 (multi)
* **mod_mam** + **mod_mam_odbc_arch** - Enables support for client-to-client archive.
* **mod_mam_muc** + **mod_mam_muc_odbc_arch** - Enables support for groupchats archive.

If you haven't chosen any of the above, skip the next part.

**Options:**

* **mod_mam_muc**
    * `host` (optional, default: `"conference.@HOST@"`) - MUC host that will be archived
* **mod_mam_odbc_arch**
    * `pm` (mandatory when `mod_mam` enabled) - Enable archiving user-to-user messages
    * `muc` (optional) - Enable group chat archive, mutually exclusive with `mod_mam_muc_odbc_arch`. **Not recommended**, `mod_mam_muc_odbc_arch` is more efficient.
    * `simple` - Same as `{simple, true}`
    * `{simple, true}` - Store messages in XML and full JIDs. Archive MUST be empty to change this option.
    * `{simple, false} (default)` - Store messages and JIDs in internal format

* **mod_mam_odbc_arch**, **mod_mam_muc_odbc_arch**
    * `no_writer` - Disables default synchronous, slow writer and uses async one (step 5 & 6) instead.
    * `{simple, true}` - Store messages in XML and full JIDs. Archive MUST be empty to change this option.
    * `{simple, false} (default)` - Store messages and JIDs in internal format

##### Step 2 (mandatory)
* **mod_mam_odbc_user** - Maps archive ID to integer.

**Options**

* `pm` - Mandatory when `mod_mam` enabled.
* `muc` - Mandatory when `mod_mam_muc` enabled.

##### Step 3 (optional, recommended)
* **mod_mam_cache_user** - Enables Archive ID -> integer mappings cache.

**Options**

* `pm` - Optional, enables cache for user-to-user messaging, works only with `mod_mam` enabled.
* `muc` - Optional, enables cache for group chat messaging, works only with `mod_mam_muc` enabled.

##### Step 4 (single, optional)
Skipping this step will make mod_mam archive all the messages and users will not be able to set their archiving preferences. It will also increase performance.

* **mod_mam_odbc_prefs** - User archiving preferences saved in ODBC. Slow and not recommended, but might be used to simplify things and keep everything in ODBC.
* **mod_mam_mnesia_prefs** - User archiving preferences saved in Mnesia and accessed without transactions. Recommended in most deployments, could be overloaded with lots of users updating their preferences at once. There's a small risk of inconsistent (in a rather harmless way) state of preferences table. Provides best performance.
* **mod_mam_cassandra_prefs** - User archiving preferences saved in Cassandra. Slow and not recommended, but might be used to simplify things and keep everything in Cassandra.

**Options:** (common for all three modules)

* `pm` - Optional, enables MAM preferences for user-to-user messaging, works only with `mod_mam` enabled.
* `muc` - Optional, enables MAM preferences for group chat messaging, works only with `mod_mam_muc` enabled.

##### Step 5 (single, optional, recommended, requires `mod_mam` module enabled and `no_writer` option set in `mod_mam_odbc_arch`)

Enabling asynchronous writers will make debugging more difficult.

* **mod_mam_odbc_async_pool_writer** - Asynchronous writer, will insert batches of messages, grouped by archive ID.

**Options:** (common for both modules)

* `pm` - Optional, enables the chosen writer for user-to-user messaging, works only with `mod_mam` enabled.
* `muc` - Optional, enables the chosen writer for group chat messaging, use only when `mod_mam_odbc_arch` has `muc` enabled. **Not recommended**.

##### Step 6 (single, optional, recommended, requires `mod_mam_muc` module enabled and `no_writer` option set in `mod_mam_muc_odbc_arch`)

Enabling asynchronous writers will make debugging more difficult.

* **mod_mam_muc_odbc_async_pool_writer** - Asychronous writer, will insert batches of messages, grouped by archive ID.

## Configuring MAM with Riak backend

In order to use Riak as the backend for one-to-one archives, the following configuration must be used:

```erlang
{mod_mam, []}.
{mod_mam_riak_timed_arch_yz, [pm]}.
```

To archive both one-to-one and multichat messages use this configuration instead:

```erlang
{mod_mam, []}.
{mod_mam_muc, []}.
{mod_mam_riak_timed_arch_yz, [pm, muc]}.
```

The Riak backend for MAM stores messages in weekly buckets so it's easier to remove old buckets.
Archive querying is done using Riak 2.0 [search mechanism](http://docs.basho.com/riak/2.1.1/dev/using/search/)
called Yokozuna. Your instance of Riak must be configured with Yokozuna enabled.

This backend works with Riak 2.0 and above, but the recommend version is 2.1.1



## Configure MAM with Cassandra backend

### Configure cassandra pool

Edit main config section adding:

```erlang
{cassandra_servers, [{default, []}]}.
```

MongooseIM will create one pool with one worker to connect to localhost:9042.

You can change default settings using extra parameters:
- 5 connections to each server with addresses from 10.0.0.1 to 10.0.0.4;
- Keyspace "mam";
- Custom connect timeout in milliseconds;
- Custom credentials.

```erlang
{cassandra_servers,
 [
  {default,
   [
    {servers,
     [
      {"10.0.0.1", 9042, 5},
      {"10.0.0.2", 9042, 5},
      {"10.0.0.3", 9042, 5},
      {"10.0.0.4", 9042, 5}
     ]
    },
    {keyspace, "mam"},
    {connect_timeout, 5000}, % five seconds
    {credentials, [{"username", "cassandra"}, {"password", "secret"}]}
   ]
  }
 ]
}.
```

### Configure cassandra modules

There are two Cassandra modules:
- mod_mam_cassandra_arch - for one-to-one messages
- mod_mam_muc_cassandra_arch - for groupchat messages

They can be used together.

```erlang
{mod_mam_cassandra_arch, []},
{mod_mam, []}
```

```erlang
{mod_mam_muc_cassandra_arch, []},
{mod_mam_muc, []}
```

Options:

`pool_name` - Poolname from `cassandra_servers` to use. Default name is `default`.
It can be different for different cassandra modules.


mod_mam options
---------------

- add_archived_element - add `<archived/>` element from MAM v0.2
- is_complete_message - module name implementing is_complete_message/3 callback.
  This callback returns true if message should be archived.


Default configuration for mod_mam:

```erlang
{mod_mam, []}.
```

It's expanded to:

```erlang
{mod_mam, [{add_archived_element, false},
           {is_complete_message, mod_mam_utils}]}
```
