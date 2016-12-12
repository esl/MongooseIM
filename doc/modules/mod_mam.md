### Module Description
This module implements [XEP-0313 (Message Archive Management)](http://xmpp.org/extensions/attic/xep-0313.html). It enables a service to store all users messages for one-to-one chats as well as group chats (MUC, MultiUser Chat). It uses [XEP-0059: Result Set Management](http://xmpp.org/extensions/xep-0059.html) for paging. It is a highly customizable module, that requires some skill and knowledge to operate properly and efficiently.

Configure MAM with different storage backends:

* ODBC (RDBMS, like MySQL, PostgreSQL, MS SQL Server)
* Riak KV (NOSQL)


`mod_mam_meta` is a meta-module that ensures all relevant `mod_mam_*` modules are loaded and properly configured.

### Options

* **backend** (atom, default: `odbc`) - Database backend to use. `odbc`, `riak` and `cassandra` are supported.
* **add_archived_element** (atom, default: `false`) - Add `<archived/>` element from MAM v0.2.
* **is_archivable_message** (module, default: `mod_mam_utils`) - Name of a module implementing [`is_archivable_message/3` callback](#is_archivable_message) that determines if the message should be archived.
* **host** (string, default: `"conference.@HOST@"`) - MUC host that will be archived if MUC archiving is enabled. **Warning**: if you are using MUC Light, make sure this option is set to MUC Light domain.
* **pm** (list | `false`, default: `[]`) - Override options for archivization of one-to-one messages. If the value of this option is `false`, one-to-one message archive is disabled.
* **muc** (list | `false`, default: `false`) - Override options for archivization of group chat messages. If the value of this option is `false`, group chat message archive is disabled.

All options described in this document can be overriden for specific type of messages through `pm` and `muc` options, e.g.:

```erlang
{mod_mam_meta, [
  {backend, odbc},
  {async_writer, true}, %% this option enables async writer for ODBC backend
  {muc, [
    {async_writer, false} %% disable async writer for MUC archive only
  ]}
]}
```

#### ODBC backend options

These options will only have effect when `odbc` backend is used:

* [**user_prefs_store**](#user_prefs_store)
* **cache_users** (boolean, default: `true`) - Enables Archive ID to integer mappings cache.
* **odbc_message_format** (atom, default: `internal`) - When set to `simple`, stores messages in XML and full JIDs. When set to `internal`, stores messages and JIDs in internal format. **Warning**: Archive MUST be empty to change this option.
* **async_writer** (boolean, default: `true`) - Enables asynchronous writer that is faster than synchronous but harder to debug.

#### Common backend options

* **user_prefs_store** (atom, default: `false`) - Leaving this option as `false` will prevent users from setting their archiving preferences. It will also increase performance. Other possible values are:
  * `odbc` (ODBC backend only) - User archiving preferences saved in ODBC. Slow and not recommended, but might be used to simplify things and keep everything in ODBC.
  * `cassandra` (Cassandra backend only) - User archiving preferences are saved in Cassandra.
  * `mnesia` (recommended) - User archiving preferences saved in Mnesia and accessed without transactions. Recommended in most deployments, could be overloaded with lots of users updating their preferences at once. There's a small risk of inconsistent (in a rather harmless way) state of preferences table.
  * `mnesia_dirty` - like `mnesia`, but dirty synchronous writes are enabled.

#### <a id="is_archivable_message"></a>`is_archivable_message/3` callback

`is_archivable_message` option has to name a module exporting `is_archivable_message/3` function conforming to the spec:

```erlang
-spec is_archivable_message(Mod :: module(), Dir :: incoming | outgoing,
                          Packet :: jlib:xmlel()) -> boolean().
```

Servers SHOULD NOT archive messages that do not have a `<body/>` child tag. Servers SHOULD NOT archive delayed messages.

From MAM v0.3 onwards it is expected that all messages that hold meaningful content, rather than state changes such as Chat State Notifications, would be archived.

### Riak backend

The Riak KV backend for MAM stores messages in weekly buckets so it's easier to remove old buckets.
Archive querying is done using Riak KV 2.0 [search mechanism](http://docs.basho.com/riak/2.1.1/dev/using/search/)
called Yokozuna. Your instance of Riak KV must be configured with Yokozuna enabled.

This backend works with Riak KV 2.0 and above, but we recommend version 2.1.1.

### Example configuration

```erlang
{mod_mam_meta, [
        {backend, odbc},

        cache_users,
        add_archived_element,

        {pm, [{user_prefs_store, odbc}]},
        {muc, [
               {host, "muc.example.com"},
               {odbc_message_format, simple},
               async_writer,
               {user_prefs_store, mnesia}
              ]}
       ]}.
```
