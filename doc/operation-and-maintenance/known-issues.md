This document provides a list of all known issues with MongooseIM operation and configuration.
You may also find proposed workarounds if any is available.

## Missing MUC Light room config fields with RDBMS backend

Before MongooseIM 3.5.x (incl.) new MUC Light rooms could be created with some config fields absent in the RDBMS table.
These options couldn't be re-added later by changing the room config via requests from the clients.

It happened when the default config was a subset of the schema and the client hasn't provided these values when a room was created.

### How to fix this?

You have to iterate over all rooms in the DB (`muc_light_rooms` table) and add missing entries to the `muc_light_config` table.
Every option is inserted as a separate row and is stored as plain text, so it should be straightforward.

Let's say you were using the following config:

```
{config_schema, [
                 "roomname",
                 "subject",
                 "background",
                 "notification_sound"
                ]},
{default_config, [
                  {"roomname", "The room"},
                  {"subject", "Chit-chat"}
                 ]}
```

Your client application has created some rooms without the `background` option by mistake.

For every `id` in the `muc_light_rooms` table, you need to execute:

```
INSERT INTO muc_light_config(room_id, opt, val) VALUES ('put id here', 'background', 'new default value');
```

## MSSQL connectivity via ODBC

The ODBC driver currently used by MongooseIM is known to work only with Ubuntu Xenial x64 and FreeTDS 0.91-6.1build1.

It does not currently work on Ubuntu Bionic, CentOS 7 and macOS Mojave with the latest FreeTDS version.

The team is working on resolving this issue.
Please watch for updates in MongooseIM release notes.

### Proposed workarounds

* Use Ubuntu Xenial x64 for MongooseIM deployment. This OS version is still maintained.

## GDPR retrieval for MAM MUC limitation

When the personal data retrieval is executed for a user in a specific domain, Message Archive Management for groupchats must be running for this particular domain.
This is the case for most configurations but the problem manifests when a MongooseIM operator configures `mod_mam_muc`/`mod_mam_meta` to start only for a subset of domains supported by the cluster (`host_config` option).

In such case, personal data stored by MAM MUC will not be retrieved for this user.

### Proposed workaround

Start a dedicated MongooseIM instance with a slightly different config, which enables Message Archive Management for the user's domain.
This instance doesn't have to be clustered with other nodes and doesn't have to be accessible for actual users.

After a successful retrieval, this instance may be terminated and deleted if necessary.

