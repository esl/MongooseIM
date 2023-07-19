## Module Description

This module implements an offline messages storage compliant with [XEP-0160: Best Practices for Handling Offline Messages](http://xmpp.org/extensions/xep-0160.html). 
It can store one-to-one and groupchat messages only when the recipient has no online resources. 
It is not well suited for applications supporting multiple user devices, because anything saved in the DB can be retrieved only once, so the message history is not synchronised between devices. 
Although `mod_offline` may be sufficient in some cases, it is preferable to use [mod_mam](mod_mam.md).

If this module is disabled, an error 503 with text "Bounce offline message"
would be sent back to the sender,
each time a message is sent to an offline user. Check [mod_offline_stub](mod_offline_stub.md)
to disable this error message.

## Options

### `modules.mod_offline.access_max_user_messages`
 * **Syntax:** non-empty string
 * **Default:** `"max_user_offline_messages"`
 * **Example:** `access_max_user_messages = "custom_max_user_offline_messages"`
 
 Access Rule to use for limiting the storage size per user.
 
### `modules.mod_offline.backend`
 * **Syntax:** string, one of `mnesia`, `rdbms`
 * **Default:** `"mnesia"`
 * **Example:** `backend = "rdbms"`

 Storage backend.

### `modules.mod_offline.store_groupchat_messages`
 * **Syntax:** boolean
 * **Default:** `false`
 * **Example:** `store_groupchat_messages = true`

 Specifies whether or not we should store groupchat messages.
!!! Warning
    This option can work only with MUC-light and is not expected to work with MUC.

## Example Configuration

```toml
[modules.mod_offline]
  access_max_user_messages = "max_user_offline_messages"
  backend = "rdbms"
  store_groupchat_messages = true
```

## Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/MongooseIM-metrics.md) page.

| Backend action | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `pop_messages` | histogram | Offline messages for a user are retrieved and deleted from a DB. |
| `write_messages` | histogram | New offline messages to a user are written in a DB. |
