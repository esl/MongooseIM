### Module Description
This module implements offline storage compliant with [XEP-0160 (Best Practices for Handling Offline Messages)](http://xmpp.org/extensions/xep-0160.html). It stores messages (only!) when recipient has no online resources. It is not well suited for applications supporting multiple user devices, because anything saved in DB, can be retrieved only once, so the message history is not synchronised between devices. Although `mod_offline` may be sufficient in some cases, it is preferable to use [mod_mam](mod_mam.md).

### Options
* **access_max_user_messages** (atom, default: `max_user_offline_messages`) - Access Rule to use for limiting storage size per user.
* **backend** (atom, default: `mnesia`) - Storage backend. Currently only `mnesia`, `odbc` and `odbc_legacy` are supported. `odbc` uses a new table in SQL named `offline_message` and `odbc_legacy` uses `spool` table. For new installations, use `odbc`. `odbc_legacy` should be used only for deployments dated before Feb 2014, (most likely they only have `spool`). The main difference between them is that the new one uses `blob` data type and old one uses `text`. As of now no differences in performance between the two have been documented.

### Example Configuration
```
{mod_offline, [{access_max_user_messages, max_user_offline_messages}]},
```