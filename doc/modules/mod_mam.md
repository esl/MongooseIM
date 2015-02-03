### Module Description
This module implements revision 0.2 of [XEP-0313 (Message Archive Management)](http://xmpp.org/extensions/attic/xep-0313-0.2.html). It is a highly customizable module, that requires some skill and knowledge to operate properly and efficiently.

### Options
* **mod_mam_odbc_prefs, mod_mam_mnesia_prefs, mod_mam_dirty_prefs**
Consider the process as a kind of recipe. For each step you can enable none ("optional"), one ("single") or more ("multi") modules, according to instructions. Provided there are any, please use the options described in a specific step. All config parameters are boolean, so you can enable them by adding an atom to the configuration list, e.g. `{mod_mam_odbc_arch, [pm, no_writer]}`

##### Step 1 (multi)
* **mod_mam** + **mod_mam_odbc_arch** - Enables support for client-to-client archive.
* **mod_mam_muc** + **mod_mam_muc_odbc_arch** - Enables support for groupchats archive.

If you haven't chosen any of the above, skip the next part.

**Options:**

* **mod_mam_odbc_arch**
    * `pm` (mandatory when `mod_mam` enabled) - Enable archiving user-to-user messages
    * `muc` (optional) - Enable group chat archive, mutually exclusive with `mod_mam_muc_odbc_arch`. **Not recommended**, `mod_mam_muc_odbc_arch` is more efficient.

* **mod_mam_odbc_arch**, **mod_mam_muc_odbc_arch**
    * `no_writer` - Disables default synchronous, slow writer and uses async one (step 5 & 6) instead.

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
* **mod_mam_mnesia_prefs** - User archiving preferences saved in Mnesia. Recommended in most deployments, could be overloaded with lots of users updating their preferences at once.
* **mod_mam_mnesia_dirty_prefs** - User archiving preferences saved in Mnesia and accessed without transactions. There's a small risk of inconsistent (in a rather harmless way) state of preferences table. Provides best performance.

**Options:** (common for all three modules)

* `pm` - Optional, enables MAM preferences for user-to-user messaging, works only with `mod_mam` enabled.
* `muc` - Optional, enables MAM preferences for group chat messaging, works only with `mod_mam_muc` enabled.

##### Step 5 (single, optional, recommended, requires `mod_mam` module enabled and `no_writer` option set in `mod_mam_odbc_arch`)

Enabling asynchronous writers will make debugging more difficult.

* **mod_mam_odbc_async_writer** - Asynchronous writer, will insert batches of messages.
* **mod_mam_odbc_async_pool_writer** - Asynchronous writer, will insert batches of messages, grouped by archive ID.

**Options:** (common for both modules)

* `pm` - Optional, enables the chosen writer for user-to-user messaging, works only with `mod_mam` enabled.
* `muc` - Optional, enables the chosen writer for group chat messaging, use only when `mod_mam_odbc_arch` has `muc` enabled. **Not recommended**.

##### Step 6 (single, optional, recommended, requires `mod_mam_muc` module enabled and `no_writer` option set in `mod_mam_muc_odbc_arch`)

Enabling asynchronous writers will make debugging more difficult.

* **mod_mam_muc_odbc_async_writer** - Asynchronous writer, will insert batches of messages.
* **mod_mam_muc_odbc_async_pool_writer** - Asychronous writer, will insert batches of messages, grouped by archive ID.