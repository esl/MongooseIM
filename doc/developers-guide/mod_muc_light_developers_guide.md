The Developer's Guide to mod_muc_light
================================

This is an in-depth guide on `mod_muc_light` design decisions and implementation.

Source, header and test suite files
-------------------------------

All source files can be found in `apps/ejabberd/src`.

* `mod_muc_light.erl`
  Main module. 
  It implements `gen_mod` behaviour. 
  It subscribes to some essential hooks and exports several functions, mostly callbacks. 
  It handles integration with `mod_disco`, `mod_privacy` and `mod_roster`.
  All operations that take place outside the room (including the room creation) are implemented here. 
  Last but not least - this module prevents `service-unavailable` errors being sent when an offline user receives a groupchat message.

* `mod_muc_light_codec.erl`
  A behaviour implemented by modules that translate the MUC Light internal data format to stanzas for clients and vice versa.
  Besides specifying callbacks, it implements generic error encoder function.

* `mod_muc_light_codec_legacy.erl`
  An implementation of XEP-0045 compatibility mode. 
  Note that while, some parts of the legacy mode are implemented directly in `mod_muc_light.erl`, the stanza translation takes place here. 
  It does not utilise the full potential of the MUC Light extension but allows using the standard MUC implementation in XMPP client libraries for prototyping or transition phase. 
  Not recommended for production systems (less efficient than modern codec and requires more round-trips).

* `mod_muc_light_codec_modern.erl`
  An implementation of modern MUC Light protocol, described in the XEP.
  Supports all MUC Light features.

* `mod_muc_light_commands.erl`
  MUC Light-related commands.
  They are registered in `mongoose_commands` module, so they are available via REST API.

* `mod_muc_light_db.erl`
  A behaviour implemented by database backends for the MUC Light extension.

* `mod_muc_light_db_mnesia.erl`
  Mnesia backend for this extension.
  Uses transactions for room metadata updates (configuration and affiliation list) and dirty reads whenever possible.

* `mod_muc_light_db_odbc.erl`
  SQL backend for `mod_muc_light`.
  `create_room`, `destroy_room`, `remove_user`, `set_config`, `modify_aff_users` execute at least one query in single transaction.
  `room_exists`, `get_user_rooms`, `get_user_rooms_count`, `get_config`, `get_blocking`, `set_blocking`, `get_aff_users` execute only one query per function call.
  `get_info` executes 3 `SELECT` queries, not protected by transaction.

* `mod_muc_light_db_odbc_sql.erl`
  SQL queries for `mod_muc_light_db_odbc.erl`.

* `mod_muc_light_room.erl`
  This module handles everything that occurs inside the room: access checks, metadata changes, message broadcasting etc.

* `mod_muc_light_utils.erl`
  Utilities shared by other MUC Light modules. 
  It includes the room configuration processing and the affiliation logic.

The header file can be found in `apps/ejabberd/include`.

* `mod_muc_light.hrl`
  It contains definitions of MUC Light namespaces, default configuration options and several common data types and records.

There are 2 test suites and one helper module in `test/ejabberd_tests/tests`.

* `muc_light_SUITE.erl`
  Main test suite, checks all the most important functionalities of the MUC Light extension.

* `muc_light_legacy_SUITE.erl`
  `muc_light_SUITE.erl` equivalent that uses XEP-0045 compatibility mode.

* `muc_helper.erl`
  Provides handy iterators over room participants. 
  Used in MUC Light suites but in the future could be used in `muc_SUITE` as well.

Hooks handled by this extension
-----------------------

* `offline_groupchat_message_hook` handled by `mod_muc_light:prevent_service_unavailable/3` - Prevents the default behaviour of sending `service-unavailable` error to the room when a groupchat message is sent to an offline occupant.

* `remove_user` handled by `mod_muc_light:remove_user/2` - Triggers DB cleanup of all data related to the removed user. 
 Includes a broadcast of a notification about user removal from occupied rooms.

* `disco_local_items` handled by `mod_muc_light:get_muc_service/5` - Adds MUC service item to the Disco result. 
 Uses either a MUC Light or a classic MUC namespace when the legacy mode is enabled.

* `roster_get` handled by `mod_muc_light:add_rooms_to_roster/2` - Injects room items to the user's roster.

* `privacy_iq_get`, `privacy_iq_set` handled by `mod_muc_light:process_iq_get/5` and `mod_muc_light:process_iq_set/4` respectively - These callbacks handle blocking settings when legacy mode is enabled.

* `is_muc_room_owner`, `muc_room_pid`, `can_access_room`, `can_access_identity` used by `mod_muc_light:is_room_owner/3`, `mod_muc_light:muc_room_pid/2`, `mod_muc_light:can_access_room/3` and `mod_muc_light:can_access_identity/3` respectively - Callbacks that provide essential data for the `mod_mam_muc` extension.

Hooks executed by this extension
-----------------------

* `filter_room_packet` by codecs - Allows `mod_mam_muc` to archive groupchat messages.

* `room_send_packet` by codecs - Handled by `mod_aws_sns`.

* `forget_room` by `mod_muc_light_db_mnesia` and `mod_muc_light_room` - It is a part of `mod_mam_muc` integration as well. 
 A hook used for MAM cleanup upon room destruction.

Advantages and drawbacks (compared to classic MUC)
-----------------------

The new MUC implementation brings quite a few benefits to the table:

* It is fully distributed - Does not have SPOF, concurrent senders do not block each other, especially in large rooms. 
 Message broadcasting is being done in sender c2s context.
* It does not use presences - Much less traffic and stable membership information, especially on mobile networks.
* It provides built-in blocking support - Instead of blocking traffic like Privacy Lists do, it handles blocklists internally, preventing the blocker from being added to or by blocked entities.
* Less round-trips - A room can be created and configured with an initial list of occupants with a single request.
* Versioning - Reduces traffic and allows clients to reliably and quickly detect that the room state has changed.
* Isolation - Processing errors are contained in a sender context, not affecting other room occupants.
* Fully customisable room configuration - Your users can store any meta room information you allow.

Drawbacks are:

* Requires DB transactions to ensure Room state consistency.
* Fetches the occupant list from DB for every message that is broadcasted.
* Due to concurrent message broadcast, it is possible for occupants to receive messages in a different order (given the messages are broadcasted at the exactly same time).
* With stream resumption disabled or when resumption times out, user may miss a message in a following scenario:
  1. Message A archived
  2. Message B archived
  3. Message B delivered to the user
  4. User loses connection
  5. Resumption timeout
  6. User queries MAM for all messages after B and misses A

Ideas for Further Development
-----------------------------

### Easy

  * Add more tests for negative cases

### Medium

  * Add optional per-room processes to avoid the need of DB transactions and ensure message ordering (maybe "hard"?).
  * Riak backend
  * Redis backend

### Hard

  * Room metadata cache (maybe "medium"?).

