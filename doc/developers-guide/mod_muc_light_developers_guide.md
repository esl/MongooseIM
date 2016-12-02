The Developer's Guide to mod_muc_light
================================

This is an in-depth guide on mod_muc_light design decisions and implementation.

Source, header and test suite files
-------------------------------

All source files can be found in `apps/ejabberd/src`.

* `mod_muc_light.erl`
  Main module. It implements `gen_mod` behaviour. It subscribes to some essential hooks and exports several functions, mostly callbacks. It handles integration with `mod_mam` and Disco Service. All operations that take place outside the room (including the room creation) are implemented here. Last but not least - this module prevents `service-unavailable` errors being sent when an offline user receives groupchat message.

* `mod_muc_light_codec.erl`
  A behaviour implemented by modules that translate MUC Light internal data format to stanzas for clients and vice versa. Besides specifying callbacks, it implements generic error encoder function.

* `mod_muc_light_codec_legacy.erl`
  An implementation of XEP-0045 compatibility mode. In fact, some parts of legacy mode are implemented directly in `mod_muc_light.erl` but stanza translation takes place here. It does not utilise full potential of MUC Light extension but allows usage of standard MUC implementation in XMPP client libraries for prototyping or transition phase. Not recommended for production systems (less efficient than modern codec and requires more round-trips).

* `mod_muc_light_codec_modern.erl`
  An implementation of modern MUC protocol, described in the XEP. Supports all MUC Light features.

* `mod_muc_light_db.erl`
  A behaviour implemented by database backends for MUC Light extension.

* `mod_muc_light_db_mnesia.erl`
  Mnesia backend for this extension. Uses transactions for room metadata updates (configuration and affiliation list) and dirty reads whenever possible.

* `mod_muc_light_room.erl`
  This module handles everything that occurs inside the room: access checks, metadata changes, message broadcasting etc.

* `mod_muc_light_utils.erl`
  Utilities shared by other MUC Light modules. It includes room configuration processing or affiliation logic.

Only header file can be found in `apps/ejabberd/include`.

* `mod_muc_light.hrl`
  It contains definitions of MUC Light namespaces, default configuration options and several common data types and records.

There are 2 test suites and one helper module in `test/ejabberd_tests/tests`.

* `muc_light_SUITE.erl`
  Main test suite, checks all most important functionalities of MUC Light extension.

* `muc_light_legacy_SUITE.erl`
  `muc_light_SUITE.erl` equivalent that uses XEP-0045 compatibility mode.

* `muc_helper.erl`
  Provides handy iterators over room participants. Used in MUC Light suites but could be used in `muc_SUITE` as well in the future.

Hooks used by this extension
-----------------------

* `offline_groupchat_message_hook` used by `mod_muc_light:prevent_service_unavailable/3` - Like the name suggests, prevents the default behaviour of sending `service-unavailable` error to the room when groupchat message is sent to an offline occupant.

* `remove_user` used by `mod_muc_light:remove_user/2` - Triggers DB cleanup of all data related to the removed user. Includes a broadcast of a notification about user removal from occupied rooms.

* `disco_local_items` used by `mod_muc_light:get_muc_service/5` - Adds MUC service item to Disco result. Uses MUC Light namespace or classic MUC one when legacy mode is enabled.

* `roster_get` used by `mod_muc_light:add_rooms_to_roster/2` - Injects room items to the user's roster.

* `privacy_iq_get`, `privacy_iq_set` used by `mod_muc_light:process_iq_get/5` and `mod_muc_light:process_iq_set/4` respectively - These callbacks handle blocking settings when legacy mode is enabled.

* `is_muc_room_owner`, `muc_room_pid` used by `mod_muc_light:is_room_owner/3` and `mod_muc_light:muc_room_pid/2` respectively - Callbacks that provide essential data for `mod_mam_muc` extension.

Hooks executed by this extension
-----------------------

* `filter_room_packet` by codecs - Allows `mod_mam_muc` to archive groupchat messages.

* `forget_room` by `mod_muc_light_db_mnesia` and `mod_muc_light_room` - It is a part of `mod_mam_muc` integration as well. A hook used for MAM cleanup upon room destruction.

Advantages and drawbacks (compared to classic MUC)
-----------------------

New MUC implementation has following benefits:

* Fully distributed - Does not have SPOF, concurrent senders do not block each other, especially in large rooms. Message broadcasting is being done in sender c2s context.
* Doesn't use presences - Much less traffic and stable membership information, especially on mobile networks.
* Built-in blocking support - Instead of blocking traffic like Privacy Lists do, it handles blocklists internally, preventing the blocker from being added to or by blocked entities.
* Less round-trips - Room can be created and configured with initial list of occupants with single request.
* Versioning - Reduces traffic and allows clients to detect reliably and early that room state has changed.
* Isolation - Any processing error is contained in sender context, not affecting other room occupants.
* Fully customisable room configuration - Your users can store any meta room information you allow.

Drawbacks are:

* Requires DB transactions to ensure Room state consistency.
* Fetches occupant list from DB for every message that is broadcasted.
* Due to concurrent message broadcast, it is possible for occupants to receive messages in different order (given the messages are broadcasted at the exactly same time).
* With stream resumption disabled or when resumption times out, user may miss message in a following scenario:
  1. Message A archived
  2. Message B archived
  3. Message B arrives to the user
  4. User loses connection
  5. Resumption timeout
  6. User queries MAM for all messages after B and misses A

Design decisions & explanations
-----------------------

### ETS table with options

There is no easy way of getting module configuration when only service domain is available (e.g. `muclight.localhost` registered when booting `localhost` domain). Maybe it can result in common module solving this problem, that other modules could use too, but for now MUC Light handles it internally by maintaining own config tab that stores mappings between actual MUC domain and configuration.

Ideas for Further Development
-----------------------------

### Easy

  * ODBC backend
  * Add more tests for negative cases

### Medium

  * Add optional per-room processes to avoid the need of DB transactions and ensure message ordering (maybe "hard"?).
  * Riak backend
  * Redis backend
  * Extract per service subdomain config tab as separate, generic module (maybe "easy"?).

### Hard

  * Room metadata cache (maybe "medium"?).

