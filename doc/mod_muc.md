### Module Description
This module implements [XEP-0045 (Multi-User Chat)](http://xmpp.org/extensions/xep-0045.html).It's a common XMPP group chat solution. This extension consists of two Erlang modules: `mod_muc` and `mod_muc_room`, the latter being the room code itself. Only `mod_muc` needs to be enabled in config file though.

### Options
* **host** (string, default: `"conference.@HOST@"`) - Domain for MUC service to reside under. `@HOST@` is replaced with each served domain.
* **access** (atom, default: `all`) - Access Rule to determine who is allowed to use MUC service.
* **access_create** (atom, default: `all`) - Who is allowed to create rooms.
* **access_admin** (atom, default: `none`) - Who is the administrator in all rooms.
* **access_persistent** (atom, default: `all`) - Who is allowed to make the rooms persistent. In order to change this parameter, the user must not only match the Access Rule but also be the owner of the room.
* **history_size** (non-negative integer, default: 20) - Room message history to be kept in RAM. After node restart, the history is lost.
* **room_shaper** (atom, default: `none`) - Limits per-room data throughput with shaper.
* **max_room_id** (atom or positive integer, default: `infinite`) - Maximum room username length (in JID).
* **max_room_name** (atom or positive integer, default: `infinite`) - Maximum room name length.
* **max_room_desc** (atom or positive integer, default: `infinite`) - Maximum room description length.
* **min_message_interval** (non-negative integer, default: 0) - Minimal interval (in seconds) between messages processed by the room.
* **min_presence_interval** (non-negative integer, default: 0) - Minimal interval (in seconds) between presences processed by the room.
* **max_users** - (positive integer, default: 200) - Absolute maximum user count per room on the node.
* **max_users_admin_threshold** (positive integer, default: 5) - Absolute maximum administrator count per room on the node.
* **user_message_shaper** (atom, default: `none`) - Shaper for user messages processed by a room (global for the room).
* **user_presence_shaper** (atom, default: `none`) - Shaper for user presences processed by a room (global for the room).
* **max_user_conferences** (non-negative, default: 10) - Specifies the number of rooms that a user can occupy simultaneously.
* **default_room_options** (list of key-value tuples, default: `[]`) - List of room configuration options to be overridden in initial state.
  * **title** (binary, default: `<<>>`) - Room title.
  * **description** (binary, default: `<<>>`) - Room description.
  * **allow_change_subj** (boolean, default: `true`) - Allow all occupants to change room subject.
  * **allow_query_users** (boolean, default: `true`) - Allow occupants to send IQ queries to other occupants.
  * **allow_private_messages** (boolean, default: `true`) - Allow private messaging between occupants.
  * **allow_visitor_status** (boolean, default: `true`) - Allow occupants to use text statuses in presences. When disabled, text is removed by the room before broadcasting.
  * **allow_visitor_nickchange** (boolean, default: `true`) - Allow occupants to change nicknames.
  * **public** (boolean, default: `true`) - Room is included in the list available via Service Discovery.
  * **public_list** (boolean, default: `true`) - Member list can be fetched by non-members.
  * **persistent** (boolean, default: `false`) - Room will be stored in DB and survive even when last occupant leaves or node is restarted.
  * **moderated** (boolean, default: `true`) - Only occupants with "voice" can send group messages.
  * **members_by_default** (boolean, default: `true) - All new occupants are members by default (unless they have different affiliation assigned).
  * **members_only** (boolean, default: `false`) - Only users with member affiliation can join the room.
  * **allow_user_invites** (boolean, default: `false`) - Allow ordinary members to send mediated invitations.
  * **password_protected** (boolean, default: `false`) - Room is protected with password.
  * **password** (binary, default: `<<>>`) - Room password is required upon joining. This option has no effect when `password_protected` is `false`.
  * **anonymous** (boolean, default: `true`) - Room is anonymous, meaning occupants can't see each others real JIDs, except for room moderators.
  * **max_users** (positive integer, default: 200) - Maximum user count per room. Admins and room owner are not affected.
  * **logging** (boolean, default: `false`) - Enables logging of room events (messages, presences) to file on disk. Uses `mod_muc_log`.


### Example Configuration
```
{mod_muc, [
             {host, "muc.example.com"},
             {access, muc},
             {access_create, muc_create}
            ]},
```