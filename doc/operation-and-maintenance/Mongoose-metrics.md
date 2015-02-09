# MongooseIM metrics

MongooseIM by default collects many metrics showing user behaviour and general system statistics. They are managed by [Feuerlabs/exometer](https://github.com/Feuerlabs/exometer). 
Metrics are organized by XMPP hosts, meaning if MongooseIM servers host `a.com` and `b.com` metrics for specific host only can be obtained.
There are also some global metrics common for every hosts.

## Metrics description

| Metric name | Type | Group | Description |
| ----------- | ---- | ----- | ----------- |
| nodeSessionCount | function | global, XMPP | Number of connected users on given MongooseIM's node|
| totalSessionCount | function | global, XMPP | Total number of connected users to MongooseIM cluster|
| uniqueSessionCount | function | global, XMPP | Number of unique users connected to MongooseIM cluster |
| adhoc_local_commands | spiral | host, system | Hook run when adhoc command was sent to the server's host |
| adhoc_local_items | spiral | host, system | Hook run when adhoc command was sent to a user|
| anonymous_purge_hook | spiral | host, system | Hook run after anonymous user disconnected |
| c2s_stream_features | spiral | host, system | Hook run to collect stream features from optional modules / extensions |
| c2s_unauthenticated_iq | spiral | host, system | Hook run when user sent iq before authentication |
| disco_info | spiral | host, system | |
| disco_local_features | spiral | host, system | |
| disco_local_identity | spiral | host, system | |
| disco_local_items | spiral | host, system | |
| disco_sm_features | spiral | host, system | |
| disco_sm_identity | spiral | host, system | |
| disco_sm_items | spiral | host, system | |
| host_config_update | spiral | host, system | |
| local_send_to_resource_hook | spiral | host, system | |
| mam_lookup_messages | spiral | host, system | |
| mam_muc_purge_multiple_message | spiral | host, system | |
| mam_purge_multiple_message | spiral | host, system | |
| modMamArchiveRemoved | spiral | host, XMPP | |
| modMamArchived | spiral | host, XMPP | |
| modMamDropped | spiral | host, XMPP | |
| modMamDropped2 | spiral | host, XMPP | |
| modMamDroppedIQ | spiral | host, XMPP | |
| modMamFlushed | spiral | host, XMPP | |
| modMamForwarded | spiral | host, XMPP | |
| modMamLookups | spiral | host, XMPP | |
| modMamMultiplePurges | spiral | host, XMPP | |
| modMamPrefsGets | spiral | host, XMPP | |
| modMamPrefsSets | spiral | host, XMPP | |
| modMamSinglePurges | spiral | host, XMPP | |
| modMucMamArchiveRemoved | spiral | host, XMPP | |
| modMucMamArchived | spiral | host, XMPP | |
| modMucMamForwarded | spiral | host, XMPP | |
| modMucMamLookups | spiral | host, XMPP | |
| modMucMamMultiplePurges | spiral | host, XMPP | |
| modMucMamPrefsGets | spiral | host, XMPP | |
| modMucMamPrefsSets | spiral | host, XMPP | |
| modMucMamSinglePurges | spiral | host, XMPP | |
| modPresenceSubscriptions | spiral | host, XMPP | Number of successful presence subscriptions (see also `roster_in_subscription`) |
| modPresenceUnsubscriptions | spiral | host, XMPP | Number of successful presence unsubscriptions (see also `roster_in_subscription`) |
| modPrivacyGets | spiral | host, XMPP | Number of privacy list gets|
| modPrivacyPush | spiral | host, XMPP | Number of privacy list pushes (after update) |
| modPrivacySets | spiral | host, XMPP | Number of privacy list updates|
| modPrivacySetsActive | spiral | host, XMPP | Number of active list sets |
| modPrivacySetsDefault | spiral | host, XMPP | Number of default list sets |
| modPrivacyStanzaAll | spiral | host, XMPP | Number of privacy checks|
| modPrivacyStanzaBlocked | spiral | host, XMPP | Number of stanzas blocked by privacy settings |
| modRegisterCount | spiral | host, XMPP | Number of user registration (via mod_register module) |
| modRosterGets | spiral | host, XMPP | Number of roster gets|
| modRosterPush | spiral | host, XMPP | Number of roster pushes (after update to all user's resources) |
| modRosterSets | spiral | host, XMPP | Number of roster changes |
| modUnregisterCount | spiral | host, XMPP | Number of unregistrations (via mod_register module) |
| offline_message_hook | spiral | host, system | Hook run when a message has been sent to a user without active resources |
| privacy_get_user_list | spiral | host, system | |
| privacy_updated_list | spiral | host, system | |
| resend_offline_messages_hook |  spiral | host, system | Hook run when offline messages have been sent to newly connected users |
| roster_get_jid_info | spiral | host, system | |
| roster_get_subscription_lists |  spiral | host, system | |
| roster_get_versioning_feature |  spiral | host, system | |
| roster_in_subscription | spiral | host, system | Number of all presence subscriptions/unsubscriptions |
| roster_out_subscription | spiral | host, system | Number of all presence subscription/unsubscription requests |
| sessionAuthAnonymous | spiral | host, XMPP | Number of anonymous athentications |
| sessionAuthFails | spiral | host, XMPP | Number of authentication failures |
| sessionCount | counter | host, XMPP |  Number of active sessions|
| sessionLogouts | spiral | host, XMPP | Number of users disconnections |
| sessionSuccessfulLogins | spiral | host, XMPP | Number of successfull authentications |
| sm_broadcast | spiral | host, system | |
| sm_remove_connection_hook | spiral | host, system | |
| unset_presence_hook | spiral | host, system | |
| xmppErrorIq | spiral | host, XMPP | Number of IQ errors |
| xmppErrorMessage | spiral | host, XMPP | Number of message errors |
| xmppErrorPresence | spiral | host, XMPP | Number of presence errors |
| xmppErrorTotal | spiral | host, XMPP | Total number of stanza errors |
| xmppIqReceived | spiral | host, XMPP | Number of IQs received by the server|
| xmppIqSent | spiral | host, XMPP | Number of IQs sent to clients|
| xmppMessageBounced | spiral | host, XMPP | Number of messages bounced (there was no recipient and mod_offline was disabled)|
| xmppMessageReceived | spiral | host, XMPP | Number of messages received by the server |
| xmppMessageSent | spiral | host, XMPP | Number of messages sent to clients |
| xmppPresenceReceived | spiral | host, XMPP | Number of presences received by the server |
| xmppPresenceSent | spiral | host, XMPP | Number of presences sent to clients |
| xmppStanzaCount | spiral | host, XMPP | Number of all stanzas sent to clients |
| xmppStanzaDropped | spiral | host, XMPP | Number of dropped stanzas |
| xmppStanzaReceived | spiral | host, XMPP | Number of stanzas received by the server|
| xmppStanzaSent | spiral | host, XMPP | Numb of stanzas sent to clients|