%%% @doc Hooks wrapper providing clear specifications for a hook caller.
%%%
%%% Every hook has its own function in this module with specs as accurate as
%%% possible. This helps to have a static analysis of the hooks callers to
%%% make sure they pass the expected arguments.
-module(mongoose_hooks).

-export([adhoc_local_items/5,
         adhoc_sm_items/5,
         anonymous_purge_hook/3,
         auth_failed/2,
         ejabberd_ctl_process/2,
         failed_to_store_message/4,
         filter_local_packet/2,
         filter_packet/1,
         host_config_update/4,
         inbox_unread_count/3,
         local_send_to_resource_hook/5,
         get_key/3,
         packet_to_component/3,
         presence_probe_hook/5,
         push_notifications/4,
         register_command/2,
         register_subhost/3,
         register_user/3,
         remove_user/3,
         resend_offline_messages_hook/3,
         rest_user_send_packet/5,
         session_cleanup/5,
         set_vcard/4,
         unacknowledged_message/3,
         unregister_command/2,
         unregister_subhost/2,
         user_available_hook/3,
         user_ping_response/5,
         user_ping_timeout/3,
         user_receive_packet/6,
         user_sent_keep_alive/2,
         user_send_packet/5,
         vcard_set/4,
         xmpp_send_element/3,
         xmpp_stanza_dropped/5]).

-export([c2s_broadcast_recipients/6,
         c2s_filter_packet/6,
         c2s_preprocessing_hook/3,
         c2s_presence_in/5,
         c2s_stream_features/2,
         c2s_unauthenticated_iq/4,
         c2s_update_presence/2,
         check_bl_c2s/2,
         forbidden_session_hook/3,
         session_opening_allowed_for_user/3]).

-export([privacy_check_packet/6,
         privacy_get_user_list/3,
         privacy_iq_get/6,
         privacy_iq_set/5,
         privacy_updated_list/4]).

-export([offline_groupchat_message_hook/5,
         offline_message_hook/5,
         set_presence_hook/5,
         sm_broadcast/6,
         sm_filter_offline_message/5,
         sm_register_connection_hook/4,
         sm_remove_connection_hook/6,
         unset_presence_hook/5,
         xmpp_bounce_message/2]).

-export([roster_get/4,
         roster_get_jid_info/4,
         roster_get_subscription_lists/3,
         roster_get_versioning_feature/2,
         roster_groups/2,
         roster_in_subscription/7,
         roster_out_subscription/5,
         roster_process_item/2,
         roster_push/4,
         roster_set/5]).

-export([is_muc_room_owner/4,
         can_access_identity/4,
         can_access_room/4,
         muc_room_pid/3]).

-export([mam_archive_id/3,
         mam_archive_size/4,
         mam_get_behaviour/5,
         mam_set_prefs/7,
         mam_get_prefs/4,
         mam_remove_archive/4,
         mam_lookup_messages/3,
         mam_archive_message/10]).

-export([mam_muc_archive_id/3,
         mam_muc_archive_size/4,
         mam_muc_get_behaviour/5,
         mam_muc_set_prefs/7,
         mam_muc_get_prefs/4,
         mam_muc_remove_archive/4,
         mam_muc_lookup_messages/3,
         mam_muc_archive_message/10,
         mam_muc_flush_messages/3]).

-export([get_mam_pm_gdpr_data/3,
         get_mam_muc_gdpr_data/3,
         get_personal_data/3]).

-export([find_s2s_bridge/3,
         s2s_allow_host/3,
         s2s_connect_hook/3,
         s2s_receive_packet/2,
         s2s_stream_features/2,
         s2s_send_packet/5]).

-export([disco_info/5,
         disco_local_features/6,
         disco_local_identity/6,
         disco_local_items/6,
         disco_sm_features/6,
         disco_sm_identity/6,
         disco_sm_items/6]).

-export([amp_check_condition/4,
         amp_check_packet/4,
         amp_determine_strategy/6,
         amp_error_action_triggered/2,
         amp_notify_action_triggered/2,
         amp_verify_support/3]).

-export([filter_room_packet/3,
         forget_room/4,
         invitation_sent/7,
         join_room/6,
         leave_room/6,
         room_packet/6,
         room_send_packet/3,
         update_inbox_for_muc/2]).

-export([caps_add/6,
         caps_recognised/5,
         caps_update/6]).

-export([pubsub_create_node/6,
         pubsub_delete_node/5,
         pubsub_publish_item/7]).

-export([mod_global_distrib_known_recipient/5,
         mod_global_distrib_unknown_recipient/2]).

-export([c2s_remote_hook/5]).

-spec c2s_remote_hook(LServer, Tag, Args, HandlerState, C2SState) -> Result when
    LServer :: jid:lserver(),
    Tag :: atom(),
    Args :: term(),
    HandlerState :: term(),
    C2SState :: ejabberd_c2s:state(),
    Result :: term(). % ok | empty_state | HandlerState
c2s_remote_hook(LServer, Tag, Args, HandlerState, C2SState) ->
    ejabberd_hooks:run_fold(c2s_remote_hook, LServer, HandlerState, [Tag, Args, C2SState]).

-spec adhoc_local_items(LServer, Acc, From, To, Lang) -> Result when
    LServer :: jid:lserver(),
    Acc :: {result, [exml:element()]},
    From :: jid:jid(),
    To :: jid:jid(),
    Lang :: ejabberd:lang(),
    Result :: {result, [exml:element()]}.
adhoc_local_items(LServer, Acc, From, To, Lang) ->
    ejabberd_hooks:run_fold(adhoc_local_items, LServer, Acc, [From, To, Lang]).

-spec adhoc_sm_items(LServer, Acc, From, To, Lang) -> Result when
    LServer :: jid:lserver(),
    Acc :: {result, [exml:element()]},
    From :: jid:jid(),
    To :: jid:jid(),
    Lang :: ejabberd:lang(),
    Result :: {result, [exml:element()]}.
adhoc_sm_items(LServer, Acc, From, To, Lang) ->
    ejabberd_hooks:run_fold(adhoc_sm_items, LServer, Acc, [From, To, Lang]).

%%% @doc The `anonymous_purge_hook' hook is called when anonymous user's data is removed.
-spec anonymous_purge_hook(LServer, Acc, LUser) -> Result when
    LServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    LUser :: jid:user(),
    Result :: mongose_acc:t().
anonymous_purge_hook(LServer, Acc, LUser) ->
    ejabberd_hooks:run_fold(anonymous_purge_hook, LServer, Acc, [LUser, LServer]).

-spec auth_failed(Server, Username) -> Result when
    Server :: jid:server(),
    Username :: jid:user() | unknown,
    Result :: ok.
auth_failed(Server, Username) ->
    ejabberd_hooks:run_fold(auth_failed, Server, ok, [Username, Server]).

-spec ejabberd_ctl_process(Acc, Args) -> Result when
    Acc :: any(),
    Args :: [string()],
    Result :: any().
ejabberd_ctl_process(Acc, Args) ->
    ejabberd_hooks:run_fold(ejabberd_ctl_process, Acc, [Args]).

-spec failed_to_store_message(LServer, Acc, From, Packet) -> Result when
    LServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    Packet :: exml:element(),
    Result :: mongoose_acc:t().
failed_to_store_message(LServer, Acc, From, Packet) ->
    ejabberd_hooks:run_fold(failed_to_store_message,
                            LServer,
                            Acc,
                            [From, Packet]).

%%% @doc The `filter_local_packet' hook is called to filter out stanzas routed with `mongoose_local_delivery'.
-spec filter_local_packet(Server, {From, To, Acc, Packet}) -> Result when
    Server :: jid:server(),
    From :: jid:jid(),
    To :: jid:jid(),
    Acc :: mongoose_acc:t(),
    Packet :: exml:element(),
    Result :: {F :: jid:jid(), T :: jid:jid(), A :: mongoose_acc:t(), P :: exml:element()} | drop.
filter_local_packet(Server, {From, To, Acc, Packet}) ->
    ejabberd_hooks:run_fold(filter_local_packet, Server, {From, To, Acc, Packet}, []).

%%% @doc The `filter_packet' hook is called to filter out stanzas routed with `mongoose_router_global'.
-spec filter_packet({From, To, Acc, Packet}) -> Result when
    From :: jid:jid(),
    To :: jid:jid(),
    Acc :: mongoose_acc:t(),
    Packet :: exml:element(),
    Result :: {F :: jid:jid(), T :: jid:jid(), A :: mongoose_acc:t(), P :: exml:element()} | drop.
filter_packet({From, To, Acc, Packet}) ->
    ejabberd_hooks:run_fold(filter_packet, {From, To, Acc, Packet}, []).

-spec host_config_update(Server, Acc, Key, Config) -> Result when
    Server :: jid:server(),
    Acc :: ok,
    Key :: atom(),
    Config :: list(),
    Result :: ok.
host_config_update(Server, Acc, Key, Config) ->
    ejabberd_hooks:run_fold(host_config_update, Server, Acc, [Server, Key, Config]).

%%% @doc The `inbox_unread_count' hook is called to get the number of unread messages in the inbox for a user.
-spec inbox_unread_count(LServer, Acc, User) -> Result when
    LServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    User :: jid:jid(),
    Result :: mongoose_acc:t().
inbox_unread_count(LServer, Acc, User) ->
    ejabberd_hooks:run_fold(inbox_unread_count, LServer, Acc, [User]).

-spec local_send_to_resource_hook(LServer, Acc, From, To, Packet) -> Result when
    LServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: mongoose_acc:t().
local_send_to_resource_hook(Server, Acc, From, To, Packet) ->
    ejabberd_hooks:run_fold(local_send_to_resource_hook,
                            Server,
                            Acc,
                            [From, To, Packet]).

%%% @doc The `get_key' hook is called to extract a key from `mod_keystore'.
-spec get_key(LServer, Acc, KeyName) -> Result when
    LServer :: jid:lserver(),
    Acc :: mod_keystore:key_list(),
    KeyName :: atom(),
    Result :: mod_keystore:key_list().
get_key(LServer, Acc, KeyName) ->
    ejabberd_hooks:run_fold(get_key, LServer, Acc,
                            [{KeyName, LServer}]).

-spec packet_to_component(Acc, From, To) -> Result when
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Result :: mongoose_acc:t().
packet_to_component(Acc, From, To) ->
    ejabberd_hooks:run_fold(packet_to_component, Acc, [From, To]).

-spec presence_probe_hook(Server, Acc, From, To, Pid) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Pid :: pid(),
    Result :: mongoose_acc:t().
presence_probe_hook(Server, Acc, From, To, Pid) ->
    ejabberd_hooks:run_fold(presence_probe_hook,
                            Server,
                            Acc,
                            [From, To, Pid]).

%%% @doc The `push_notifications' hook is called to push notifications.
-spec push_notifications(Server, Acc, NotificationForms, Options) -> Result when
    Server :: jid:server(),
    Acc :: ok | mongoose_acc:t(),
    NotificationForms :: [#{atom() => binary()}],
    Options :: #{atom() => binary()},
    Result :: ok | {error, any()}.
push_notifications(Server, Acc, NotificationForms, Options) ->
    ejabberd_hooks:run_fold(push_notifications,
                            Server,
                            Acc,
                            [Server, NotificationForms, Options]).

%%% @doc The `register_command' hook is called when a command is registered in `mongoose_commands'.
-spec register_command(Server, Command) -> Result when
    Server :: jid:server() | global,
    Command :: mongoose_commands:t(),
    Result :: drop.
register_command(Server, Command) ->
    ejabberd_hooks:run_fold(register_command, Server, Command, []).

%%% @doc The `register_subhost' hook is called when a component is registered for ejabberd_router.
-spec register_subhost(Acc, LDomain, IsHidden) -> Result when
    Acc :: any(),
    LDomain :: binary(),
    IsHidden :: boolean(),
    Result :: any().
register_subhost(Acc, LDomain, IsHidden) ->
    ejabberd_hooks:run_fold(register_subhost, Acc, [LDomain, IsHidden]).

%%% @doc The `register_user' hook is called when a user is successfully registered in an authentication backend.
-spec register_user(LServer, Acc, LUser) -> Result when
    LServer :: jid:lserver(),
    Acc :: any(),
    LUser :: jid:luser(),
    Result :: any().
register_user(LServer, Acc, LUser) ->
    ejabberd_hooks:run_fold(register_user, LServer, Acc, [LUser, LServer]).

%%% @doc The `remove_user' hook is called when a user is removed.
-spec remove_user(LServer, Acc, LUser) -> Result when
    LServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    LUser :: jid:luser(),
    Result :: mongoose_acc:t().
remove_user(LServer, Acc, LUser) ->
    ejabberd_hooks:run_fold(remove_user, LServer, Acc, [LUser, LServer]).

-spec resend_offline_messages_hook(Server, Acc, User) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    User :: jid:user(),
    Result :: mongoose_acc:t().
resend_offline_messages_hook(Server, Acc, User) ->
    ejabberd_hooks:run_fold(resend_offline_messages_hook,
                            Server,
                            Acc,
                            [User, Server]).

%%% @doc The `rest_user_send_packet' hook is called when a user sends a message using the REST API.
-spec rest_user_send_packet(LServer, Acc, From, To, Packet) -> Result when
    LServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: mongoose_acc:t().
rest_user_send_packet(LServer, Acc, From, To, Packet) ->
    ejabberd_hooks:run_fold(rest_user_send_packet, LServer, Acc,
                            [From, To, Packet]).

%%% @doc The `session_cleanup' hook is called when sm backend cleans up a user's session.
-spec session_cleanup(Server, Acc, User, Resource, SID) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    User :: jid:user(),
    Resource :: jid:resource(),
    SID :: ejabberd_sm:sid(),
    Result :: mongoose_acc:t().
session_cleanup(Server, Acc, User, Resource, SID) ->
    ejabberd_hooks:run_fold(session_cleanup,
                            Server, Acc,
                            [User, Server, Resource, SID]).

%%% @doc The `set_vcard' hook is called when the caller wants to set the VCard.
-spec set_vcard(LServer, Acc, User, VCard) -> Result when
    LServer :: jid:lserver(),
    Acc :: {error, no_handler_defined},
    User :: jid:jid(),
    VCard :: exml:element(),
    Result :: ok | {error, any()}.
set_vcard(LServer, Acc, User, VCard) ->
    ejabberd_hooks:run_fold(set_vcard, LServer, Acc, [User, VCard]).

-spec unacknowledged_message(Server, Acc, JID) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Result :: mongoose_acc:t().
unacknowledged_message(Server, Acc, JID) ->
    ejabberd_hooks:run_fold(unacknowledged_message, Server, Acc, [JID]).

%%% @doc The `unregister_command' hook is called when a command is unregistered from `mongoose_commands'.
-spec unregister_command(Server, Command) -> Result when
    Server :: jid:server() | global,
    Command :: mongoose_commands:t(),
    Result :: drop.
unregister_command(Server, Command) ->
    ejabberd_hooks:run_fold(unregister_command, Server, Command, []).

%%% @doc The `unregister_subhost' hook is called when a component is unregistered from ejabberd_router.
-spec unregister_subhost(Acc, LDomain) -> Result when
    Acc :: any(),
    LDomain :: binary(),
    Result :: any().
unregister_subhost(Acc, LDomain) ->
    ejabberd_hooks:run_fold(unregister_subhost, Acc, [LDomain]).

-spec user_available_hook(Server, Acc, JID) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Result :: mongoose_acc:t().
user_available_hook(Server, Acc, JID) ->
    ejabberd_hooks:run_fold(user_available_hook,
                            Server,
                            Acc,
                            [JID]).

%%% @doc The `user_ping_response' hook is called when a user responds to a ping.
-spec user_ping_response(Server, Acc, JID, Response, TDelta) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Response :: timeout | jlib:iq(),
    TDelta :: non_neg_integer(),
    Result :: mongoose_acc:t().
user_ping_response(Server, Acc, JID, Response, TDelta) ->
    ejabberd_hooks:run_fold(user_ping_response, Server,
                            Acc, [JID, Response, TDelta]).

%%% @doc The `user_ping_timeout' hook is called when there is a timeout when waiting for a ping response from a user.
-spec user_ping_timeout(Server, Acc, JID) -> Result when
    Server :: jid:server(),
    Acc :: any(),
    JID :: jid:jid(),
    Result :: any().
user_ping_timeout(Server, Acc, JID) ->
    ejabberd_hooks:run_fold(user_ping_timeout,
                            Server,
                            Acc,
                            [JID]).

-spec user_receive_packet(Server, Acc, JID, From, To, El) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    From :: jid:jid(),
    To :: jid:jid(),
    El :: exml:element(),
    Result :: mongoose_acc:t().
user_receive_packet(Server, Acc, JID, From, To, El) ->
    ejabberd_hooks:run_fold(user_receive_packet,
                            Server,
                            Acc,
                            [JID, From, To, El]).

-spec user_sent_keep_alive(Server, JID) -> Result when
    Server :: jid:server(),
    JID :: jid:jid(),
    Result :: any().
user_sent_keep_alive(Server, JID) ->
    ejabberd_hooks:run_fold(user_sent_keep_alive, Server, ok, [JID]).

%%% @doc A hook called when a user sends an XMPP stanza.
%%% The hook's handler is expected to accept four parameters: `Acc', `From', `To' and `Packet'
%%% The arguments and the return value types correspond to the following spec.
-spec user_send_packet(LServer, Acc, From, To, Packet) -> Result when
    LServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: mongoose_acc:t().
user_send_packet(LServer, Acc, From, To, Packet) ->
    ejabberd_hooks:run_fold(user_send_packet,
                            LServer,
                            Acc,
                            [From, To, Packet]).

%%% @doc The `vcard_set' hook is called to inform that the vcard has been set in mod_vcard backend.
-spec vcard_set(Server, Acc, LUser, VCard) -> Result when
    Server :: jid:server(),
    Acc :: any(),
    LUser :: jid:luser(),
    VCard :: exml:element(),
    Result :: any().
vcard_set(Server, Acc, LUser, VCard) ->
    ejabberd_hooks:run_fold(vcard_set, Server, Acc, [LUser, Server, VCard]).

-spec xmpp_send_element(Server, Acc, El) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    El :: exml:element(),
    Result :: mongoose_acc:t().
xmpp_send_element(Server, Acc, El) ->
    ejabberd_hooks:run_fold(xmpp_send_element, Server, Acc, [El]).

%%% @doc The `xmpp_stanza_dropped' hook is called to inform that an xmpp stanza has been dropped.
-spec xmpp_stanza_dropped(Server, Acc, From, To, Packet) -> Result when
    Server :: jid:lserver(),
    Acc :: any(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: any().
xmpp_stanza_dropped(Server, Acc, From, To, Packet) ->
    ejabberd_hooks:run_fold(xmpp_stanza_dropped,
                            Server,
                            Acc,
                            [From, To, Packet]).

%% C2S related hooks

-spec c2s_broadcast_recipients(Server, Acc, State, Type, From, Packet) -> Result when
    Server :: jid:server(),
    Acc :: list(),
    State :: ejabberd_c2s:state(),
    Type :: {atom(), any()},
    From :: jid:jid(),
    Packet :: exml:element(),
    Result :: list().
c2s_broadcast_recipients(Server, Acc, State, Type, From, Packet) ->
    ejabberd_hooks:run_fold(c2s_broadcast_recipients,
                            Server,
                            Acc,
                            [Server, State, Type, From, Packet]).

-spec c2s_filter_packet(Server, Drop, State, Feature, To, Packet) -> Result when
    Server :: jid:server(),
    Drop :: boolean(),
    State :: ejabberd_c2s:state(),
    Feature :: {atom(), binary()},
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: boolean().
c2s_filter_packet(Server, Drop, State, Feature, To, Packet) ->
    ejabberd_hooks:run_fold(c2s_filter_packet,
                            Server,
                            Drop,
                            [Server, State, Feature, To, Packet]).

-spec c2s_preprocessing_hook(Server, Acc, State) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    State :: ejabberd_c2s:state(),
    Result :: mongoose_acc:t().
c2s_preprocessing_hook(Server, Acc, State) ->
    ejabberd_hooks:run_fold(c2s_preprocessing_hook, Server, Acc, [State]).

-spec c2s_presence_in(Server, State, From, To, Packet) -> Result when
    Server :: jid:server(),
    State :: ejabberd_c2s:state(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: ejabberd_c2s:state().
c2s_presence_in(Server, State, From, To, Packet) ->
    ejabberd_hooks:run_fold(c2s_presence_in, Server, State, [{From, To, Packet}]).

-spec c2s_stream_features(Server, Acc) -> Result when
    Server :: jid:server(),
    Acc :: [exml:element()],
    Result :: [exml:element()].
c2s_stream_features(Server, Acc) ->
    ejabberd_hooks:run_fold(c2s_stream_features, Server, Acc, [Server]).

-spec c2s_unauthenticated_iq(Server, Acc, IQ, IP) -> Result when
    Server :: jid:server(),
    Acc :: empty,
    IQ :: jlib:iq(),
    IP :: {inet:ip_address(), inet:port_number()} | undefined,
    Result :: exml:element() | empty.
c2s_unauthenticated_iq(Server, Acc, IQ, IP) ->
    ejabberd_hooks:run_fold(c2s_unauthenticated_iq,
                            Server,
                            Acc,
                            [Server, IQ, IP]).

-spec c2s_update_presence(LServer, Acc) -> Result when
    LServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    Result :: mongoose_acc:t().
c2s_update_presence(LServer, Acc) ->
    ejabberd_hooks:run_fold(c2s_update_presence, LServer, Acc, []).

-spec check_bl_c2s(Blacklisted, IP) -> Result when
    Blacklisted :: boolean(),
    IP ::  inet:ip_address(),
    Result :: boolean().
check_bl_c2s(Blacklisted, IP) ->
    ejabberd_hooks:run_fold(check_bl_c2s, Blacklisted, [IP]).

-spec forbidden_session_hook(Server, Acc, JID) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Result :: mongoose_acc:t().
forbidden_session_hook(Server, Acc, JID) ->
    ejabberd_hooks:run_fold(forbidden_session_hook, Server, Acc, [JID]).

-spec session_opening_allowed_for_user(Server, Allow, JID) -> Result when
    Server :: jid:server(),
    Allow :: any(),
    JID :: jid:jid(),
    Result :: any().
session_opening_allowed_for_user(Server, Allow, JID) ->
    ejabberd_hooks:run_fold(session_opening_allowed_for_user,
                            Server,
                            Allow, [JID]).

%% Privacy related hooks

-spec privacy_check_packet(LServer, Acc, User, PrivacyList, FromToNameType, Dir) -> Result when
    LServer :: jid:lserver(), Acc :: mongoose_acc:t(), User :: jid:luser(),
    PrivacyList :: mongoose_privacy:userlist(),
    FromToNameType :: {jid:jid(), jid:jid(), binary(), binary()},
    Dir :: in | out,
    Result :: mongoose_acc:t().
privacy_check_packet(LServer, Acc, User, PrivacyList, FromToNameType, Dir) ->
    ejabberd_hooks:run_fold(privacy_check_packet,
                            LServer,
                            mongoose_acc:set(hook, result, allow, Acc),
                            [User,
                             LServer,
                             PrivacyList,
                             FromToNameType,
                             Dir]).

-spec privacy_get_user_list(Server, UserList, User) -> Result when
    Server :: jid:server(),
    UserList :: mongoose_privacy:userlist(),
    User :: jid:user(),
    Result :: mongoose_privacy:userlist().
privacy_get_user_list(Server, UserList, User) ->
    ejabberd_hooks:run_fold(privacy_get_user_list, Server,
                            UserList, [User, Server]).

-spec privacy_iq_get(Server, Acc, From, To, IQ, PrivList) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    IQ :: jlib:iq(),
    PrivList :: mongoose_privacy:userlist(),
    Result :: mongoose_acc:t().
privacy_iq_get(Server, Acc, From, To, IQ, PrivList) ->
    ejabberd_hooks:run_fold(privacy_iq_get,
                            Server,
                            Acc,
                            [From, To, IQ, PrivList]).

-spec privacy_iq_set(Server, Acc, From, To, IQ) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    IQ :: jlib:iq(),
    Result :: mongoose_acc:t().
privacy_iq_set(Server, Acc, From, To, IQ) ->
    ejabberd_hooks:run_fold(privacy_iq_set,
                            Server,
                            Acc,
                            [From, To, IQ]).

-spec privacy_updated_list(Server, Acc, OldList, NewList) -> Result when
    Server :: jid:server(),
    Acc :: false,
    OldList :: mongoose_privacy:userlist(),
    NewList :: mongoose_privacy:userlist(),
    Result :: false | mongoose_privacy:userlist().
privacy_updated_list(Server, Acc, OldList, NewList) ->
    ejabberd_hooks:run_fold(privacy_updated_list, Server,
                            Acc, [OldList, NewList]).

%% Session management related hooks

-spec offline_groupchat_message_hook(LServer, Acc, From, To, Packet) -> Result when
    LServer :: jid:lserver(),
    Acc :: map(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: map().
offline_groupchat_message_hook(LServer, Acc, From, To, Packet) ->
    ejabberd_hooks:run_fold(offline_groupchat_message_hook,
                            LServer,
                            Acc,
                            [From, To, Packet]).

-spec offline_message_hook(LServer, Acc, From, To, Packet) -> Result when
    LServer :: jid:lserver(),
    Acc :: map(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: map().
offline_message_hook(LServer, Acc, From, To, Packet) ->
    ejabberd_hooks:run_fold(offline_message_hook,
                            LServer,
                            Acc,
                            [From, To, Packet]).

-spec set_presence_hook(LServer, Acc, LUser, LResource, Presence) -> Result when
    LServer :: jid:lserver(), Acc :: mongoose_acc:t(), LUser :: jid:luser(),
    LResource :: jid:lresource(),
    Presence :: any(),
    Result :: mongoose_acc:t().
set_presence_hook(LServer, Acc, LUser, LResource, Presence) ->
    ejabberd_hooks:run_fold(set_presence_hook, LServer, Acc, [LUser, LServer, LResource, Presence]).

-spec sm_broadcast(LServer, Acc, From, To, Broadcast, SessionCount) -> Result when
    LServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Broadcast :: ejabberd_c2s:broadcast(),
    SessionCount :: non_neg_integer(),
    Result :: mongoose_acc:t().
sm_broadcast(LServer, Acc, From, To, Broadcast, SessionCount) ->
    ejabberd_hooks:run_fold(sm_broadcast, LServer, Acc,
                            [From, To, Broadcast, SessionCount]).

-spec sm_filter_offline_message(LServer, Drop, From, To, Packet) -> Result when
    LServer :: jid:lserver(),
    Drop :: boolean(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: boolean().
sm_filter_offline_message(LServer, Drop, From, To, Packet) ->
    ejabberd_hooks:run_fold(sm_filter_offline_message, LServer,
                            Drop, [From, To, Packet]).

-spec sm_register_connection_hook(LServer, SID, JID, Info) -> Result when
    LServer :: jid:lserver(),
    SID :: 'undefined' | ejabberd_sm:sid(),
    JID :: jid:jid(),
    Info :: ejabberd_sm:info(),
    Result :: ok.
sm_register_connection_hook(LServer, SID, JID, Info) ->
    ejabberd_hooks:run_fold(sm_register_connection_hook, LServer, ok,
                            [SID, JID, Info]).

-spec sm_remove_connection_hook(LServer, Acc, SID, JID, Info, Reason) -> Result when
    LServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    SID :: 'undefined' | ejabberd_sm:sid(),
    JID :: jid:jid(),
    Info :: ejabberd_sm:info(),
    Reason :: ejabberd_sm:close_reason(),
    Result :: mongoose_acc:t().
sm_remove_connection_hook(LServer, Acc, SID, JID, Info, Reason) ->
    ejabberd_hooks:run_fold(sm_remove_connection_hook, LServer, Acc,
                            [SID, JID, Info, Reason]).

-spec unset_presence_hook(LServer, Acc, LUser, LResource, Status) -> Result when
    LServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    LUser :: jid:luser(),
    LResource :: jid:lresource(),
    Status :: binary(),
    Result :: mongoose_acc:t().
unset_presence_hook(LServer, Acc, LUser, LResource, Status) ->
    ejabberd_hooks:run_fold(unset_presence_hook, LServer, Acc,
                            [LUser, LServer, LResource, Status]).

-spec xmpp_bounce_message(Server, Acc) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    Result :: mongoose_acc:t().
xmpp_bounce_message(Server, Acc) ->
    ejabberd_hooks:run_fold(xmpp_bounce_message, Server, Acc, []).

%% Roster related hooks

%%% @doc The `roster_get' hook is called to extract a user's roster.
-spec roster_get(LServer, Acc, User, UserServer) -> Result when
    LServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    User :: jid:luser(),
    UserServer :: jid:lserver(),
    Result :: mongoose_acc:t().
roster_get(LServer, Acc, User, UserServer) ->
    ejabberd_hooks:run_fold(roster_get, LServer, Acc, [{User, UserServer}]).

%%% @doc The `roster_groups' hook is called to extract roster groups.
-spec roster_groups(LServer, Acc) -> Result when
    LServer :: jid:lserver(),
    Acc :: list(),
    Result :: list().
roster_groups(LServer, Acc) ->
    ejabberd_hooks:run_fold(roster_groups, LServer, Acc, [LServer]).

%%% @doc The `roster_get_jid_info' hook is called to determine the subscription state between a given pair of users.
%%% The hook handlers need to expect following arguments:
%%% * Acc with an initial value of {none, []},
%%% * LUser, a stringprepd username part of the roster's owner,
%%% * LServer, a stringprepd server part of the roster's owner (same value as HookServer),
%%% * RemoteBareJID, a bare JID of the other user.
%%%
%%% The arguments and the return value types correspond to the following spec.
-spec roster_get_jid_info(LServer, InitialValue, LUser, RemoteJID) -> Result when
    LServer :: jid:lserver(),
      InitialValue :: {mod_roster:subscription_state(), []},
      LUser :: jid:luser(),
      RemoteJID :: jid:jid() | jid:simple_jid(),
      Result :: {mod_roster:subscription_state(), [binary()]}.
roster_get_jid_info(LServer, InitialValue, LUser, RemBareJID) ->
    ejabberd_hooks:run_fold(roster_get_jid_info, LServer, InitialValue,
                            [LUser, LServer, RemBareJID]).

%%% @doc The `roster_get_subscription_lists' hook is called to extract user's subscription list.
-spec roster_get_subscription_lists(Server, Acc, User) -> Result when
    Server :: jid:server(),
    Acc ::mongoose_acc:t(),
    User :: jid:user(),
    Result :: mongoose_acc:t().
roster_get_subscription_lists(Server, Acc, User) ->
    ejabberd_hooks:run_fold(roster_get_subscription_lists, Server, Acc, [User, Server]).

%%% @doc The `roster_get_versioning_feature' hook is called to determine if roster versioning is enabled.
-spec roster_get_versioning_feature(Server, Acc) -> Result when
    Server :: jid:server(),
    Acc :: [exml:element()],
    Result :: [exml:element()].
roster_get_versioning_feature(Server, Acc) ->
    ejabberd_hooks:run_fold(roster_get_versioning_feature,
                            Server, Acc, [Server]).

%%% @doc The `roster_in_subscription' hook is called to determine if a subscription presence is routed to a user.
-spec roster_in_subscription(LServer, Acc, User, Server, From, Type, Reason) -> Result when
    LServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    User :: jid:user(),
    Server :: jid:server(),
    From :: jid:jid(),
    Type :: mod_roster:sub_presence(),
    Reason :: any(),
    Result :: mongoose_acc:t().
roster_in_subscription(LServer, Acc, User, Server, From, Type, Reason) ->
    ejabberd_hooks:run_fold(
        roster_in_subscription,
        LServer,
        Acc,
        [User, Server, From, Type, Reason]).

%%% @doc The `roster_out_subscription' hook is called when a user sends out subscription.
-spec roster_out_subscription(Server, Acc, User, To, Type) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    User :: jid:user(),
    To :: jid:jid(),
    Type :: mod_roster:sub_presence(),
    Result :: mongoose_acc:t().
roster_out_subscription(Server, Acc, User, To, Type) ->
    ejabberd_hooks:run_fold(roster_out_subscription,
                            Server,
                            Acc,
                            [User, Server, To, Type]).

%%% @doc The `roster_process_item' hook is called when a user's roster is set.
-spec roster_process_item(LServer, Item) -> Result when
    LServer :: jid:lserver(),
    Item :: mod_roster:roster(),
    Result :: mod_roster:roster().
roster_process_item(LServer, Item) ->
    ejabberd_hooks:run_fold(roster_process_item, LServer, Item, [LServer]).

%%% @doc The `roster_push' hook is called when a roster item is being pushed and roster versioning is not enabled.
-spec roster_push(LServer, Acc, From, Item) -> Result when
    LServer :: jid:lserver(),
    Acc :: any(),
    From :: jid:jid(),
    Item :: mod_roster:roster(),
    Result :: any().
roster_push(LServer, Acc, From, Item) ->
    ejabberd_hooks:run_fold(roster_push, LServer, Acc, [From, Item]).

%%% @doc The `roster_set' hook is called when a user's roster is set through an IQ.
-spec roster_set(LServer, Acc, From, To, SubEl) -> Result when
    LServer :: jid:lserver(),
    Acc :: any(),
    From :: jid:jid(),
    To :: jid:jid(),
    SubEl :: exml:element(),
    Result :: any().
roster_set(LServer, Acc, From, To, SubEl) ->
    ejabberd_hooks:run_fold(roster_set, LServer, Acc, [From, To, SubEl]).

%% MUC related hooks

%%% @doc The `is_muc_room_owner' hooks is called to determine if a given user is a room's owner.
%%%
%%% The hook's handler needs to expect the following arguments `Acc', `Room', `User'.
%%% The arguments and the return value types correspond to the following spec.
-spec is_muc_room_owner(HookServer, Acc, Room, User) -> Result when
      HookServer :: jid:lserver(),
      Acc :: boolean(),
      Room :: jid:jid(),
      User :: jid:jid(),
      Result :: boolean().
is_muc_room_owner(HookServer, Acc, Room, User) ->
    ejabberd_hooks:run_fold(is_muc_room_owner, HookServer, Acc, [Room, User]).

%%% @doc The `can_access_identity' hook is called to determine if a given user can see the real identity of the people in a room.
-spec can_access_identity(HookServer, Acc, Room, User) -> Result when
      HookServer :: jid:lserver(),
      Acc :: boolean(),
      Room :: jid:jid(),
      User :: jid:jid(),
      Result :: boolean().
can_access_identity(HookServer, Acc, Room, User) ->
    ejabberd_hooks:run_fold(can_access_identity, HookServer, Acc, [Room, User]).

%%% @doc The `muc_room_pid' hooks is called to get the pid for a given room's JID
-spec muc_room_pid(HookServer, InitialValue, Room) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: undefined,
      Room :: jid:jid(),
      Result :: undefined | {ok, processless | pid()} | {error, not_found}.
muc_room_pid(HookServer, InitialValue, Room) ->
    ejabberd_hooks:run_fold(muc_room_pid, HookServer, InitialValue, [Room]).

%%% @doc The `can_access_room' hook is called to determine if a given user can access a room.
-spec can_access_room(HookServer, Acc, Room, User) -> Result when
      HookServer :: jid:lserver(),
      Acc :: boolean(),
      Room :: jid:jid(),
      User :: jid:jid(),
      Result :: boolean().
can_access_room(HookServer, Acc, Room, User) ->
    ejabberd_hooks:run_fold(can_access_room, HookServer, Acc, [Room, User]).

%% MAM related hooks

%%% @doc The `mam_archive_id' hook is called to determine the id of an archive for a particular user or entity.
%%% The hook handler is expected to accept the following arguments:
%%% * Acc with an initial value of `undefined',
%%% * Host as passed in the `HooksServer' variable,
%%% * OwnerJID,
%%%
%%% and to return an integer value corresponding to the given owner's archive.
%%%
%%% If a MAM backend doesn't support or doesn't require archive IDs,
%%% `undefined' may be returned.
-spec mam_archive_id(HookServer, InitialValue, OwnerJID) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: undefined,
      OwnerJID :: jid:jid(),
      Result :: undefined | mod_mam:archive_id().
mam_archive_id(HookServer, InitialValue, OwnerJID) ->
    ejabberd_hooks:run_fold(mam_archive_id, HookServer, InitialValue, [HookServer, OwnerJID]).

%%% @doc The `mam_archive_size' hook is called to determine the size of the archive for a given JID
%%%
-spec mam_archive_size(HookServer, InitialValue, ArchiveID, OwnerJID) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: 0,
      ArchiveID :: undefined | mod_mam:archive_id(),
      OwnerJID :: jid:jid(),
      Result :: integer().
mam_archive_size(HookServer, InitialValue, ArchiveID, OwnerJID) ->
    ejabberd_hooks:run_fold(mam_archive_size, HookServer, InitialValue, [HookServer, ArchiveID, OwnerJID]).

%%% @doc The `mam_get_behaviour' hooks is called to determine if a message should be archived or not based on a given pair of JIDs.
%%%
-spec mam_get_behaviour(HookServer, DefaultBehaviour, ArchiveID, OwnerJID, RemoteJID) -> Result when
      HookServer :: jid:lserver(),
      DefaultBehaviour :: always,
      ArchiveID :: undefined | mod_mam:archive_id(),
      OwnerJID :: jid:jid(),
      RemoteJID :: jid:jid(),
      Result :: mod_mam:archive_behaviour().
mam_get_behaviour(HookServer, DefaultBehaviour, ArchiveID, OwnerJID, RemoteJID) ->
    ejabberd_hooks:run_fold(mam_get_behaviour, HookServer, DefaultBehaviour,
                            [HookServer, ArchiveID, OwnerJID, RemoteJID]).

%%% @doc The `mam_set_prefs' hook is called to set a user's archive preferences.
%%%
%%% It's possible to set which JIDs are always or never allowed in the archive
-spec mam_set_prefs(HookServer, InitialValue, ArchiveId, OwnerJID, DefaultMode, AlwaysJIDs, NeverJIDs) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: {error, not_implemented},
      ArchiveId :: undefined | mod_mam:archive_id(),
      OwnerJID :: jid:jid(),
      DefaultMode :: mod_mam:archive_behaviour(),
      AlwaysJIDs :: [jid:literal_jid()],
      NeverJIDs :: [jid:literel_jid()],
      Result :: any().
mam_set_prefs(HookServer, InitialValue, ArchiveID, OwnerJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    ejabberd_hooks:run_fold(mam_set_prefs, HookServer, InitialValue,
                            [HookServer, ArchiveID, OwnerJID, DefaultMode, AlwaysJIDs, NeverJIDs]).

%%% @doc The `mam_get_prefs' hook is called to read the archive settings for a given user.
-spec mam_get_prefs(HookServer, InitialValue, ArchiveID, OwnerJID) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: {mod_mam:archive_behaviour(), [], []},
      ArchiveID :: undefined | mod_mam:archive_id(),
      OwnerJID :: jid:jid(),
      Result :: mod_mam:preference() | {error, Reason :: term()}.
mam_get_prefs(HookServer, InitialValue, ArchiveID, OwnerJID) ->
    ejabberd_hooks:run_fold(mam_get_prefs, HookServer,
                            InitialValue,
                            [HookServer, ArchiveID, OwnerJID]).

%%% @doc The `mam_remove_archive' hook is called in order to remove the entire archive for a particular user.
-spec mam_remove_archive(HookServer, InitialValue, ArchiveID, OwnerJID) -> any() when
      HookServer :: jid:lserver(),
      InitialValue :: ok,
      ArchiveID :: undefined | mod_mam:archive_id(),
      OwnerJID :: jid:jid().
mam_remove_archive(HookServer, InitialValue, ArchiveID, OwnerJID) ->
    ejabberd_hooks:run_fold(mam_remove_archive, HookServer, InitialValue,
                            [HookServer, ArchiveID, OwnerJID]).

%%% @doc The `mam_lookup_messages' hook is to retrieve archived messages for given search parameters.
-spec mam_lookup_messages(HookServer, InitialValue, Params) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: {ok, {0, 0, []}},
      Params :: map(),
      Result :: {ok, mod_mam:lookup_result()}.
mam_lookup_messages(HookServer, InitialValue, Params) ->
    ejabberd_hooks:run_fold(mam_lookup_messages, HookServer, InitialValue,
                            [HookServer, Params]).

%%% @doc The `mam_archive_message' hook is called in order to store the message in the archive.
-spec mam_archive_message(HookServer, InitialValue, MessageID, ArchiveID, OwnerJID, RemoteJID, SenderJID, OriginID, Dir, Packet) ->
    Result when
    HookServer :: jid:lserver(),
    InitialValue :: ok,
    MessageID :: mod_mam:message_id(),
    ArchiveID :: undefined | mod_mam:archive_id(),
    OwnerJID :: jid:jid(),
    RemoteJID :: jid:jid(),
    SenderJID :: jid:jid(),
    OriginID :: binary() | none,
    Dir :: incoming | outgoing,
    Packet :: term(),
    Result :: ok | {error, timeout}.
mam_archive_message(HookServer, InitialValue, MessageID, ArchiveID, OwnerJID, RemoteJID, SenderJID, OriginID, Dir, Packet) ->
    ejabberd_hooks:run_fold(mam_archive_message, HookServer, InitialValue,
                            [HookServer, MessageID, ArchiveID, OwnerJID,
                             RemoteJID, SenderJID, OriginID, Dir, Packet]).


%% MAM MUC related hooks

%%% @doc The `mam_muc_archive_id' hook is called to determine the archive ID for a particular room.
%%% The hook handler is expected to accept the following arguments:
%%% * Acc with initial value `undefined',
%%% * Host as passed in `HooksServer' variable,
%%% * OwnerJID,
%%%
%%% and return an integer value corresponding to the given owner's archive.
%%%
%%% If a MAM backend doesn't support or doesn't require archive IDs,
%%% `undefined' may be returned.
-spec mam_muc_archive_id(HookServer, InitialValue, OwnerJID) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: undefined,
      OwnerJID :: jid:jid(),
      Result :: undefined | mod_mam:archive_id().
mam_muc_archive_id(HookServer, InitialValue, OwnerJID) ->
    ejabberd_hooks:run_fold(mam_muc_archive_id, HookServer, InitialValue, [HookServer, OwnerJID]).

%%% @doc The `mam_muc_archive_size' hook is called to determine the archive's size for a given room.
%%%
-spec mam_muc_archive_size(HookServer, InitialValue, ArchiveID, RoomJID) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: 0,
      ArchiveID :: undefined | mod_mam:archive_id(),
      RoomJID :: jid:jid(),
      Result :: integer().
mam_muc_archive_size(HookServer, InitialValue, ArchiveID, RoomJID) ->
    ejabberd_hooks:run_fold(mam_muc_archive_size, HookServer, InitialValue, [HookServer, ArchiveID, RoomJID]).

%%% @doc The `mam_muc_get_behaviour' hooks is called to determine if a message should be archived or not based on the given room and user JIDs.
%%%
-spec mam_muc_get_behaviour(HookServer, DefaultBehaviour, ArchiveID, RoomJID, RemoteJID) -> Result when
      HookServer :: jid:lserver(),
      DefaultBehaviour :: always,
      ArchiveID :: undefined | mod_mam:archive_id(),
      RoomJID :: jid:jid(),
      RemoteJID :: jid:jid(),
      Result :: mod_mam:archive_behaviour().
mam_muc_get_behaviour(HookServer, DefaultBehaviour, ArchiveID, RoomJID, RemoteJID) ->
    ejabberd_hooks:run_fold(mam_muc_get_behaviour, HookServer, DefaultBehaviour,
                            [HookServer, ArchiveID, RoomJID, RemoteJID]).

%%% @doc The `mam_muc_set_prefs' hook is called to set a room's archive preferences.
%%%
%%% It's possible to set which JIDs are always or never allowed in the archive
-spec mam_muc_set_prefs(HookServer, InitialValue, ArchiveId, RoomJID, DefaultMode, AlwaysJIDs, NeverJIDs) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: {error, not_implemented},
      ArchiveId :: undefined | mod_mam:archive_id(),
      RoomJID :: jid:jid(),
      DefaultMode :: mod_mam:archive_behaviour(),
      AlwaysJIDs :: [jid:literal_jid()],
      NeverJIDs :: [jid:literel_jid()],
      Result :: any().
mam_muc_set_prefs(HookServer, InitialValue, ArchiveID, RoomJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    ejabberd_hooks:run_fold(mam_muc_set_prefs, HookServer, InitialValue,
                            [HookServer, ArchiveID, RoomJID, DefaultMode, AlwaysJIDs, NeverJIDs]).

%%% @doc The `mam_muc_get_prefs' hook is called to read the archive settings for a given room.
-spec mam_muc_get_prefs(HookServer, InitialValue, ArchiveID, RoomJID) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: {mod_mam:archive_behaviour(), [], []},
      ArchiveID :: undefined | mod_mam:archive_id(),
      RoomJID :: jid:jid(),
      Result :: mod_mam:preference() | {error, Reason :: term()}.
mam_muc_get_prefs(HookServer, InitialValue, ArchiveID, RoomJID) ->
    ejabberd_hooks:run_fold(mam_muc_get_prefs, HookServer,
                            InitialValue,
                            [HookServer, ArchiveID, RoomJID]).

%%% @doc The `mam_muc_remove_archive' hook is called in order to remove the entire archive for a particular user.
-spec mam_muc_remove_archive(HookServer, InitialValue, ArchiveID, RoomJID) -> any() when
      HookServer :: jid:lserver(),
      InitialValue :: ok,
      ArchiveID :: undefined | mod_mam:archive_id(),
      RoomJID :: jid:jid().
mam_muc_remove_archive(HookServer, InitialValue, ArchiveID, RoomJID) ->
    ejabberd_hooks:run_fold(mam_muc_remove_archive, HookServer, InitialValue,
                            [HookServer, ArchiveID, RoomJID]).

%%% @doc The `mam_muc_lookup_messages' hook is to retrieve archived MUC messages for any given search parameters.
-spec mam_muc_lookup_messages(HookServer, InitialValue, Params) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: {ok, {0, 0, []}},
      Params :: map(),
      Result :: {ok, mod_mam:lookup_result()}.
mam_muc_lookup_messages(HookServer, InitialValue, Params) ->
    ejabberd_hooks:run_fold(mam_muc_lookup_messages, HookServer, InitialValue,
                            [HookServer, Params]).

%%% @doc The `mam_muc_archive_message' hook is called in order to store the MUC message in the archive.
-spec mam_muc_archive_message(HookServer, InitialValue, MessageID, ArchiveID, OwnerJID, RemoteJID, SenderJID, OriginID, Dir, Packet) ->
    Result when
    HookServer :: jid:lserver(),
    InitialValue :: ok,
    MessageID :: mod_mam:message_id(),
    ArchiveID :: undefined | mod_mam:archive_id(),
    OwnerJID :: jid:jid(),
    RemoteJID :: jid:jid(),
    SenderJID :: jid:jid(),
    OriginID :: binary() | none,
    Dir :: incoming | outgoing,
    Packet :: term(),
    Result :: ok | {error, timeout}.
mam_muc_archive_message(HookServer, InitialValue, MessageID, ArchiveID, OwnerJID, RemoteJID, SenderJID, OriginID, Dir, Packet) ->
    ejabberd_hooks:run_fold(mam_muc_archive_message, HookServer, InitialValue,
                            [HookServer, MessageID, ArchiveID, OwnerJID,
                             RemoteJID, SenderJID, OriginID, Dir, Packet]).

%%% @doc The `mam_muc_flush_messages' hook is run after the async bulk write happens for MUC messages despite the result of the write.
-spec mam_muc_flush_messages(HookServer :: jid:lserver(), InitialValue :: ok, MessageCount :: integer()) -> ok.
mam_muc_flush_messages(HookServer, InitialValue, MessageCount) ->
    ejabberd_hooks:run_fold(mam_muc_flush_messages, HookServer, InitialValue, [HookServer, MessageCount]).

%% GDPR related hooks

%%% @doc `get_mam_pm_gdpr_data' hook is called to provide a user's archive for GDPR purposes.
-spec get_mam_pm_gdpr_data(HookServer, InitialValue, JID) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: [],
      JID :: jid:jid(),
      Result :: ejabberd_gen_mam_archive:mam_pm_gdpr_data().
get_mam_pm_gdpr_data(HookServer, InitialValue, JID) ->
    ejabberd_hooks:run_fold(get_mam_pm_gdpr_data, HookServer, InitialValue, [JID]).

%%% @doc `get_mam_muc_gdpr_data' hook is called to provide a user's archive for GDPR purposes.
-spec get_mam_muc_gdpr_data(HookServer, InitialValue, JID) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: [],
      JID :: jid:jid(),
      Result :: ejabberd_gen_mam_archive:mam_muc_gdpr_data().
get_mam_muc_gdpr_data(HookServer, InitialValue, JID) ->
    ejabberd_hooks:run_fold(get_mam_muc_gdpr_data, HookServer, InitialValue, [JID]).

%%% @doc `get_personal_data' hook is called to retrieve a user's personal data for GDPR purposes.
-spec get_personal_data(LServer, InitialValue, JID) -> Result when
    LServer :: jid:lserver(),
    InitialValue :: [],
    JID :: jid:jid(),
    Result :: gdpr:personal_data().
get_personal_data(LServer, InitialValue, JID) ->
    ejabberd_hooks:run_fold(get_personal_data, LServer, InitialValue, [JID]).

%% S2S related hooks

%%% @doc `find_s2s_bridge' hook is called to find a s2s bridge to a foreign protocol when opening a socket to a different XMPP server fails.
-spec find_s2s_bridge(Acc, Name, Server) -> Result when
    Acc :: any(),
    Name :: any(),
    Server :: jid:server(),
    Result :: any().
find_s2s_bridge(Acc, Name, Server) ->
    ejabberd_hooks:run_fold(find_s2s_bridge, Acc, [Name, Server]).

%%% @doc `s2s_allow_host' hook is called to check whether a server should be allowed to be connected to.
%%%
%%% A handler can decide that a server should not be allowed and pass this information to the caller.
-spec s2s_allow_host(MyHost, Allow, S2SHost) -> Result when
    MyHost :: jid:server(),
    Allow :: allow,
    S2SHost :: jid:server(),
    Result :: allow | deny.
s2s_allow_host(MyHost, Allow, S2SHost) ->
    ejabberd_hooks:run_fold(s2s_allow_host, MyHost, Allow, [MyHost, S2SHost]).

%%% @doc `s2s_connect_hook' hook is called when a s2s connection is established.
-spec s2s_connect_hook(Name, Acc, Server) -> Result when
    Name :: any(),
    Acc :: any(),
    Server :: jid:server(),
    Result :: any().
s2s_connect_hook(Name, Acc, Server) ->
    ejabberd_hooks:run_fold(s2s_connect_hook, Name, Acc, [Server]).

%%% @doc `s2s_send_packet' hook is called when a message is routed.
-spec s2s_send_packet(Server, Acc, From, To, Packet) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: mongoose_acc:t().
s2s_send_packet(Server, Acc, From, To, Packet) ->
    ejabberd_hooks:run_fold(s2s_send_packet,
                            Server,
                            Acc,
                            [From, To, Packet]).

%%% @doc `s2s_stream_features' hook is used to extract the stream management features supported by the server.
-spec s2s_stream_features(Server, Acc) -> Result when
    Server :: jid:server(),
    Acc :: [exml:element()],
    Result :: [exml:element()].
s2s_stream_features(Server, Acc) ->
    ejabberd_hooks:run_fold(s2s_stream_features, Server, Acc, [Server]).

%%% @doc `s2s_receive_packet' hook is called when an incoming stanza is routed by the server.
-spec s2s_receive_packet(LServer, Acc) -> Result when
    LServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    Result :: mongoose_acc:t().
s2s_receive_packet(LServer, Acc) ->
    ejabberd_hooks:run_fold(s2s_receive_packet, LServer, Acc, []).

%% Discovery related hooks

%%% @doc `disco_info' hook is called to extract information about the server.
-spec disco_info(Server, Acc, Module, Node, Lang) -> Result when
    Server :: jid:server(),
    Acc :: [exml:element()],
    Module :: module(),
    Node :: binary(),
    Lang :: ejabberd:lang(),
    Result :: [exml:element()].
disco_info(Server, Acc, Module, Node, Lang) ->
    ejabberd_hooks:run_fold(disco_info, Server, Acc,
                            [Server, Module, Node, Lang]).

%%% @doc `disco_local_features' hook is called to extract features offered by the server.
-spec disco_local_features(Server, Acc, From, To, Node, Lang) -> Result when
    Server :: jid:server(),
    Acc :: empty | {error, any()} | {result, [exml:element()]},
    From :: jid:jid(),
    To :: jid:jid(),
    Node :: binary(),
    Lang :: ejabberd:lang(),
    Result :: {error, any()} | {result, [exml:element()]}.
disco_local_features(Server, Acc, From, To, Node, Lang) ->
    ejabberd_hooks:run_fold(disco_local_features,
                            Server,
                            Acc,
                            [From, To, Node, Lang]).

%%% @doc `disco_local_items' hook is called to extract items associated with the server.
-spec disco_local_items(Server, Acc, From, To, Node, Lang) -> Result when
    Server :: jid:server(),
    Acc :: empty | {result, [exml:element()]} | {error, any()},
    From :: jid:jid(),
    To :: jid:jid(),
    Node :: binary(),
    Lang :: ejabberd:lang(),
    Result :: {result, [exml:element()]} | {error, any()}.
disco_local_items(Server, Acc, From, To, Node, Lang) ->
    ejabberd_hooks:run_fold(disco_local_items,
                            Server,
                            Acc,
                            [From, To, Node, Lang]).

%%% @doc `disco_local_identity' hook is called to get the identity of the server.
-spec disco_local_identity(Server, Acc, From, To, Node, Lang) -> Result when
    Server :: jid:server(),
    Acc :: [exml:element()],
    From :: jid:jid(),
    To :: jid:jid(),
    Node :: binary(),
    Lang :: ejabberd:lang(),
    Result :: [exml:element()].
disco_local_identity(Server, Acc, From, To, Node, Lang) ->
    ejabberd_hooks:run_fold(disco_local_identity,
                            Server,
                            Acc,
                            [From, To, Node, Lang]).

%%% @doc `disco_sm_features' hook is called to get the features of the client when a discovery IQ gets to session management.
-spec disco_sm_features(Server, Acc, From, To, Node, Lang) -> Result when
    Server :: jid:server(),
    Acc :: empty | {error, any()} | {result, [exml:element()]},
    From :: jid:jid(),
    To :: jid:jid(),
    Node :: binary(),
    Lang :: ejabberd:lang(),
    Result :: {error, any()} | {result, [exml:element()]}.
disco_sm_features(Server, Acc, From, To, Node, Lang) ->
    ejabberd_hooks:run_fold(disco_local_features,
                            Server,
                            Acc,
                            [From, To, Node, Lang]).

%%% @doc `disco_sm_identity' hook is called to get the identity of the client when a discovery IQ gets to session management.
-spec disco_sm_identity(Server :: jid:server(),
                        Acc :: [exml:element()],
                        From :: jid:jid(),
                        To :: jid:jid(),
                        Node :: mod_pubsub:nodeId(),
                        Lang :: ejabberd:lang()) -> [exml:element()].
disco_sm_identity(Server, Acc, From, To, Node, Lang) ->
    ejabberd_hooks:run_fold(disco_sm_identity,
                            Server,
                            Acc,
                            [From, To, Node, Lang]).

%%% @doc `disco_sm_items' hook is called to get the items associated with the client when a discovery IQ gets to session management.
-spec disco_sm_items(Server :: jid:server(),
                     Acc :: empty | {result, [exml:element()]} | {error, any()},
                     From :: jid:jid(),
                     To :: jid:jid(),
                     Node :: binary(),
                     Lang :: ejabberd:lang()) -> {error, any()} | {result, [exml:element()]}.
disco_sm_items(Server, Acc, From, To, Node, Lang) ->
    ejabberd_hooks:run_fold(disco_sm_items,
                            Server,
                            Acc,
                            [From, To, Node, Lang]).

%% AMP related hooks

%%% @doc The `amp_check_condition' hook is called to determine whether the AMP strategy matches the given AMP rule.
-spec amp_check_condition(Server, MatchResult, Strategy, Rule) -> Result when
    Server :: jid:server(),
    MatchResult :: mod_amp:amp_match_result(),
    Strategy :: mod_amp:amp_strategy(),
    Rule :: mod_amp:amp_rule(),
    Result :: mod_amp:amp_match_result().
amp_check_condition(Server, MatchResult, Strategy, Rule) ->
    ejabberd_hooks:run_fold(amp_check_condition, Server, MatchResult, [Strategy, Rule]).

%%% @doc The `amp_check_packet' hook is called when one wants to check a message against amp rules.
%%% Calling it may result in `mod_amp' taking actions according to the rules and the event (i.e. notify the sender).
-spec amp_check_packet(Server, Acc, From, Event) -> Result when
    Server :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    Event :: mod_amp:amp_event(),
    Result :: mongoose_acc:t().
amp_check_packet(Server, Acc, From, Event) ->
    ejabberd_hooks:run_fold(amp_check_packet, Server, Acc, [From, Event]).

%%% @doc The `amp_determine_strategy' hook is called when checking to determine  which strategy will be chosen when executing AMP rules.
-spec amp_determine_strategy(Server, Strategy, From, To, Packet, Event) -> Result when
    Server :: jid:server(),
    Strategy :: mod_amp:amp_strategy(),
    From :: jid:jid(),
    To :: jid:jid() | undefined,
    Packet :: exml:element(),
    Event :: mod_amp:amp_event(),
    Result :: mod_amp:amp_strategy().
amp_determine_strategy(Server, Strategy, From, To, Packet, Event) ->
    ejabberd_hooks:run_fold(amp_determine_strategy, Server,
                            Strategy, [From, To, Packet, Event]).

%%% @doc The `amp_error_action_triggered' hook is called to inform that the `error' action has been triggered.
-spec amp_error_action_triggered(Server, Acc) -> Result when
    Server :: jid:server(),
    Acc :: any(),
    Result :: any().
amp_error_action_triggered(Server, Acc) ->
    ejabberd_hooks:run_fold(amp_error_action_triggered, Server, Acc, [Server]).

%%% @doc The `amp_notify_action_triggered' hook is called to inform that the `notify' action has been triggered.
-spec amp_notify_action_triggered(Server, Acc) -> Result when
    Server :: jid:server(),
    Acc :: any(),
    Result :: any().
amp_notify_action_triggered(Server, Acc) ->
    ejabberd_hooks:run_fold(amp_notify_action_triggered, Server, Acc, [Server]).

%%% @doc The `amp_verify_support' hook is called when checking whether the host supports given AMP rules.
-spec amp_verify_support(Server, Acc, Rules) -> Result when
    Server :: jid:lserver(),
    Acc :: [mod_amp:amp_rule_support()],
    Rules :: mod_amp:amp_rules(),
    Result :: [mod_amp:amp_rule_support()].
amp_verify_support(Server, Acc, Rules) ->
    ejabberd_hooks:run_fold(amp_verify_support, Server, Acc, [Rules]).

%% MUC and MUC Light related hooks

-spec filter_room_packet(Server, Packet, EventData) -> Result when
    Server :: jid:lserver(),
    Packet :: exml:element(),
    EventData :: [{atom(), any()}],
    Result :: exml:element().
filter_room_packet(Server, Packet, EventData) ->
    ejabberd_hooks:run_fold(filter_room_packet, Server, Packet, [EventData]).

%%% @doc The `forget_room' hook is called when a room is removed from the database.
-spec forget_room(HookServer, Acc, Host, Room) -> Result when
    HookServer :: jid:server(),
    Acc :: any(),
    Host :: jid:server(),
    Room :: jid:luser(),
    Result :: any().
forget_room(HookServer, Acc, Host, Room) ->
    ejabberd_hooks:run_fold(forget_room, HookServer, Acc, [Host, Room]).

-spec invitation_sent(HookServer, Acc, Host, RoomJID, From, To, Reason) -> Result when
    HookServer :: jid:server(),
    Acc :: any(),
    Host :: jid:server(),
    RoomJID :: jid:jid(),
    From :: jid:jid(),
    To :: jid:jid(),
    Reason :: binary(),
    Result :: any().
invitation_sent(HookServer, Acc, Host, RoomJID, From, To, Reason) ->
    ejabberd_hooks:run_fold(invitation_sent,
                            HookServer,
                            Acc,
                            [HookServer, Host, RoomJID, From, To, Reason]).

%%% @doc The `join_room' hook is called when a user joins a MUC room.
-spec join_room(HookServer, Acc, Room, Host, JID, MucJID) -> Result when
    HookServer :: jid:server(),
    Acc :: any(),
    Room :: mod_muc:room(),
    Host :: jid:server(),
    JID :: jid:jid(),
    MucJID :: jid:jid(),
    Result :: any().
join_room(HookServer, Acc, Room, Host, JID, MucJID) ->
    ejabberd_hooks:run_fold(join_room, HookServer, Acc, [HookServer, Room, Host, JID, MucJID]).

%%% @doc The `leave_room' hook is called when a user joins a MUC room.
-spec leave_room(HookServer, Acc, Room, Host, JID, MucJID) -> Result when
    HookServer :: jid:server(),
    Acc :: any(),
    Room :: mod_muc:room(),
    Host :: jid:server(),
    JID :: jid:jid(),
    MucJID :: jid:jid(),
    Result :: any().
leave_room(HookServer, Acc, Room, Host, JID, MucJID) ->
    ejabberd_hooks:run_fold(leave_room, HookServer, Acc, [HookServer, Room, Host, JID, MucJID]).

%%% @doc The `room_packet' hook is called when a message is added to room's history.
-spec room_packet(Server, Acc, FromNick, FromJID, JID, Packet) -> Result when
    Server :: jid:lserver(),
    Acc :: any(),
    FromNick :: mod_muc:nick(),
    FromJID :: jid:jid(),
    JID :: jid:jid(),
    Packet :: exml:element(),
    Result :: any().
room_packet(Server, Acc, FromNick, FromJID, JID, Packet) ->
    ejabberd_hooks:run_fold(room_packet,
                            Server,
                            Acc,
                            [FromNick, FromJID, JID, Packet]).

%%% @doc The `room_send_packet' hook is called when a message is sent to a room.
-spec room_send_packet(Server, Packet, EventData) -> Result when
    Server :: jid:lserver(),
    Packet :: exml:element(),
    EventData :: [{atom(), any()}],
    Result :: exml:element().
room_send_packet(Server, Packet, EventData) ->
    ejabberd_hooks:run_fold(room_send_packet, Server, Packet, [EventData]).

-spec update_inbox_for_muc(Server, Info) -> Result when
    Server :: jid:server(),
    Info :: mod_muc_room:update_inbox_for_muc_payload(),
    Result :: mod_muc_room:update_inbox_for_muc_payload().
update_inbox_for_muc(Server, Info) ->
    ejabberd_hooks:run_fold(update_inbox_for_muc, Server, Info, []).

%% Caps related hooks

-spec caps_add(Server, Acc, From, To, Pid, Features) -> Result when
    Server :: jid:server(),
    Acc :: any(),
    From :: jid:jid(),
    To :: jid:jid(),
    Pid :: pid(),
    Features :: unknown | list(),
    Result :: any().
caps_add(Server, Acc, From, To, Pid, Features) ->
    ejabberd_hooks:run_fold(caps_add,
                            Server,
                            Acc,
                            [From, To, Pid, Features]).

-spec caps_recognised(Server, Acc, From, Pid, Features) -> Result when
    Server :: jid:server(),
    Acc :: any(),
    From :: jid:jid(),
    Pid :: pid(),
    Features :: unknown | list(),
    Result :: any().
caps_recognised(Server, Acc, From, Pid, Features) ->
    ejabberd_hooks:run_fold(caps_recognised,
                            Server,
                            Acc,
                            [From, Pid, Features]).

-spec caps_update(Server, Acc, From, To, Pid, Features) -> Result when
    Server :: jid:server(),
    Acc :: any(),
    From :: jid:jid(),
    To :: jid:jid(),
    Pid :: pid(),
    Features :: unknown | list(),
    Result :: any().
caps_update(Server, Acc, From, To, Pid, Features) ->
    ejabberd_hooks:run_fold(caps_update,
                            Server,
                            Acc,
                            [From, To, Pid, Features]).

%% PubSub related hooks

%%% @doc The `pubsub_create_node' hook is called to inform that a pubsub node is created.
-spec pubsub_create_node(Server, Acc, PubSubHost, NodeId, Nidx, NodeOptions) -> Result when
    Server :: jid:server(),
    Acc :: any(),
    PubSubHost :: mod_pubsub:host(),
    NodeId :: mod_pubsub:nodeId(),
    Nidx :: mod_pubsub:nodeIdx(),
    NodeOptions :: list(),
    Result :: any().
pubsub_create_node(Server, Acc, PubSubHost, NodeId, Nidx, NodeOptions) ->
    ejabberd_hooks:run_fold(pubsub_create_node,
                            Server,
                            Acc,
                            [Server, PubSubHost, NodeId, Nidx, NodeOptions]).

%%% @doc The `pubsub_delete_node' hook is called to inform that a pubsub node is deleted.
-spec pubsub_delete_node(Server, Acc, PubSubHost, NodeId, Nidx) -> Result when
    Server :: jid:server(),
    Acc :: any(),
    PubSubHost :: mod_pubsub:host(),
    NodeId :: mod_pubsub:nodeId(),
    Nidx :: mod_pubsub:nodeIdx(),
    Result :: any().
pubsub_delete_node(Server, Acc, PubSubHost, NodeId, Nidx) ->
    ejabberd_hooks:run_fold(pubsub_delete_node,
                            Server,
                            Acc,
                            [Server, PubSubHost, NodeId, Nidx]).

%%% @doc The `pubsub_publish_item' hook is called to inform that a pubsub item is published.
-spec pubsub_publish_item(Server, Acc, NodeId, Publisher, ServiceJID, ItemId, BrPayload) -> Result when
    Server :: jid:server(),
    Acc :: any(),
    NodeId :: mod_pubsub:nodeId(),
    Publisher :: jid:jid(),
    ServiceJID :: jid:jid(),
    ItemId :: mod_pubsub:itemId(),
    BrPayload :: mod_pubsub:payload(),
    Result :: any().
pubsub_publish_item(Server, Acc, NodeId, Publisher, ServiceJID, ItemId, BrPayload) ->
    ejabberd_hooks:run_fold(pubsub_publish_item,
                            Server,
                            Acc,
                            [Server, NodeId, Publisher, ServiceJID, ItemId, BrPayload]).

%% Global distribution related hooks

%%% @doc The `mod_global_distrib_known_recipient' hook is called when the recipient is known to `global_distrib'.
-spec mod_global_distrib_known_recipient(GlobalHost, Acc, From, To, LocalHost) -> Result when
    GlobalHost :: jid:server(),
    Acc :: any(),
    From :: jid:jid(),
    To :: jid:jid(),
    LocalHost :: jid:server(),
    Result :: any().
mod_global_distrib_known_recipient(GlobalHost, Acc, From, To, LocalHost) ->
    ejabberd_hooks:run_fold(mod_global_distrib_known_recipient,
                            GlobalHost,
                            Acc,
                            [From, To, LocalHost]).

%%% @doc The `mod_global_distrib_unknown_recipient' hook is called when the recipient is unknown to `global_distrib'.
-spec mod_global_distrib_unknown_recipient(GlobalHost, Info) -> Result when
    GlobalHost :: jid:server(),
    Info :: {From :: jid:jid(), To :: jid:jid(), Acc :: mongoose_acc:t(), Packet :: exml:element()},
    Result :: any().
mod_global_distrib_unknown_recipient(GlobalHost, {From, To, Acc, Packet}) ->
    ejabberd_hooks:run_fold(mod_global_distrib_unknown_recipient,
                            GlobalHost,
                            {From, To, Acc, Packet},
                            []).
