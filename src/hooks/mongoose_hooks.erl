%%% @doc Hooks wrapper providing clear specifications for a hook caller.
%%%
%%% Every hook has its own function in this module with specs as accurate as
%%% possible. This helps to have a static analysis of the hooks callers to
%%% make sure they pass the expected arguments.
-module(mongoose_hooks).

-include("mod_privacy.hrl").
-include("mongoose.hrl").

-export([adhoc_local_commands/4,
         adhoc_sm_commands/4,
         anonymous_purge_hook/3,
         auth_failed/3,
         does_user_exist/3,
         failed_to_store_message/1,
         filter_local_packet/1,
         filter_packet/1,
         inbox_unread_count/3,
         extend_inbox_result/3,
         get_key/2,
         packet_to_component/3,
         presence_probe_hook/5,
         push_notifications/4,
         register_subhost/2,
         register_user/3,
         remove_user/3,
         resend_offline_messages_hook/2,
         session_cleanup/5,
         set_vcard/3,
         unacknowledged_message/2,
         filter_unacknowledged_messages/3,
         unregister_subhost/1,
         user_available_hook/2,
         user_ping_response/5,
         vcard_set/4,
         xmpp_send_element/3,
         xmpp_stanza_dropped/4]).

%% sasl2 handlers
-export([sasl2_stream_features/2,
         bind2_stream_features/2,
         bind2_enable_features/3,
         sasl2_start/3,
         sasl2_success/3]).

-export([get_pep_recipients/2,
         filter_pep_recipient/3,
         c2s_stream_features/3,
         check_bl_c2s/1,
         forbidden_session_hook/3,
         session_opening_allowed_for_user/2]).

-export([privacy_check_packet/5,
         privacy_get_user_list/2,
         privacy_iq_get/6,
         privacy_iq_set/5,
         privacy_updated_list/3,
         privacy_list_push/5]).

-export([offline_groupchat_message_hook/4,
         offline_message_hook/4,
         set_presence_hook/3,
         sm_filter_offline_message/4,
         sm_register_connection_hook/4,
         sm_remove_connection_hook/5,
         unset_presence_hook/3,
         xmpp_bounce_message/1]).

-export([roster_get/3,
         roster_get_jid_info/3,
         roster_get_subscription_lists/3,
         roster_get_versioning_feature/1,
         roster_groups/1,
         roster_in_subscription/5,
         roster_out_subscription/4,
         roster_process_item/3,
         roster_push/3,
         roster_set/4]).

-export([is_muc_room_owner/4,
         can_access_identity/3,
         can_access_room/4,
         acc_room_affiliations/2,
         room_new_affiliations/4,
         room_exists/2]).

-export([mam_archive_id/2,
         mam_archive_size/3,
         mam_get_behaviour/4,
         mam_set_prefs/6,
         mam_get_prefs/4,
         mam_remove_archive/3,
         mam_lookup_messages/2,
         mam_archive_message/2,
         mam_flush_messages/2,
         mam_archive_sync/1,
         mam_retraction/3]).

-export([mam_muc_archive_id/2,
         mam_muc_archive_size/3,
         mam_muc_get_behaviour/4,
         mam_muc_set_prefs/6,
         mam_muc_get_prefs/4,
         mam_muc_remove_archive/3,
         mam_muc_lookup_messages/2,
         mam_muc_archive_message/2,
         mam_muc_flush_messages/2,
         mam_muc_archive_sync/1,
         mam_muc_retraction/3]).

-export([get_mam_pm_gdpr_data/2,
         get_mam_muc_gdpr_data/2,
         get_personal_data/2]).

-export([s2s_allow_host/2,
         s2s_receive_packet/1,
         s2s_stream_features/2,
         s2s_send_packet/4]).

-export([disco_local_identity/1,
         disco_sm_identity/1,
         disco_local_items/1,
         disco_sm_items/1,
         disco_local_features/1,
         disco_sm_features/1,
         disco_muc_features/1,
         disco_info/1]).

-export([amp_check_condition/3,
         amp_determine_strategy/5,
         amp_verify_support/2]).

-export([filter_room_packet/3,
         forget_room/3,
         invitation_sent/6,
         join_room/5,
         leave_room/5,
         room_packet/5,
         update_inbox_for_muc/2]).

-export([caps_recognised/4]).

-export([mod_global_distrib_known_recipient/4,
         mod_global_distrib_unknown_recipient/2]).

-export([remove_domain/2,
         node_cleanup/1,
         node_cleanup_for_host_type/2]).

-ignore_xref([remove_domain/2]).
-ignore_xref([mam_archive_sync/1, mam_muc_archive_sync/1]).

%% Just a map, used by some hooks as a first argument.
%% Not mongoose_acc:t().
-type simple_acc() :: #{}.
-export_type([simple_acc/0]).

-type filter_packet_acc() :: {From :: jid:jid(),
                              To :: jid:jid(),
                              Acc :: mongoose_acc:t(),
                              Packet :: exml:element()}.
-export_type([filter_packet_acc/0]).

-spec adhoc_local_commands(HostType, From, To, AdhocRequest) -> Result when
    HostType :: mongooseim:host_type(),
    From :: jid:jid(),
    To :: jid:jid(),
    AdhocRequest :: adhoc:request(),
    Result :: mod_adhoc:command_hook_acc().
adhoc_local_commands(HostType, From, To, AdhocRequest) ->
    Params = #{from => From, to => To, adhoc_request => AdhocRequest},
    run_hook_for_host_type(adhoc_local_commands, HostType, empty, Params).

-spec adhoc_sm_commands(HostType, From, To, AdhocRequest) -> Result when
    HostType :: mongooseim:host_type(),
    From :: jid:jid(),
    To :: jid:jid(),
    AdhocRequest :: adhoc:request(),
    Result :: mod_adhoc:command_hook_acc().
adhoc_sm_commands(HostType, From, To, AdhocRequest) ->
    Params = #{from => From, to => To, request => AdhocRequest},
    run_hook_for_host_type(adhoc_sm_commands, HostType, empty, Params).

%%% @doc The `anonymous_purge_hook' hook is called when anonymous user's data is removed.
-spec anonymous_purge_hook(LServer, Acc, LUser) -> Result when
    LServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    LUser :: jid:user(),
    Result :: mongoose_acc:t().
anonymous_purge_hook(LServer, Acc, LUser) ->
    Jid = jid:make_bare(LUser, LServer),
    Params = #{jid => Jid},
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(anonymous_purge_hook, HostType, Acc, Params).

-spec auth_failed(HostType, Server, Username) -> Result when
    HostType :: mongooseim:host_type(),
    Server :: jid:server(),
    Username :: jid:user() | undefined,
    Result :: ok.
auth_failed(HostType, Server, Username) ->
    Params = #{username => Username, server => Server},
    run_hook_for_host_type(auth_failed, HostType, ok, Params).

-spec does_user_exist(HostType, Jid, RequestType) -> Result when
      HostType :: mongooseim:host_type(),
      Jid :: jid:jid(),
      RequestType :: ejabberd_auth:exist_type(),
      Result :: boolean().
does_user_exist(HostType, Jid, RequestType) ->
    Params = #{jid => Jid, request_type => RequestType},
    run_hook_for_host_type(does_user_exist, HostType, false, Params).

-spec remove_domain(HostType, Domain) -> Result when
    HostType :: mongooseim:host_type(),
    Domain :: jid:lserver(),
    Result :: mongoose_domain_api:remove_domain_acc().
remove_domain(HostType, Domain) ->
    Params = #{domain => Domain},
    run_hook_for_host_type(remove_domain, HostType, #{failed => []}, Params).

-spec node_cleanup(Node :: node()) -> Acc :: map().
node_cleanup(Node) ->
    Params = #{node => Node},
    run_global_hook(node_cleanup, #{}, Params).

-spec node_cleanup_for_host_type(HostType :: mongooseim:host_type(), Node :: node()) -> Acc :: map().
node_cleanup_for_host_type(HostType, Node) ->
    Params = #{node => Node},
    run_hook_for_host_type(node_cleanup_for_host_type, HostType, #{}, Params).

-spec failed_to_store_message(Acc) -> Result when
    Acc :: mongoose_acc:t(),
    Result :: mongoose_acc:t().
failed_to_store_message(Acc) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(failed_to_store_message, HostType, Acc, #{}).

%%% @doc The `filter_local_packet' hook is called to filter out
%%% stanzas routed with `mongoose_local_delivery'.
-spec filter_local_packet(FilterAcc) -> Result when
    FilterAcc :: filter_packet_acc(),
    Result :: drop | filter_packet_acc().
filter_local_packet(FilterAcc = {_From, _To, Acc, _Packet}) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(filter_local_packet, HostType, FilterAcc, #{}).

%%% @doc The `filter_packet' hook is called to filter out
%%% stanzas routed with `mongoose_router_global'.
-spec filter_packet(Acc) -> Result when
    Acc :: filter_packet_acc(),
    Result ::  drop | filter_packet_acc().
filter_packet(Acc) ->
    run_global_hook(filter_packet, Acc, #{}).

%%% @doc The `inbox_unread_count' hook is called to get the number
%%% of unread messages in the inbox for a user.
-spec inbox_unread_count(LServer, Acc, User) -> Result when
    LServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    User :: jid:jid(),
    Result :: mongoose_acc:t().
inbox_unread_count(LServer, Acc, User) ->
    Params = #{user => User},
    run_hook_for_host_type(inbox_unread_count, LServer, Acc, Params).

-spec extend_inbox_result(mongoose_acc:t(), [mod_inbox:inbox_res()], jlib:iq()) ->
    [mod_inbox:inbox_res()].
extend_inbox_result(MongooseAcc, InboxResults, IQ) ->
    HostType = mongoose_acc:host_type(MongooseAcc),
    HookParams = #{mongoose_acc => MongooseAcc, iq => IQ},
    run_hook_for_host_type(extend_inbox_result, HostType, InboxResults, HookParams).

%%% @doc The `get_key' hook is called to extract a key from `mod_keystore'.
-spec get_key(HostType, KeyName) -> Result when
    HostType :: mongooseim:host_type(),
    KeyName :: atom(),
    Result :: mod_keystore:key_list().
get_key(HostType, KeyName) ->
    Params = #{key_id => {KeyName, HostType}},
    run_hook_for_host_type(get_key, HostType, [], Params).

-spec packet_to_component(Acc, From, To) -> Result when
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Result :: mongoose_acc:t().
packet_to_component(Acc, From, To) ->
    Params = #{from => From, to => To},
    run_global_hook(packet_to_component, Acc, Params).

-spec presence_probe_hook(HostType, Acc, From, To, Pid) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Pid :: pid(),
    Result :: mongoose_acc:t().
presence_probe_hook(HostType, Acc, From, To, Pid) ->
    Params = #{from => From, to => To, pid => Pid},
    run_hook_for_host_type(presence_probe_hook, HostType, Acc, Params).

%%% @doc The `push_notifications' hook is called to push notifications.
-spec push_notifications(HostType, Acc, NotificationForms, Options) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: ok | mongoose_acc:t(),
    NotificationForms :: [#{binary() => binary()}],
    Options :: #{binary() => binary()},
    Result :: ok | {error, any()}.
push_notifications(HostType, Acc, NotificationForms, Options) ->
    Params = #{options => Options, notification_forms => NotificationForms},
    run_hook_for_host_type(push_notifications, HostType, Acc, Params).

%%% @doc The `register_subhost' hook is called when a component
%%% is registered for ejabberd_router or a subdomain is added to mongoose_subdomain_core.
-spec register_subhost(LDomain, IsHidden) -> Result when
    LDomain :: binary(),
    IsHidden :: boolean(),
    Result :: any().
register_subhost(LDomain, IsHidden) ->
    Params = #{ldomain => LDomain, is_hidden => IsHidden},
    run_global_hook(register_subhost, ok, Params).

%%% @doc The `register_user' hook is called when a user is successfully
%%% registered in an authentication backend.
-spec register_user(HostType, LServer, LUser) -> Result when
    HostType :: mongooseim:host_type(),
    LServer :: jid:lserver(),
    LUser :: jid:luser(),
    Result :: any().
register_user(HostType, LServer, LUser) ->
    Jid = jid:make_bare(LUser, LServer),
    Params = #{jid => Jid},
    run_hook_for_host_type(register_user, HostType, ok, Params).

%%% @doc The `remove_user' hook is called when a user is removed.
-spec remove_user(Acc, LServer, LUser) -> Result when
    Acc :: mongoose_acc:t(),
    LServer :: jid:lserver(),
    LUser :: jid:luser(),
    Result :: mongoose_acc:t().
remove_user(Acc, LServer, LUser) ->
    Jid = jid:make_bare(LUser, LServer),
    Params = #{jid => Jid},
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(remove_user, HostType, Acc, Params).

-spec resend_offline_messages_hook(Acc, JID) -> Result when
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Result :: mongoose_acc:t().
resend_offline_messages_hook(Acc, JID) ->
    Params = #{jid => JID},
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(resend_offline_messages_hook, HostType, Acc, Params).

%%% @doc The `session_cleanup' hook is called when sm backend cleans up a user's session.
-spec session_cleanup(Server, Acc, User, Resource, SID) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    User :: jid:user(),
    Resource :: jid:resource(),
    SID :: ejabberd_sm:sid(),
    Result :: mongoose_acc:t().
session_cleanup(Server, Acc, User, Resource, SID) ->
    JID = jid:make(User, Server, Resource),
    Params = #{jid => JID, sid => SID},
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(session_cleanup, HostType, Acc, Params).

%%% @doc The `set_vcard' hook is called when the caller wants to set the VCard.
-spec set_vcard(HostType, UserJID, VCard) -> Result when
    HostType :: mongooseim:host_type(),
    UserJID :: jid:jid(),
    VCard :: exml:element(),
    Result :: ok | {error, any()}.
set_vcard(HostType, UserJID, VCard) ->
    Params = #{user => UserJID, vcard => VCard},
    run_hook_for_host_type(set_vcard, HostType, {error, no_handler_defined}, Params).

-spec unacknowledged_message(Acc, JID) -> Result when
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Result :: mongoose_acc:t().
unacknowledged_message(Acc, JID) ->
    HostType = mongoose_acc:host_type(Acc),
    Params = #{jid => JID},
    run_hook_for_host_type(unacknowledged_message, HostType, Acc, Params).

-spec filter_unacknowledged_messages(HostType, Jid, Buffer) -> Result when
    HostType :: mongooseim:host_type(),
    Jid :: jid:jid(),
    Buffer :: [mongoose_acc:t()],
    Result :: [mongoose_acc:t()].
filter_unacknowledged_messages(HostType, Jid, Buffer) ->
    run_hook_for_host_type(filter_unacknowledged_messages, HostType, Buffer, #{jid => Jid}).

%%% @doc The `unregister_subhost' hook is called when a component
%%% is unregistered from ejabberd_router or a subdomain is removed from mongoose_subdomain_core.
-spec unregister_subhost(LDomain) -> Result when
    LDomain :: binary(),
    Result :: any().
unregister_subhost(LDomain) ->
    Params = #{ldomain => LDomain},
    run_global_hook(unregister_subhost, ok, Params).

-spec user_available_hook(Acc, JID) -> Result when
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Result :: mongoose_acc:t().
user_available_hook(Acc, JID) ->
    Params = #{jid => JID},
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(user_available_hook, HostType, Acc, Params).

%%% @doc The `user_ping_response' hook is called when a user responds to a ping, or times out
-spec user_ping_response(HostType, Acc, JID, Response, TDelta) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: simple_acc(),
    JID :: jid:jid(),
    Response :: timeout | exml:element(),
    TDelta :: non_neg_integer(),
    Result :: simple_acc().
user_ping_response(HostType, Acc, JID, Response, TDelta) ->
    Params = #{jid => JID, response => Response, time_delta => TDelta},
    run_hook_for_host_type(user_ping_response, HostType, Acc, Params).

%%% @doc The `vcard_set' hook is called to inform that the vcard
%%% has been set in mod_vcard backend.
-spec vcard_set(HostType, Server, LUser, VCard) -> Result when
    HostType :: mongooseim:host_type(),
    Server :: jid:server(),
    LUser :: jid:luser(),
    VCard :: exml:element(),
    Result :: any().
vcard_set(HostType, Server, LUser, VCard) ->
    JID = jid:make_bare(LUser, Server),
    Params = #{jid => JID, vcard => VCard},
    run_hook_for_host_type(vcard_set, HostType, ok, Params).

-spec xmpp_send_element(HostType, Acc, El) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    El :: exml:element(),
    Result :: mongoose_acc:t().
xmpp_send_element(HostType, Acc, El) ->
    Params = #{el => El},
    run_hook_for_host_type(xmpp_send_element, HostType, Acc, Params).

%%% @doc The `xmpp_stanza_dropped' hook is called to inform that
%%% an xmpp stanza has been dropped.
-spec xmpp_stanza_dropped(Acc, From, To, Packet) -> Result when
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: any().
xmpp_stanza_dropped(Acc, From, To, Packet) ->
    Params = #{from => From, to => To, packet => Packet},
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(xmpp_stanza_dropped, HostType, Acc, Params).

%% C2S related hooks

-spec get_pep_recipients(C2SData, Feature) -> Result when
    C2SData :: mongoose_c2s:data(),
    Feature :: binary(),
    Result :: [jid:simple_jid()].
get_pep_recipients(C2SData, Feature) ->
    Params = #{c2s_data => C2SData, feature => Feature},
    HostType = mongoose_c2s:get_host_type(C2SData),
    run_hook_for_host_type(get_pep_recipients, HostType, [], Params).

-spec filter_pep_recipient(C2SData, Feature, To) -> Result when
    C2SData :: mongoose_c2s:data(),
    Feature :: binary(),
    To :: jid:jid(),
    Result :: boolean().
filter_pep_recipient(C2SData, Feature, To) ->
    Params = #{c2s_data => C2SData, feature => Feature, to => To},
    HostType = mongoose_c2s:get_host_type(C2SData),
    run_hook_for_host_type(filter_pep_recipient, HostType, true, Params).

-spec c2s_stream_features(HostType, Params, InitialFeatures) -> Result when
    HostType :: mongooseim:host_type(),
    Params :: #{c2s_data := mongoose_c2s:data(), lserver := jid:lserver()},
    InitialFeatures :: [exml:element()],
    Result :: [exml:element()].
c2s_stream_features(HostType, Params, InitialFeatures) ->
    run_hook_for_host_type(c2s_stream_features, HostType, InitialFeatures, Params).

-spec sasl2_stream_features(C2SData, InitialFeatures) -> Result when
    C2SData :: mongoose_c2s:data(),
    InitialFeatures :: [exml:element()],
    Result :: [exml:element()].
sasl2_stream_features(C2SData, InitialFeatures) ->
    Params = #{c2s_data => C2SData},
    HostType = mongoose_c2s:get_host_type(C2SData),
    run_hook_for_host_type(sasl2_stream_features, HostType, InitialFeatures, Params).

-spec bind2_stream_features(C2SData, InitialFeatures) -> Result when
    C2SData :: mongoose_c2s:data(),
    InitialFeatures :: [exml:element()],
    Result :: [exml:element()].
bind2_stream_features(C2SData, InitialFeatures) ->
    Params = #{c2s_data => C2SData},
    HostType = mongoose_c2s:get_host_type(C2SData),
    run_hook_for_host_type(bind2_stream_features, HostType, InitialFeatures, Params).

-spec bind2_enable_features(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: mod_sasl2:c2s_state_data(),
    Result :: mongoose_acc:t().
bind2_enable_features(HostType, Acc, Params) ->
    run_hook_for_host_type(bind2_enable_features, HostType, Acc, Params).

%% This hook will cache in the accumulator all the requests from sasl2 inlined features
-spec sasl2_start(HostType, Acc, Element) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Element :: exml:element(),
    Result :: mongoose_acc:t().
sasl2_start(HostType, Acc, Element) ->
    Params = #{stanza => Element},
    run_hook_for_host_type(sasl2_start, HostType, Acc, Params).

%% If SASL authentication is successful, inline features can be triggered
-spec sasl2_success(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: mod_sasl2:c2s_state_data(),
    Result :: mongoose_acc:t().
sasl2_success(HostType, Acc, Params) ->
    run_hook_for_host_type(sasl2_success, HostType, Acc, Params).

-spec check_bl_c2s(IP) -> Result when
    IP ::  inet:ip_address(),
    Result :: boolean().
check_bl_c2s(IP) ->
    Params = #{ip => IP},
    run_global_hook(check_bl_c2s, false, Params).

-spec forbidden_session_hook(HostType, Acc, JID) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Result :: mongoose_acc:t().
forbidden_session_hook(HostType, Acc, JID) ->
    Params = #{jid => JID},
    run_hook_for_host_type(forbidden_session_hook, HostType, Acc, Params).

-spec session_opening_allowed_for_user(HostType, JID) -> Result when
    HostType :: mongooseim:host_type(),
    JID :: jid:jid(),
    Result :: allow | any(). %% anything else than 'allow' is interpreted
                             %% as not allowed
session_opening_allowed_for_user(HostType, JID) ->
    Params = #{jid => JID},
    run_hook_for_host_type(session_opening_allowed_for_user, HostType, allow, Params).

%% Privacy related hooks

-spec privacy_check_packet(Acc, JID, PrivacyList,
                           FromToNameType, Dir) -> Result when
    Acc :: mongoose_acc:t(), JID :: jid:jid(),
    PrivacyList :: mongoose_privacy:userlist(),
    FromToNameType :: {jid:jid(), jid:jid(), binary(), binary()},
    Dir :: in | out,
    Result :: mongoose_acc:t().
privacy_check_packet(Acc, JID, PrivacyList, FromToNameType, Dir) ->
    Params = #{jid => JID, privacy_list => PrivacyList,
               from_to_name_type => FromToNameType, dir => Dir},
    HostType = mongoose_acc:host_type(Acc),
    AccWithRes = mongoose_acc:set(hook, result, allow, Acc),
    run_hook_for_host_type(privacy_check_packet, HostType, AccWithRes, Params).

-spec privacy_get_user_list(HostType, JID) -> Result when
    HostType :: mongooseim:host_type(),
    JID :: jid:jid(),
    Result :: mongoose_privacy:userlist().
privacy_get_user_list(HostType, JID) ->
    Params = #{jid => JID},
    run_hook_for_host_type(privacy_get_user_list, HostType, #userlist{}, Params).

-spec privacy_iq_get(HostType, Acc, From, To, IQ, PrivList) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    IQ :: jlib:iq(),
    PrivList :: mongoose_privacy:userlist(),
    Result :: mongoose_acc:t().
privacy_iq_get(HostType, Acc, From, To, IQ, PrivList) ->
    Params = #{from => From, to => To, iq => IQ, priv_list => PrivList},
    run_hook_for_host_type(privacy_iq_get, HostType, Acc, Params).

-spec privacy_iq_set(HostType, Acc, From, To, IQ) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    IQ :: jlib:iq(),
    Result :: mongoose_acc:t().
privacy_iq_set(HostType, Acc, From, To, IQ) ->
    Params = #{from => From, to => To, iq => IQ},
    run_hook_for_host_type(privacy_iq_set, HostType, Acc, Params).

-spec privacy_updated_list(HostType, OldList, NewList) -> Result when
    HostType :: mongooseim:host_type(),
    OldList :: mongoose_privacy:userlist(),
    NewList :: mongoose_privacy:userlist(),
    Result :: false | mongoose_privacy:userlist().
privacy_updated_list(HostType, OldList, NewList) ->
    Params = #{old_list => OldList, new_list => NewList},
    run_hook_for_host_type(privacy_updated_list, HostType, false, Params).

-spec privacy_list_push(HostType, LUser, LServer, Item, SessionCount) -> Result when
    HostType :: mongooseim:host_type(),
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    Item :: term(),
    SessionCount :: non_neg_integer(),
    Result :: any().
privacy_list_push(HostType, LUser, LServer, Item, SessionCount) ->
    Params = #{luse => LUser, lserver => LServer, item => Item, session_count => SessionCount},
    run_hook_for_host_type(privacy_list_push, HostType, ok, Params).

%% Session management related hooks

-spec offline_groupchat_message_hook(Acc, From, To, Packet) -> Result when
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: mongoose_acc:t().
offline_groupchat_message_hook(Acc, From, To, Packet) ->
    Params = #{from => From, to => To, packet => Packet},
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(offline_groupchat_message_hook, HostType, Acc, Params).

-spec offline_message_hook(Acc, From, To, Packet) -> Result when
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: mongoose_acc:t().
offline_message_hook(Acc, From, To, Packet) ->
    Params = #{from => From, to => To, packet => Packet},
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(offline_message_hook, HostType, Acc, Params).

-spec set_presence_hook(Acc, JID, Presence) -> Result when
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Presence :: any(),
    Result :: mongoose_acc:t().
set_presence_hook(Acc, JID, Presence) ->
    Params = #{jid => JID, presence => Presence},
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(set_presence_hook, HostType, Acc, Params).

-spec sm_filter_offline_message(HostType, From, To, Packet) -> Result when
    HostType :: mongooseim:host_type(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: boolean().
sm_filter_offline_message(HostType, From, To, Packet) ->
    Params = #{from => From, to => To, packet => Packet},
    run_hook_for_host_type(sm_filter_offline_message, HostType, false, Params).

-spec sm_register_connection_hook(HostType, SID, JID, Info) -> Result when
    HostType :: mongooseim:host_type(),
    SID :: 'undefined' | ejabberd_sm:sid(),
    JID :: jid:jid(),
    Info :: ejabberd_sm:info(),
    Result :: ok.
sm_register_connection_hook(HostType, SID, JID, Info) ->
    Params = #{sid => SID, jid => JID, info => Info},
    run_hook_for_host_type(sm_register_connection_hook, HostType, ok, Params).

-spec sm_remove_connection_hook(Acc, SID, JID, Info, Reason) -> Result when
    Acc :: mongoose_acc:t(),
    SID :: 'undefined' | ejabberd_sm:sid(),
    JID :: jid:jid(),
    Info :: ejabberd_sm:info(),
    Reason :: ejabberd_sm:close_reason(),
    Result :: mongoose_acc:t().
sm_remove_connection_hook(Acc, SID, JID, Info, Reason) ->
    Params = #{sid => SID, jid => JID, info => Info, reason => Reason},
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(sm_remove_connection_hook, HostType, Acc, Params).

-spec unset_presence_hook(Acc, JID, Status) -> Result when
    Acc :: mongoose_acc:t(),
    JID:: jid:jid(),
    Status :: binary(),
    Result :: mongoose_acc:t().
unset_presence_hook(Acc, JID, Status) ->
    Params = #{jid => JID, status => Status},
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(unset_presence_hook, HostType, Acc, Params).

-spec xmpp_bounce_message(Acc) -> Result when
    Acc :: mongoose_acc:t(),
    Result :: mongoose_acc:t().
xmpp_bounce_message(Acc) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(xmpp_bounce_message, HostType, Acc, #{}).

%% Roster related hooks

%%% @doc The `roster_get' hook is called to extract a user's roster.
-spec roster_get(Acc, JID, Full) -> Result when
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Full :: boolean(),
    Result :: [mod_roster:roster()].
roster_get(Acc, JID, Full) ->
    Params = #{mongoose_acc => Acc, show_full_roster => Full, jid => JID},
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(roster_get, HostType, [], Params).

%%% @doc The `roster_groups' hook is called to extract roster groups.
-spec roster_groups(LServer) -> Result when
    LServer :: jid:lserver(),
    Result :: list().
roster_groups(LServer) ->
    Params = #{lserver => LServer},
    run_hook_for_host_type(roster_groups, LServer, [], Params).

%%% @doc The `roster_get_jid_info' hook is called to determine the
%%% subscription state between a given pair of users.
%%% The hook handlers need to expect following arguments:
%%% * Acc with an initial value of {none, []},
%%% * ToJID, a stringprepped roster's owner's jid
%%% * RemoteBareJID, a bare JID of the other user.
%%%
%%% The arguments and the return value types correspond to the following spec.
-spec roster_get_jid_info(HostType, ToJID, RemoteJID) -> Result when
      HostType :: mongooseim:host_type(),
      ToJID :: jid:jid(),
      RemoteJID :: jid:jid() | jid:simple_jid(),
      Result :: {mod_roster:subscription_state(), [binary()]}.
roster_get_jid_info(HostType, ToJID, RemBareJID) ->
    Params = #{to => ToJID, remote => RemBareJID},
    run_hook_for_host_type(roster_get_jid_info, HostType, {none, []}, Params).

%%% @doc The `roster_get_subscription_lists' hook is called to extract
%%% user's subscription list.
-spec roster_get_subscription_lists(HostType, Acc, JID) -> Result when
    HostType :: mongooseim:host_type(),
    Acc ::mongoose_acc:t(),
    JID :: jid:jid(),
    Result :: mongoose_acc:t().
roster_get_subscription_lists(HostType, Acc, JID) ->
    BareJID = jid:to_bare(JID),
    Params = #{jid => BareJID},
    run_hook_for_host_type(roster_get_subscription_lists, HostType, Acc, Params).

%%% @doc The `roster_get_versioning_feature' hook is
%%% called to determine if roster versioning is enabled.
-spec roster_get_versioning_feature(HostType) -> Result when
    HostType :: mongooseim:host_type(),
    Result :: [exml:element()].
roster_get_versioning_feature(HostType) ->
    run_hook_for_host_type(roster_get_versioning_feature, HostType, [], #{}).

%%% @doc The `roster_in_subscription' hook is called to determine
%%% if a subscription presence is routed to a user.
-spec roster_in_subscription(Acc, To, From, Type, Reason) -> Result when
    Acc :: mongoose_acc:t(),
    To :: jid:jid(),
    From :: jid:jid(),
    Type :: mod_roster:sub_presence(),
    Reason :: any(),
    Result :: mongoose_acc:t().
roster_in_subscription(Acc, To, From, Type, Reason) ->
    ToJID = jid:to_bare(To),
    Params = #{to => ToJID, from => From, type => Type, reason => Reason},
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(roster_in_subscription, HostType, Acc, Params).

%%% @doc The `roster_out_subscription' hook is called
%%% when a user sends out subscription.
-spec roster_out_subscription(Acc, From, To, Type) -> Result when
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Type :: mod_roster:sub_presence(),
    Result :: mongoose_acc:t().
roster_out_subscription(Acc, From, To, Type) ->
    FromJID = jid:to_bare(From),
    Params = #{to => To, from => FromJID, type => Type},
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(roster_out_subscription, HostType, Acc, Params).

%%% @doc The `roster_process_item' hook is called when a user's roster is set.
-spec roster_process_item(HostType, LServer, Item) -> Result when
    HostType :: mongooseim:host_type(),
    LServer :: jid:lserver(),
    Item :: mod_roster:roster(),
    Result :: mod_roster:roster().
roster_process_item(HostType, LServer, Item) ->
    Params = #{lserver => LServer},
    run_hook_for_host_type(roster_process_item, HostType, Item, Params).

%%% @doc The `roster_push' hook is called when a roster item is
%%% being pushed and roster versioning is not enabled.
-spec roster_push(HostType, From, Item) -> Result when
    HostType :: mongooseim:host_type(),
    From :: jid:jid(),
    Item :: mod_roster:roster(),
    Result :: any().
roster_push(HostType, From, Item) ->
    Params = #{from => From, item => Item},
    run_hook_for_host_type(roster_push, HostType, ok, Params).

%%% @doc The `roster_set' hook is called when a user's roster is set through an IQ.
-spec roster_set(HostType, From, To, SubEl) -> Result when
    HostType :: mongooseim:host_type(),
    From :: jid:jid(),
    To :: jid:jid(),
    SubEl :: exml:element(),
    Result :: any().
roster_set(HostType, From, To, SubEl) ->
    Params = #{from => From, to => To, sub_el => SubEl},
    run_hook_for_host_type(roster_set, HostType, ok, Params).

%% MUC related hooks

%%% @doc The `is_muc_room_owner' hooks is called to determine
%%% if a given user is a room's owner.
%%%
%%% The hook's handler needs to expect the following arguments:
%%% `Acc', `Room', `User'.
%%% The arguments and the return value types correspond to the
%%% following spec.
-spec is_muc_room_owner(HostType, Acc, Room, User) -> Result when
      HostType :: mongooseim:host_type(),
      Acc :: mongoose_acc:t(),
      Room :: jid:jid(),
      User :: jid:jid(),
      Result :: boolean().
is_muc_room_owner(HostType, Acc, Room, User) ->
    Params = #{acc => Acc, room => Room, user => User},
    run_hook_for_host_type(is_muc_room_owner, HostType, false, Params).

%%% @doc The `can_access_identity' hook is called to determine if
%%% a given user can see the real identity of the people in a room.
-spec can_access_identity(HostType, Room, User) -> Result when
      HostType :: mongooseim:host_type(),
      Room :: jid:jid(),
      User :: jid:jid(),
      Result :: boolean().
can_access_identity(HostType, Room, User) ->
    Params = #{room => Room, user => User},
    run_hook_for_host_type(can_access_identity, HostType, false, Params).

%%% @doc The `can_access_room' hook is called to determine
%%% if a given user can access a room.
-spec can_access_room(HostType, Acc, Room, User) -> Result when
      HostType :: mongooseim:host_type(),
      Acc :: mongoose_acc:t(),
      Room :: jid:jid(),
      User :: jid:jid(),
      Result :: boolean().
can_access_room(HostType, Acc, Room, User) ->
    Params = #{acc => Acc, room => Room, user => User},
    run_hook_for_host_type(can_access_room, HostType, false, Params).

-spec acc_room_affiliations(Acc, Room) -> NewAcc when
      Acc :: mongoose_acc:t(),
      Room :: jid:jid(),
      NewAcc :: mongoose_acc:t().
acc_room_affiliations(Acc, Room) ->
    Params = #{room => Room},
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    run_hook_for_host_type(acc_room_affiliations, HostType, Acc, Params).

-spec room_exists(HostType, Room) -> Result when
      HostType :: mongooseim:host_type(),
      Room :: jid:jid(),
      Result :: boolean().
room_exists(HostType, Room) ->
    Params = #{room => Room},
    run_hook_for_host_type(room_exists, HostType, false, Params).

-spec room_new_affiliations(Acc, Room, NewAffs, Version) -> NewAcc when
      Acc :: mongoose_acc:t(),
      Room :: jid:jid(),
      NewAffs :: mod_muc_light:aff_users(),
      Version :: binary(),
      NewAcc :: mongoose_acc:t().
room_new_affiliations(Acc, Room, NewAffs, Version) ->
    Params = #{room => Room, new_affs => NewAffs, version => Version},
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    run_hook_for_host_type(room_new_affiliations, HostType, Acc, Params).

%% MAM related hooks

%%% @doc The `mam_archive_id' hook is called to determine
%%% the integer id of an archive for a particular user or entity.
%%%
%%% If a MAM backend doesn't support or doesn't require archive IDs,
%%% `undefined' may be returned.
-spec mam_archive_id(HostType, Owner) -> Result when
      HostType :: mongooseim:host_type(),
      Owner :: jid:jid(),
      Result :: undefined | mod_mam:archive_id().
mam_archive_id(HostType, Owner) ->
    Params = #{owner => Owner},
    run_hook_for_host_type(mam_archive_id, HostType, undefined, Params).

%%% @doc The `mam_archive_size' hook is called to determine the size
%%% of the archive for a given JID
-spec mam_archive_size(HostType, ArchiveID, Owner) -> Result when
      HostType :: mongooseim:host_type(),
      ArchiveID :: undefined | mod_mam:archive_id(),
      Owner :: jid:jid(),
      Result :: integer().
mam_archive_size(HostType, ArchiveID, Owner) ->
    Params = #{archive_id => ArchiveID, owner => Owner},
    run_hook_for_host_type(mam_archive_size, HostType, 0, Params).

%%% @doc The `mam_get_behaviour' hooks is called to determine if a message
%%% should be archived or not based on a given pair of JIDs.
-spec mam_get_behaviour(HostType, ArchiveID,
                        Owner, Remote) -> Result when
      HostType :: mongooseim:host_type(),
      ArchiveID :: undefined | mod_mam:archive_id(),
      Owner :: jid:jid(),
      Remote :: jid:jid(),
      Result :: mod_mam:archive_behaviour().
mam_get_behaviour(HostType, ArchiveID, Owner, Remote) ->
    Params = #{archive_id => ArchiveID, owner => Owner, remote => Remote},
    run_hook_for_host_type(mam_get_behaviour, HostType, always, Params).

%%% @doc The `mam_set_prefs' hook is called to set a user's archive preferences.
%%%
%%% It's possible to set which JIDs are always or never allowed in the archive
-spec mam_set_prefs(HostType, ArchiveId, Owner,
                    DefaultMode, AlwaysJIDs, NeverJIDs) -> Result when
      HostType :: mongooseim:host_type(),
      ArchiveId :: undefined | mod_mam:archive_id(),
      Owner :: jid:jid(),
      DefaultMode :: mod_mam:archive_behaviour(),
      AlwaysJIDs :: [jid:literal_jid()],
      NeverJIDs :: [jid:literal_jid()],
      Result :: any().
mam_set_prefs(HostType,  ArchiveID, Owner, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    Params = #{archive_id => ArchiveID, owner => Owner,
               default_mode => DefaultMode, always_jids => AlwaysJIDs, never_jids => NeverJIDs},
    run_hook_for_host_type(mam_set_prefs, HostType, {error, not_implemented}, Params).

%%% @doc The `mam_get_prefs' hook is called to read
%%% the archive settings for a given user.
-spec mam_get_prefs(HostType, DefaultMode, ArchiveID, Owner) -> Result when
      HostType :: mongooseim:host_type(),
      DefaultMode :: mod_mam:archive_behaviour(),
      ArchiveID :: undefined | mod_mam:archive_id(),
      Owner :: jid:jid(),
      Result :: mod_mam:preference() | {error, Reason :: term()}.
mam_get_prefs(HostType, DefaultMode, ArchiveID, Owner) ->
    Params = #{archive_id => ArchiveID, owner => Owner},
    InitialAccValue = {DefaultMode, [], []}, %% mod_mam:preference() type
    run_hook_for_host_type(mam_get_prefs, HostType, InitialAccValue, Params).

%%% @doc The `mam_remove_archive' hook is called in order to
%%% remove the entire archive for a particular user.
-spec mam_remove_archive(HostType, ArchiveID, Owner) -> any() when
      HostType :: mongooseim:host_type(),
      ArchiveID :: undefined | mod_mam:archive_id(),
      Owner :: jid:jid().
mam_remove_archive(HostType, ArchiveID, Owner) ->
    Params = #{archive_id => ArchiveID, owner => Owner},
    run_hook_for_host_type(mam_remove_archive, HostType, ok, Params).

%%% @doc The `mam_lookup_messages' hook is to retrieve
%%% archived messages for given search parameters.
-spec mam_lookup_messages(HostType, Params) -> Result when
      HostType :: mongooseim:host_type(),
      Params :: map(),
      Result :: {ok, mod_mam:lookup_result()}.
mam_lookup_messages(HostType, Params) ->
    InitialLookupValue = {0, 0, []}, %% mod_mam:lookup_result() type
    run_hook_for_host_type(mam_lookup_messages, HostType, {ok, InitialLookupValue},
                           Params).

%%% @doc The `mam_archive_message' hook is called in order
%%% to store the message in the archive.
-spec mam_archive_message(HostType, Params) ->
    Result when
    HostType :: mongooseim:host_type(),
    Params :: mod_mam:archive_message_params(),
    Result :: ok | {error, timeout}.
mam_archive_message(HostType, Params) ->
    run_hook_for_host_type(mam_archive_message, HostType, ok, Params).

%%% @doc The `mam_flush_messages' hook is run after the async bulk write
%%% happens for messages despite the result of the write.
-spec mam_flush_messages(HostType :: mongooseim:host_type(),
                         MessageCount :: integer()) -> ok.
mam_flush_messages(HostType, MessageCount) ->
    Params = #{count => MessageCount},
    run_hook_for_host_type(mam_flush_messages, HostType, ok, Params).

%% @doc Waits until all pending messages are written
-spec mam_archive_sync(HostType :: mongooseim:host_type()) -> ok.
mam_archive_sync(HostType) ->
    run_hook_for_host_type(mam_archive_sync, HostType, ok, #{}).

%% @doc Notifies of a message retraction
-spec mam_retraction(mongooseim:host_type(),
                     mod_mam_utils:retraction_info(),
                     mod_mam:archive_message_params()) ->
    mod_mam_utils:retraction_info().
mam_retraction(HostType, RetractionInfo, Env) ->
    run_hook_for_host_type(mam_retraction, HostType, RetractionInfo, Env).

%% MAM MUC related hooks

%%% @doc The `mam_muc_archive_id' hook is called to determine the
%%% archive ID for a particular room.
%%% The hook handler is expected to accept the following arguments:
%%% * Acc with initial value `undefined',
%%% * Host as passed in `HooksServer' variable,
%%% * OwnerJID,
%%%
%%% and return an integer value corresponding to the given owner's archive.
%%%
%%% If a MAM backend doesn't support or doesn't require archive IDs,
%%% `undefined' may be returned.
-spec mam_muc_archive_id(HostType, Owner) -> Result when
      HostType :: mongooseim:host_type(),
      Owner :: jid:jid(),
      Result :: undefined | mod_mam:archive_id().
mam_muc_archive_id(HostType, Owner) ->
    Params = #{owner => Owner},
    run_hook_for_host_type(mam_muc_archive_id, HostType, undefined, Params).

%%% @doc The `mam_muc_archive_size' hook is called to determine
%%% the archive size for a given room.
-spec mam_muc_archive_size(HostType, ArchiveID, Room) -> Result when
      HostType :: mongooseim:host_type(),
      ArchiveID :: undefined | mod_mam:archive_id(),
      Room :: jid:jid(),
      Result :: integer().
mam_muc_archive_size(HostType, ArchiveID, Room) ->
    Params = #{archive_id => ArchiveID, room => Room},
    run_hook_for_host_type(mam_muc_archive_size, HostType, 0, Params).

%%% @doc The `mam_muc_get_behaviour' hooks is called to determine if a message should
%%% be archived or not based on the given room and user JIDs.
-spec mam_muc_get_behaviour(HostType, ArchiveID,
                            Room, Remote) -> Result when
      HostType :: mongooseim:host_type(),
      ArchiveID :: undefined | mod_mam:archive_id(),
      Room :: jid:jid(),
      Remote :: jid:jid(),
      Result :: mod_mam:archive_behaviour().
mam_muc_get_behaviour(HostType, ArchiveID, Room, Remote) ->
    Params = #{archive_id => ArchiveID, room => Room, remote => Remote},
    DefaultBehaviour = always, %% mod_mam:archive_behaviour() type
    run_hook_for_host_type(mam_muc_get_behaviour, HostType, DefaultBehaviour, Params).

%%% @doc The `mam_muc_set_prefs' hook is called to set a room's archive preferences.
%%%
%%% It's possible to set which JIDs are always or never allowed in the archive
-spec mam_muc_set_prefs(HostType, ArchiveId, Room,
                        DefaultMode, AlwaysJIDs, NeverJIDs) -> Result when
      HostType :: mongooseim:host_type(),
      ArchiveId :: undefined | mod_mam:archive_id(),
      Room :: jid:jid(),
      DefaultMode :: mod_mam:archive_behaviour(),
      AlwaysJIDs :: [jid:literal_jid()],
      NeverJIDs :: [jid:literal_jid()],
      Result :: any().
mam_muc_set_prefs(HostType, ArchiveID, Room, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    Params = #{archive_id => ArchiveID, room => Room, default_mode => DefaultMode,
               always_jids => AlwaysJIDs, never_jids => NeverJIDs},
    InitialAcc = {error, not_implemented},
    run_hook_for_host_type(mam_muc_set_prefs, HostType, InitialAcc, Params).

%%% @doc The `mam_muc_get_prefs' hook is called to read
%%% the archive settings for a given room.
-spec mam_muc_get_prefs(HostType, DefaultMode, ArchiveID, Room) -> Result when
      HostType :: mongooseim:host_type(),
      DefaultMode :: mod_mam:archive_behaviour(),
      ArchiveID :: undefined | mod_mam:archive_id(),
      Room :: jid:jid(),
      Result :: mod_mam:preference() | {error, Reason :: term()}.
mam_muc_get_prefs(HostType, DefaultMode, ArchiveID, Room) ->
    Params = #{archive_id => ArchiveID, room => Room},
    InitialAcc = {DefaultMode, [], []}, %% mod_mam:preference() type
    run_hook_for_host_type(mam_muc_get_prefs, HostType, InitialAcc, Params).

%%% @doc The `mam_muc_remove_archive' hook is called in order to remove the entire
%%% archive for a particular user.
-spec mam_muc_remove_archive(HostType, ArchiveID, Room) -> any() when
      HostType :: mongooseim:host_type(),
      ArchiveID :: undefined | mod_mam:archive_id(),
      Room :: jid:jid().
mam_muc_remove_archive(HostType, ArchiveID, Room) ->
    Params = #{archive_id => ArchiveID, room => Room},
    run_hook_for_host_type(mam_muc_remove_archive, HostType, ok, Params).

%%% @doc The `mam_muc_lookup_messages' hook is to retrieve archived
%%% MUC messages for any given search parameters.
-spec mam_muc_lookup_messages(HostType, Params) -> Result when
      HostType :: mongooseim:host_type(),
      Params :: map(),
      Result :: {ok, mod_mam:lookup_result()}.
mam_muc_lookup_messages(HostType, Params) ->
    InitialLookupValue = {0, 0, []}, %% mod_mam:lookup_result() type
    run_hook_for_host_type(mam_muc_lookup_messages, HostType, {ok, InitialLookupValue},
                           Params).

%%% @doc The `mam_muc_archive_message' hook is called in order
%%% to store the MUC message in the archive.
-spec mam_muc_archive_message(HostType, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Params :: mod_mam:archive_message_params(),
    Result :: ok | {error, timeout}.
mam_muc_archive_message(HostType, Params) ->
    run_hook_for_host_type(mam_muc_archive_message, HostType, ok, Params).

%%% @doc The `mam_muc_flush_messages' hook is run after the async bulk write
%%% happens for MUC messages despite the result of the write.
-spec mam_muc_flush_messages(HostType :: mongooseim:host_type(),
                             MessageCount :: integer()) -> ok.
mam_muc_flush_messages(HostType, MessageCount) ->
    Params = #{count => MessageCount},
    run_hook_for_host_type(mam_muc_flush_messages, HostType, ok, Params).

%% @doc Waits until all pending messages are written
-spec mam_muc_archive_sync(HostType :: mongooseim:host_type()) -> ok.
mam_muc_archive_sync(HostType) ->
    run_hook_for_host_type(mam_muc_archive_sync, HostType, ok, #{}).

%% @doc Notifies of a muc message retraction
-spec mam_muc_retraction(mongooseim:host_type(),
                         mod_mam_utils:retraction_info(),
                         mod_mam:archive_message_params()) ->
    mod_mam_utils:retraction_info().
mam_muc_retraction(HostType, RetractionInfo, Env) ->
    run_hook_for_host_type(mam_muc_retraction, HostType, RetractionInfo, Env).

%% GDPR related hooks

%%% @doc `get_mam_pm_gdpr_data' hook is called to provide
%%% a user's archive for GDPR purposes.
-spec get_mam_pm_gdpr_data(HostType, JID) -> Result when
      HostType :: mongooseim:host_type(),
      JID :: jid:jid(),
      Result :: ejabberd_gen_mam_archive:mam_pm_gdpr_data().
get_mam_pm_gdpr_data(HostType, JID) ->
    Params = #{jid => JID},
    run_hook_for_host_type(get_mam_pm_gdpr_data, HostType, [], Params).

%%% @doc `get_mam_muc_gdpr_data' hook is called to provide
%%% a user's archive for GDPR purposes.
-spec get_mam_muc_gdpr_data(HostType, JID) -> Result when
      HostType :: mongooseim:host_type(),
      JID :: jid:jid(),
      Result :: ejabberd_gen_mam_archive:mam_muc_gdpr_data().
get_mam_muc_gdpr_data(HostType, JID) ->
    Params = #{jid => JID},
    run_hook_for_host_type(get_mam_muc_gdpr_data, HostType, [], Params).

%%% @doc `get_personal_data' hook is called to retrieve
%%% a user's personal data for GDPR purposes.
-spec get_personal_data(HostType, JID) -> Result when
    HostType :: mongooseim:host_type(),
    JID :: jid:jid(),
    Result :: gdpr:personal_data().
get_personal_data(HostType, JID) ->
    Params = #{jid => JID},
    run_hook_for_host_type(get_personal_data, HostType, [], Params).

%% S2S related hooks

%%% @doc `s2s_allow_host' hook is called to check whether a server
%%% should be allowed to be connected to.
%%%
%%% A handler can decide that a server should not be allowed and pass this
%%% information to the caller.
-spec s2s_allow_host(MyHost, S2SHost) -> Result when
    MyHost :: jid:server(),
    S2SHost :: jid:server(),
    Result :: allow | deny.
s2s_allow_host(MyHost, S2SHost) ->
    Params = #{my_host => MyHost, s2s_host => S2SHost},
    run_global_hook(s2s_allow_host, allow, Params).

%%% @doc `s2s_send_packet' hook is called when a message is routed.
-spec s2s_send_packet(Acc, From, To, Packet) -> Result when
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: mongoose_acc:t().
s2s_send_packet(Acc, From, To, Packet) ->
    Params = #{from => From, to => To, packet => Packet},
    run_global_hook(s2s_send_packet, Acc, Params).

%%% @doc `s2s_stream_features' hook is used to extract
%%% the stream management features supported by the server.
-spec s2s_stream_features(HostType, LServer) -> Result when
    HostType :: mongooseim:host_type(),
    LServer :: jid:lserver(),
    Result :: [exml:element()].
s2s_stream_features(HostType, LServer) ->
    Params = #{lserver => LServer},
    run_hook_for_host_type(s2s_stream_features, HostType, [], Params).

%%% @doc `s2s_receive_packet' hook is called when
%%% an incoming stanza is routed by the server.
-spec s2s_receive_packet(Acc) -> Result when
    Acc :: mongoose_acc:t(),
    Result :: mongoose_acc:t().
s2s_receive_packet(Acc) ->
    run_global_hook(s2s_receive_packet, Acc, #{}).

%% Discovery related hooks

%%% @doc `disco_local_identity' hook is called to get the identity of the server.
-spec disco_local_identity(mongoose_disco:identity_acc()) ->
    mongoose_disco:identity_acc().
disco_local_identity(Acc = #{host_type := HostType}) ->
    run_hook_for_host_type(disco_local_identity, HostType, Acc, #{}).

%%% @doc `disco_sm_identity' hook is called to get the identity of the
%%% client when a discovery IQ gets to session management.
-spec disco_sm_identity(mongoose_disco:identity_acc()) -> mongoose_disco:identity_acc().
disco_sm_identity(Acc = #{host_type := HostType}) ->
    run_hook_for_host_type(disco_sm_identity, HostType, Acc, #{}).

%%% @doc `disco_local_items' hook is called to extract items associated with the server.
-spec disco_local_items(mongoose_disco:item_acc()) -> mongoose_disco:item_acc().
disco_local_items(Acc = #{host_type := HostType}) ->
    run_hook_for_host_type(disco_local_items, HostType, Acc, #{}).

%%% @doc `disco_sm_items' hook is called to get the items associated
%%% with the client when a discovery IQ gets to session management.
-spec disco_sm_items(mongoose_disco:item_acc()) -> mongoose_disco:item_acc().
disco_sm_items(Acc = #{host_type := HostType}) ->
    run_hook_for_host_type(disco_sm_items, HostType, Acc, #{}).

%%% @doc `disco_local_features' hook is called to extract features
%%% offered by the server.
-spec disco_local_features(mongoose_disco:feature_acc()) -> mongoose_disco:feature_acc().
disco_local_features(Acc = #{host_type := HostType}) ->
    run_hook_for_host_type(disco_local_features, HostType, Acc, #{}).

%%% @doc `disco_sm_features' hook is called to get the features of the client
%%% when a discovery IQ gets to session management.
-spec disco_sm_features(mongoose_disco:feature_acc()) -> mongoose_disco:feature_acc().
disco_sm_features(Acc = #{host_type := HostType}) ->
    run_hook_for_host_type(disco_sm_features, HostType, Acc, #{}).

%%% @doc `disco_muc_features' hook is called to get the features
%%% supported by the MUC (Light) service.
-spec disco_muc_features(mongoose_disco:feature_acc()) -> mongoose_disco:feature_acc().
disco_muc_features(Acc = #{host_type := HostType}) ->
    run_hook_for_host_type(disco_muc_features, HostType, Acc, #{}).

%%% @doc `disco_info' hook is called to extract information about the server.
-spec disco_info(mongoose_disco:info_acc()) -> mongoose_disco:info_acc().
disco_info(Acc = #{host_type := HostType}) ->
    run_hook_for_host_type(disco_info, HostType, Acc, #{}).

%% AMP related hooks

%%% @doc The `amp_check_condition' hook is called to determine whether
%%% the AMP strategy matches the given AMP rule.
-spec amp_check_condition(HostType, Strategy, Rule) -> Result when
    HostType :: mongooseim:host_type(),
    Strategy :: mod_amp:amp_strategy(),
    Rule :: mod_amp:amp_rule(),
    Result :: mod_amp:amp_match_result().
amp_check_condition(HostType, Strategy, Rule) ->
    Params = #{strategy => Strategy, rule => Rule},
    InitialAcc = no_match, %% mod_amp:amp_match_result() type
    run_hook_for_host_type(amp_check_condition, HostType, InitialAcc, Params).

%%% @doc The `amp_determine_strategy' hook is called when checking to determine
%%% which strategy will be chosen when executing AMP rules.
-spec amp_determine_strategy(HostType, From, To, Packet, Event) -> Result when
    HostType :: mongooseim:host_type(),
    From :: jid:jid(),
    To :: jid:jid() | undefined,
    Packet :: exml:element(),
    Event :: mod_amp:amp_event(),
    Result :: mod_amp:amp_strategy().
amp_determine_strategy(HostType, From, To, Packet, Event) ->
    Params = #{from => From, to => To, packet => Packet, event => Event},
    DefaultStrategy = amp_strategy:null_strategy(),
    run_hook_for_host_type(amp_determine_strategy, HostType, DefaultStrategy, Params).

%%% @doc The `amp_verify_support' hook is called when checking
%%% whether the host supports given AMP rules.
-spec amp_verify_support(HostType, Rules) -> Result when
    HostType :: mongooseim:host_type(),
    Rules :: mod_amp:amp_rules(),
    Result :: [mod_amp:amp_rule_support()].
amp_verify_support(HostType, Rules) ->
    Params = #{rules => Rules},
    run_hook_for_host_type(amp_verify_support, HostType, [], Params).

%% MUC and MUC Light related hooks

-spec filter_room_packet(HostType, Packet, EventData) -> Result when
    HostType :: mongooseim:host_type(),
    Packet :: exml:element(),
    EventData :: mod_muc:room_event_data(),
    Result :: exml:element().
filter_room_packet(HostType, Packet, EventData) ->
    run_hook_for_host_type(filter_room_packet, HostType, Packet, EventData).

%%% @doc The `forget_room' hook is called when a room is removed from the database.
-spec forget_room(HostType, MucHost, Room) -> Result when
    HostType :: mongooseim:host_type(),
    MucHost :: jid:server(),
    Room :: jid:luser(),
    Result :: any().
forget_room(HostType, MucHost, Room) ->
    Params = #{muc_host => MucHost, room => Room},
    run_hook_for_host_type(forget_room, HostType, #{}, Params).

-spec invitation_sent(HookServer, Host, RoomJID, From, To, Reason) -> Result when
    HookServer :: jid:server(),
    Host :: jid:server(),
    RoomJID :: jid:jid(),
    From :: jid:jid(),
    To :: jid:jid(),
    Reason :: binary(),
    Result :: any().
invitation_sent(HookServer, Host, RoomJID, From, To, Reason) ->
    Params = #{host => Host, room_jid => RoomJID, from => From, to => To, reason => Reason},
    run_hook_for_host_type(invitation_sent, HookServer, ok, Params).

%%% @doc The `join_room' hook is called when a user joins a MUC room.
-spec join_room(HookServer, Room, Host, JID, MucJID) -> Result when
    HookServer :: jid:server(),
    Room :: mod_muc:room(),
    Host :: jid:server(),
    JID :: jid:jid(),
    MucJID :: jid:jid(),
    Result :: any().
join_room(HookServer, Room, Host, JID, MucJID) ->
    Params = #{room => Room, host => Host, jid => JID, muc_jid => MucJID},
    run_hook_for_host_type(join_room, HookServer, ok, Params).

%%% @doc The `leave_room' hook is called when a user joins a MUC room.
-spec leave_room(HookServer, Room, Host, JID, MucJID) -> Result when
    HookServer :: jid:server(),
    Room :: mod_muc:room(),
    Host :: jid:server(),
    JID :: jid:jid(),
    MucJID :: jid:jid(),
    Result :: any().
leave_room(HookServer, Room, Host, JID, MucJID) ->
    Params = #{room => Room, host => Host, jid => JID, muc_jid => MucJID},
    run_hook_for_host_type(leave_room, HookServer, ok, Params).

%%% @doc The `room_packet' hook is called when a message is added to room's history.
-spec room_packet(Server, FromNick, FromJID, JID, Packet) -> Result when
    Server :: jid:lserver(),
    FromNick :: mod_muc:nick(),
    FromJID :: jid:jid(),
    JID :: jid:jid(),
    Packet :: exml:element(),
    Result :: any().
room_packet(Server, FromNick, FromJID, JID, Packet) ->
    Params = #{from_nick => FromNick, from_jid => FromJID, jid => JID, packet => Packet},
    run_hook_for_host_type(room_packet, Server, ok, Params).

-spec update_inbox_for_muc(HostType, Info) -> Result when
    HostType :: mongooseim:host_type(),
    Info :: mod_muc_room:update_inbox_for_muc_payload(),
    Result :: mod_muc_room:update_inbox_for_muc_payload().
update_inbox_for_muc(HostType, Info) ->
    run_hook_for_host_type(update_inbox_for_muc, HostType, Info, #{}).

%% Caps related hooks

-spec caps_recognised(Acc, From, Pid, Features) -> Result when
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    Pid :: pid(),
    Features :: unknown | list(),
    Result :: mongoose_acc:t().
caps_recognised(Acc, From, Pid, Features) ->
    Params = #{from => From, pid => Pid, features => Features},
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(caps_recognised, HostType, Acc, Params).

%% Global distribution related hooks

%%% @doc The `mod_global_distrib_known_recipient' hook is called when
%%% the recipient is known to `global_distrib'.
-spec mod_global_distrib_known_recipient(GlobalHost, From, To, LocalHost) -> Result when
    GlobalHost :: jid:server(),
    From :: jid:jid(),
    To :: jid:jid(),
    LocalHost :: jid:server(),
    Result :: any().
mod_global_distrib_known_recipient(GlobalHost, From, To, LocalHost) ->
    Params = #{from => From, to => To, target_host => LocalHost},
    run_hook_for_host_type(mod_global_distrib_known_recipient, GlobalHost, ok, Params).

%%% @doc The `mod_global_distrib_unknown_recipient' hook is called when
%%% the recipient is unknown to `global_distrib'.
-spec mod_global_distrib_unknown_recipient(GlobalHost, Info) -> Result when
    GlobalHost :: jid:server(),
    Info :: filter_packet_acc(),
    Result :: any().
mod_global_distrib_unknown_recipient(GlobalHost, Info) ->
    run_hook_for_host_type(mod_global_distrib_unknown_recipient, GlobalHost, Info, #{}).


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
run_global_hook(HookName, Acc, Params) when is_map(Params) ->
    run_fold(HookName, global, Acc, Params).

run_hook_for_host_type(HookName, undefined, Acc, Args) ->
    ?LOG_ERROR(#{what => undefined_host_type,
                 text => <<"Running hook for an undefined host type">>,
                 hook_name => HookName, hook_acc => Acc, hook_args => Args}),
    Acc;
run_hook_for_host_type(HookName, HostType, Acc, Params) when is_binary(HostType),
                                                             is_map(Params) ->
    run_fold(HookName, HostType, Acc, Params).

run_fold(HookName, HostType, Acc, Params) when is_map(Params) ->
    {_, RetValue} = gen_hook:run_fold(HookName, HostType, Acc, Params),
    RetValue.
