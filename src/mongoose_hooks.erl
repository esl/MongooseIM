%%% @doc Hooks wrapper providing clear specifications for a hook caller.
%%%
%%% Every hook has its own function in this module with specs as accurate as
%%% possible. This helps to have a static analysis of the hooks callers to
%%% make sure they pass the expected arguments.
-module(mongoose_hooks).

-include("jlib.hrl").
-include("mod_privacy.hrl").
-include("mongoose.hrl").

-export([adhoc_local_commands/4,
         adhoc_sm_commands/4,
         anonymous_purge_hook/3,
         auth_failed/3,
         does_user_exist/3,
         ejabberd_ctl_process/2,
         failed_to_store_message/1,
         filter_local_packet/1,
         filter_packet/1,
         inbox_unread_count/3,
         extend_inbox_message/3,
         get_key/2,
         packet_to_component/3,
         presence_probe_hook/5,
         push_notifications/4,
         register_command/1,
         register_subhost/2,
         register_user/3,
         remove_user/3,
         resend_offline_messages_hook/2,
         rest_user_send_packet/4,
         session_cleanup/5,
         set_vcard/3,
         unacknowledged_message/2,
         filter_unacknowledged_messages/3,
         unregister_command/1,
         unregister_subhost/1,
         user_available_hook/2,
         user_ping_response/5,
         user_ping_timeout/2,
         user_receive_packet/6,
         user_sent_keep_alive/2,
         user_send_packet/4,
         vcard_set/4,
         xmpp_send_element/3,
         xmpp_stanza_dropped/4]).

-export([user_send_packet/3,
         user_send_message/3,
         user_send_iq/3,
         user_send_presence/3,
         user_send_non_stanza/3]).

-export([c2s_broadcast_recipients/4,
         c2s_filter_packet/4,
         c2s_preprocessing_hook/3,
         c2s_presence_in/4,
         c2s_stream_features/2,
         c2s_unauthenticated_iq/4,
         c2s_update_presence/2,
         check_bl_c2s/1,
         forbidden_session_hook/3,
         session_opening_allowed_for_user/2]).

-export([privacy_check_packet/5,
         privacy_get_user_list/2,
         privacy_iq_get/6,
         privacy_iq_set/5,
         privacy_updated_list/3]).

-export([offline_groupchat_message_hook/4,
         offline_message_hook/4,
         set_presence_hook/3,
         sm_broadcast/5,
         sm_filter_offline_message/4,
         sm_register_connection_hook/4,
         sm_remove_connection_hook/5,
         unset_presence_hook/3,
         xmpp_bounce_message/1]).

-export([roster_get/2,
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

-export([find_s2s_bridge/2,
         s2s_allow_host/2,
         s2s_connect_hook/2,
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

-export([pubsub_create_node/5,
         pubsub_delete_node/4,
         pubsub_publish_item/6]).

-export([mod_global_distrib_known_recipient/4,
         mod_global_distrib_unknown_recipient/2]).

-export([c2s_remote_hook/5]).

-export([remove_domain/2,
         node_cleanup/1]).

-ignore_xref([node_cleanup/1, remove_domain/2]).
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

-spec c2s_remote_hook(HostType, Tag, Args, HandlerState, C2SState) -> Result when
    HostType :: binary(),
    Tag :: atom(),
    Args :: term(),
    HandlerState :: term(),
    C2SState :: ejabberd_c2s:state(),
    Result :: term(). % ok | empty_state | HandlerState
c2s_remote_hook(HostType, Tag, Args, HandlerState, C2SState) ->
    run_hook_for_host_type(c2s_remote_hook, HostType, HandlerState,
                           [Tag, Args, C2SState]).

-spec adhoc_local_commands(HostType, From, To, AdhocRequest) -> Result when
    HostType :: mongooseim:host_type(),
    From :: jid:jid(),
    To :: jid:jid(),
    AdhocRequest :: adhoc:request(),
    Result :: mod_adhoc:command_hook_acc().
adhoc_local_commands(HostType, From, To, AdhocRequest) ->
    run_hook_for_host_type(adhoc_local_commands, HostType, empty,
                           [From, To, AdhocRequest]).

-spec adhoc_sm_commands(HostType, From, To, AdhocRequest) -> Result when
    HostType :: mongooseim:host_type(),
    From :: jid:jid(),
    To :: jid:jid(),
    AdhocRequest :: adhoc:request(),
    Result :: mod_adhoc:command_hook_acc().
adhoc_sm_commands(HostType, From, To, AdhocRequest) ->
    run_hook_for_host_type(adhoc_sm_commands, HostType, empty, [From, To, AdhocRequest]).

%%% @doc The `anonymous_purge_hook' hook is called when anonymous user's data is removed.
-spec anonymous_purge_hook(LServer, Acc, LUser) -> Result when
    LServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    LUser :: jid:user(),
    Result :: mongose_acc:t().
anonymous_purge_hook(LServer, Acc, LUser) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(anonymous_purge_hook, HostType, Acc, [LUser, LServer]).

-spec auth_failed(HostType, Server, Username) -> Result when
    HostType :: binary(),
    Server :: jid:server(),
    Username :: jid:user() | unknown,
    Result :: ok.
auth_failed(HostType, Server, Username) ->
    run_hook_for_host_type(auth_failed, HostType, ok, [Username, Server]).

-spec does_user_exist(HostType, Jid, RequestType) -> Result when
      HostType :: mongooseim:host_type(),
      Jid :: jid:jid(),
      RequestType :: ejabberd_auth:exist_type(),
      Result :: boolean().
does_user_exist(HostType, Jid, RequestType) ->
    run_hook_for_host_type(does_user_exist, HostType, false,
                           [HostType, Jid, RequestType]).

-spec remove_domain(HostType, Domain) -> Result when
    HostType :: binary(),
    Domain :: jid:lserver(),
    Result :: ok.
remove_domain(HostType, Domain) ->
    run_hook_for_host_type(remove_domain, HostType, #{}, [HostType, Domain]).

-spec node_cleanup(Node :: node()) -> Acc :: map().
node_cleanup(Node) ->
    Params = #{node => Node},
    Args = [Node],
    ParamsWithLegacyArgs = ejabberd_hooks:add_args(Params, Args),
    run_global_hook(node_cleanup, #{}, ParamsWithLegacyArgs).

-spec ejabberd_ctl_process(Acc, Args) -> Result when
    Acc :: any(),
    Args :: [string()],
    Result :: any().
ejabberd_ctl_process(Acc, Args) ->
    run_global_hook(ejabberd_ctl_process, Acc, [Args]).

-spec failed_to_store_message(Acc) -> Result when
    Acc :: mongoose_acc:t(),
    Result :: mongoose_acc:t().
failed_to_store_message(Acc) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(failed_to_store_message, HostType, Acc, []).

%%% @doc The `filter_local_packet' hook is called to filter out
%%% stanzas routed with `mongoose_local_delivery'.
-spec filter_local_packet(FilterAcc) -> Result when
    FilterAcc :: filter_packet_acc(),
    Result :: drop | filter_packet_acc().
filter_local_packet(FilterAcc = {_From, _To, Acc, _Packet}) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(filter_local_packet, HostType, FilterAcc, []).

%%% @doc The `filter_packet' hook is called to filter out
%%% stanzas routed with `mongoose_router_global'.
-spec filter_packet(Acc) -> Result when
    Acc :: filter_packet_acc(),
    Result ::  drop | filter_packet_acc().
filter_packet(Acc) ->
    run_global_hook(filter_packet, Acc, []).

%%% @doc The `inbox_unread_count' hook is called to get the number
%%% of unread messages in the inbox for a user.
-spec inbox_unread_count(LServer, Acc, User) -> Result when
    LServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    User :: jid:jid(),
    Result :: mongoose_acc:t().
inbox_unread_count(LServer, Acc, User) ->
    run_hook_for_host_type(inbox_unread_count, LServer, Acc, [User]).

-spec extend_inbox_message(mongoose_acc:t(), mod_inbox:inbox_res(), jlib:iq()) ->
    [exml:element()].
extend_inbox_message(MongooseAcc, InboxRes, IQ) ->
    HostType = mongoose_acc:host_type(MongooseAcc),
    HookParams = #{mongoose_acc => MongooseAcc, inbox_res => InboxRes, iq => IQ},
    run_fold(extend_inbox_message, HostType, [], HookParams).

%%% @doc The `get_key' hook is called to extract a key from `mod_keystore'.
-spec get_key(HostType, KeyName) -> Result when
    HostType :: mongooseim:host_type(),
    KeyName :: atom(),
    Result :: mod_keystore:key_list().
get_key(HostType, KeyName) ->
    run_hook_for_host_type(get_key, HostType, [], [{KeyName, HostType}]).

-spec packet_to_component(Acc, From, To) -> Result when
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Result :: mongoose_acc:t().
packet_to_component(Acc, From, To) ->
    run_global_hook(packet_to_component, Acc, [From, To]).

-spec presence_probe_hook(HostType, Acc, From, To, Pid) -> Result when
    HostType :: binary(),
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Pid :: pid(),
    Result :: mongoose_acc:t().
presence_probe_hook(HostType, Acc, From, To, Pid) ->
    run_hook_for_host_type(presence_probe_hook, HostType, Acc, [From, To, Pid]).

%%% @doc The `push_notifications' hook is called to push notifications.
-spec push_notifications(HostType, Acc, NotificationForms, Options) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: ok | mongoose_acc:t(),
    NotificationForms :: [#{atom() => binary()}],
    Options :: #{atom() => binary()},
    Result :: ok | {error, any()}.
push_notifications(HostType, Acc, NotificationForms, Options) ->
    Params = #{host_type => HostType, options => Options,
               notification_forms => NotificationForms},
    Args = [HostType, NotificationForms, Options],
    ParamsWithLegacyArgs = ejabberd_hooks:add_args(Params, Args),
    run_hook_for_host_type(push_notifications, HostType, Acc, ParamsWithLegacyArgs).

%%% @doc The `register_command' hook is called when a command
%%% is registered in `mongoose_commands'.
-spec register_command(Command) -> Result when
    Command :: mongoose_commands:t(),
    Result :: drop.
register_command(Command) ->
    run_global_hook(register_command, Command, []).

%%% @doc The `register_subhost' hook is called when a component
%%% is registered for ejabberd_router or a subdomain is added to mongoose_subdomain_core.
-spec register_subhost(LDomain, IsHidden) -> Result when
    LDomain :: binary(),
    IsHidden :: boolean(),
    Result :: any().
register_subhost(LDomain, IsHidden) ->
    run_global_hook(register_subhost, ok, [LDomain, IsHidden]).

%%% @doc The `register_user' hook is called when a user is successfully
%%% registered in an authentication backend.
-spec register_user(HostType, LServer, LUser) -> Result when
    HostType :: binary(),
    LServer :: jid:lserver(),
    LUser :: jid:luser(),
    Result :: any().
register_user(HostType, LServer, LUser) ->
    run_hook_for_host_type(register_user, HostType, ok, [LUser, LServer]).

%%% @doc The `remove_user' hook is called when a user is removed.
-spec remove_user(Acc, LServer, LUser) -> Result when
    Acc :: mongoose_acc:t(),
    LServer :: jid:lserver(),
    LUser :: jid:luser(),
    Result :: mongoose_acc:t().
remove_user(Acc, LServer, LUser) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(remove_user, HostType, Acc, [LUser, LServer]).

-spec resend_offline_messages_hook(Acc, JID) -> Result when
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Result :: mongoose_acc:t().
resend_offline_messages_hook(Acc, JID) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(resend_offline_messages_hook, HostType, Acc, [JID]).

%%% @doc The `rest_user_send_packet' hook is called when a user sends
%%% a message using the REST API.
-spec rest_user_send_packet(Acc, From, To, Packet) -> Result when
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: mongoose_acc:t().
rest_user_send_packet(Acc, From, To, Packet) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(rest_user_send_packet, HostType, Acc, [From, To, Packet]).

%%% @doc The `session_cleanup' hook is called when sm backend cleans up a user's session.
-spec session_cleanup(Server, Acc, User, Resource, SID) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    User :: jid:user(),
    Resource :: jid:resource(),
    SID :: ejabberd_sm:sid(),
    Result :: mongoose_acc:t().
session_cleanup(Server, Acc, User, Resource, SID) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(session_cleanup, HostType, Acc,
                           [User, Server, Resource, SID]).

%%% @doc The `set_vcard' hook is called when the caller wants to set the VCard.
-spec set_vcard(HostType, UserJID, VCard) -> Result when
    HostType :: mongooseim:host_type(),
    UserJID :: jid:jid(),
    VCard :: exml:element(),
    Result :: ok | {error, any()}.
set_vcard(HostType, UserJID, VCard) ->
    run_hook_for_host_type(set_vcard, HostType, {error, no_handler_defined},
                           [HostType, UserJID, VCard]).

-spec unacknowledged_message(Acc, JID) -> Result when
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Result :: mongoose_acc:t().
unacknowledged_message(Acc, JID) ->
    HostType = mongoose_acc:host_type(Acc),
    Params = #{jid => JID},
    Args = [JID],
    ParamsWithLegacyArgs = ejabberd_hooks:add_args(Params, Args),
    run_hook_for_host_type(unacknowledged_message, HostType, Acc, ParamsWithLegacyArgs).

-spec filter_unacknowledged_messages(HostType, Jid, Buffer) -> Result when
    HostType :: mongooseim:host_type(),
    Jid :: jid:jid(),
    Buffer :: [mongoose_acc:t()],
    Result :: [mongoose_acc:t()].
filter_unacknowledged_messages(HostType, Jid, Buffer) ->
    run_fold(filter_unacknowledged_messages, HostType, Buffer, #{jid => Jid}).

%%% @doc The `unregister_command' hook is called when a command
%%% is unregistered from `mongoose_commands'.
-spec unregister_command(Command) -> Result when
    Command :: mongoose_commands:t(),
    Result :: drop.
unregister_command(Command) ->
    run_global_hook(unregister_command, Command, []).

%%% @doc The `unregister_subhost' hook is called when a component
%%% is unregistered from ejabberd_router or a subdomain is removed from mongoose_subdomain_core.
-spec unregister_subhost(LDomain) -> Result when
    LDomain :: binary(),
    Result :: any().
unregister_subhost(LDomain) ->
    run_global_hook(unregister_subhost, ok, [LDomain]).

-spec user_available_hook(Acc, JID) -> Result when
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Result :: mongoose_acc:t().
user_available_hook(Acc, JID) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(user_available_hook, HostType, Acc, [JID]).

%%% @doc The `user_ping_response' hook is called when a user responds to a ping.
-spec user_ping_response(HostType, Acc, JID, Response, TDelta) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Response :: timeout | jlib:iq(),
    TDelta :: non_neg_integer(),
    Result :: mongoose_acc:t().
user_ping_response(HostType, Acc, JID, Response, TDelta) ->
    Params = #{jid => JID, response => Response, time_delta => TDelta},
    Args =  [HostType, JID, Response, TDelta],
    ParamsWithLegacyArgs = ejabberd_hooks:add_args(Params, Args),
    run_hook_for_host_type(user_ping_response, HostType, Acc, ParamsWithLegacyArgs).

%%% @doc The `user_ping_timeout' hook is called when there is a timeout
%%% when waiting for a ping response from a user.
-spec user_ping_timeout(HostType, JID) -> Result when
    HostType :: mongooseim:host_type(),
    JID :: jid:jid(),
    Result :: any().
user_ping_timeout(HostType, JID) ->
    run_hook_for_host_type(user_ping_timeout, HostType, ok, [JID]).

-spec user_receive_packet(HostType, Acc, JID, From, To, El) -> Result when
    HostType :: binary(),
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    From :: jid:jid(),
    To :: jid:jid(),
    El :: exml:element(),
    Result :: mongoose_acc:t().
user_receive_packet(HostType, Acc, JID, From, To, El) ->
    run_hook_for_host_type(user_receive_packet, HostType, Acc, [JID, From, To, El]).

-spec user_sent_keep_alive(HostType, JID) -> Result when
    HostType :: binary(),
    JID :: jid:jid(),
    Result :: any().
user_sent_keep_alive(HostType, JID) ->
    run_hook_for_host_type(user_sent_keep_alive, HostType, ok, [JID]).

%%% @doc A hook called when a user sends an XMPP stanza.
%%% The hook's handler is expected to accept four parameters:
%%% `Acc', `From', `To' and `Packet'
%%% The arguments and the return value types correspond to the following spec.
-spec user_send_packet(Acc, From, To, Packet) -> Result when
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: mongoose_acc:t().
user_send_packet(Acc, From, To, Packet) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(user_send_packet, HostType, Acc, [From, To, Packet]).

-spec user_send_packet(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: mongoose_c2s:handler_params(),
    Result :: mongoose_acc:t().
user_send_packet(HostType, Acc, Params) ->
    run_fold(user_send_packet, HostType, Acc, Params).

-spec user_send_message(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: mongoose_c2s:handler_params(),
    Result :: mongoose_acc:t().
user_send_message(HostType, Acc, Params) ->
    run_fold(user_send_message, HostType, Acc, Params).

-spec user_send_iq(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: mongoose_c2s:handler_params(),
    Result :: mongoose_acc:t().
user_send_iq(HostType, Acc, Params) ->
    run_fold(user_send_iq, HostType, Acc, Params).

-spec user_send_presence(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: mongoose_c2s:handler_params(),
    Result :: mongoose_acc:t().
user_send_presence(HostType, Acc, Params) ->
    run_fold(user_send_presence, HostType, Acc, Params).

-spec user_send_non_stanza(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: mongoose_c2s:handler_params(),
    Result :: mongoose_acc:t().
user_send_non_stanza(HostType, Acc, Params) ->
    run_fold(user_send_non_stanza, HostType, Acc, Params).

%%% @doc The `vcard_set' hook is called to inform that the vcard
%%% has been set in mod_vcard backend.
-spec vcard_set(HostType, Server, LUser, VCard) -> Result when
    HostType :: mongooseim:host_type(),
    Server :: jid:server(),
    LUser :: jid:luser(),
    VCard :: exml:element(),
    Result :: any().
vcard_set(HostType, Server, LUser, VCard) ->
    run_hook_for_host_type(vcard_set, HostType, ok, [HostType, LUser, Server, VCard]).

-spec xmpp_send_element(HostType, Acc, El) -> Result when
    HostType :: binary(),
    Acc :: mongoose_acc:t(),
    El :: exml:element(),
    Result :: mongoose_acc:t().
xmpp_send_element(HostType, Acc, El) ->
    run_hook_for_host_type(xmpp_send_element, HostType, Acc, [El]).

%%% @doc The `xmpp_stanza_dropped' hook is called to inform that
%%% an xmpp stanza has been dropped.
-spec xmpp_stanza_dropped(Acc, From, To, Packet) -> Result when
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: any().
xmpp_stanza_dropped(Acc, From, To, Packet) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(xmpp_stanza_dropped, HostType, Acc, [From, To, Packet]).

%% C2S related hooks

-spec c2s_broadcast_recipients(State, Type, From, Packet) -> Result when
    State :: ejabberd_c2s:state(),
    Type :: {atom(), any()},
    From :: jid:jid(),
    Packet :: exml:element(),
    Result :: [jid:simple_jid()].
c2s_broadcast_recipients(State, Type, From, Packet) ->
    HostType = ejabberd_c2s_state:host_type(State),
    run_hook_for_host_type(c2s_broadcast_recipients, HostType, [],
                           [State, Type, From, Packet]).

-spec c2s_filter_packet(State, Feature, To, Packet) -> Result when
    State :: ejabberd_c2s:state(),
    Feature :: {atom(), binary()},
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: boolean().
c2s_filter_packet(State, Feature, To, Packet) ->
    HostType = ejabberd_c2s_state:host_type(State),
    run_hook_for_host_type(c2s_filter_packet, HostType, true,
                           [State, Feature, To, Packet]).

-spec c2s_preprocessing_hook(HostType, Acc, State) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    State :: tuple(),
    Result :: mongoose_acc:t().
c2s_preprocessing_hook(HostType, Acc, State) ->
    run_hook_for_host_type(c2s_preprocessing_hook, HostType, Acc, [State]).

-spec c2s_presence_in(State, From, To, Packet) -> Result when
    State :: ejabberd_c2s:state(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: ejabberd_c2s:state().
c2s_presence_in(State, From, To, Packet) ->
    HostType = ejabberd_c2s_state:host_type(State),
    run_hook_for_host_type(c2s_presence_in, HostType, State, [From, To, Packet]).

-spec c2s_stream_features(HostType, LServer) -> Result when
    HostType :: mongooseim:host_type(),
    LServer :: jid:lserver(),
    Result :: [exml:element()].
c2s_stream_features(HostType, LServer) ->
    run_hook_for_host_type(c2s_stream_features, HostType, [], [HostType, LServer]).

-spec c2s_unauthenticated_iq(HostType, Server, IQ, IP) -> Result when
    HostType :: mongooseim:host_type(),
    Server :: jid:server(),
    IQ :: jlib:iq(),
    IP :: {inet:ip_address(), inet:port_number()} | undefined,
    Result :: exml:element() | empty.
c2s_unauthenticated_iq(HostType, Server, IQ, IP) ->
    run_hook_for_host_type(c2s_unauthenticated_iq, HostType, empty, [HostType, Server, IQ, IP]).

-spec c2s_update_presence(HostType, Acc) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Result :: mongoose_acc:t().
c2s_update_presence(HostType, Acc) ->
    run_hook_for_host_type(c2s_update_presence, HostType, Acc, []).

-spec check_bl_c2s(IP) -> Result when
    IP ::  inet:ip_address(),
    Result :: boolean().
check_bl_c2s(IP) ->
    run_global_hook(check_bl_c2s, false, [IP]).

-spec forbidden_session_hook(HostType, Acc, JID) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Result :: mongoose_acc:t().
forbidden_session_hook(HostType, Acc, JID) ->
    run_hook_for_host_type(forbidden_session_hook, HostType, Acc, [JID]).

-spec session_opening_allowed_for_user(HostType, JID) -> Result when
    HostType :: mongooseim:host_type(),
    JID :: jid:jid(),
    Result :: allow | any(). %% anything else than 'allow' is interpreted
                             %% as not allowed
session_opening_allowed_for_user(HostType, JID) ->
    run_hook_for_host_type(session_opening_allowed_for_user, HostType, allow, [JID]).

%% Privacy related hooks

-spec privacy_check_packet(Acc, JID, PrivacyList,
                           FromToNameType, Dir) -> Result when
    Acc :: mongoose_acc:t(), JID :: jid:jid(),
    PrivacyList :: mongoose_privacy:userlist(),
    FromToNameType :: {jid:jid(), jid:jid(), binary(), binary()},
    Dir :: in | out,
    Result :: mongoose_acc:t().
privacy_check_packet(Acc, JID, PrivacyList, FromToNameType, Dir) ->
    HostType = mongoose_acc:host_type(Acc),
    AccWithRes = mongoose_acc:set(hook, result, allow, Acc),
    run_hook_for_host_type(privacy_check_packet, HostType, AccWithRes,
                           [JID, PrivacyList, FromToNameType, Dir]).

-spec privacy_get_user_list(HostType, JID) -> Result when
    HostType :: binary(),
    JID :: jid:jid(),
    Result :: mongoose_privacy:userlist().
privacy_get_user_list(HostType, JID) ->
    run_hook_for_host_type(privacy_get_user_list, HostType, #userlist{}, [HostType, JID]).

-spec privacy_iq_get(HostType, Acc, From, To, IQ, PrivList) -> Result when
    HostType :: binary(),
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    IQ :: jlib:iq(),
    PrivList :: mongoose_privacy:userlist(),
    Result :: mongoose_acc:t().
privacy_iq_get(HostType, Acc, From, To, IQ, PrivList) ->
    run_hook_for_host_type(privacy_iq_get, HostType, Acc, [From, To, IQ, PrivList]).

-spec privacy_iq_set(HostType, Acc, From, To, IQ) -> Result when
    HostType :: binary(),
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    IQ :: jlib:iq(),
    Result :: mongoose_acc:t().
privacy_iq_set(HostType, Acc, From, To, IQ) ->
    run_hook_for_host_type(privacy_iq_set, HostType, Acc, [From, To, IQ]).

-spec privacy_updated_list(HostType, OldList, NewList) -> Result when
    HostType :: binary(),
    OldList :: mongoose_privacy:userlist(),
    NewList :: mongoose_privacy:userlist(),
    Result :: false | mongoose_privacy:userlist().
privacy_updated_list(HostType, OldList, NewList) ->
    run_hook_for_host_type(privacy_updated_list, HostType, false, [OldList, NewList]).

%% Session management related hooks

-spec offline_groupchat_message_hook(Acc, From, To, Packet) -> Result when
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: mongoose_acc:t().
offline_groupchat_message_hook(Acc, From, To, Packet) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(offline_groupchat_message_hook, HostType, Acc,
                           [From, To, Packet]).

-spec offline_message_hook(Acc, From, To, Packet) -> Result when
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: mongoose_acc:t().
offline_message_hook(Acc, From, To, Packet) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(offline_message_hook, HostType, Acc, [From, To, Packet]).

-spec set_presence_hook(Acc, JID, Presence) -> Result when
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Presence :: any(),
    Result :: mongoose_acc:t().
set_presence_hook(Acc, JID, Presence) ->
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(set_presence_hook, HostType, Acc,
                           [LUser, LServer, LResource, Presence]).

-spec sm_broadcast(Acc, From, To, Broadcast, SessionCount) -> Result when
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Broadcast :: ejabberd_c2s:broadcast(),
    SessionCount :: non_neg_integer(),
    Result :: mongoose_acc:t().
sm_broadcast(Acc, From, To, Broadcast, SessionCount) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(sm_broadcast, HostType, Acc,
                           [From, To, Broadcast, SessionCount]).

-spec sm_filter_offline_message(HostType, From, To, Packet) -> Result when
    HostType :: binary(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: boolean().
sm_filter_offline_message(HostType, From, To, Packet) ->
    run_hook_for_host_type(sm_filter_offline_message, HostType, false,
                           [From, To, Packet]).

-spec sm_register_connection_hook(HostType, SID, JID, Info) -> Result when
    HostType :: binary(),
    SID :: 'undefined' | ejabberd_sm:sid(),
    JID :: jid:jid(),
    Info :: ejabberd_sm:info(),
    Result :: ok.
sm_register_connection_hook(HostType, SID, JID, Info) ->
    run_hook_for_host_type(sm_register_connection_hook, HostType, ok,
                           [HostType, SID, JID, Info]).

-spec sm_remove_connection_hook(Acc, SID, JID, Info, Reason) -> Result when
    Acc :: mongoose_acc:t(),
    SID :: 'undefined' | ejabberd_sm:sid(),
    JID :: jid:jid(),
    Info :: ejabberd_sm:info(),
    Reason :: ejabberd_sm:close_reason(),
    Result :: mongoose_acc:t().
sm_remove_connection_hook(Acc, SID, JID, Info, Reason) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(sm_remove_connection_hook, HostType, Acc,
                           [SID, JID, Info, Reason]).

-spec unset_presence_hook(Acc, JID, Status) -> Result when
    Acc :: mongoose_acc:t(),
    JID:: jid:jid(),
    Status :: binary(),
    Result :: mongoose_acc:t().
unset_presence_hook(Acc, JID, Status) ->
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(unset_presence_hook, HostType, Acc,
                           [LUser, LServer, LResource, Status]).

-spec xmpp_bounce_message(Acc) -> Result when
    Acc :: mongoose_acc:t(),
    Result :: mongoose_acc:t().
xmpp_bounce_message(Acc) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(xmpp_bounce_message, HostType, Acc, []).

%% Roster related hooks

%%% @doc The `roster_get' hook is called to extract a user's roster.
-spec roster_get(Acc, JID) -> Result when
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Result :: mongoose_acc:t().
roster_get(Acc, JID) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(roster_get, HostType, Acc, [JID]).

%%% @doc The `roster_groups' hook is called to extract roster groups.
-spec roster_groups(LServer) -> Result when
    LServer :: jid:lserver(),
    Result :: list().
roster_groups(LServer) ->
    run_hook_for_host_type(roster_groups, LServer, [], [LServer]).

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
    run_hook_for_host_type(roster_get_jid_info, HostType, {none, []},
                           [HostType, ToJID, RemBareJID]).

%%% @doc The `roster_get_subscription_lists' hook is called to extract
%%% user's subscription list.
-spec roster_get_subscription_lists(HostType, Acc, JID) -> Result when
    HostType :: binary(),
    Acc ::mongoose_acc:t(),
    JID :: jid:jid(),
    Result :: mongoose_acc:t().
roster_get_subscription_lists(HostType, Acc, JID) ->
    run_hook_for_host_type(roster_get_subscription_lists, HostType, Acc,
                           [jid:to_bare(JID)]).

%%% @doc The `roster_get_versioning_feature' hook is
%%% called to determine if roster versioning is enabled.
-spec roster_get_versioning_feature(HostType) -> Result when
    HostType :: binary(),
    Result :: [exml:element()].
roster_get_versioning_feature(HostType) ->
    run_hook_for_host_type(roster_get_versioning_feature, HostType, [], [HostType]).

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
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(roster_in_subscription, HostType, Acc,
                           [jid:to_bare(To), From, Type, Reason]).

%%% @doc The `roster_out_subscription' hook is called
%%% when a user sends out subscription.
-spec roster_out_subscription(Acc, From, To, Type) -> Result when
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Type :: mod_roster:sub_presence(),
    Result :: mongoose_acc:t().
roster_out_subscription(Acc, From, To, Type) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(roster_out_subscription, HostType, Acc,
                           [jid:to_bare(From), To, Type]).

%%% @doc The `roster_process_item' hook is called when a user's roster is set.
-spec roster_process_item(HostType, LServer, Item) -> Result when
    HostType :: mongooseim:host_type(),
    LServer :: jid:lserver(),
    Item :: mod_roster:roster(),
    Result :: mod_roster:roster().
roster_process_item(HostType, LServer, Item) ->
    run_hook_for_host_type(roster_process_item, HostType, Item, [LServer]).

%%% @doc The `roster_push' hook is called when a roster item is
%%% being pushed and roster versioning is not enabled.
-spec roster_push(HostType, From, Item) -> Result when
    HostType :: mongooseim:host_type(),
    From :: jid:jid(),
    Item :: mod_roster:roster(),
    Result :: any().
roster_push(HostType, From, Item) ->
    run_hook_for_host_type(roster_push, HostType, ok, [HostType, From, Item]).

%%% @doc The `roster_set' hook is called when a user's roster is set through an IQ.
-spec roster_set(HostType, From, To, SubEl) -> Result when
    HostType :: mongooseim:host_type(),
    From :: jid:jid(),
    To :: jid:jid(),
    SubEl :: exml:element(),
    Result :: any().
roster_set(HostType, From, To, SubEl) ->
    run_hook_for_host_type(roster_set, HostType, ok, [From, To, SubEl]).

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
    run_hook_for_host_type(is_muc_room_owner, HostType, false, [Acc, Room, User]).

%%% @doc The `can_access_identity' hook is called to determine if
%%% a given user can see the real identity of the people in a room.
-spec can_access_identity(HostType, Room, User) -> Result when
      HostType :: mongooseim:host_type(),
      Room :: jid:jid(),
      User :: jid:jid(),
      Result :: boolean().
can_access_identity(HostType, Room, User) ->
    run_hook_for_host_type(can_access_identity, HostType, false, [HostType, Room, User]).

%%% @doc The `can_access_room' hook is called to determine
%%% if a given user can access a room.
-spec can_access_room(HostType, Acc, Room, User) -> Result when
      HostType :: mongooseim:host_type(),
      Acc :: mongoose_acc:t(),
      Room :: jid:jid(),
      User :: jid:jid(),
      Result :: boolean().
can_access_room(HostType, Acc, Room, User) ->
    run_hook_for_host_type(can_access_room, HostType, false, [Acc, Room, User]).

-spec acc_room_affiliations(Acc, Room) -> NewAcc when
      Acc :: mongoose_acc:t(),
      Room :: jid:jid(),
      NewAcc :: mongoose_acc:t().
acc_room_affiliations(Acc, Room) ->
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    run_hook_for_host_type(acc_room_affiliations, HostType, Acc, [Room]).

-spec room_exists(HostType, Room) -> Result when
      HostType :: mongooseim:host_type(),
      Room :: jid:jid(),
      Result :: boolean().
room_exists(HostType, Room) ->
    run_hook_for_host_type(room_exists, HostType, false, [HostType, Room]).

-spec room_new_affiliations(Acc, Room, NewAffs, Version) -> NewAcc when
      Acc :: mongoose_acc:t(),
      Room :: jid:jid(),
      NewAffs :: mod_muc_light:aff_users(),
      Version :: binary(),
      NewAcc :: mongoose_acc:t().
room_new_affiliations(Acc, Room, NewAffs, Version) ->
    HostType = mod_muc_light_utils:acc_to_host_type(Acc),
    run_hook_for_host_type(room_new_affiliations, HostType, Acc, [Room, NewAffs, Version]).

%% MAM related hooks

%%% @doc The `mam_archive_id' hook is called to determine
%%% the integer id of an archive for a particular user or entity.
%%%
%%% If a MAM backend doesn't support or doesn't require archive IDs,
%%% `undefined' may be returned.
-spec mam_archive_id(HostType, OwnerJID) -> Result when
      HostType :: mongooseim:host_type(),
      OwnerJID :: jid:jid(),
      Result :: undefined | mod_mam:archive_id().
mam_archive_id(HostType, OwnerJID) ->
    run_hook_for_host_type(mam_archive_id, HostType, undefined, [HostType, OwnerJID]).

%%% @doc The `mam_archive_size' hook is called to determine the size
%%% of the archive for a given JID
-spec mam_archive_size(HostType, ArchiveID, OwnerJID) -> Result when
      HostType :: mongooseim:host_type(),
      ArchiveID :: undefined | mod_mam:archive_id(),
      OwnerJID :: jid:jid(),
      Result :: integer().
mam_archive_size(HostType, ArchiveID, OwnerJID) ->
    run_hook_for_host_type(mam_archive_size, HostType, 0,
                           [HostType, ArchiveID, OwnerJID]).

%%% @doc The `mam_get_behaviour' hooks is called to determine if a message
%%% should be archived or not based on a given pair of JIDs.
-spec mam_get_behaviour(HookServer, ArchiveID,
                        OwnerJID, RemoteJID) -> Result when
      HookServer :: jid:lserver(),
      ArchiveID :: undefined | mod_mam:archive_id(),
      OwnerJID :: jid:jid(),
      RemoteJID :: jid:jid(),
      Result :: mod_mam:archive_behaviour().
mam_get_behaviour(HookServer, ArchiveID, OwnerJID, RemoteJID) ->
    run_hook_for_host_type(mam_get_behaviour, HookServer, always,
                           [HookServer, ArchiveID, OwnerJID, RemoteJID]).

%%% @doc The `mam_set_prefs' hook is called to set a user's archive preferences.
%%%
%%% It's possible to set which JIDs are always or never allowed in the archive
-spec mam_set_prefs(HookServer, ArchiveId, OwnerJID,
                    DefaultMode, AlwaysJIDs, NeverJIDs) -> Result when
      HookServer :: jid:lserver(),
      ArchiveId :: undefined | mod_mam:archive_id(),
      OwnerJID :: jid:jid(),
      DefaultMode :: mod_mam:archive_behaviour(),
      AlwaysJIDs :: [jid:literal_jid()],
    NeverJIDs :: [jid:literel_jid()],
    Result :: any().
mam_set_prefs(HookServer,  ArchiveID, OwnerJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    run_hook_for_host_type(mam_set_prefs, HookServer, {error, not_implemented},
                           [HookServer, ArchiveID, OwnerJID,
                            DefaultMode, AlwaysJIDs, NeverJIDs]).

%%% @doc The `mam_get_prefs' hook is called to read
%%% the archive settings for a given user.
-spec mam_get_prefs(HookServer, DefaultMode, ArchiveID, OwnerJID) -> Result when
      HookServer :: jid:lserver(),
      DefaultMode :: mod_mam:archive_behaviour(),
      ArchiveID :: undefined | mod_mam:archive_id(),
      OwnerJID :: jid:jid(),
      Result :: mod_mam:preference() | {error, Reason :: term()}.
mam_get_prefs(HookServer, DefaultMode, ArchiveID, OwnerJID) ->
    InitialAccValue = {DefaultMode, [], []}, %% mod_mam:preference() type
    run_hook_for_host_type(mam_get_prefs, HookServer, InitialAccValue,
                           [HookServer, ArchiveID, OwnerJID]).

%%% @doc The `mam_remove_archive' hook is called in order to
%%% remove the entire archive for a particular user.
-spec mam_remove_archive(HookServer, ArchiveID, OwnerJID) -> any() when
      HookServer :: jid:lserver(),
      ArchiveID :: undefined | mod_mam:archive_id(),
      OwnerJID :: jid:jid().
mam_remove_archive(HookServer, ArchiveID, OwnerJID) ->
    run_hook_for_host_type(mam_remove_archive, HookServer, ok,
                           [HookServer, ArchiveID, OwnerJID]).

%%% @doc The `mam_lookup_messages' hook is to retrieve
%%% archived messages for given search parameters.
-spec mam_lookup_messages(HookServer, Params) -> Result when
      HookServer :: jid:lserver(),
      Params :: map(),
      Result :: {ok, mod_mam:lookup_result()}.
mam_lookup_messages(HookServer, Params) ->
    InitialLookupValue = {0, 0, []}, %% mod_mam:lookup_result() type
    run_hook_for_host_type(mam_lookup_messages, HookServer, {ok, InitialLookupValue},
                           [HookServer, Params]).

%%% @doc The `mam_archive_message' hook is called in order
%%% to store the message in the archive.
-spec mam_archive_message(HookServer, Params) ->
    Result when
    HookServer :: jid:lserver(),
    Params :: mod_mam:archive_message_params(),
    Result :: ok | {error, timeout}.
mam_archive_message(HookServer, Params) ->
    run_hook_for_host_type(mam_archive_message, HookServer, ok, [HookServer, Params]).

%%% @doc The `mam_flush_messages' hook is run after the async bulk write
%%% happens for messages despite the result of the write.
-spec mam_flush_messages(HookServer :: jid:lserver(),
                         MessageCount :: integer()) -> ok.
mam_flush_messages(HookServer, MessageCount) ->
    run_hook_for_host_type(mam_flush_messages, HookServer, ok,
                           [HookServer, MessageCount]).

%% @doc Waits until all pending messages are written
-spec mam_archive_sync(HostType :: mongooseim:host_type()) -> ok.
mam_archive_sync(HostType) ->
    run_hook_for_host_type(mam_archive_sync, HostType, ok, [HostType]).

%% @doc Notifies of a message retraction
-spec mam_retraction(mongooseim:host_type(),
                     mod_mam_utils:retraction_info(),
                     mod_mam:archive_message_params()) ->
    mod_mam_utils:retraction_info().
mam_retraction(HostType, RetractionInfo, Env) ->
    run_fold(mam_retraction, HostType, RetractionInfo, Env).

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
-spec mam_muc_archive_id(HookServer, OwnerJID) -> Result when
      HookServer :: jid:lserver(),
      OwnerJID :: jid:jid(),
      Result :: undefined | mod_mam:archive_id().
mam_muc_archive_id(HookServer, OwnerJID) ->
    run_hook_for_host_type(mam_muc_archive_id, HookServer, undefined,
                           [HookServer, OwnerJID]).

%%% @doc The `mam_muc_archive_size' hook is called to determine
%%% the archive size for a given room.
-spec mam_muc_archive_size(HostType, ArchiveID, RoomJID) -> Result when
      HostType :: mongooseim:host_type(),
      ArchiveID :: undefined | mod_mam:archive_id(),
      RoomJID :: jid:jid(),
      Result :: integer().
mam_muc_archive_size(HostType, ArchiveID, RoomJID) ->
    run_hook_for_host_type(mam_muc_archive_size, HostType, 0,
                           [HostType, ArchiveID, RoomJID]).

%%% @doc The `mam_muc_get_behaviour' hooks is called to determine if a message should
%%% be archived or not based on the given room and user JIDs.
-spec mam_muc_get_behaviour(HostType, ArchiveID,
                            RoomJID, RemoteJID) -> Result when
      HostType :: mongooseim:host_type(),
      ArchiveID :: undefined | mod_mam:archive_id(),
      RoomJID :: jid:jid(),
      RemoteJID :: jid:jid(),
      Result :: mod_mam:archive_behaviour().
mam_muc_get_behaviour(HostType, ArchiveID, RoomJID, RemoteJID) ->
    DefaultBehaviour = always, %% mod_mam:archive_behaviour() type
    run_hook_for_host_type(mam_muc_get_behaviour, HostType, DefaultBehaviour,
                           [HostType, ArchiveID, RoomJID, RemoteJID]).

%%% @doc The `mam_muc_set_prefs' hook is called to set a room's archive preferences.
%%%
%%% It's possible to set which JIDs are always or never allowed in the archive
-spec mam_muc_set_prefs(HostType, ArchiveId, RoomJID,
                        DefaultMode, AlwaysJIDs, NeverJIDs) -> Result when
      HostType :: mongooseim:host_type(),
      ArchiveId :: undefined | mod_mam:archive_id(),
      RoomJID :: jid:jid(),
      DefaultMode :: mod_mam:archive_behaviour(),
      AlwaysJIDs :: [jid:literal_jid()],
      NeverJIDs :: [jid:literel_jid()],
      Result :: any().
mam_muc_set_prefs(HostType, ArchiveID, RoomJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    InitialAcc = {error, not_implemented},
    run_hook_for_host_type(mam_muc_set_prefs, HostType, InitialAcc,
                           [HostType, ArchiveID, RoomJID, DefaultMode,
                            AlwaysJIDs, NeverJIDs]).

%%% @doc The `mam_muc_get_prefs' hook is called to read
%%% the archive settings for a given room.
-spec mam_muc_get_prefs(HostType, DefaultMode, ArchiveID, RoomJID) -> Result when
      HostType :: mongooseim:host_type(),
      DefaultMode :: mod_mam:archive_behaviour(),
      ArchiveID :: undefined | mod_mam:archive_id(),
      RoomJID :: jid:jid(),
      Result :: mod_mam:preference() | {error, Reason :: term()}.
mam_muc_get_prefs(HostType, DefaultMode, ArchiveID, RoomJID) ->
    InitialAcc = {DefaultMode, [], []}, %% mod_mam:preference() type
    run_hook_for_host_type(mam_muc_get_prefs, HostType, InitialAcc,
                           [HostType, ArchiveID, RoomJID]).

%%% @doc The `mam_muc_remove_archive' hook is called in order to remove the entire
%%% archive for a particular user.
-spec mam_muc_remove_archive(HostType, ArchiveID, RoomJID) -> any() when
      HostType :: mongooseim:host_type(),
      ArchiveID :: undefined | mod_mam:archive_id(),
      RoomJID :: jid:jid().
mam_muc_remove_archive(HostType, ArchiveID, RoomJID) ->
    run_hook_for_host_type(mam_muc_remove_archive, HostType, ok,
                           [HostType, ArchiveID, RoomJID]).

%%% @doc The `mam_muc_lookup_messages' hook is to retrieve archived
%%% MUC messages for any given search parameters.
-spec mam_muc_lookup_messages(HostType, Params) -> Result when
      HostType :: mongooseim:host_type(),
      Params :: map(),
      Result :: {ok, mod_mam:lookup_result()}.
mam_muc_lookup_messages(HostType, Params) ->
    InitialLookupValue = {0, 0, []}, %% mod_mam:lookup_result() type
    run_hook_for_host_type(mam_muc_lookup_messages, HostType, {ok, InitialLookupValue},
                           [HostType, Params]).

%%% @doc The `mam_muc_archive_message' hook is called in order
%%% to store the MUC message in the archive.
-spec mam_muc_archive_message(HostType, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Params :: mod_mam:archive_message_params(),
    Result :: ok | {error, timeout}.
mam_muc_archive_message(HostType, Params) ->
    run_hook_for_host_type(mam_muc_archive_message, HostType, ok, [HostType, Params]).

%%% @doc The `mam_muc_flush_messages' hook is run after the async bulk write
%%% happens for MUC messages despite the result of the write.
-spec mam_muc_flush_messages(HookServer :: jid:lserver(),
                             MessageCount :: integer()) -> ok.
mam_muc_flush_messages(HookServer, MessageCount) ->
    run_hook_for_host_type(mam_muc_flush_messages, HookServer, ok,
                           [HookServer, MessageCount]).

%% @doc Waits until all pending messages are written
-spec mam_muc_archive_sync(HostType :: mongooseim:host_type()) -> ok.
mam_muc_archive_sync(HostType) ->
    run_hook_for_host_type(mam_muc_archive_sync, HostType, ok, [HostType]).

%% @doc Notifies of a muc message retraction
-spec mam_muc_retraction(mongooseim:host_type(),
                         mod_mam_utils:retraction_info(),
                         mod_mam:archive_message_params()) ->
    mod_mam_utils:retraction_info().
mam_muc_retraction(HostType, RetractionInfo, Env) ->
    run_fold(mam_muc_retraction, HostType, RetractionInfo, Env).

%% GDPR related hooks

%%% @doc `get_mam_pm_gdpr_data' hook is called to provide
%%% a user's archive for GDPR purposes.
-spec get_mam_pm_gdpr_data(HostType, JID) -> Result when
      HostType :: mongooseim:host_type(),
      JID :: jid:jid(),
      Result :: ejabberd_gen_mam_archive:mam_pm_gdpr_data().
get_mam_pm_gdpr_data(HostType, JID) ->
    run_hook_for_host_type(get_mam_pm_gdpr_data, HostType, [], [HostType, JID]).

%%% @doc `get_mam_muc_gdpr_data' hook is called to provide
%%% a user's archive for GDPR purposes.
-spec get_mam_muc_gdpr_data(HostType, JID) -> Result when
      HostType :: mongooseim:host_type(),
      JID :: jid:jid(),
      Result :: ejabberd_gen_mam_archive:mam_muc_gdpr_data().
get_mam_muc_gdpr_data(HostType, JID) ->
    run_hook_for_host_type(get_mam_muc_gdpr_data, HostType, [], [HostType, JID]).

%%% @doc `get_personal_data' hook is called to retrieve
%%% a user's personal data for GDPR purposes.
-spec get_personal_data(HostType, JID) -> Result when
    HostType :: binary(),
    JID :: jid:jid(),
    Result :: gdpr:personal_data().
get_personal_data(HostType, JID) ->
    run_hook_for_host_type(get_personal_data, HostType, [], [HostType, JID]).

%% S2S related hooks

%%% @doc `find_s2s_bridge' hook is called to find a s2s bridge to a foreign protocol
%%% when opening a socket to a different XMPP server fails.
-spec find_s2s_bridge(Name, Server) -> Result when
    Name :: any(),
    Server :: jid:server(),
    Result :: any().
find_s2s_bridge(Name, Server) ->
    run_global_hook(find_s2s_bridge, undefined, [Name, Server]).

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
    run_global_hook(s2s_allow_host, allow, [MyHost, S2SHost]).

%%% @doc `s2s_connect_hook' hook is called when a s2s connection is established.
-spec s2s_connect_hook(Name, Server) -> Result when
    Name :: any(),
    Server :: jid:server(),
    Result :: any().
s2s_connect_hook(Name, Server) ->
    run_global_hook(s2s_connect_hook, ok, [Name, Server]).

%%% @doc `s2s_send_packet' hook is called when a message is routed.
-spec s2s_send_packet(Acc, From, To, Packet) -> Result when
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: mongoose_acc:t().
s2s_send_packet(Acc, From, To, Packet) ->
    run_global_hook(s2s_send_packet, Acc, [From, To, Packet]).

%%% @doc `s2s_stream_features' hook is used to extract
%%% the stream management features supported by the server.
-spec s2s_stream_features(HostType, LServer) -> Result when
    HostType :: mongooseim:host_type(),
    LServer :: jid:lserver(),
    Result :: [exml:element()].
s2s_stream_features(HostType, LServer) ->
    run_hook_for_host_type(s2s_stream_features, HostType, [], [HostType, LServer]).

%%% @doc `s2s_receive_packet' hook is called when
%%% an incoming stanza is routed by the server.
-spec s2s_receive_packet(Acc) -> Result when
    Acc :: mongoose_acc:t(),
    Result :: mongoose_acc:t().
s2s_receive_packet(Acc) ->
    run_global_hook(s2s_receive_packet, Acc, []).

%% Discovery related hooks

%%% @doc `disco_local_identity' hook is called to get the identity of the server.
-spec disco_local_identity(mongoose_disco:identity_acc()) ->
    mongoose_disco:identity_acc().
disco_local_identity(Acc = #{host_type := HostType}) ->
    run_hook_for_host_type(disco_local_identity, HostType, Acc, []).

%%% @doc `disco_sm_identity' hook is called to get the identity of the
%%% client when a discovery IQ gets to session management.
-spec disco_sm_identity(mongoose_disco:identity_acc()) -> mongoose_disco:identity_acc().
disco_sm_identity(Acc = #{host_type := HostType}) ->
    run_hook_for_host_type(disco_sm_identity, HostType, Acc, []).

%%% @doc `disco_local_items' hook is called to extract items associated with the server.
-spec disco_local_items(mongoose_disco:item_acc()) -> mongoose_disco:item_acc().
disco_local_items(Acc = #{host_type := HostType}) ->
    run_hook_for_host_type(disco_local_items, HostType, Acc, []).

%%% @doc `disco_sm_items' hook is called to get the items associated
%%% with the client when a discovery IQ gets to session management.
-spec disco_sm_items(mongoose_disco:item_acc()) -> mongoose_disco:item_acc().
disco_sm_items(Acc = #{host_type := HostType}) ->
    run_hook_for_host_type(disco_sm_items, HostType, Acc, []).

%%% @doc `disco_local_features' hook is called to extract features
%%% offered by the server.
-spec disco_local_features(mongoose_disco:feature_acc()) -> mongoose_disco:feature_acc().
disco_local_features(Acc = #{host_type := HostType}) ->
    run_hook_for_host_type(disco_local_features, HostType, Acc, []).

%%% @doc `disco_sm_features' hook is called to get the features of the client
%%% when a discovery IQ gets to session management.
-spec disco_sm_features(mongoose_disco:feature_acc()) -> mongoose_disco:feature_acc().
disco_sm_features(Acc = #{host_type := HostType}) ->
    run_hook_for_host_type(disco_sm_features, HostType, Acc, []).

%%% @doc `disco_muc_features' hook is called to get the features
%%% supported by the MUC (Light) service.
-spec disco_muc_features(mongoose_disco:feature_acc()) -> mongoose_disco:feature_acc().
disco_muc_features(Acc = #{host_type := HostType}) ->
    run_hook_for_host_type(disco_muc_features, HostType, Acc, []).

%%% @doc `disco_info' hook is called to extract information about the server.
-spec disco_info(mongoose_disco:info_acc()) -> mongoose_disco:info_acc().
disco_info(Acc = #{host_type := HostType}) ->
    run_hook_for_host_type(disco_info, HostType, Acc, []).

%% AMP related hooks

%%% @doc The `amp_check_condition' hook is called to determine whether
%%% the AMP strategy matches the given AMP rule.
-spec amp_check_condition(HostType, Strategy, Rule) -> Result when
    HostType :: mongooseim:host_type(),
    Strategy :: mod_amp:amp_strategy(),
    Rule :: mod_amp:amp_rule(),
    Result :: mod_amp:amp_match_result().
amp_check_condition(HostType, Strategy, Rule) ->
    InitialAcc = no_match, %% mod_amp:amp_match_result() type
    run_hook_for_host_type(amp_check_condition, HostType, InitialAcc, [Strategy, Rule]).

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
    DefaultStrategy = amp_strategy:null_strategy(),
    run_hook_for_host_type(amp_determine_strategy, HostType, DefaultStrategy,
                           [From, To, Packet, Event]).

%%% @doc The `amp_verify_support' hook is called when checking
%%% whether the host supports given AMP rules.
-spec amp_verify_support(HostType, Rules) -> Result when
    HostType :: mongooseim:host_type(),
    Rules :: mod_amp:amp_rules(),
    Result :: [mod_amp:amp_rule_support()].
amp_verify_support(HostType, Rules) ->
    run_hook_for_host_type(amp_verify_support, HostType, [], [Rules]).

%% MUC and MUC Light related hooks

-spec filter_room_packet(HostType, Packet, EventData) -> Result when
    HostType :: mongooseim:host_type(),
    Packet :: exml:element(),
    EventData :: mod_muc:room_event_data(),
    Result :: exml:element().
filter_room_packet(HostType, Packet, EventData) ->
    run_hook_for_host_type(filter_room_packet, HostType, Packet, [HostType, EventData]).

%%% @doc The `forget_room' hook is called when a room is removed from the database.
-spec forget_room(HostType, MucHost, Room) -> Result when
    HostType :: mongooseim:host_type(),
    MucHost :: jid:server(),
    Room :: jid:luser(),
    Result :: any().
forget_room(HostType, MucHost, Room) ->
    run_hook_for_host_type(forget_room, HostType, #{}, [HostType, MucHost, Room]).

-spec invitation_sent(HookServer, Host, RoomJID, From, To, Reason) -> Result when
    HookServer :: jid:server(),
    Host :: jid:server(),
    RoomJID :: jid:jid(),
    From :: jid:jid(),
    To :: jid:jid(),
    Reason :: binary(),
    Result :: any().
invitation_sent(HookServer, Host, RoomJID, From, To, Reason) ->
    run_hook_for_host_type(invitation_sent, HookServer, ok,
                           [HookServer, Host, RoomJID, From, To, Reason]).

%%% @doc The `join_room' hook is called when a user joins a MUC room.
-spec join_room(HookServer, Room, Host, JID, MucJID) -> Result when
    HookServer :: jid:server(),
    Room :: mod_muc:room(),
    Host :: jid:server(),
    JID :: jid:jid(),
    MucJID :: jid:jid(),
    Result :: any().
join_room(HookServer, Room, Host, JID, MucJID) ->
    run_hook_for_host_type(join_room, HookServer, ok,
                           [HookServer, Room, Host, JID, MucJID]).

%%% @doc The `leave_room' hook is called when a user joins a MUC room.
-spec leave_room(HookServer, Room, Host, JID, MucJID) -> Result when
    HookServer :: jid:server(),
    Room :: mod_muc:room(),
    Host :: jid:server(),
    JID :: jid:jid(),
    MucJID :: jid:jid(),
    Result :: any().
leave_room(HookServer, Room, Host, JID, MucJID) ->
    run_hook_for_host_type(leave_room, HookServer, ok,
                           [HookServer, Room, Host, JID, MucJID]).

%%% @doc The `room_packet' hook is called when a message is added to room's history.
-spec room_packet(Server, FromNick, FromJID, JID, Packet) -> Result when
    Server :: jid:lserver(),
    FromNick :: mod_muc:nick(),
    FromJID :: jid:jid(),
    JID :: jid:jid(),
    Packet :: exml:element(),
    Result :: any().
room_packet(Server, FromNick, FromJID, JID, Packet) ->
    run_hook_for_host_type(room_packet, Server, ok, [FromNick, FromJID, JID, Packet]).

-spec update_inbox_for_muc(HostType, Info) -> Result when
    HostType :: mongooseim:host_type(),
    Info :: mod_muc_room:update_inbox_for_muc_payload(),
    Result :: mod_muc_room:update_inbox_for_muc_payload().
update_inbox_for_muc(HostType, Info) ->
    run_hook_for_host_type(update_inbox_for_muc, HostType, Info, []).

%% Caps related hooks

-spec caps_recognised(Acc, From, Pid, Features) -> Result when
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    Pid :: pid(),
    Features :: unknown | list(),
    Result :: mongoose_acc:t().
caps_recognised(Acc, From, Pid, Features) ->
    HostType = mongoose_acc:host_type(Acc),
    run_hook_for_host_type(caps_recognised, HostType, Acc, [From, Pid, Features]).

%% PubSub related hooks

%%% @doc The `pubsub_create_node' hook is called to
%%% inform that a pubsub node is created.
-spec pubsub_create_node(Server, PubSubHost, NodeId, Nidx, NodeOptions) -> Result when
    Server :: jid:server(),
    PubSubHost :: mod_pubsub:host(),
    NodeId :: mod_pubsub:nodeId(),
    Nidx :: mod_pubsub:nodeIdx(),
    NodeOptions :: list(),
    Result :: any().
pubsub_create_node(Server, PubSubHost, NodeId, Nidx, NodeOptions) ->
    run_hook_for_host_type(pubsub_create_node, Server, ok,
                           [Server, PubSubHost, NodeId, Nidx, NodeOptions]).

%%% @doc The `pubsub_delete_node' hook is called to inform
%%% that a pubsub node is deleted.
-spec pubsub_delete_node(Server, PubSubHost, NodeId, Nidx) -> Result when
    Server :: jid:server(),
    PubSubHost :: mod_pubsub:host(),
    NodeId :: mod_pubsub:nodeId(),
    Nidx :: mod_pubsub:nodeIdx(),
    Result :: any().
pubsub_delete_node(Server, PubSubHost, NodeId, Nidx) ->
    run_hook_for_host_type(pubsub_delete_node, Server, ok,
                           [Server, PubSubHost, NodeId, Nidx]).

%%% @doc The `pubsub_publish_item' hook is called to inform
%%% that a pubsub item is published.
-spec pubsub_publish_item(Server, NodeId, Publisher,
                          ServiceJID, ItemId, BrPayload) -> Result when
    Server :: jid:server(),
    NodeId :: mod_pubsub:nodeId(),
    Publisher :: jid:jid(),
    ServiceJID :: jid:jid(),
    ItemId :: mod_pubsub:itemId(),
    BrPayload :: mod_pubsub:payload(),
    Result :: any().
pubsub_publish_item(Server, NodeId, Publisher, ServiceJID, ItemId, BrPayload) ->
    run_hook_for_host_type(pubsub_publish_item, Server, ok,
                           [Server, NodeId, Publisher, ServiceJID,
                            ItemId, BrPayload]).

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
    run_hook_for_host_type(mod_global_distrib_known_recipient, GlobalHost, ok,
                           [From, To, LocalHost]).

%%% @doc The `mod_global_distrib_unknown_recipient' hook is called when
%%% the recipient is unknown to `global_distrib'.
-spec mod_global_distrib_unknown_recipient(GlobalHost, Info) -> Result when
    GlobalHost :: jid:server(),
    Info :: filter_packet_acc(),
    Result :: any().
mod_global_distrib_unknown_recipient(GlobalHost, Info) ->
    run_hook_for_host_type(mod_global_distrib_unknown_recipient, GlobalHost, Info, []).


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
run_global_hook(HookName, Acc, Params) when is_map(Params) ->
    run_fold(HookName, global, Acc, Params);
run_global_hook(HookName, Acc, Args) when is_list(Args) ->
    ParamsWithLegacyArgs = ejabberd_hooks:add_args(#{}, Args),
    run_fold(HookName, global, Acc, ParamsWithLegacyArgs).

run_hook_for_host_type(HookName, undefined, Acc, Args) ->
    ?LOG_ERROR(#{what => undefined_host_type,
                 text => <<"Running hook for an undefined host type">>,
                 hook_name => HookName, hook_acc => Acc, hook_args => Args}),
    Acc;
run_hook_for_host_type(HookName, HostType, Acc, Params) when is_binary(HostType),
                                                             is_map(Params) ->
    run_fold(HookName, HostType, Acc, Params);
run_hook_for_host_type(HookName, HostType, Acc, Args) when is_binary(HostType),
                                                           is_list(Args) ->
    ParamsWithLegacyArgs = ejabberd_hooks:add_args(#{}, Args),
    run_fold(HookName, HostType, Acc, ParamsWithLegacyArgs).

run_fold(HookName, HostType, Acc, Params) when is_map(Params) ->
    {_, RetValue} = gen_hook:run_fold(HookName, HostType, Acc, Params),
    RetValue.
