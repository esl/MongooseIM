%%% @doc Hooks wrapper providing clear specification for a hook caller
%%%
%%% Every hook has its own function in this module with specs as accurate as
%%% possible. This helps to have a static analyzation of the hooks callers to
%%% make sure they pass the expected arguments.
-module(mongoose_hooks).

-export([auth_failed/2,
         c2s_broadcast_recipients/6,
         c2s_filter_packet/6,
         c2s_preprocessing_hook/3,
         c2s_presence_in/5,
         c2s_stream_features/2,
         c2s_unauthenticated_iq/4,
         c2s_update_presence/2,
         check_bl_c2s/2,
         failed_to_store_message/4,
         forbidden_session_hook/3,
         offline_groupchat_message_hook/5,
         offline_message_hook/5,
         presence_probe_hook/5,
         privacy_check_packet/6,
         privacy_get_user_list/3,
         privacy_iq_get/6,
         privacy_iq_set/5,
         privacy_updated_list/4,
         resend_offline_messages_hook/3,
         roster_get_subscription_lists/3,
         roster_get_versioning_feature/2,
         roster_in_subscription/7,
         roster_out_subscription/5,
         session_opening_allowed_for_user/3,
         set_presence_hook/5,
         sm_broadcast/6,
         sm_filter_offline_message/5,
         sm_register_connection_hook/4,
         sm_remove_connection_hook/6,
         unacknowledged_message/3,
         unset_presence_hook/5,
         user_available_hook/3,
         user_receive_packet/6,
         user_sent_keep_alive/2,
         user_send_packet/5,
         xmpp_bounce_message/2,
         xmpp_send_element/3]).

-export([roster_get_jid_info/4]).

-export([is_muc_room_owner/4,
         can_access_identity/4,
         can_access_room/4,
         muc_room_pid/3]).

-export([mam_drop_iq/6,
         mam_archive_id/3,
         mam_archive_size/4,
         mam_get_behaviour/5,
         mam_set_prefs/7,
         mam_get_prefs/4,
         mam_remove_archive/4,
         mam_lookup_messages/3,
         mam_archive_message/9,
         mam_drop_message/2,
         mam_drop_messages/3,
         mam_flush_messages/3]).

-export([mam_muc_drop_iq/6,
         mam_muc_archive_id/3,
         mam_muc_archive_size/4,
         mam_muc_get_behaviour/5,
         mam_muc_set_prefs/7,
         mam_muc_get_prefs/4,
         mam_muc_remove_archive/4,
         mam_muc_lookup_messages/3,
         mam_muc_archive_message/9,
         mam_muc_flush_messages/3]).

-export([get_mam_pm_gdpr_data/3,
         get_mam_muc_gdpr_data/3]).

-spec auth_failed(Server, Username) -> Result when
    Server :: jid:server(),
    Username :: jid:user() | unknown,
    Result :: ok.
auth_failed(Server, Username) ->
    ejabberd_hooks:run_fold(auth_failed, Server, ok, [Username, Server]).

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

-spec forbidden_session_hook(Server, Acc, JID) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Result :: mongoose_acc:t().
forbidden_session_hook(Server, Acc, JID) ->
    ejabberd_hooks:run_fold(forbidden_session_hook, Server, Acc, [JID]).

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

-spec roster_get_subscription_lists(Server, Acc, User) -> Result when
    Server :: jid:server(),
    Acc ::mongoose_acc:t(),
    User :: jid:user(),
    Result :: mongoose_acc:t().
roster_get_subscription_lists(Server, Acc, User) ->
    ejabberd_hooks:run_fold(roster_get_subscription_lists, Server, Acc, [User, Server]).

-spec roster_get_versioning_feature(Server, Acc) -> Result when
    Server :: jid:server(),
    Acc :: [exml:element()],
    Result :: [exml:element()].
roster_get_versioning_feature(Server, Acc) ->
    ejabberd_hooks:run_fold(roster_get_versioning_feature,
                            Server, Acc, [Server]).

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

-spec session_opening_allowed_for_user(Server, Allow, JID) -> Result when
    Server :: jid:server(),
    Allow :: any(),
    JID :: jid:jid(),
    Result :: any().
session_opening_allowed_for_user(Server, Allow, JID) ->
    ejabberd_hooks:run_fold(session_opening_allowed_for_user,
                            Server,
                            Allow, [JID]).

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

-spec unacknowledged_message(Server, Acc, JID) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Result :: mongoose_acc:t().
unacknowledged_message(Server, Acc, JID) ->
    ejabberd_hooks:run_fold(unacknowledged_message, Server, Acc, [JID]).

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

%%% @doc A hook called when a user sends an XMPP stanza
%%% The hook's handler is expected to accept four parameters: `Acc`, `From`, `To` and `Packet`
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

-spec xmpp_bounce_message(Server, Acc) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    Result :: mongoose_acc:t().
xmpp_bounce_message(Server, Acc) ->
    ejabberd_hooks:run_fold(xmpp_bounce_message, Server, Acc, []).

-spec xmpp_send_element(Server, Acc, El) -> Result when
    Server :: jid:server(),
    Acc :: mongoose_acc:t(),
    El :: exml:element(),
    Result :: mongoose_acc:t().
xmpp_send_element(Server, Acc, El) ->
    ejabberd_hooks:run_fold(xmpp_send_element, Server, Acc, [El]).

%% Roster related hooks

%%% @doc The `roster_get_jid_info` hook is called to determine the state of subscription between a given pair of users.
%%% The hooks handler needs to expect following arguments
%%% * Acc with initial value of {none, []},
%%% * LUser, a stringprepd username part of the roster's owner
%%% * LServer, a stringprepd server part of the roster's owner (same value as HookServer)
%%% * RemoteBareJID, a bare JID of the other user
%%%
%%% The arguments and the return value types correspond to the following spec.
-spec roster_get_jid_info(HookServer, InitialValue, LUser, RemoteJID) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: {mod_roster:subscription_state(), []},
      LUser :: jid:luser(),
      RemoteJID :: jid:jid() | jid:simple_jid(),
      Result :: {mod_roster:subscription_state(), [binary()]}.
roster_get_jid_info(HookServer, InitialValue, LUser, RemBareJID) ->
    ejabberd_hooks:run_fold(roster_get_jid_info, HookServer, InitialValue,
                            [LUser, HookServer, RemBareJID]).

%% MUC related hooks

%%% @doc The `is_muc_room_owner` hooks is called to determine if a given user is a room's owner.
%%%
%%% The hook's handler needs to expect following arguments `Acc`, `Room`, `User`.
%%% The arguemtns and the return value types correspond the following spec.
-spec is_muc_room_owner(HookServer, Acc, Room, User) -> Result when
      HookServer :: jid:lserver(),
      Acc :: boolean(),
      Room :: jid:jid(),
      User :: jid:jid(),
      Result :: boolean().
is_muc_room_owner(HookServer, Acc, Room, User) ->
    ejabberd_hooks:run_fold(is_muc_room_owner, HookServer, Acc, [Room, User]).

%%% @doc The `can_access_identity` hook is called to determine if a given user can see the real identity of people in a room.
-spec can_access_identity(HookServer, Acc, Room, User) -> Result when
      HookServer :: jid:lserver(),
      Acc :: boolean(),
      Room :: jid:jid(),
      User :: jid:jid(),
      Result :: boolean().
can_access_identity(HookServer, Acc, Room, User) ->
    ejabberd_hooks:run_fold(can_access_identity, HookServer, Acc, [Room, User]).

%%% @doc The `muc_room_pid` hooks is called to get a pid for a given room's JID
-spec muc_room_pid(HookServer, InitialValue, Room) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: undefined,
      Room :: jid:jid(),
      Result :: undefined | {ok, processless | pid()} | {error, not_found}.
muc_room_pid(HookServer, InitialValue, Room) ->
    ejabberd_hooks:run_fold(muc_room_pid, HookServer, InitialValue, [Room]).

%%% @doc The `can_access_room` hook is called to determine if a given user can access a room.
-spec can_access_room(HookServer, Acc, Room, User) -> Result when
      HookServer :: jid:lserver(),
      Acc :: boolean(),
      Room :: jid:jid(),
      User :: jid:jid(),
      Result :: boolean().
can_access_room(HookServer, Acc, Room, User) ->
    ejabberd_hooks:run_fold(can_access_room, HookServer, Acc, [Room, User]).

%% MAM related hooks

%%% @doc The `mam_drop_iq` hooks is called when a MAM related IQ was dropped by the server.
-spec mam_drop_iq(HookServer, Acc, To, IQ, Action, Reason) -> ok when
      HookServer :: jid:lserver(),
      Acc :: mongoose_acc:t(),
      To :: jid:jid(),
      IQ :: jlib:iq(),
      Action :: mam_iq:action(),
      Reason :: term().
mam_drop_iq(HookServer, Acc, To, IQ, Action, Reason) ->
    ejabberd_hooks:run_fold(mam_drop_iq, HookServer, Acc, [HookServer, To, IQ, Action, Reason]).

%%% @doc The `mam_archive_id` hook is called to determine the id of an archive for a particular user or entity.
%%% The hook handler is expected to accept the following arguments:
%%% * Acc with an initial value of `undefined`
%%% * Host as passed in the `HooksServer` variable
%%% * OwnerJID
%%%
%%% and to return an integer value corresponding to the given owner's archive.
%%%
%%% If a MAM backend doesn't support or doesn't require archive IDs,
%%% `undefined` may be returned.
-spec mam_archive_id(HookServer, InitialValue, OwnerJID) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: undefined,
      OwnerJID :: jid:jid(),
      Result :: undefined | mod_mam:archive_id().
mam_archive_id(HookServer, InitialValue, OwnerJID) ->
    ejabberd_hooks:run_fold(mam_archive_id, HookServer, InitialValue, [HookServer, OwnerJID]).

%%% @doc The `mam_archive_size` hook is called to determine the size of the archive for a given JID
%%%
-spec mam_archive_size(HookServer, InitialValue, ArchiveID, OwnerJID) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: 0,
      ArchiveID :: undefined | mod_mam:archive_id(),
      OwnerJID :: jid:jid(),
      Result :: integer().
mam_archive_size(HookServer, InitialValue, ArchiveID, OwnerJID) ->
    ejabberd_hooks:run_fold(mam_archive_size, HookServer, InitialValue, [HookServer, ArchiveID, OwnerJID]).

%%% @doc The `mam_get_behaviour` hooks is called to determine if a message should be archived or not based on a given pair of JIDs.
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

%%% @doc The `mam_set_prefs` hook is called to set a user's archive preferences.
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

%%% @doc The `mam_get_prefs` hook is called to read the archive settings for a given user.
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

%%% @doc The `mam_remove_archive` hook is called in order to remove the entire archive for a particular user
-spec mam_remove_archive(HookServer, InitialValue, ArchiveID, OwnerJID) -> any() when
      HookServer :: jid:lserver(),
      InitialValue :: ok,
      ArchiveID :: undefined | mod_mam:archive_id(),
      OwnerJID :: jid:jid().
mam_remove_archive(HookServer, InitialValue, ArchiveID, OwnerJID) ->
    ejabberd_hooks:run_fold(mam_remove_archive, HookServer, InitialValue,
                            [HookServer, ArchiveID, OwnerJID]).

%%% @doc The `mam_lookup_messages` hook is to retrive archived messages for given search parameters
-spec mam_lookup_messages(HookServer, InitialValue, Params) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: {ok, {0, 0, []}},
      Params :: map(),
      Result :: {ok, mod_mam:lookup_result()}.
mam_lookup_messages(HookServer, InitialValue, Params) ->
    ejabberd_hooks:run_fold(mam_lookup_messages, HookServer, InitialValue,
                            [HookServer, Params]).

%%% @doc The `mam_archive_message` hook is called in order to store the message in the archive.
-spec mam_archive_message(HookServer, InitialValue, MessageID, ArchiveID, OwnerJID, RemoteJID, SenderJID, Dir, Packet) ->
    Result when
    HookServer :: jid:lserver(),
    InitialValue :: ok,
    MessageID :: mod_mam:message_id(),
    ArchiveID :: undefined | mod_mam:archive_id(),
    OwnerJID :: jid:jid(),
    RemoteJID :: jid:jid(),
    SenderJID :: jid:jid(),
    Dir :: incoming | outgoing,
    Packet :: term(),
    Result :: ok | {error, timeout}.
mam_archive_message(HookServer, InitialValue, MessageID, ArchiveID, OwnerJID, RemoteJID, SenderJID, Dir, Packet) ->
    ejabberd_hooks:run_fold(mam_archive_message, HookServer, InitialValue,
                            [HookServer, MessageID, ArchiveID, OwnerJID,
                             RemoteJID, SenderJID, Dir, Packet]).

%%% @doc The `mam_drop_message` hook is called when message archive failed.
%%%
%%% For those MAM backends which perform synchronous writes of messages the hook is called when there was an error while writing the message.
%%% In case of backends performing asynchronous writes,
%%% this hook is called when the process responsible for async and bulk writes died when talking to it.
-spec mam_drop_message(HookServer :: jid:lserver(), InitialValue :: ok) -> ok.
mam_drop_message(HookServer, InitialValue) ->
    ejabberd_hooks:run_fold(mam_drop_message, HookServer, InitialValue, [HookServer]).

%%% @doc The `mam_drop_messages` hook is called when the async bulk write operation failed
-spec mam_drop_messages(HookServer :: jid:lserver(), InitialValue :: ok, MessageCount :: integer()) -> ok.
mam_drop_messages(HookServer, InitialValue, MessageCount) ->
    ejabberd_hooks:run_fold(mam_drop_messages, HookServer, InitialValue, [HookServer, MessageCount]).

%%% @doc The `mam_flush_messages` hook is run after the async bulk write happens despite the write operation's result
-spec mam_flush_messages(HookServer :: jid:lserver(), InitialValue :: ok, MessageCount :: integer()) -> ok.
mam_flush_messages(HookServer, InitialValue, MessageCount) ->
    ejabberd_hooks:run_fold(mam_flush_messages, HookServer, InitialValue, [HookServer, MessageCount]).

%% MAM MUC related hooks

%%% @doc The `mam_muc_drop_iq` hooks is called when a MAM related IQ was dropped by the server.
-spec mam_muc_drop_iq(HookServer, Acc, To, IQ, Action, Reason) -> ok when
      HookServer :: jid:lserver(),
      Acc :: mongoose_acc:t(),
      To :: jid:jid(),
      IQ :: jlib:iq(),
      Action :: mam_iq:action(),
      Reason :: term().
mam_muc_drop_iq(HookServer, Acc, To, IQ, Action, Reason) ->
    ejabberd_hooks:run_fold(mam_muc_drop_iq, HookServer, Acc, [HookServer, To, IQ, Action, Reason]).

%%% @doc The `mam_muc_archive_id` hook is called to determine the archive ID for a particular room.
%%% The hook handler is expected to accept the following arguments:
%%% * Acc with initial value `undefined`
%%% * Host as passed in `HooksServer` variable
%%% * OwnerJID
%%%
%%% and return an integer value corresponding to the given owner's archive.
%%%
%%% If a MAM backend doesn't support or doesn't require archive IDs,
%%% `undefined` may be returned.
-spec mam_muc_archive_id(HookServer, InitialValue, OwnerJID) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: undefined,
      OwnerJID :: jid:jid(),
      Result :: undefined | mod_mam:archive_id().
mam_muc_archive_id(HookServer, InitialValue, OwnerJID) ->
    ejabberd_hooks:run_fold(mam_muc_archive_id, HookServer, InitialValue, [HookServer, OwnerJID]).

%%% @doc The `mam_muc_archive_size` hook is called to determine the archive's size for a given room.
%%%
-spec mam_muc_archive_size(HookServer, InitialValue, ArchiveID, RoomJID) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: 0,
      ArchiveID :: undefined | mod_mam:archive_id(),
      RoomJID :: jid:jid(),
      Result :: integer().
mam_muc_archive_size(HookServer, InitialValue, ArchiveID, RoomJID) ->
    ejabberd_hooks:run_fold(mam_muc_archive_size, HookServer, InitialValue, [HookServer, ArchiveID, RoomJID]).

%%% @doc The `mam_muc_get_behaviour` hooks is called to determine if a message should be archived or not based on the given room and user JIDs.
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

%%% @doc The `mam_muc_set_prefs` hook is called to set a room's archive preferences.
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

%%% @doc The `mam_muc_get_prefs` hook is called to read the archive settings for a given room.
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

%%% @doc The `mam_muc_remove_archive` hook is called in order to remove the entire archive for a particular user
-spec mam_muc_remove_archive(HookServer, InitialValue, ArchiveID, RoomJID) -> any() when
      HookServer :: jid:lserver(),
      InitialValue :: ok,
      ArchiveID :: undefined | mod_mam:archive_id(),
      RoomJID :: jid:jid().
mam_muc_remove_archive(HookServer, InitialValue, ArchiveID, RoomJID) ->
    ejabberd_hooks:run_fold(mam_muc_remove_archive, HookServer, InitialValue,
                            [HookServer, ArchiveID, RoomJID]).

%%% @doc The `mam_muc_lookup_messages` hook is to retrieve archived MUC messages for any given search parameters
-spec mam_muc_lookup_messages(HookServer, InitialValue, Params) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: {ok, {0, 0, []}},
      Params :: map(),
      Result :: {ok, mod_mam:lookup_result()}.
mam_muc_lookup_messages(HookServer, InitialValue, Params) ->
    ejabberd_hooks:run_fold(mam_muc_lookup_messages, HookServer, InitialValue,
                            [HookServer, Params]).

%%% @doc The `mam_muc_archive_message` hook is called in order to store the MUC message in the archive.
-spec mam_muc_archive_message(HookServer, InitialValue, MessageID, ArchiveID, OwnerJID, RemoteJID, SenderJID, Dir, Packet) ->
    Result when
    HookServer :: jid:lserver(),
    InitialValue :: ok,
    MessageID :: mod_mam:message_id(),
    ArchiveID :: undefined | mod_mam:archive_id(),
    OwnerJID :: jid:jid(),
    RemoteJID :: jid:jid(),
    SenderJID :: jid:jid(),
    Dir :: incoming | outgoing,
    Packet :: term(),
    Result :: ok | {error, timeout}.
mam_muc_archive_message(HookServer, InitialValue, MessageID, ArchiveID, OwnerJID, RemoteJID, SenderJID, Dir, Packet) ->
    ejabberd_hooks:run_fold(mam_muc_archive_message, HookServer, InitialValue,
                            [HookServer, MessageID, ArchiveID, OwnerJID,
                             RemoteJID, SenderJID, Dir, Packet]).

%%% @doc The `mam_muc_flush_messages` hook is run after the async bulk write happens for MUC messages despite the result of the write
-spec mam_muc_flush_messages(HookServer :: jid:lserver(), InitialValue :: ok, MessageCount :: integer()) -> ok.
mam_muc_flush_messages(HookServer, InitialValue, MessageCount) ->
    ejabberd_hooks:run_fold(mam_muc_flush_messages, HookServer, InitialValue, [HookServer, MessageCount]).

%% GDRP related hooks

%%% @doc `get_mam_pm_gdpr_data` hook is called to provide a user's archive for GDPR purposes
-spec get_mam_pm_gdpr_data(HookServer, InitialValue, JID) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: [],
      JID :: jid:jid(),
      Result :: ejabberd_gen_mam_archive:mam_pm_gdpr_data().
get_mam_pm_gdpr_data(HookServer, InitialValue, JID) ->
    ejabberd_hooks:run_fold(get_mam_pm_gdpr_data, HookServer, InitialValue, [JID]).

%%% @doc `get_mam_muc_gdpr_data` hook is called to provide a user's archive for GDPR purposes
-spec get_mam_muc_gdpr_data(HookServer, InitialValue, JID) -> Result when
      HookServer :: jid:lserver(),
      InitialValue :: [],
      JID :: jid:jid(),
      Result :: ejabberd_gen_mam_archive:mam_muc_gdpr_data().
get_mam_muc_gdpr_data(HookServer, InitialValue, JID) ->
    ejabberd_hooks:run_fold(get_mam_muc_gdpr_data, HookServer, InitialValue, [JID]).
