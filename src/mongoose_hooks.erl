-module(mongoose_hooks).

-export([auth_failed/3,
         c2s_broadcast_recipients/7,
         c2s_filter_packet/7,
         c2s_preprocessing_hook/3,
         c2s_presence_in/5,
         c2s_stream_features/2,
         c2s_stream_features/3,
         c2s_unauthenticated_iq/5,
         c2s_update_presence/2,
         check_bl_c2s/3,
         failed_to_store_message/4,
         forbidden_session_hook/3,
         offline_groupchat_message_hook/5,
         offline_message_hook/5,
         presence_probe_hook/5,
         privacy_check_packet/7,
         privacy_get_user_list/4,
         privacy_iq_get/6,
         privacy_iq_set/5,
         privacy_updated_list/4,
         resend_offline_messages_hook/4,
         roster_get_subscription_lists/4,
         roster_get_versioning_feature/2,
         roster_get_versioning_feature/3,
         roster_in_subscription/7,
         roster_out_subscription/6,
         session_opening_allowed_for_user/3,
         set_presence_hook/6,
         sm_broadcast/6,
         sm_filter_offline_message/5,
         sm_register_connection_hook/4,
         sm_remove_connection_hook/6,
         unacknowledged_message/3,
         unset_presence_hook/6,
         user_available_hook/3,
         user_receive_packet/6,
         user_sent_keep_alive/2,
         user_send_packet/5,
         xmpp_bounce_message/2,
         xmpp_send_element/3]).

-spec auth_failed(HookServer, Username, Server) -> Result when
    HookServer :: jid:server() | global,
    Username :: jid:user() | unknown,
    Server :: jid:server(),
    Result :: ok.
auth_failed(HookServer, Username, Server) ->
    ejabberd_hooks:run_fold(auth_failed, HookServer, ok, [Username, Server]).

-spec c2s_broadcast_recipients(HookServer, Acc, Server, State, Type, From, Packet) -> Result when
    HookServer :: jid:server() | global,
    Server :: jid:server(),
    Acc :: list(),
    State :: ejabberd_c2s:state(),
    Type :: {atom(), any()},
    From :: jid:jid(),
    Packet :: exml:element(),
    Result :: list().
c2s_broadcast_recipients(HookServer, Acc, Server, State, Type, From, Packet) ->
    ejabberd_hooks:run_fold(c2s_broadcast_recipients,
                            HookServer,
                            Acc,
                            [Server, State, Type, From, Packet]).

-spec c2s_filter_packet(HookServer, Drop, Server, State, Feature, To, Packet) -> Result when
    HookServer :: jid:server() | global,
    Drop :: boolean(),
    Server :: jid:server(),
    State :: ejabberd_c2s:state(),
    Feature :: {atom(), binary()},
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: boolean().
c2s_filter_packet(HookServer, Drop, Server, State, Feature, To, Packet) ->
    ejabberd_hooks:run_fold(c2s_filter_packet,
                            HookServer,
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

-spec c2s_stream_features(HookServer, Server) -> Result when
    HookServer :: jid:server() | global,
    Server :: jid:server(),
    Result :: [exml:element()].
c2s_stream_features(HookServer, Server) ->
    c2s_stream_features(HookServer, [], Server).

-spec c2s_stream_features(HookServer, Acc, Server) -> Result when
    HookServer :: jid:server() | global,
    Acc :: [exml:element()],
    Server :: jid:server(),
    Result :: [exml:element()].
c2s_stream_features(HookServer, Acc, Server) ->
    ejabberd_hooks:run_fold(c2s_stream_features, HookServer, Acc, [Server]).

-spec c2s_unauthenticated_iq(HookServer, Acc, Server, IQ, IP) -> Result when
    HookServer :: jid:server() | global,
    Acc :: empty,
    Server :: jid:server(),
    IQ :: jlib:iq(),
    IP :: {inet:ip_address(), inet:port_number()} | undefined,
    Result :: exml:element() | empty.
c2s_unauthenticated_iq(HookServer, Acc, Server, IQ, IP) ->
    ejabberd_hooks:run_fold(c2s_unauthenticated_iq,
                            HookServer,
                            Acc,
                            [Server, IQ, IP]).

-spec c2s_update_presence(HookServer, Acc) -> Result when
    HookServer :: jid:server() | global,
    Acc :: mongoose_acc:t(),
    Result :: mongoose_acc:t().
c2s_update_presence(HookServer, Acc) ->
    ejabberd_hooks:run_fold(c2s_update_presence, HookServer, Acc, []).

-spec check_bl_c2s(HookServer, Blacklisted, IP) -> Result when
    HookServer :: jid:server() | global,
    Blacklisted :: boolean(),
    IP ::  inet:ip_address(),
    Result :: boolean().
check_bl_c2s(HookServer, Blacklisted, IP) ->
    ejabberd_hooks:run_fold(check_bl_c2s, HookServer, Blacklisted, [IP]).

-spec failed_to_store_message(HookServer, Acc, From, Packet) -> Result when
    HookServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    Packet :: exml:element(),
    Result :: mongoose_acc:t().
failed_to_store_message(HookServer, Acc, From, Packet) ->
    ejabberd_hooks:run_fold(failed_to_store_message,
                            HookServer,
                            Acc,
                            [From, Packet]).

-spec forbidden_session_hook(HookServer, Acc, JID) -> Result when
    HookServer :: jid:server() | global,
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Result :: mongoose_acc:t().
forbidden_session_hook(HookServer, Acc, JID) ->
    ejabberd_hooks:run_fold(forbidden_session_hook, HookServer, Acc, [JID]).

-spec offline_groupchat_message_hook(HookServer, Acc, From, To, Packet) -> Result when
    HookServer :: jid:lserver(),
    Acc :: map(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: map().
offline_groupchat_message_hook(HookServer, Acc, From, To, Packet) ->
    ejabberd_hooks:run_fold(offline_groupchat_message_hook,
                            HookServer,
                            Acc,
                            [From, To, Packet]).

-spec offline_message_hook(HookServer, Acc, From, To, Packet) -> Result when
    HookServer :: jid:lserver(),
    Acc :: map(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: map().
offline_message_hook(HookServer, Acc, From, To, Packet) ->
    ejabberd_hooks:run_fold(offline_message_hook,
                            HookServer,
                            Acc,
                            [From, To, Packet]).

-spec presence_probe_hook(HookServer, Acc, From, To, Pid) -> Result when
    HookServer :: jid:server() | global,
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Pid :: pid(),
    Result :: mongoose_acc:t().
presence_probe_hook(HookServer, Acc, From, To, Pid) ->
    ejabberd_hooks:run_fold(presence_probe_hook,
                            HookServer,
                            Acc,
                            [From, To, Pid]).

-spec privacy_check_packet(HookServer, Acc, User, LServer, PrivacyList, FromToNameType, Dir) -> Result when
      HookServer :: jid:server() | global,
      Acc :: mongoose_acc:t(), User :: jid:luser(), LServer :: jid:lserver(),
      PrivacyList :: mongoose_privacy:userlist(),
      FromToNameType :: {jid:jid(), jid:jid(), binary(), binary()},
      Dir :: in | out,
      Result :: mongoose_acc:t().
privacy_check_packet(HookServer, Acc, User, LServer, PrivacyList, FromToNameType, Dir) ->
    ejabberd_hooks:run_fold(privacy_check_packet,
                            HookServer,
                            mongoose_acc:set(hook, result, allow, Acc),
                            [User,
                             LServer,
                             PrivacyList,
                             FromToNameType,
                             Dir]).

-spec privacy_get_user_list(HookServer, UserList, User, Server) -> Result when
    HookServer :: jid:server() | global,
    UserList :: mongoose_privacy:userlist(),
    User :: jid:user(),
    Server :: jid:server(),
    Result :: mongoose_privacy:userlist().
privacy_get_user_list(HookServer, UserList, User, Server) ->
    ejabberd_hooks:run_fold(privacy_get_user_list, HookServer,
                                          UserList, [User, Server]).

-spec privacy_iq_get(HookServer, Acc, From, To, IQ, PrivList) -> Result when
    HookServer :: jid:server() | global,
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    IQ :: jlib:iq(),
    PrivList :: mongoose_privacy:userlist(),
    Result :: mongoose_acc:t().
privacy_iq_get(HookServer, Acc, From, To, IQ, PrivList) ->
    ejabberd_hooks:run_fold(privacy_iq_get,
                            HookServer,
                            Acc,
                            [From, To, IQ, PrivList]).

-spec privacy_iq_set(HookServer, Acc, From, To, IQ) -> Result when
    HookServer :: jid:server() | global,
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    IQ :: jlib:iq(),
    Result :: mongoose_acc:t().
privacy_iq_set(HookServer, Acc, From, To, IQ) ->
    ejabberd_hooks:run_fold(privacy_iq_set,
                            HookServer,
                            Acc,
                            [From, To, IQ]).

-spec privacy_updated_list(HookServer, Acc, OldList, NewList) -> Result when
    HookServer :: jid:server() | global,
    Acc :: false,
    OldList :: mongoose_privacy:userlist(),
    NewList :: mongoose_privacy:userlist(),
    Result :: false | mongoose_privacy:userlist().
privacy_updated_list(HookServer, Acc, OldList, NewList) ->
    ejabberd_hooks:run_fold(privacy_updated_list, HookServer,
                            Acc, [OldList, NewList]).

-spec resend_offline_messages_hook(HookServer, Acc, User, Server) -> Result when
    HookServer :: jid:server() | global,
    Acc :: mongoose_acc:t(),
    User :: jid:user(),
    Server :: jid:server(),
    Result :: mongoose_acc:t().
resend_offline_messages_hook(HookServer, Acc, User, Server) ->
    ejabberd_hooks:run_fold(resend_offline_messages_hook,
                            HookServer,
                            Acc,
                            [User, Server]).

-spec roster_get_subscription_lists(HookServer, Acc, User, Server) -> Result when
    HookServer :: jid:server() | global,
    Acc ::mongoose_acc:t(),
    User :: jid:user(),
    Server :: jid:server(),
    Result :: mongoose_acc:t().
roster_get_subscription_lists(HookServer, Acc, User, Server) ->
    ejabberd_hooks:run_fold(roster_get_subscription_lists, HookServer, Acc, [User, Server]).

-spec roster_get_versioning_feature(HookServer, Server) -> Result when
    HookServer :: jid:server() | global,
    Server :: jid:server(),
    Result :: [exml:element()].
roster_get_versioning_feature(HookServer, Server) ->
    roster_get_versioning_feature(HookServer, [], Server).

-spec roster_get_versioning_feature(HookServer, Acc, Server) -> Result when
    HookServer :: jid:server() | global,
    Acc :: [exml:element()],
    Server :: jid:server(),
    Result :: [exml:element()].
roster_get_versioning_feature(HookServer, Acc, Server) ->
    ejabberd_hooks:run_fold(roster_get_versioning_feature,
                            HookServer, Acc, [Server]).


-spec roster_in_subscription(HookServer, Acc, User, LServer, From, Type, Reason) -> Result when
    HookServer :: jid:server() | global,
    Acc :: mongoose_acc:t(),
    User :: jid:user(),
    LServer :: jid:lserver(),
    From :: jid:jid(),
    Type :: mod_roster:sub_presence(),
    Reason :: any(),
    Result :: mongoose_acc:t().
roster_in_subscription(HookServer, Acc, User, LServer, From, Type, Reason) ->
    ejabberd_hooks:run_fold(
        roster_in_subscription,
        HookServer,
        Acc,
        [User, LServer, From, Type, Reason]).

-spec roster_out_subscription(HookServer, Acc, User, Server, To, Type) -> Result when
    HookServer :: jid:server() | global,
    Acc :: mongoose_acc:t(),
    User :: jid:user(),
    Server :: jid:server(),
    To :: jid:jid(),
    Type :: mod_roster:sub_presence(),
    Result :: mongoose_acc:t().
roster_out_subscription(HookServer, Acc, User, Server, To, Type) ->
    ejabberd_hooks:run_fold(roster_out_subscription,
                            HookServer,
                            Acc,
                            [User, Server, To, Type]).

-spec session_opening_allowed_for_user(HookServer, Allow, JID) -> Result when
    HookServer :: jid:server() | global,
    Allow :: any(),
    JID :: jid:jid(),
    Result :: any().
session_opening_allowed_for_user(HookServer, Allow, JID) ->
    ejabberd_hooks:run_fold(session_opening_allowed_for_user,
                            HookServer,
                            Allow, [JID]).

-spec set_presence_hook(HookServer, Acc, LUser, LServer, LResource, Presence) -> Result when
    HookServer :: jid:server() | global,
    Acc :: mongoose_acc:t(),
    LUser :: jid:luser(), LServer :: jid:lserver(), LResource :: jid:lresource(),
    Presence :: any(),
    Result :: mongoose_acc:t().
set_presence_hook(HookServer, Acc, LUser, LServer, LResource, Presence) ->
    ejabberd_hooks:run_fold(set_presence_hook, HookServer, Acc, [LUser, LServer, LResource, Presence]).

-spec sm_broadcast(HookServer, Acc, From, To, Broadcast, SessionCount) -> Result when
    HookServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Broadcast :: ejabberd_c2s:broadcast(),
    SessionCount :: non_neg_integer(),
    Result :: mongoose_acc:t().
sm_broadcast(HookServer, Acc, From, To, Broadcast, SessionCount) ->
    ejabberd_hooks:run_fold(sm_broadcast, HookServer, Acc,
                            [From, To, Broadcast, SessionCount]).

-spec sm_filter_offline_message(HookServer, Drop, From, To, Packet) -> Result when
    HookServer :: jid:lserver(),
    Drop :: boolean(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: boolean().
sm_filter_offline_message(HookServer, Drop, From, To, Packet) ->
    ejabberd_hooks:run_fold(sm_filter_offline_message, HookServer,
                            Drop, [From, To, Packet]).

-spec sm_register_connection_hook(HookServer, SID, JID, Info) -> Result when
    HookServer :: jid:lserver(),
    SID :: 'undefined' | ejabberd_sm:sid(),
    JID :: jid:jid(),
    Info :: ejabberd_sm:info(),
    Result :: ok.
sm_register_connection_hook(HookServer, SID, JID, Info) ->
    ejabberd_hooks:run_fold(sm_register_connection_hook, HookServer, ok,
                       [SID, JID, Info]).

-spec sm_remove_connection_hook(HookServer, Acc, SID, JID, Info, Reason) -> Result when
    HookServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    SID :: 'undefined' | ejabberd_sm:sid(),
    JID :: jid:jid(),
    Info :: ejabberd_sm:info(),
    Reason :: ejabberd_sm:close_reason(),
    Result :: mongoose_acc:t().
sm_remove_connection_hook(HookServer, Acc, SID, JID, Info, Reason) ->
    ejabberd_hooks:run_fold(sm_remove_connection_hook, HookServer, Acc,
                            [SID, JID, Info, Reason]).

-spec unacknowledged_message(HookServer, Acc, JID) -> Result when
    HookServer :: jid:server() | global,
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Result :: mongoose_acc:t().
unacknowledged_message(HookServer, Acc, JID) ->
    ejabberd_hooks:run_fold(unacknowledged_message, HookServer, Acc, [JID]).

-spec unset_presence_hook(HookServer, Acc, LUser, LServer, LResource, Status) -> Result when
    HookServer :: jid:server() | global,
    Acc :: mongoose_acc:t(),
    LUser :: jid:luser(),
    LServer :: jid:lserver(),
    LResource :: jid:lresource(),
    Status :: binary(),
    Result :: mongoose_acc:t().
unset_presence_hook(HookServer, Acc, LUser, LServer, LResource, Status) ->
    ejabberd_hooks:run_fold(unset_presence_hook, HookServer, Acc,
                            [LUser, LServer, LResource, Status]).

-spec user_available_hook(HookServer, Acc, JID) -> Result when
    HookServer :: jid:server() | global,
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    Result :: mongoose_acc:t().
user_available_hook(HookServer, Acc, JID) ->
    ejabberd_hooks:run_fold(user_available_hook,
                            HookServer,
                            Acc,
                            [JID]).

-spec user_receive_packet(HookServer, Acc, JID, From, To, El) -> Result when
    HookServer :: jid:server() | global,
    Acc :: mongoose_acc:t(),
    JID :: jid:jid(),
    From :: jid:jid(),
    To :: jid:jid(),
    El :: exml:element(),
    Result :: mongoose_acc:t().
user_receive_packet(HookServer, Acc, JID, From, To, El) ->
    ejabberd_hooks:run_fold(user_receive_packet,
                            HookServer,
                            Acc,
                            [JID, From, To, El]).

-spec user_sent_keep_alive(HookServer, JID) -> Result when
    HookServer :: jid:server() | global,
    JID :: jid:jid(),
    Result :: any().
user_sent_keep_alive(HookServer, JID) ->
    ejabberd_hooks:run_fold(user_sent_keep_alive, HookServer, ok, [JID]).

-spec user_send_packet(HookServer, Acc, From, To, Packet) -> Result when
    HookServer :: jid:lserver(),
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Packet :: exml:element(),
    Result :: mongoose_acc:t().
user_send_packet(HookServer, Acc, From, To, Packet) ->
    ejabberd_hooks:run_fold(user_send_packet,
                            HookServer,
                            Acc,
                            [From, To, Packet]).

-spec xmpp_bounce_message(HookServer, Acc) -> Result when
    HookServer :: jid:server() | global,
    Acc :: mongoose_acc:t(),
    Result :: mongoose_acc:t().
xmpp_bounce_message(HookServer, Acc) ->
    ejabberd_hooks:run_fold(xmpp_bounce_message, HookServer, Acc, []).

-spec xmpp_send_element(HookServer, Acc, El) -> Result when
    HookServer :: jid:server() | global,
    Acc :: mongoose_acc:t(),
    El :: exml:element(),
    Result :: mongoose_acc:t().
xmpp_send_element(HookServer, Acc, El) ->
    ejabberd_hooks:run_fold(xmpp_send_element, HookServer, Acc, [El]).
