-module(mongoose_hooks).

-export([auth_failed/2,
         c2s_broadcast_recipients/6,
         c2s_filter_packet/6,
         c2s_loop_debug/1,
         c2s_loop_debug/2,
         c2s_preprocessing_hook/3,
         c2s_presence_in/5,
         c2s_stream_features/1,
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
         roster_get_versioning_feature/1,
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

-spec c2s_loop_debug(Info) -> Result when
    Info :: any(),
    Result :: any().
c2s_loop_debug(Info) ->
    ejabberd_hooks:run(c2s_loop_debug, [Info]).
-spec c2s_loop_debug(Acc, Info) -> Result when
    Acc :: mongoose_acc:t(),
    Info :: any(),
    Result :: any().
c2s_loop_debug(Acc, Info) ->
    ejabberd_hooks:run_fold(c2s_loop_debug, Acc, [Info]).

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

-spec c2s_stream_features(Server) -> Result when
    Server :: jid:server(),
    Result :: [exml:element()].
c2s_stream_features(Server) ->
    ejabberd_hooks:run_fold(c2s_stream_features, Server, [], [Server]).

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

-spec roster_get_versioning_feature(Server) -> Result when
    Server :: jid:server(),
    Result :: [exml:element()].
roster_get_versioning_feature(Server) ->
    ejabberd_hooks:run_fold(roster_get_versioning_feature,
                            Server, [], [Server]).

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
