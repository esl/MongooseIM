-module(mongoose_hooks).

-export([failed_to_store_message/4,
         offline_groupchat_message_hook/5,
         offline_message_hook/5,
         privacy_check_packet/6,
         privacy_get_user_list/3,
         roster_in_subscription/7,
         set_presence_hook/5,
         sm_broadcast/6,
         sm_filter_offline_message/5,
         sm_register_connection_hook/4,
         sm_remove_connection_hook/6,
         unset_presence_hook/5,
         xmpp_bounce_message/2]).

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
