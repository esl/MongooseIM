%%%-------------------------------------------------------------------
%%% @author Michal Piotrowski <michal.piotrowski@erlang-solutions.com>
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Ejabberd hooks for general metrics
%%%
%%% @end
%%% Created : 23 Apr 2013 by Michal Piotrowski <michal.piotrowski@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(mongoose_metrics_hooks).

-include("ejabberd.hrl").
-include("jlib.hrl").

-export([get_hooks/1]).

%%-------------------
%% Internal exports
%%-------------------
-export([sm_register_connection_hook/3,
         sm_remove_connection_hook/4,
         auth_failed/2,
         user_send_packet/3,
         user_receive_packet/4,
         xmpp_bounce_message/2,
         xmpp_stanza_dropped/3,
         xmpp_send_element/2,
         roster_get/2,
         roster_set/3,
         roster_push/2,
         roster_in_subscription/6,
         register_user/2,
         remove_user/2,
         privacy_iq_get/5,
         privacy_iq_set/4,
         privacy_check_packet/6,
         privacy_list_push/3,
         mam_get_prefs/4,
         mam_set_prefs/7,
         mam_remove_archive/3,
         mam_lookup_messages/14,
         mam_archive_message/9,
         mam_flush_messages/2,
         mam_drop_message/1,
         mam_drop_iq/5,
         mam_drop_messages/2,
         mam_purge_single_message/6,
         mam_purge_multiple_messages/9,
         mam_muc_get_prefs/4,
         mam_muc_set_prefs/7,
         mam_muc_remove_archive/3,
         mam_muc_lookup_messages/14,
         mam_muc_archive_message/9,
         mam_muc_flush_messages/2,
         mam_muc_drop_message/1,
         mam_muc_drop_iq/5,
         mam_muc_drop_messages/2,
         mam_muc_purge_single_message/6,
         mam_muc_purge_multiple_messages/9]).

-type hook() :: [atom() | ejabberd:server() | integer(),...].
-type metrics_notify_return() ::
                'ok'
                | {'error',_,'nonexistent_metric' | 'unsupported_metric_type'}.

%%-------------------
%% Implementation
%%-------------------

%% @doc Here will be declared which hooks should be registered
-spec get_hooks(_) -> [hook(),...].
get_hooks(Host) ->
    [[sm_register_connection_hook, Host, ?MODULE, sm_register_connection_hook, 50],
     [sm_remove_connection_hook, Host, ?MODULE, sm_remove_connection_hook, 50],
     [auth_failed, Host, ?MODULE, auth_failed, 50],
     [user_send_packet, Host, ?MODULE, user_send_packet, 50],
     [user_receive_packet, Host, ?MODULE, user_receive_packet, 50],
     [xmpp_stanza_dropped, Host, ?MODULE, xmpp_stanza_dropped, 50],
     [xmpp_bounce_message, Host, ?MODULE, xmpp_bounce_message, 50],
     [xmpp_send_element, Host, ?MODULE, xmpp_send_element, 50],
     [roster_get, Host, ?MODULE, roster_get, 55],
     [roster_set, Host, ?MODULE, roster_set, 50],
     [roster_push, Host, ?MODULE, roster_push, 50],
     [roster_in_subscription, Host, ?MODULE, roster_in_subscription, 55],
     [register_user, Host, ?MODULE, register_user, 50],
     [remove_user, Host, ?MODULE, remove_user, 50],
     [privacy_iq_get,         Host, ?MODULE, privacy_iq_get, 1],
     [privacy_iq_set,         Host, ?MODULE, privacy_iq_set, 1],
     [privacy_check_packet,   Host, ?MODULE, privacy_check_packet, 55],
     [sm_broadcast,           Host, ?MODULE, privacy_list_push, 1],
     [mam_set_prefs, Host, ?MODULE, mam_set_prefs, 50],
     [mam_get_prefs, Host, ?MODULE, mam_get_prefs, 50],
     [mam_archive_message, Host, ?MODULE, mam_archive_message, 50],
     [mam_flush_messages, Host, ?MODULE, mam_flush_messages, 50],
     [mam_drop_message, Host, ?MODULE, mam_drop_message, 50],
     [mam_drop_iq, Host, ?MODULE, mam_drop_iq, 50],
     [mam_drop_messages, Host, ?MODULE, mam_drop_messages, 50],
     [mam_remove_archive, Host, ?MODULE, mam_remove_archive, 50],
     [mam_lookup_messages, Host, ?MODULE, mam_lookup_messages, 100],
     [mam_purge_single_message, Host, ?MODULE, mam_purge_single_message, 50],
     [mam_purge_multiple_message, Host, ?MODULE, mam_purge_multiple_message, 50],
     [mam_muc_set_prefs, Host, ?MODULE, mam_muc_set_prefs, 50],
     [mam_muc_get_prefs, Host, ?MODULE, mam_muc_get_prefs, 50],
     [mam_muc_archive_message, Host, ?MODULE, mam_muc_archive_message, 50],
     [mam_muc_remove_archive, Host, ?MODULE, mam_muc_remove_archive, 50],
     [mam_muc_lookup_messages, Host, ?MODULE, mam_muc_lookup_messages, 100],
     [mam_muc_purge_single_message, Host, ?MODULE, mam_muc_purge_single_message, 50],
     [mam_muc_purge_multiple_message, Host, ?MODULE, mam_muc_purge_multiple_message, 50]].


-spec sm_register_connection_hook(tuple(), ejabberd:jid(), term()
                                 ) -> metrics_notify_return().
sm_register_connection_hook(_,#jid{server = Server}, _) ->
    mongoose_metrics:update({Server, sessionSuccessfulLogins}, 1),
    mongoose_metrics:update({Server, sessionCount}, 1).

-spec sm_remove_connection_hook(tuple(), ejabberd:jid(),
                                term(), ejabberd_sm:close_reason()
                               ) -> metrics_notify_return().
sm_remove_connection_hook(_,#jid{server = Server},_, _Reason) ->
    mongoose_metrics:update({Server, sessionLogouts}, 1),
    mongoose_metrics:update({Server, sessionCount}, -1).

-spec auth_failed(binary(), binary()) -> metrics_notify_return().
auth_failed(_,Server) ->
    mongoose_metrics:update({Server, sessionAuthFails},1).

-spec user_send_packet(ejabberd:jid(), tuple(), tuple()
                      ) -> metrics_notify_return().
user_send_packet(#jid{server = Server},_,Packet) ->
    mongoose_metrics:update({Server, xmppStanzaSent}, 1),
    user_send_packet_type(Server, Packet).

-spec user_send_packet_type(Server :: ejabberd:server(),
                            Packet :: jlib:xmlel()) -> metrics_notify_return().
user_send_packet_type(Server, #xmlel{name = <<"message">>}) ->
    mongoose_metrics:update({Server, xmppMessageSent}, 1);
user_send_packet_type(Server, #xmlel{name = <<"iq">>}) ->
    mongoose_metrics:update({Server, xmppIqSent}, 1);
user_send_packet_type(Server, #xmlel{name = <<"presence">>}) ->
    mongoose_metrics:update({Server, xmppPresenceSent}, 1).

-spec user_receive_packet(ejabberd:jid(), tuple(), tuple(), tuple()) -> term().
user_receive_packet(#jid{server = Server} ,_,_,Packet) ->
    mongoose_metrics:update({Server, xmppStanzaReceived},1),
    user_receive_packet_type(Server, Packet).

-spec user_receive_packet_type(Server :: ejabberd:server(),
                               Packet :: jlib:xmlel()) -> metrics_notify_return().
user_receive_packet_type(Server, #xmlel{name = <<"message">>}) ->
    mongoose_metrics:update({Server, xmppMessageReceived}, 1);
user_receive_packet_type(Server, #xmlel{name = <<"iq">>}) ->
    mongoose_metrics:update({Server, xmppIqReceived}, 1);
user_receive_packet_type(Server, #xmlel{name = <<"presence">>}) ->
    mongoose_metrics:update({Server, xmppPresenceReceived}, 1).

-spec xmpp_bounce_message(Server :: ejabberd:server(),
                          tuple()) -> metrics_notify_return().
xmpp_bounce_message(Server, _) ->
    mongoose_metrics:update({Server, xmppMessageBounced}, 1).

-spec xmpp_stanza_dropped(ejabberd:jid(), tuple(), tuple()) -> metrics_notify_return().
xmpp_stanza_dropped(#jid{server = Server} ,_,_) ->
   mongoose_metrics:update({Server, xmppStanzaDropped}, 1).

-spec xmpp_send_element(Server :: ejabberd:server(),
                        Packet :: jlib:xmlel()) -> ok | metrics_notify_return().
xmpp_send_element(Server, #xmlel{name = Name, attrs = Attrs}) ->
    mongoose_metrics:update({Server, xmppStanzaCount}, 1),
    case lists:keyfind(<<"type">>, 1, Attrs) of
        {<<"type">>, <<"error">>} ->
            mongoose_metrics:update({Server, xmppErrorTotal}, 1),
            case Name of
                <<"iq">> ->
                    mongoose_metrics:update({Server, xmppErrorIq},1);
                <<"message">> ->
                    mongoose_metrics:update({Server, xmppErrorMessage},1);
                <<"presence">> ->
                    mongoose_metrics:update({Server, xmppErrorPresence}, 1)
            end;
        _ -> ok
    end;
xmpp_send_element(_, _) ->
    ok.


%% Roster

-spec roster_get(list(), {_, ejabberd:server()}) -> list().
roster_get(Acc, {_, Server}) ->
    mongoose_metrics:update({Server, modRosterGets}, 1),
    Acc.

-spec roster_set(JID :: ejabberd:jid(), tuple(), tuple()) -> metrics_notify_return().
roster_set(#jid{server = Server},_,_) ->
    mongoose_metrics:update({Server, modRosterSets}, 1).

-spec roster_in_subscription(term(), binary(), binary(), tuple(), atom(), term()) -> term().
roster_in_subscription(Acc,_,Server,_,subscribed,_) ->
    mongoose_metrics:update({Server, modPresenceSubscriptions}, 1),
    Acc;
roster_in_subscription(Acc,_,Server,_,unsubscribed,_) ->
    mongoose_metrics:update({Server, modPresenceUnsubscriptions}, 1),
    Acc;
roster_in_subscription(Acc,_,_,_,_,_) ->
    Acc.

-spec roster_push(ejabberd:jid(),term()) -> metrics_notify_return().
roster_push(#jid{server = Server},_) ->
    mongoose_metrics:update({Server, modRosterPush}, 1).

%% Register

-spec register_user(binary(), ejabberd:server()) -> metrics_notify_return().
register_user(_,Server) ->
    mongoose_metrics:update({Server, modRegisterCount}, 1).

-spec remove_user(binary(), ejabberd:server()) -> metrics_notify_return().
remove_user(_,Server) ->
    mongoose_metrics:update({Server, modUnregisterCount}, 1).

%% Privacy

-spec privacy_iq_get(term(), ejabberd:jid(), ejabberd:jid(), term(), term()) -> term().
privacy_iq_get(Acc, #jid{server  = Server}, _, _, _) ->
    mongoose_metrics:update({Server, modPrivacyGets}, 1),
    Acc.

-spec privacy_iq_set(Acc :: term(),
                     From :: ejabberd:jid(),
                     _To :: ejabberd:jid(),
                     _IQ :: ejabberd:iq()) -> ok | metrics_notify_return() | term().
privacy_iq_set(Acc, #jid{server = Server}, _To, #iq{sub_el = SubEl}) ->
    #xmlel{children = Els} = SubEl,
    case xml:remove_cdata(Els) of
        [#xmlel{name = <<"active">>}] ->
            mongoose_metrics:update({Server, modPrivacySetsActive}, 1);
        [#xmlel{name = <<"default">>}] ->
            mongoose_metrics:update({Server, modPrivacySetsDefault}, 1);
        _ ->
            ok
    end,
    mongoose_metrics:update({Server, modPrivacySets}, 1),
    Acc.

-spec privacy_list_push(_From :: ejabberd:jid(),
                        To :: ejabberd:jid(),
                        Packet :: jlib:xmlel()) -> ok | metrics_notify_return().
privacy_list_push(_From, #jid{server = Server} = To, Packet) ->
    case Packet of
        #xmlel{name = <<"broadcast">>, children = [{privacy_list, _, _}]} ->
            #jid{user = User, server = Server} = To,
            Count = length(ejabberd_sm:get_user_resources(User, Server)),
            mongoose_metrics:update({Server, modPrivacyPush}, Count);
        _ ->
            ok
    end.

-spec privacy_check_packet(Acc :: allow | deny,
                          binary(),
                          Server :: ejabberd:server(),
                          term(), term(), term()) -> allow | deny.
privacy_check_packet(Acc, _, Server, _, _, _) ->
    mongoose_metrics:update({Server, modPrivacyStanzaAll}, 1),
    case Acc of
        deny ->
            mongoose_metrics:update({Server, modPrivacyStanzaBlocked}, 1);
        allow ->
            ok
    end,
    Acc.

%% ----------------------------------------------------------------------------
%% mod_mam

-spec mam_get_prefs(Result :: any(),
                    Host :: ejabberd:server(),
                    _ArcID :: mod_mam:archive_id(),
                    _ArcJID :: ejabberd:jid()) -> any().
mam_get_prefs(Result, Host, _ArcID, _ArcJID) ->
    mongoose_metrics:update({Host, modMamPrefsGets}, 1),
    Result.

-spec mam_set_prefs(Result :: any(), Host :: ejabberd:server(),
    _ArcID :: mod_mam:archive_id(), _ArcJID :: ejabberd:jid(),
    _DefaultMode :: any(), _AlwaysJIDs :: [mod_mam:literal_jid()],
    _NeverJIDs :: [mod_mam:literal_jid()]) -> any().
mam_set_prefs(Result, Host, _ArcID, _ArcJID, _DefaultMode, _AlwaysJIDs, _NeverJIDs) ->
    mongoose_metrics:update({Host, modMamPrefsSets}, 1),
    Result.

-spec mam_remove_archive(Host :: ejabberd:server(),
                         _ArcID :: mod_mam:archive_id(),
                         _ArcJID :: ejabberd:jid()) -> metrics_notify_return().
mam_remove_archive(Host, _ArcID, _ArcJID) ->
    mongoose_metrics:update({Host, modMamArchiveRemoved}, 1).

mam_lookup_messages(Result = {ok, {_TotalCount, _Offset, MessageRows}},
    Host, _ArcID, _ArcJID,
    _RSM, _Borders,
    _Start, _End, _Now, _WithJID,
    _PageSize, _LimitPassed, _MaxResultLimit, _IsSimple) ->
    mongoose_metrics:update({Host, modMamForwarded}, length(MessageRows)),
    mongoose_metrics:update({Host, modMamLookups}, 1),
    Result;
mam_lookup_messages(Result = {error, _},
    _Host, _ArcID, _ArcJID,
    _RSM, _Borders,
    _Start, _End, _Now, _WithJID,
    _PageSize, _LimitPassed, _MaxResultLimit, _IsSimple) ->
    Result.

-spec mam_archive_message(Result :: any(), Host :: ejabberd:server(),
    _MessId :: mod_mam:message_id(), _ArcID :: mod_mam:archive_id(),
    _LocJID :: ejabberd:jid(), _RemJID :: ejabberd:jid(),
    _SrcJID :: ejabberd:jid(), _Dir :: atom(), _Packet :: jlib:xmlel()) -> any().
mam_archive_message(Result, Host,
    _MessID, _ArcID, _LocJID, _RemJID, _SrcJID, _Dir, _Packet) ->
    mongoose_metrics:update({Host, modMamArchived}, 1),
    Result.

-spec mam_flush_messages(Host :: ejabberd:server(),
                         MessageCount :: integer()) -> metrics_notify_return().
mam_flush_messages(Host, MessageCount) ->
    mongoose_metrics:update({Host, modMamFlushed}, MessageCount).

-spec mam_drop_message(Host :: ejabberd:server()) -> metrics_notify_return().
mam_drop_message(Host) ->
    mongoose_metrics:update({Host, modMamDropped}, 1).

-spec mam_drop_iq(Host :: ejabberd:server(), _To :: ejabberd:jid(),
    _IQ :: ejabberd:iq(), _Action :: any(), _Reason :: any()) -> metrics_notify_return().
mam_drop_iq(Host, _To, _IQ, _Action, _Reason) ->
    mongoose_metrics:update({Host, modMamDroppedIQ}, 1).

-spec mam_drop_messages(Host :: ejabberd:server(),
                        Count :: integer()) -> metrics_notify_return().
mam_drop_messages(Host, Count) ->
    mongoose_metrics:update({Host, modMamDropped2}, Count).

mam_purge_single_message(Result, Host, _MessID, _ArcID, _ArcJID, _Now) ->
    mongoose_metrics:update({Host, modMamSinglePurges}, 1),
    Result.

mam_purge_multiple_messages(Result, Host,
    _ArcID, _ArcJID, _Borders, _Start, _End, _Now, _WithJID) ->
    mongoose_metrics:update({Host, modMamMultiplePurges}, 1),
    Result.


%% ----------------------------------------------------------------------------
%% mod_mam_muc

mam_muc_get_prefs(Result, Host, _ArcID, _ArcJID) ->
    mongoose_metrics:update({Host, modMucMamPrefsGets}, 1),
    Result.

mam_muc_set_prefs(Result, Host, _ArcID, _ArcJID, _DefaultMode, _AlwaysJIDs, _NeverJIDs) ->
    mongoose_metrics:update({Host, modMucMamPrefsSets}, 1),
    Result.

mam_muc_remove_archive(Host, _ArcID, _ArcJID) ->
    mongoose_metrics:update({Host, modMucMamArchiveRemoved}, 1).

mam_muc_lookup_messages(Result = {ok, {_TotalCount, _Offset, MessageRows}},
    Host, _ArcID, _ArcJID,
    _RSM, _Borders,
    _Start, _End, _Now, _WithJID,
    _PageSize, _LimitPassed, _MaxResultLimit, _IsSimple) ->
    mongoose_metrics:update({Host, modMucMamForwarded}, length(MessageRows)),
    mongoose_metrics:update({Host, modMucMamLookups}, 1),
    Result;
mam_muc_lookup_messages(Result = {error, _},
    _Host, _ArcID, _ArcJID,
    _RSM, _Borders,
    _Start, _End, _Now, _WithJID,
    _PageSize, _LimitPassed, _MaxResultLimit, _IsSimple) ->
    Result.
    

mam_muc_archive_message(Result, Host,
    _MessID, _ArcID, _LocJID, _RemJID, _SrcJID, _Dir, _Packet) ->
    mongoose_metrics:update({Host, modMucMamArchived}, 1),
    Result.

mam_muc_flush_messages(Host, MessageCount) ->
    mongoose_metrics:update({Host, modMucMamFlushed}, MessageCount).

mam_muc_drop_message(Host) ->
    mongoose_metrics:update({Host, modMucMamDropped}, 1).

mam_muc_drop_iq(Host, _To, _IQ, _Action, _Reason) ->
    mongoose_metrics:update({Host, modMucMamDroppedIQ}, 1).

mam_muc_drop_messages(Host, Count) ->
    mongoose_metrics:update({Host, modMucMamDropped2}, Count).

-spec mam_muc_purge_single_message(Result :: any(), Host :: ejabberd:server(),
    _MessID :: mod_mam:message_id(), _ArcID :: mod_mam:archive_id(),
    _ArcJID :: ejabberd:jid(), _Now :: mod_mam:unix_timestamp()) -> any().
mam_muc_purge_single_message(Result, Host, _MessID, _ArcID, _ArcJID, _Now) ->
    mongoose_metrics:update({Host, modMucMamSinglePurges}, 1),
    Result.

-spec mam_muc_purge_multiple_messages(Result :: any(),
    Host :: ejabberd:server(), _ArcID :: mod_mam:archive_id(),
    _ArcJID :: ejabberd:jid(), _Borders :: any(), _Start :: any(),
    _End :: any(), _Now :: mod_mam:unix_timestamp(), _WithJID :: any()) -> any().
mam_muc_purge_multiple_messages(Result, Host,
    _ArcID, _ArcJID, _Borders, _Start, _End, _Now, _WithJID) ->
    mongoose_metrics:update({Host, modMucMamMultiplePurges}, 1),
    Result.



%%% vim: set sts=4 ts=4 sw=4 et filetype=erlang foldmarker=%%%',%%%. foldmethod=marker:
