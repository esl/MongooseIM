%%%-------------------------------------------------------------------
%%% @author Michal Piotrowski <michal.piotrowski@erlang-solutions.com>
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Ejabberd hooks for general metrics
%%%
%%% @end
%%% Created : 23 Apr 2013 by Michal Piotrowski <michal.piotrowski@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(ejabberd_metrics_hooks).

-include("ejabberd.hrl").
-include("jlib.hrl").

-export([get_hooks/1]).

%%-------------------
%% Internal exports
%%-------------------
-export([sm_register_connection_hook/3,
         sm_remove_connection_hook/3,
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

%%-------------------
%% Implementation
%%-------------------

%% Here will be declared which hooks should be registered
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


-spec sm_register_connection_hook(tuple(), jid(), term()) -> term().
sm_register_connection_hook(_,#jid{server = Server}, _) ->
    folsom_metrics:notify({Server, sessionSuccessfulLogins}, 1),
    folsom_metrics:notify({Server, sessionCount}, {inc, 1}).

-spec sm_remove_connection_hook(tuple(), jid(), term()) -> term().
sm_remove_connection_hook(_,#jid{server = Server},_) ->
    folsom_metrics:notify({Server, sessionLogouts}, 1),
    folsom_metrics:notify({Server, sessionCount}, {dec, 1}).

-spec auth_failed(binary(), binary()) -> term().
auth_failed(_,Server) ->
    folsom_metrics:notify({Server, sessionAuthFails},1).

-spec user_send_packet(jid(), tuple(), tuple()) -> term().
user_send_packet(#jid{server = Server},_,Packet) ->
    folsom_metrics:notify({Server, xmppStanzaSent}, 1),
    user_send_packet_type(Server, Packet).

user_send_packet_type(Server, #xmlel{name = <<"message">>}) ->
    folsom_metrics:notify({Server, xmppMessageSent}, 1);
user_send_packet_type(Server, #xmlel{name = <<"iq">>}) ->
    folsom_metrics:notify({Server, xmppIqSent}, 1);
user_send_packet_type(Server, #xmlel{name = <<"presence">>}) ->
    folsom_metrics:notify({Server, xmppPresenceSent}, 1).

-spec user_receive_packet(jid(), tuple(), tuple(), tuple()) -> term().
user_receive_packet(#jid{server = Server} ,_,_,Packet) ->
    folsom_metrics:notify({Server, xmppStanzaReceived},1),
    user_receive_packet_type(Server, Packet).

user_receive_packet_type(Server, #xmlel{name = <<"message">>}) ->
    folsom_metrics:notify({Server, xmppMessageReceived}, 1);
user_receive_packet_type(Server, #xmlel{name = <<"iq">>}) ->
    folsom_metrics:notify({Server, xmppIqReceived}, 1);
user_receive_packet_type(Server, #xmlel{name = <<"presence">>}) ->
    folsom_metrics:notify({Server, xmppPresenceReceived}, 1).

-spec xmpp_bounce_message(binary(), tuple()) -> term().
xmpp_bounce_message(Server, _) ->
    folsom_metrics:notify({Server, xmppMessageBounced}, 1).

-spec xmpp_stanza_dropped(jid(), tuple(), tuple()) -> term().
xmpp_stanza_dropped(#jid{server = Server} ,_,_) ->
   folsom_metrics:notify({Server, xmppStanzaDropped}, 1).

-spec xmpp_send_element(binary(), tuple()) -> term().
xmpp_send_element(Server, #xmlel{name = Name, attrs = Attrs}) ->
    folsom_metrics:notify({Server, xmppStanzaCount}, 1),
    case lists:keyfind(<<"type">>, 1, Attrs) of
        {<<"type">>, <<"error">>} ->
            folsom_metrics:notify({Server, xmppErrorTotal}, 1),
            case Name of
                <<"iq">> ->
                    folsom_metrics:notify({Server, xmppErrorIq},1);
                <<"message">> ->
                    folsom_metrics:notify({Server, xmppErrorMessage},1);
                <<"presence">> ->
                    folsom_metrics:notify({Server, xmppErrorPresence}, 1)
            end;
        _ -> ok
    end;
xmpp_send_element(_, _) ->
    ok.


%% Roster

-spec roster_get(list(), term()) -> list().
roster_get(Acc, {_, Server}) ->
    folsom_metrics:notify({Server, modRosterGets}, 1),
    Acc.

-spec roster_set(jid(), tuple(), tuple()) -> list().
roster_set(#jid{server = Server},_,_) ->
    folsom_metrics:notify({Server, modRosterSets}, 1).

-spec roster_in_subscription(term(), binary(), binary(), tuple(), atom(), term()) -> term().
roster_in_subscription(Acc,_,Server,_,subscribed,_) ->
    folsom_metrics:notify({Server, modPresenceSubscriptions}, 1),
    Acc;
roster_in_subscription(Acc,_,Server,_,unsubscribed,_) ->
    folsom_metrics:notify({Server, modPresenceUnsubscriptions}, 1),
    Acc;
roster_in_subscription(Acc,_,_,_,_,_) ->
    Acc.

-spec roster_push(jid(),term()) -> term().
roster_push(#jid{server = Server},_) ->
    folsom_metrics:notify({Server, modRosterPush}, 1).

%% Register

-spec register_user(binary(),binary()) -> term().
register_user(_,Server) ->
    folsom_metrics:notify({Server, modRegisterCount}, 1).

-spec remove_user(binary(),binary()) -> term().
remove_user(_,Server) ->
    folsom_metrics:notify({Server, modUnregisterCount}, 1).

%% Privacy

-spec privacy_iq_get(term(), jid(), jid(), term(), term()) -> term().
privacy_iq_get(Acc, #jid{server  = Server}, _, _, _) ->
    folsom_metrics:notify({Server, modPrivacyGets}, 1),
    Acc.

-spec privacy_iq_set(term(), jid(), jid(), term()) -> term().
privacy_iq_set(Acc, #jid{server = Server}, _To, #iq{sub_el = SubEl}) ->
    #xmlel{children = Els} = SubEl,
    case xml:remove_cdata(Els) of
        [#xmlel{name = <<"active">>}] ->
            folsom_metrics:notify({Server, modPrivacySetsActive}, 1);
        [#xmlel{name = <<"default">>}] ->
            folsom_metrics:notify({Server, modPrivacySetsDefault}, 1);
        _ ->
            ok
    end,
    folsom_metrics:notify({Server, modPrivacySets}, 1),
    Acc.

-spec privacy_list_push(jid(), jid(), term()) -> term().
privacy_list_push(_From, #jid{server = Server} = To, Packet) ->
    case Packet of
        #xmlel{name = <<"broadcast">>, children = [{privacy_list, _, _}]} ->
            #jid{user = User, server = Server} = To,
            Count = length(ejabberd_sm:get_user_resources(User, Server)),
            folsom_metrics:notify({Server, modPrivacyPush}, Count);
        _ ->
            ok
    end.

-spec privacy_check_packet(Acc :: allow | deny, binary(), binary(), term(), term(), term()) -> allow | deny.
privacy_check_packet(Acc, _, Server, _, _, _) ->
    folsom_metrics:notify({Server, modPrivacyStanzaAll}, 1),
    case Acc of
        deny ->
            folsom_metrics:notify({Server, modPrivacyStanzaBlocked}, 1);
        allow ->
            ok
    end,
    Acc.

%% ----------------------------------------------------------------------------
%% mod_mam

mam_get_prefs(Result, Host, _ArcID, _ArcJID) ->
    folsom_metrics:notify({Host, modMamPrefsGets}, 1),
    Result.

mam_set_prefs(Result, Host, _ArcID, _ArcJID, _DefaultMode, _AlwaysJIDs, _NeverJIDs) ->
    folsom_metrics:notify({Host, modMamPrefsSets}, 1),
    Result.

mam_remove_archive(Host, _ArcID, _ArcJID) ->
    folsom_metrics:notify({Host, modMamArchiveRemoved}, 1).

mam_lookup_messages(Result = {ok, {_TotalCount, _Offset, MessageRows}},
    Host, _ArcID, _ArcJID,
    _RSM, _Borders,
    _Start, _End, _Now, _WithJID,
    _PageSize, _LimitPassed, _MaxResultLimit, _IsSimple) ->
    folsom_metrics:notify({Host, modMamForwarded}, length(MessageRows)),
    folsom_metrics:notify({Host, modMamLookups}, 1),
    Result;
mam_lookup_messages(Result = {error, _},
    _Host, _ArcID, _ArcJID,
    _RSM, _Borders,
    _Start, _End, _Now, _WithJID,
    _PageSize, _LimitPassed, _MaxResultLimit, _IsSimple) ->
    Result.
    

mam_archive_message(Result, Host,
    _MessID, _ArcID, _LocJID, _RemJID, _SrcJID, _Dir, _Packet) ->
    folsom_metrics:notify({Host, modMamArchived}, 1),
    Result.

mam_flush_messages(Host, MessageCount) ->
    folsom_metrics:notify({Host, modMamFlushed}, MessageCount).

mam_drop_message(Host) ->
    folsom_metrics:notify({Host, modMamDropped}, 1).

mam_drop_iq(Host, _To, _IQ, _Action, _Reason) ->
    folsom_metrics:notify({Host, modMamDroppedIQ}, 1).

mam_drop_messages(Host, Count) ->
    folsom_metrics:notify({Host, modMamDropped2}, Count).

mam_purge_single_message(Result, Host, _MessID, _ArcID, _ArcJID, _Now) ->
    folsom_metrics:notify({Host, modMamSinglePurges}, 1),
    Result.

mam_purge_multiple_messages(Result, Host,
    _ArcID, _ArcJID, _Borders, _Start, _End, _Now, _WithJID) ->
    folsom_metrics:notify({Host, modMamMultiplePurges}, 1),
    Result.


%% ----------------------------------------------------------------------------
%% mod_mam_muc

mam_muc_get_prefs(Result, Host, _ArcID, _ArcJID) ->
    folsom_metrics:notify({Host, modMucMamPrefsGets}, 1),
    Result.

mam_muc_set_prefs(Result, Host, _ArcID, _ArcJID, _DefaultMode, _AlwaysJIDs, _NeverJIDs) ->
    folsom_metrics:notify({Host, modMucMamPrefsSets}, 1),
    Result.

mam_muc_remove_archive(Host, _ArcID, _ArcJID) ->
    folsom_metrics:notify({Host, modMucMamArchiveRemoved}, 1).

mam_muc_lookup_messages(Result = {ok, {_TotalCount, _Offset, MessageRows}},
    Host, _ArcID, _ArcJID,
    _RSM, _Borders,
    _Start, _End, _Now, _WithJID,
    _PageSize, _LimitPassed, _MaxResultLimit, _IsSimple) ->
    folsom_metrics:notify({Host, modMucMamForwarded}, length(MessageRows)),
    folsom_metrics:notify({Host, modMucMamLookups}, 1),
    Result;
mam_muc_lookup_messages(Result = {error, _},
    _Host, _ArcID, _ArcJID,
    _RSM, _Borders,
    _Start, _End, _Now, _WithJID,
    _PageSize, _LimitPassed, _MaxResultLimit, _IsSimple) ->
    Result.
    

mam_muc_archive_message(Result, Host,
    _MessID, _ArcID, _LocJID, _RemJID, _SrcJID, _Dir, _Packet) ->
    folsom_metrics:notify({Host, modMucMamArchived}, 1),
    Result.

mam_muc_flush_messages(Host, MessageCount) ->
    folsom_metrics:notify({Host, modMucMamFlushed}, MessageCount).

mam_muc_drop_message(Host) ->
    folsom_metrics:notify({Host, modMucMamDropped}, 1).

mam_muc_drop_iq(Host, _To, _IQ, _Action, _Reason) ->
    folsom_metrics:notify({Host, modMucMamDroppedIQ}, 1).

mam_muc_drop_messages(Host, Count) ->
    folsom_metrics:notify({Host, modMucMamDropped2}, Count).

mam_muc_purge_single_message(Result, Host, _MessID, _ArcID, _ArcJID, _Now) ->
    folsom_metrics:notify({Host, modMucMamSinglePurges}, 1),
    Result.

mam_muc_purge_multiple_messages(Result, Host,
    _ArcID, _ArcJID, _Borders, _Start, _End, _Now, _WithJID) ->
    folsom_metrics:notify({Host, modMucMamMultiplePurges}, 1),
    Result.



%%% vim: set sts=4 ts=4 sw=4 et filetype=erlang foldmarker=%%%',%%%. foldmethod=marker:
