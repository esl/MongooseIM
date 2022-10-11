%%%-------------------------------------------------------------------
%%% @author Michal Piotrowski <michal.piotrowski@erlang-solutions.com>
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc MongooseIM hooks for general metrics
%%%
%%% @end
%%% Created : 23 Apr 2013 by Michal Piotrowski <michal.piotrowski@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(mongoose_metrics_hooks).

-include("mongoose.hrl").
-include("jlib.hrl").

-export([hooks/1]).
-export([c2s_hooks/1]).

%%-------------------
%% Internal exports
%%-------------------
-export([sm_register_connection_hook/5,
         sm_remove_connection_hook/5,
         auth_failed/3,
         user_send_packet/3,
         user_open_session/3,
         xmpp_bounce_message/1,
         xmpp_stanza_dropped/4,
         xmpp_send_element/2,
         roster_get/2,
         roster_set/4,
         roster_push/4,
         roster_in_subscription/5,
         register_user/3,
         remove_user/3,
         privacy_iq_get/5,
         privacy_iq_set/4,
         privacy_check_packet/5,
         privacy_list_push/5
        ]).

-ignore_xref([auth_failed/3, privacy_check_packet/5, privacy_iq_get/5, privacy_iq_set/4,
             privacy_list_push/5, register_user/3, remove_user/3, roster_get/2,
             roster_in_subscription/5, roster_push/4, roster_set/4,
             sm_register_connection_hook/5, sm_remove_connection_hook/5,
             user_send_packet/3, user_open_session/3,
             xmpp_bounce_message/1, xmpp_send_element/2, xmpp_stanza_dropped/4]).

%%-------------------
%% Implementation
%%-------------------

%% @doc Here will be declared which hooks should be registered
-spec hooks(mongooseim:host_type()) -> [ejabberd_hooks:hook()].
hooks(HostType) ->
    [{sm_register_connection_hook, HostType, ?MODULE, sm_register_connection_hook, 50},
     {sm_remove_connection_hook, HostType, ?MODULE, sm_remove_connection_hook, 50},
     {auth_failed, HostType, ?MODULE, auth_failed, 50},
     {xmpp_stanza_dropped, HostType, ?MODULE, xmpp_stanza_dropped, 50},
     {xmpp_bounce_message, HostType, ?MODULE, xmpp_bounce_message, 50},
     {xmpp_send_element, HostType, ?MODULE, xmpp_send_element, 50},
     {roster_get, HostType, ?MODULE, roster_get, 55},
     {roster_set, HostType, ?MODULE, roster_set, 50},
     {roster_push, HostType, ?MODULE, roster_push, 50},
     {roster_in_subscription, HostType, ?MODULE, roster_in_subscription, 55},
     {register_user, HostType, ?MODULE, register_user, 50},
     {remove_user, HostType, ?MODULE, remove_user, 50},
     {privacy_iq_get, HostType, ?MODULE, privacy_iq_get, 1},
     {privacy_iq_set, HostType, ?MODULE, privacy_iq_set, 1},
     {privacy_check_packet, HostType, ?MODULE, privacy_check_packet, 55},
     {sm_broadcast, HostType, ?MODULE, privacy_list_push, 1}].

-spec c2s_hooks(mongooseim:host_type()) -> gen_hook:hook_list(mongoose_c2s_hooks:hook_fn()).
c2s_hooks(HostType) ->
    [{user_send_packet, HostType, fun ?MODULE:user_send_packet/3, #{}, 50},
     {user_open_session, HostType, fun ?MODULE:user_open_session/3, #{}, 50}].

-spec sm_register_connection_hook(any(), mongooseim:host_type(), tuple(), jid:jid(), term()
                                 ) -> any().
sm_register_connection_hook(Acc, HostType, _, _, _) ->
    mongoose_metrics:update(HostType, sessionSuccessfulLogins, 1),
    mongoose_metrics:update(HostType, sessionCount, 1),
    Acc.

-spec sm_remove_connection_hook(mongoose_acc:t(), tuple(), jid:jid(),
                                term(), ejabberd_sm:close_reason()
                               ) -> mongoose_acc:t().
sm_remove_connection_hook(Acc, _, _, _, _Reason) ->
    HT = mongoose_acc:host_type(Acc),
    mongoose_metrics:update(HT, sessionLogouts, 1),
    mongoose_metrics:update(HT, sessionCount, -1),
    Acc.

-spec auth_failed(any(), jid:user(), jid:server()) -> any().
auth_failed(Acc, _, Server) ->
    LServer = jid:nameprep(Server),
    {ok, HostType} = mongoose_domain_api:get_host_type(LServer),
    mongoose_metrics:update(HostType, sessionAuthFails, 1),
    Acc.

-spec user_send_packet(mongoose_acc:t(), mongoose_c2s:hook_params(), map()) ->
    mongoose_c2s_hooks:hook_result().
user_send_packet(Acc, _Params, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, xmppStanzaSent, 1),
    mongoose_metrics:update(HostType, xmppStanzaCount, 1),
    El = mongoose_acc:element(Acc),
    user_send_packet_type(HostType, El),
    {ok, Acc}.

-spec user_send_packet_type(HostType :: mongooseim:host_type(),
                            Packet :: exml:element()) -> ok | {error, not_found}.
user_send_packet_type(HostType, #xmlel{name = <<"message">>}) ->
    mongoose_metrics:update(HostType, xmppMessageSent, 1);
user_send_packet_type(HostType, #xmlel{name = <<"iq">>}) ->
    mongoose_metrics:update(HostType, xmppIqSent, 1);
user_send_packet_type(HostType, #xmlel{name = <<"presence">>}) ->
    mongoose_metrics:update(HostType, xmppPresenceSent, 1).

-spec xmpp_bounce_message(Acc :: mongoose_acc:t()) -> mongoose_acc:t().
xmpp_bounce_message(Acc) ->
    HostType = mongoose_acc:host_type(Acc),
    mongoose_metrics:update(HostType, xmppMessageBounced, 1),
    Acc.

-spec xmpp_stanza_dropped(mongoose_acc:t(), jid:jid(), jid:jid(), exml:element()) ->
    mongoose_acc:t().
xmpp_stanza_dropped(Acc, _, _, _) ->
    HostType = mongoose_acc:host_type(Acc),
    mongoose_metrics:update(HostType, xmppStanzaDropped, 1),
    Acc.

-spec xmpp_send_element(Acc :: mongoose_acc:t(), El :: exml:element()) -> mongoose_acc:t().
xmpp_send_element(Acc, #xmlel{name = Name} = El)
  when Name == <<"iq">>; Name == <<"message">>; Name == <<"presence">> ->
    HostType = mongoose_acc:host_type(Acc),
    mongoose_metrics:update(HostType, xmppStanzaCount, 1),
    case exml_query:attr(El, <<"type">>) of
        <<"error">> ->
            mongoose_metrics:update(HostType, xmppErrorTotal, 1),
            xmpp_send_element_error(HostType, El);
        _ ->
            mongoose_metrics:update(HostType, xmppStanzaReceived, 1),
            xmpp_send_element_type(HostType, El)
    end,
    Acc;
xmpp_send_element(Acc, _El) ->
    Acc.

-spec xmpp_send_element_error(HostType :: mongooseim:host_type(),
                              Packet :: exml:element()) -> ok | {error, not_found}.
xmpp_send_element_error(HostType, #xmlel{name = <<"message">>}) ->
    mongoose_metrics:update(HostType, xmppErrorMessage, 1);
xmpp_send_element_error(HostType, #xmlel{name = <<"iq">>}) ->
    mongoose_metrics:update(HostType, xmppErrorIq, 1);
xmpp_send_element_error(HostType, #xmlel{name = <<"presence">>}) ->
    mongoose_metrics:update(HostType, xmppErrorPresence, 1).

-spec xmpp_send_element_type(HostType :: mongooseim:host_type(),
                             Packet :: exml:element()) -> ok | {error, not_found}.
xmpp_send_element_type(HostType, #xmlel{name = <<"message">>}) ->
    mongoose_metrics:update(HostType, xmppMessageReceived, 1);
xmpp_send_element_type(HostType, #xmlel{name = <<"iq">>}) ->
    mongoose_metrics:update(HostType, xmppIqReceived, 1);
xmpp_send_element_type(HostType, #xmlel{name = <<"presence">>}) ->
    mongoose_metrics:update(HostType, xmppPresenceReceived, 1).

-spec user_open_session(mongoose_acc:t(), mongoose_c2s:hook_params(), map()) ->
    mongoose_c2s_hooks:hook_result().
user_open_session(Acc, _Params, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, xmppStanzaSent, 1),
    mongoose_metrics:update(HostType, xmppStanzaCount, 1),
    mongoose_metrics:update(HostType, xmppIqSent, 1),
    {ok, Acc}.

%% Roster

-spec roster_get(mongoose_acc:t(), jid:jid()) -> mongoose_acc:t().
roster_get(Acc, _) ->
    HostType = mongoose_acc:host_type(Acc),
    mongoose_metrics:update(HostType, modRosterGets, 1),
    Acc.

-spec roster_set(Acc :: any(), JID :: jid:jid(), tuple(), tuple()) ->
    any().
roster_set(Acc, #jid{lserver = LServer}, _, _) ->
    {ok, HostType} = mongoose_domain_api:get_host_type(LServer),
    mongoose_metrics:update(HostType, modRosterSets, 1),
    Acc.

-spec roster_in_subscription(term(), jid:jid(), jid:jid(), atom(), term()) -> term().
roster_in_subscription(Acc, _, _, subscribed, _) ->
    HostType = mongoose_acc:host_type(Acc),
    mongoose_metrics:update(HostType, modPresenceSubscriptions, 1),
    Acc;
roster_in_subscription(Acc, _, _, unsubscribed, _) ->
    HostType = mongoose_acc:host_type(Acc),
    mongoose_metrics:update(HostType, modPresenceUnsubscriptions, 1),
    Acc;
roster_in_subscription(Acc, _, _, _, _) ->
    Acc.

-spec roster_push(any(), mongooseim:host_type(), jid:jid(), mod_roster:roster()) -> any().
roster_push(HookAcc, HostType, _JID, _Item) ->
    mongoose_metrics:update(HostType, modRosterPush, 1),
    HookAcc.

%% Register

%% #rh
-spec register_user(any(), binary(), jid:lserver()) -> any().
register_user(Acc, _, LServer) ->
    {ok, HostType} = mongoose_domain_api:get_host_type(LServer),
    mongoose_metrics:update(HostType, modRegisterCount, 1),
    Acc.

-spec remove_user(mongoose_acc:t(), binary(), jid:server()) -> mongoose_acc:t().
remove_user(Acc, _, _) ->
    HostType = mongoose_acc:host_type(Acc),
    mongoose_metrics:update(HostType, modUnregisterCount, 1),
    Acc.

%% Privacy

-spec privacy_iq_get(mongoose_acc:t(), jid:jid(), jid:jid(), term(), term()) -> mongoose_acc:t().
privacy_iq_get(Acc, _, _, _, _) ->
    HostType = mongoose_acc:host_type(Acc),
    mongoose_metrics:update(HostType, modPrivacyGets, 1),
    Acc.

-spec privacy_iq_set(Acc :: mongoose_acc:t(),
                     From :: jid:jid(),
                     _To :: jid:jid(),
                     _IQ :: jlib:iq()) -> mongoose_acc:t().
privacy_iq_set(Acc, _, _To, #iq{sub_el = SubEl}) ->
    HostType = mongoose_acc:host_type(Acc),
    #xmlel{children = Els} = SubEl,
    case xml:remove_cdata(Els) of
        [#xmlel{name = <<"active">>}] ->
            mongoose_metrics:update(HostType, modPrivacySetsActive, 1);
        [#xmlel{name = <<"default">>}] ->
            mongoose_metrics:update(HostType, modPrivacySetsDefault, 1);
        _ ->
            ok
    end,
    mongoose_metrics:update(HostType, modPrivacySets, 1),
    Acc.

-spec privacy_list_push(Acc :: mongoose_acc:t(),
                        _From :: jid:jid(),
                        To :: jid:jid(),
                        Broadcast :: ejabberd_c2s:broadcast(),
                        SessionCount :: non_neg_integer()) -> mongoose_acc:t().
privacy_list_push(Acc, _From, _To, _Broadcast, SessionCount) ->
    HostType = mongoose_acc:host_type(Acc),
    mongoose_metrics:update(HostType, modPrivacyPush, SessionCount),
    Acc.

-spec privacy_check_packet(Acc :: mongoose_acc:t(),
                          JID :: jid:jid(),
                          term(), term(), term()) -> mongoose_acc:t().
privacy_check_packet(Acc, _, _, {_, _, _, _}, _) ->
    HostType = mongoose_acc:host_type(Acc),
    mongoose_metrics:update(HostType, modPrivacyStanzaAll, 1),
    case mongoose_acc:get(privacy, check, allow, Acc) of
        deny ->
            mongoose_metrics:update(HostType, modPrivacyStanzaDenied, 1);
        block ->
            mongoose_metrics:update(HostType, modPrivacyStanzaBlocked, 1);
        allow ->
            ok
    end,
    Acc.

%%% vim: set sts=4 ts=4 sw=4 et filetype=erlang foldmarker=%%%',%%%. foldmethod=marker:
