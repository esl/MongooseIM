%%%-------------------------------------------------------------------
%%% @author Michal Piotrowski <michal.piotrowski@erlang-solutions.com>
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc MongooseIM hooks for general metrics
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
-export([sm_register_connection_hook/4,
         sm_remove_connection_hook/5,
         auth_failed/3,
         user_send_packet/4,
         user_receive_packet/5,
         xmpp_bounce_message/3,
         xmpp_stanza_dropped/4,
         xmpp_send_element/2,
         roster_get/2,
         roster_set/4,
         roster_push/3,
         roster_in_subscription/6,
         register_user/3,
         remove_user/3,
         privacy_iq_get/5,
         privacy_iq_set/4,
         privacy_check_packet/6,
         user_ping_timeout/2,
         privacy_list_push/5
        ]).

-type hook() :: [atom() | ejabberd:server() | integer(), ...].
-type t() :: hook().
-type metrics_notify_return() ::
                map()
                | {'error', _, 'nonexistent_metric' | 'unsupported_metric_type'}.

-export_type([t/0, metrics_notify_return/0]).

%%-------------------
%% Implementation
%%-------------------

%% @doc Here will be declared which hooks should be registered
-spec get_hooks(_) -> [hook(), ...].
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
     [user_ping_timeout,      Host, ?MODULE, user_ping_timeout, 50]
     | mongoose_metrics_mam_hooks:get_hooks(Host)].

-spec sm_register_connection_hook(map(), tuple(), ejabberd:jid(), term()
                                 ) -> metrics_notify_return().
sm_register_connection_hook(Acc, _, #jid{server = Server}, _) ->
    mongoose_metrics:update(Server, sessionSuccessfulLogins, 1),
    mongoose_metrics:update(Server, sessionCount, 1),
    Acc.

-spec sm_remove_connection_hook(map(), tuple(), ejabberd:jid(),
                                term(), ejabberd_sm:close_reason()
                               ) -> metrics_notify_return().
sm_remove_connection_hook(Acc, _, #jid{server = Server}, _, _Reason) ->
    mongoose_metrics:update(Server, sessionLogouts, 1),
    mongoose_metrics:update(Server, sessionCount, -1),
    Acc.

-spec auth_failed(map(), binary(), binary()) -> metrics_notify_return().
auth_failed(Acc, _, Server) ->
    mongoose_metrics:update(Server, sessionAuthFails, 1),
    Acc.

-spec user_send_packet(map(), ejabberd:jid(), tuple(), tuple()
                      ) -> metrics_notify_return().
user_send_packet(Acc, #jid{server = Server}, _, Packet) ->
    mongoose_metrics:update(Server, xmppStanzaSent, 1),
    user_send_packet_type(Server, Packet),
    Acc.

-spec user_send_packet_type(Server :: ejabberd:server(),
                            Packet :: jlib:xmlel()) -> ok | {error, not_found}.
user_send_packet_type(Server, #xmlel{name = <<"message">>}) ->
    mongoose_metrics:update(Server, xmppMessageSent, 1);
user_send_packet_type(Server, #xmlel{name = <<"iq">>}) ->
    mongoose_metrics:update(Server, xmppIqSent, 1);
user_send_packet_type(Server, #xmlel{name = <<"presence">>}) ->
    mongoose_metrics:update(Server, xmppPresenceSent, 1).

-spec user_receive_packet(map(), ejabberd:jid(), tuple(), tuple(), tuple()) -> term().
user_receive_packet(Acc, #jid{server = Server}, _, _, Packet) ->
    mongoose_metrics:update(Server, xmppStanzaReceived, 1),
    user_receive_packet_type(Server, Packet),
    Acc.

-spec user_receive_packet_type(Server :: ejabberd:server(),
                               Packet :: jlib:xmlel()) -> ok | {error, not_found}.
user_receive_packet_type(Server, #xmlel{name = <<"message">>}) ->
    mongoose_metrics:update(Server, xmppMessageReceived, 1);
user_receive_packet_type(Server, #xmlel{name = <<"iq">>}) ->
    mongoose_metrics:update(Server, xmppIqReceived, 1);
user_receive_packet_type(Server, #xmlel{name = <<"presence">>}) ->
    mongoose_metrics:update(Server, xmppPresenceReceived, 1).

-spec xmpp_bounce_message(Acc :: map(), Server :: ejabberd:server(),
                          tuple()) -> metrics_notify_return().
xmpp_bounce_message(Acc, Server, _) ->
    mongoose_metrics:update(Server, xmppMessageBounced, 1),
    Acc.

-spec xmpp_stanza_dropped(map(), ejabberd:jid(), tuple(), tuple()) -> metrics_notify_return().
xmpp_stanza_dropped(Acc, #jid{server = Server} , _, _) ->
    mongoose_metrics:update(Server, xmppStanzaDropped, 1),
    Acc.

-spec xmpp_send_element(Acc :: map(), Server :: ejabberd:server()) -> ok | metrics_notify_return().
xmpp_send_element(Acc, Server) ->
    mongoose_metrics:update(Server, xmppStanzaCount, 1),
    case mongoose_acc:get(type, Acc) of
        <<"error">> ->
            mongoose_metrics:update(Server, xmppErrorTotal, 1),
            case mongoose_acc:get(name, Acc) of
                <<"iq">> ->
                    mongoose_metrics:update(Server, xmppErrorIq, 1);
                <<"message">> ->
                    mongoose_metrics:update(Server, xmppErrorMessage, 1);
                <<"presence">> ->
                    mongoose_metrics:update(Server, xmppErrorPresence, 1)
            end;
        _ -> ok
    end,
    Acc.


%% Roster

-spec roster_get(list(), {_, ejabberd:server()}) -> list().
roster_get(Acc, {_, Server}) ->
    mongoose_metrics:update(Server, modRosterGets, 1),
    Acc.

-spec roster_set(Acc :: map(), JID :: ejabberd:jid(), tuple(), tuple()) ->
    metrics_notify_return().
roster_set(Acc, #jid{server = Server}, _, _) ->
    mongoose_metrics:update(Server, modRosterSets, 1),
    Acc.

-spec roster_in_subscription(term(), binary(), binary(), tuple(), atom(), term()) -> term().
roster_in_subscription(Acc, _, Server, _, subscribed, _) ->
    mongoose_metrics:update(Server, modPresenceSubscriptions, 1),
    Acc;
roster_in_subscription(Acc, _, Server, _, unsubscribed, _) ->
    mongoose_metrics:update(Server, modPresenceUnsubscriptions, 1),
    Acc;
roster_in_subscription(Acc, _, _, _, _, _) ->
    Acc.

-spec roster_push(map(), ejabberd:jid(), term()) -> metrics_notify_return().
roster_push(Acc, #jid{server = Server}, _) ->
    mongoose_metrics:update(Server, modRosterPush, 1),
    Acc.

%% Register

%% #rh
-spec register_user(map(), binary(), ejabberd:server()) -> metrics_notify_return().
register_user(Acc, _, Server) ->
    mongoose_metrics:update(Server, modRegisterCount, 1),
    Acc.

-spec remove_user(map(), binary(), ejabberd:server()) -> metrics_notify_return().
remove_user(Acc, _, Server) ->
    mongoose_metrics:update(Server, modUnregisterCount, 1),
    Acc.

%% Privacy

-spec privacy_iq_get(term(), ejabberd:jid(), ejabberd:jid(), term(), term()) -> term().
privacy_iq_get(Acc, #jid{server  = Server}, _, _, _) ->
    mongoose_metrics:update(Server, modPrivacyGets, 1),
    Acc.

-spec privacy_iq_set(Acc :: term(),
                     From :: ejabberd:jid(),
                     _To :: ejabberd:jid(),
                     _IQ :: ejabberd:iq()) -> ok | metrics_notify_return() | term().
privacy_iq_set(Acc, #jid{server = Server}, _To, #iq{sub_el = SubEl}) ->
    #xmlel{children = Els} = SubEl,
    case xml:remove_cdata(Els) of
        [#xmlel{name = <<"active">>}] ->
            mongoose_metrics:update(Server, modPrivacySetsActive, 1);
        [#xmlel{name = <<"default">>}] ->
            mongoose_metrics:update(Server, modPrivacySetsDefault, 1);
        _ ->
            ok
    end,
    mongoose_metrics:update(Server, modPrivacySets, 1),
    Acc.

-spec privacy_list_push(Acc :: map(),
                        _From :: ejabberd:jid(),
                        To :: ejabberd:jid(),
                        Broadcast :: ejabberd_c2s:broadcast(),
                        SessionCount :: non_neg_integer()) -> ok | metrics_notify_return().
privacy_list_push(Acc, _From, #jid{server = Server} = _To, _Broadcast, SessionCount) ->
    mongoose_metrics:update(Server, modPrivacyPush, SessionCount),
    Acc.

user_ping_timeout(Acc, _JID) ->
    Acc.

-spec privacy_check_packet(Acc :: mongoose_acc:t(),
                          binary(),
                          Server :: ejabberd:server(),
                          term(), term(), term()) -> mongoose_acc:t().
privacy_check_packet(Acc, _, Server, _, {_, _, _}, _) ->
    mongoose_metrics:update(Server, modPrivacyStanzaAll, 1),
    case mongoose_acc:get(privacy_check, Acc, allow) of
        deny ->
            mongoose_metrics:update(Server, modPrivacyStanzaDenied, 1);
        block ->
            mongoose_metrics:update(Server, modPrivacyStanzaBlocked, 1);
        allow ->
            ok
    end,
    Acc.

%%% vim: set sts=4 ts=4 sw=4 et filetype=erlang foldmarker=%%%',%%%. foldmethod=marker:
