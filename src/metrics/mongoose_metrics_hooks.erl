%%%-------------------------------------------------------------------
%%% @author Michal Piotrowski <michal.piotrowski@erlang-solutions.com>
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc MongooseIM hooks for general metrics
%%%
%%% @end
%%% Created : 23 Apr 2013 by Michal Piotrowski <michal.piotrowski@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(mongoose_metrics_hooks).

-include("jlib.hrl").

-export([get_hooks/1]).

%%-------------------
%% Internal exports
%%-------------------
-export([sm_register_connection_hook/3,
         sm_remove_connection_hook/3,
         auth_failed/3,
         user_send_packet/3,
         user_open_session/3,
         xmpp_bounce_message/3,
         xmpp_stanza_dropped/3,
         xmpp_send_element/3,
         roster_get/3,
         roster_set/3,
         roster_push/3,
         roster_in_subscription/3,
         register_user/3,
         remove_user/3,
         privacy_iq_get/3,
         privacy_iq_set/3,
         privacy_check_packet/3,
         privacy_list_push/3
        ]).

%%-------------------
%% Implementation
%%-------------------

%% @doc Here will be declared which hooks should be registered
-spec get_hooks(_) -> [gen_hook:hook_tuple(), ...].
get_hooks(HostType) ->
    [ {sm_register_connection_hook, HostType, fun ?MODULE:sm_register_connection_hook/3, #{}, 50},
      {sm_remove_connection_hook, HostType, fun ?MODULE:sm_remove_connection_hook/3, #{}, 50},
      {auth_failed, HostType, fun ?MODULE:auth_failed/3, #{}, 50},
      {xmpp_stanza_dropped, HostType, fun ?MODULE:xmpp_stanza_dropped/3, #{}, 50},
      {xmpp_bounce_message, HostType, fun ?MODULE:xmpp_bounce_message/3, #{}, 50},
      {xmpp_send_element, HostType, fun ?MODULE:xmpp_send_element/3, #{}, 50},
      {roster_get, HostType, fun ?MODULE:roster_get/3, #{}, 55},
      {roster_set, HostType, fun ?MODULE:roster_set/3, #{}, 50},
      {roster_push, HostType, fun ?MODULE:roster_push/3, #{}, 50},
      {roster_in_subscription, HostType, fun ?MODULE:roster_in_subscription/3, #{}, 55},
      {register_user, HostType, fun ?MODULE:register_user/3, #{}, 50},
      {remove_user, HostType, fun ?MODULE:remove_user/3, #{}, 50},
      {privacy_iq_get, HostType, fun ?MODULE:privacy_iq_get/3, #{}, 1},
      {privacy_iq_set, HostType, fun ?MODULE:privacy_iq_set/3, #{}, 1},
      {privacy_check_packet, HostType, fun ?MODULE:privacy_check_packet/3, #{}, 55},
      {privacy_list_push, HostType, fun ?MODULE:privacy_list_push/3, #{}, 1}
      | c2s_hooks(HostType)].

-spec c2s_hooks(mongooseim:host_type()) -> gen_hook:hook_list(mongoose_c2s_hooks:fn()).
c2s_hooks(HostType) ->
    [{user_send_packet, HostType, fun ?MODULE:user_send_packet/3, #{}, 50},
     {user_open_session, HostType, fun ?MODULE:user_open_session/3, #{}, 50}].

-spec sm_register_connection_hook(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: any(),
      Params :: map(),
      Extra :: #{host_type := mongooseim:host_type()}.
sm_register_connection_hook(Acc, _, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, sessionSuccessfulLogins, 1),
    mongoose_metrics:update(HostType, sessionCount, 1),
    {ok, Acc}.

-spec sm_remove_connection_hook(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: map(),
      Extra :: #{host_type := mongooseim:host_type()}.
sm_remove_connection_hook(Acc, _, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, sessionLogouts, 1),
    mongoose_metrics:update(HostType, sessionCount, -1),
    {ok, Acc}.

-spec auth_failed(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: any(),
      Params :: #{server := jid:server()},
      Extra :: map().
auth_failed(Acc, #{server := Server}, _) ->
    LServer = jid:nameprep(Server),
    {ok, HostType} = mongoose_domain_api:get_host_type(LServer),
    mongoose_metrics:update(HostType, sessionAuthFails, 1),
    {ok, Acc}.

-spec user_send_packet(mongoose_acc:t(), mongoose_c2s_hooks:params(), map()) ->
    mongoose_c2s_hooks:result().
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
    mongoose_metrics:update(HostType, xmppPresenceSent, 1);
user_send_packet_type(_, _) ->
    ok.

-spec xmpp_bounce_message(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: map(),
      Extra :: #{host_type := mongooseim:host_type()}.
xmpp_bounce_message(Acc, _, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, xmppMessageBounced, 1),
    {ok, Acc}.

-spec xmpp_stanza_dropped(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: map(),
      Extra :: #{host_type := mongooseim:host_type()}.
xmpp_stanza_dropped(Acc, _, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, xmppStanzaDropped, 1),
    {ok, Acc}.

-spec xmpp_send_element(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: map(),
      Extra :: #{host_type := mongooseim:host_type()}.
xmpp_send_element(Acc, _, #{host_type := HostType}) ->
    case mongoose_acc:element(Acc) of
        #xmlel{name = Name} = El
          when Name == <<"iq">>; Name == <<"message">>; Name == <<"presence">> ->
            mongoose_metrics:update(HostType, xmppStanzaCount, 1),
            case exml_query:attr(El, <<"type">>) of
                <<"error">> ->
                    mongoose_metrics:update(HostType, xmppErrorTotal, 1),
                    xmpp_send_element_error(HostType, El);
                _ ->
                    mongoose_metrics:update(HostType, xmppStanzaReceived, 1),
                    xmpp_send_element_type(HostType, El)
            end;
        _ -> ok
    end,
    {ok, Acc}.

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

-spec user_open_session(mongoose_acc:t(), mongoose_c2s_hooks:params(), map()) ->
    mongoose_c2s_hooks:result().
user_open_session(Acc, _Params, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, xmppStanzaSent, 1),
    mongoose_metrics:update(HostType, xmppStanzaCount, 1),
    mongoose_metrics:update(HostType, xmppIqSent, 1),
    {ok, Acc}.

%% Roster

-spec roster_get(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: term(),
      Params :: map(),
      Extra :: #{host_type := mongooseim:host_type()}.
roster_get(Acc, _, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, modRosterGets, 1),
    {ok, Acc}.

-spec roster_set(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: any(),
      Params :: #{from := jid:jid()},
      Extra :: map().
roster_set(Acc, #{from := #jid{lserver = LServer}}, _) ->
    {ok, HostType} = mongoose_domain_api:get_host_type(LServer),
    mongoose_metrics:update(HostType, modRosterSets, 1),
    {ok, Acc}.

-spec roster_in_subscription(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: #{type := mod_roster:sub_presence()},
      Extra :: #{host_type := mongooseim:host_type()}.
roster_in_subscription(Acc, #{type := subscribed}, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, modPresenceSubscriptions, 1),
    {ok, Acc};
roster_in_subscription(Acc, #{type := unsubscribed}, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, modPresenceUnsubscriptions, 1),
    {ok, Acc};
roster_in_subscription(Acc, _, _) ->
    {ok, Acc}.

-spec roster_push(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: any(),
      Params :: map(),
      Extra :: #{host_type := mongooseim:host_type()}.
roster_push(HookAcc, _, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, modRosterPush, 1),
    {ok, HookAcc}.

%% Register

%% #rh
-spec register_user(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: any(),
      Params :: #{jid := jid:jid()},
      Extra :: map().
register_user(Acc, #{jid := #jid{lserver = LServer}}, _) ->
    {ok, HostType} = mongoose_domain_api:get_host_type(LServer),
    mongoose_metrics:update(HostType, modRegisterCount, 1),
    {ok, Acc}.

-spec remove_user(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: map(),
      Extra :: #{host_type := mongooseim:host_type()}.
remove_user(Acc, _, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, modUnregisterCount, 1),
    {ok, Acc}.

%% Privacy

-spec privacy_iq_get(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: map(),
      Extra :: #{host_type := mongooseim:host_type()}.
privacy_iq_get(Acc, _, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, modPrivacyGets, 1),
    {ok, Acc}.

-spec privacy_iq_set(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: #{iq := jlib:iq()},
      Extra :: #{host_type := mongooseim:host_type()}.
privacy_iq_set(Acc, #{iq := #iq{sub_el = SubEl}}, #{host_type := HostType}) ->
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
    {ok, Acc}.

-spec privacy_list_push(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: any(),
      Params :: #{session_count := non_neg_integer()},
      Extra :: #{host_type := mongooseim:host_type()}.
privacy_list_push(Acc, #{session_count := SessionCount}, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, modPrivacyPush, SessionCount),
    {ok, Acc}.

-spec privacy_check_packet(Acc, Params, Extra) -> {ok, Acc} when
      Acc :: mongoose_acc:t(),
      Params :: map(),
      Extra :: #{host_type := mongooseim:host_type()}.
privacy_check_packet(Acc, _, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, modPrivacyStanzaAll, 1),
    case mongoose_acc:get(privacy, check, allow, Acc) of
        deny ->
            mongoose_metrics:update(HostType, modPrivacyStanzaDenied, 1);
        block ->
            mongoose_metrics:update(HostType, modPrivacyStanzaBlocked, 1);
        allow ->
            ok
    end,
    {ok, Acc}.

%%% vim: set sts=4 ts=4 sw=4 et filetype=erlang foldmarker=%%%',%%%. foldmethod=marker:
