%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C) 2017 Erlang Solutions Ltd.
%%% This software is released under the Apache License, Version 2.0
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Default plugin module for mod_event_pusher_push.
%%% This module allows for some dynamic customizations.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_event_pusher_push_plugin_defaults).
-behavior(mod_event_pusher_push_plugin).
-author('rafal.slota@erlang-solutions.com').

-include("jlib.hrl").
-include("mongoose.hrl").
-include("mod_event_pusher_events.hrl").

%% Callback API
-export([prepare_notification/2,
         should_publish/3,
         publish_notification/4]).

-define(PUSH_FORM_TYPE, <<"urn:xmpp:push:summary">>).
%%--------------------------------------------------------------------
%% mod_event_pusher_push_plugin callbacks
%%--------------------------------------------------------------------
-spec should_publish(Acc :: mongooseim_acc:t(),
                     Event :: mod_event_pusher:event(),
                     Services :: [mod_event_pusher_push:publish_service()]) ->
                        [mod_event_pusher_push:publish_service()].
should_publish(Acc, #chat_event{to = To}, Services) ->
    PublishedServices = mongoose_acc:get(event_pusher, published_services, [], Acc),
    case should_publish(To) of
        true -> Services -- PublishedServices;
        false -> []
    end;
should_publish(_Acc, _Event, _Services) -> [].

-spec prepare_notification(Acc :: mongooseim_acc:t(),
                           Event :: mod_event_pusher:event()) ->
                              mod_event_pusher_push_plugin:push_payload() | skip.
prepare_notification(Acc, _) ->
    {From, To, Packet} = mongoose_acc:packet(Acc),
    case exml_query:subelement(Packet, <<"body">>) of
        undefined -> skip;
        Body ->
            BodyCData = exml_query:cdata(Body),
            MessageCount = get_unread_count(Acc, To),
            SenderId = sender_id(From, Packet),
            push_content_fields(SenderId, BodyCData, MessageCount)
    end.

-spec publish_notification(Acc :: mongooseim_acc:t(),
                           Event :: mod_event_pusher:event(),
                           Payload :: mod_event_pusher_push_plugin:push_payload(),
                           Services :: [mod_event_pusher_push:publish_service()]) ->
                              mongooseim_acc:t().
publish_notification(Acc, _, Payload, Services) ->
    To = mongoose_acc:to_jid(Acc),
    #jid{lserver = Host} = To,
    VirtualPubsubHosts = mod_event_pusher_push:virtual_pubsub_hosts(Host),
    lists:foreach(fun({PubsubJID, _Node, _Form} = Service) ->
                      case lists:member(PubsubJID#jid.lserver, VirtualPubsubHosts) of
                          true ->
                              publish_via_hook(Acc, Host, To, Service, Payload);
                          false ->
                              publish_via_pubsub(Host, To, Service, Payload)
                      end
                  end, Services),

    mongoose_acc:append(event_pusher, published_services, Services, Acc).

%%--------------------------------------------------------------------
%% local functions
%%--------------------------------------------------------------------

-spec should_publish(To :: jid:jid()) -> boolean().
should_publish(#jid{luser = LUser, lserver = LServer} = To) ->
    try ejabberd_users:does_user_exist(LUser, LServer) of
        false ->
            false;
        true ->
            ejabberd_sm:is_offline(To)
    catch
        _:_ ->
            ejabberd_sm:is_offline(To)
    end.

-spec get_unread_count(mongoose_acc:t(), jid:jid()) -> pos_integer().
get_unread_count(Acc, To) ->
    NewAcc = ejabberd_hooks:run_fold(inbox_unread_count, To#jid.lserver, Acc, [To]),
    mongoose_acc:get(inbox, unread_count, 1, NewAcc).

-spec sender_id(jid:jid(), exml:element()) -> binary().
sender_id(From, Packet) ->
    case exml_query:attr(Packet, <<"type">>) of
        <<"chat">> ->
            jid:to_binary(jid:to_bare(jid:to_lower(From)));
        <<"groupchat">> ->
            jid:to_binary(jid:to_lower(From))
    end.

-spec push_content_fields(binary(), binary(), non_neg_integer()) ->
    mod_event_pusher_push_plugin:push_payload() | skip.
push_content_fields(_SenderId, <<"">>, _MessageCount) ->
    skip;
push_content_fields(SenderId, BodyCData, MessageCount) ->
    [
        {<<"message-count">>, integer_to_binary(MessageCount)},
        {<<"last-message-sender">>, SenderId},
        {<<"last-message-body">>, BodyCData}
    ].

-spec publish_via_hook(Acc :: mongooseim_acc:t(),
                       Host :: jid:server(),
                       To :: jid:jid(),
                       Service :: mod_event_pusher_push:publish_service(),
                       PushPayload :: mod_event_pusher_push_plugin:push_payload()) ->
                          any().
publish_via_hook(Acc0, Host, To, {PubsubJID, Node, Form}, PushPayload) ->
    OptionMap = maps:from_list(Form),
    HookArgs = [Host, [maps:from_list(PushPayload)], OptionMap],
    %% Acc is ignored by mod_push_service_mongoosepush, added here only for
    %% tracability purposes and push_SUITE code unification
    Acc = mongoose_acc:set(push_notifications, pubsub_jid, PubsubJID, Acc0),
    case ejabberd_hooks:run_fold(push_notifications, Host, Acc, HookArgs) of
        {error, device_not_registered} ->
            %% We disable the push node in case the error type is device_not_registered
            mod_event_pusher_push:disable_node(To,PubsubJID, Node);
        _ -> ok
    end.

-spec publish_via_pubsub(Host :: jid:server(),
                         To :: jid:jid(),
                         Service :: mod_event_pusher_push:publish_service(),
                         PushPayload :: mod_event_pusher_push_plugin:push_payload()) ->
                            any().
publish_via_pubsub(Host, To, {PubsubJID, Node, Form}, PushPayload) ->
    Stanza = push_notification_iq(Node, Form, PushPayload),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => To#jid.lserver,
                              element => jlib:iq_to_xml(Stanza),
                              from_jid => To,
                              to_jid => PubsubJID }),

    ResponseHandler =
    fun(_From, _To, Acc, Response) ->
            mod_event_pusher_push:cast(Host, fun handle_publish_response/4,
                                       [To, PubsubJID, Node, Response]),
            Acc
    end,
    %% The IQ is routed from the recipient's server JID to pubsub JID
    %% This is recommended in the XEP and also helps process replies to this IQ
    NotificationFrom = jid:make(<<>>, Host, <<>>),
    mod_event_pusher_push:cast(Host, fun ejabberd_local:route_iq/5,
                               [NotificationFrom, PubsubJID, Acc, Stanza, ResponseHandler]).

-spec handle_publish_response(Recipient :: jid:jid(), PubsubJID :: jid:jid(),
                              Node :: mod_event_pusher_push:pubsub_node(),
                              Result :: timeout | jlib:iq()) -> ok.
handle_publish_response(_Recipient, _PubsubJID, _Node, timeout) ->
    ok;
handle_publish_response(_Recipient, _PubsubJID, _Node, #iq{type = result}) ->
    ok;
handle_publish_response(Recipient, PubsubJID, Node, #iq{type = error, sub_el = Els}) ->
    [Error | _ ] = [Err || #xmlel{name = <<"error">>} = Err <- Els],
    case exml_query:attr(Error, <<"type">>) of
        <<"cancel">> ->
            %% We disable the push node in case the error type is cancel
            mod_event_pusher_push:disable_node(Recipient,PubsubJID, Node);
        _ ->
            ok
    end.

-spec push_notification_iq(Node :: mod_event_pusher_push:pubsub_node(),
                           Form :: mod_event_pusher_push:form(),
                           PushPayload :: mod_event_pusher_push_plugin:push_payload()) ->
                              jlib:iq().
push_notification_iq(Node, Form, PushPayload) ->
    NotificationFields = [{<<"FORM_TYPE">>, ?PUSH_FORM_TYPE} | PushPayload ],

    #iq{type = set, sub_el = [
        #xmlel{name = <<"pubsub">>, attrs = [{<<"xmlns">>, ?NS_PUBSUB}], children = [
            #xmlel{name = <<"publish">>, attrs = [{<<"node">>, Node}], children = [
                #xmlel{name = <<"item">>, children = [
                    #xmlel{name = <<"notification">>,
                           attrs = [{<<"xmlns">>, ?NS_PUSH}],
                           children = [make_form(NotificationFields)]}
                ]}
            ]}
        ] ++ maybe_publish_options(Form)}
    ]}.

-spec make_form(mod_event_pusher_push:form()) -> exml:element().
make_form(Fields) ->
    #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"submit">>}],
           children = [make_form_field(Field) || Field <- Fields]}.

-spec make_form_field(mod_event_pusher_push:form_field()) -> exml:element().
make_form_field({Name, Value}) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, Name}],
           children = [#xmlel{name = <<"value">>, children = [#xmlcdata{content = Value}]}]}.

-spec maybe_publish_options(mod_event_pusher_push:form()) -> [exml:element()].
maybe_publish_options([]) ->
    [];
maybe_publish_options(FormFields) ->
    Children = [make_form([{<<"FORM_TYPE">>, ?NS_PUBSUB_PUB_OPTIONS}] ++ FormFields)],
    [#xmlel{name = <<"publish-options">>, children = Children}].
