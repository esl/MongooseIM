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
-spec should_publish(Acc :: mongoose_acc:t(),
                     Event :: mod_event_pusher:event(),
                     Services :: [mod_event_pusher_push:publish_service()]) ->
                        [mod_event_pusher_push:publish_service()].
should_publish(Acc, #chat_event{to = To}, Services) ->
    PublishedServices = mongoose_acc:get(event_pusher, published_services, [], Acc),
    case should_publish(Acc, To) of
        true -> Services -- PublishedServices;
        false -> []
    end;
should_publish(_Acc, _Event, _Services) -> [].

-spec prepare_notification(Acc :: mongoose_acc:t(),
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

-spec publish_notification(Acc :: mongoose_acc:t(),
                           Event :: mod_event_pusher:event(),
                           Payload :: mod_event_pusher_push_plugin:push_payload(),
                           Services :: [mod_event_pusher_push:publish_service()]) ->
                              mongoose_acc:t().
publish_notification(Acc, _, Payload, Services) ->
    To = mongoose_acc:to_jid(Acc),
    HostType = mongoose_acc:host_type(Acc),
    lists:foreach(fun({PubsubJID, _Node, _Form} = Service) ->
                      case mod_event_pusher_push:is_virtual_pubsub_host(HostType,
                                                                        To#jid.lserver,
                                                                        PubsubJID#jid.lserver) of
                          true ->
                              publish_via_hook(Acc, HostType, To, Service, Payload);
                          false ->
                              publish_via_pubsub(HostType, To, Service, Payload)
                      end
                  end, Services),

    mongoose_acc:append(event_pusher, published_services, Services, Acc).

%%--------------------------------------------------------------------
%% local functions
%%--------------------------------------------------------------------

-spec should_publish(Acc :: mongoose_acc:t(), To :: jid:jid()) -> boolean().
should_publish(Acc, #jid{} = To) ->
    HostType = mongoose_acc:host_type(Acc),
    try ejabberd_auth:does_user_exist(HostType, To, stored) of
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
    NewAcc = mongoose_hooks:inbox_unread_count(To#jid.lserver, Acc, To),
    mongoose_acc:get(inbox, unread_count, 1, NewAcc).

-spec sender_id(jid:jid(), exml:element()) -> binary().
sender_id(From, Packet) ->
    case exml_query:attr(Packet, <<"type">>) of
        <<"chat">> ->
            jid:to_bare_binary(jid:to_lower(From));
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

-spec publish_via_hook(Acc :: mongoose_acc:t(),
                       HostType :: mongooseim:host_type(),
                       To :: jid:jid(),
                       Service :: mod_event_pusher_push:publish_service(),
                       PushPayload :: mod_event_pusher_push_plugin:push_payload()) ->
                          any().
publish_via_hook(Acc0, HostType, To, {PubsubJID, Node, Form}, PushPayload) ->
    %% Acc is ignored by mod_push_service_mongoosepush, added here only for
    %% traceability purposes and push_SUITE code unification
    Acc = mongoose_acc:set(push_notifications, pubsub_jid, PubsubJID, Acc0),
    case mongoose_hooks:push_notifications(HostType, Acc, [maps:from_list(PushPayload)], Form) of
        {error, device_not_registered} ->
            %% We disable the push node in case the error type is device_not_registered
            mod_event_pusher_push:disable_node(HostType, To, PubsubJID, Node);
        _ -> ok
    end.

-spec publish_via_pubsub(mongooseim:host_type(), To :: jid:jid(),
                         Service :: mod_event_pusher_push:publish_service(),
                         PushPayload :: mod_event_pusher_push_plugin:push_payload()) ->
                            any().
publish_via_pubsub(HostType, To, {PubsubJID, Node, Form}, PushPayload) ->
    Stanza = push_notification_iq(Node, Form, PushPayload),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              host_type => HostType,
                              lserver => To#jid.lserver,
                              element => jlib:iq_to_xml(Stanza),
                              from_jid => To,
                              to_jid => PubsubJID }),

    ResponseHandler =
    fun(_From, _To, FAcc, Response) ->
            mod_event_pusher_push:cast(HostType, fun handle_publish_response/5,
                                       [HostType, To, PubsubJID, Node, Response]),
            FAcc
    end,
    %% The IQ is routed from the recipient's server JID to pubsub JID
    %% This is recommended in the XEP and also helps process replies to this IQ
    NotificationFrom = jid:make(<<>>, To#jid.lserver, <<>>),
    mod_event_pusher_push:cast(HostType, fun ejabberd_local:route_iq/5,
                               [NotificationFrom, PubsubJID, Acc, Stanza, ResponseHandler]).

-spec handle_publish_response(mongooseim:host_type(),
                              Recipient :: jid:jid(), PubsubJID :: jid:jid(),
                              Node :: mod_event_pusher_push:pubsub_node(),
                              Result :: timeout | jlib:iq()) -> ok.
handle_publish_response(_HostType, _Recipient, _PubsubJID, _Node, timeout) ->
    ok;
handle_publish_response(_HostType, _Recipient, _PubsubJID, _Node, #iq{type = result}) ->
    ok;
handle_publish_response(HostType, Recipient, PubsubJID, Node, #iq{type = error, sub_el = Els}) ->
    [Error | _ ] = [Err || #xmlel{name = <<"error">>} = Err <- Els],
    case exml_query:attr(Error, <<"type">>) of
        <<"cancel">> ->
            %% We disable the push node in case the error type is cancel
            mod_event_pusher_push:disable_node(HostType, Recipient, PubsubJID, Node);
        _ ->
            ok
    end.

-spec push_notification_iq(Node :: mod_event_pusher_push:pubsub_node(),
                           Form :: mod_event_pusher_push:form(),
                           PushPayload :: mod_event_pusher_push_plugin:push_payload()) ->
                              jlib:iq().
push_notification_iq(Node, Form, PushPayload) ->
    #iq{type = set, sub_el = [
        #xmlel{name = <<"pubsub">>, attrs = [{<<"xmlns">>, ?NS_PUBSUB}], children = [
            #xmlel{name = <<"publish">>, attrs = [{<<"node">>, Node}], children = [
                #xmlel{name = <<"item">>, children = [
                    #xmlel{name = <<"notification">>,
                           attrs = [{<<"xmlns">>, ?NS_PUSH}],
                           children = [make_form(?PUSH_FORM_TYPE, PushPayload)]}
                ]}
            ]}
        ] ++ maybe_publish_options(maps:to_list(Form))}
    ]}.

-spec make_form(binary(), mod_event_pusher_push_plugin:push_payload()) -> exml:element().
make_form(FormType, FieldKVs) ->
    Fields = [#{var => Name, values => [Value]} || {Name, Value} <- FieldKVs],
    mongoose_data_forms:form(#{ns => FormType, type => <<"submit">>, fields => Fields}).

-spec maybe_publish_options([{binary(), binary()}]) -> [exml:element()].
maybe_publish_options([]) ->
    [];
maybe_publish_options(FormFields) ->
    Children = [make_form(?NS_PUBSUB_PUB_OPTIONS, FormFields)],
    [#xmlel{name = <<"publish-options">>, children = Children}].
