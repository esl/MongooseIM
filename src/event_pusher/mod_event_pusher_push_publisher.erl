-module(mod_event_pusher_push_publisher).
-moduledoc "Publishes push notification content".

-include("jlib.hrl").
-include("mongoose.hrl").

-export([publish_notification/3]).

-define(PUSH_FORM_TYPE, <<"urn:xmpp:push:summary">>).

-spec publish_notification(Acc :: mongoose_acc:t(),
                           Payload :: mod_event_pusher_push_plugin:push_payload(),
                           Services :: [mod_event_pusher_push:publish_service()]) ->
                              mongoose_acc:t().
publish_notification(Acc, Payload, Services) ->
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
        #xmlel{name = <<"pubsub">>, attrs = #{<<"xmlns">> => ?NS_PUBSUB}, children = [
            #xmlel{name = <<"publish">>, attrs = #{<<"node">> => Node}, children = [
                #xmlel{name = <<"item">>, children = [
                    #xmlel{name = <<"notification">>,
                           attrs = #{<<"xmlns">> => ?NS_PUSH},
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
