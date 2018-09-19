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

%% Callback API
-export([should_publish/3, sender_id/2]).
-export([publish_notification/5]).

-define(PUSH_FORM_TYPE, <<"urn:xmpp:push:summary">>).

%%--------------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------------

%% Callback 'should_publish'
-spec should_publish(From :: jid:jid(), To :: jid:jid(), Packet :: exml:element()) ->
                            boolean().
should_publish(_From, To = #jid{luser = LUser, lserver = LServer}, _Packet) ->
    try ejabberd_users:does_user_exist(LUser, LServer) of
        false ->
            false;
        true ->
            ejabberd_sm:is_offline(To)
    catch
        _:_ ->
            ejabberd_sm:is_offline(To)
    end.

%% Callback 'sender_id'
-spec sender_id(From :: jid:jid(), Packet :: exml:element()) -> SenderId :: binary().
sender_id(From, Packet) ->
    case exml_query:attr(Packet, <<"type">>) of
        <<"chat">> ->
            jid:to_binary(jid:to_bare(jid:to_lower(From)));
        <<"groupchat">> ->
            jid:to_binary(jid:to_lower(From))
    end.

-spec publish_notification(Acc :: mongooseim_acc:t(),
                           From :: jid:jid(),
                           To :: jid:jid(),
                           Packet :: exml:element(),
                           Services :: [mod_event_pusher_push:publish_service()]) ->
    mongooseim_acc:t().
publish_notification(Acc0, From, #jid{lserver = Host} = To, Packet, Services) ->
    MessageCount = read_unread_count(Acc0),
    BareRecipient = jid:to_bare(To),
    lists:foreach(
      fun({PubsubJID, Node, Form}) ->
              Stanza = push_notification_iq(From, Packet, Node, Form, MessageCount),
              Acc = mongoose_acc:new(#{ location => ?LOCATION,
                                        lserver => To#jid.lserver,
                                        element => jlib:iq_to_xml(Stanza),
                                        from_jid => To,
                                        to_jid => PubsubJID }),

              ResponseHandler =
                  fun(_From, _To, Acc1, Response) ->
                          mod_event_pusher_push:cast(Host, handle_publish_response,
                                                     [BareRecipient, PubsubJID, Node, Response]),
                          Acc1
                  end,
              mod_event_pusher_push:cast(Host, ejabberd_local, route_iq,
                                         [To, PubsubJID, Acc, Stanza, ResponseHandler])
      end, Services),
    Acc0.


-spec push_notification_iq(From :: jid:jid(),
                           Packet :: exml:element(), Node :: mod_event_pusher_push:pubsub_node(),
                           Form :: mod_event_pusher_push:form(),
                           MessageCount :: binary()) -> jlib:iq().
push_notification_iq(From, Packet, Node, Form, MessageCount) ->
    ContentFields =
        [
         {<<"FORM_TYPE">>, ?PUSH_FORM_TYPE},
         {<<"message-count">>, MessageCount},
         {<<"last-message-sender">>, sender_id(From, Packet)},
         {<<"last-message-body">>, exml_query:cdata(exml_query:subelement(Packet, <<"body">>))}
        ],

    #iq{type = set, sub_el = [
        #xmlel{name = <<"pubsub">>, attrs = [{<<"xmlns">>, ?NS_PUBSUB}], children = [
            #xmlel{name = <<"publish">>, attrs = [{<<"node">>, Node}], children = [
                #xmlel{name = <<"item">>, children = [
                    #xmlel{name = <<"notification">>,
                           attrs = [{<<"xmlns">>, ?NS_PUSH}], children = [make_form(ContentFields)]}
                ]}
            ]}
        ] ++ maybe_publish_options(Form)}
    ]}.

-spec maybe_publish_options(mod_event_pusher_push:form()) -> [exml:element()].
maybe_publish_options([]) ->
    [];
maybe_publish_options(FormFields) ->
    [#xmlel{name = <<"publish-options">>,
            children = [
                        make_form([{<<"FORM_TYPE">>, ?NS_PUBSUB_PUB_OPTIONS}] ++ FormFields)
                       ]}].

-spec make_form(mod_event_pusher_push:form()) -> exml:element().
make_form(Fields) ->
    #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"submit">>}],
           children = [make_form_field(Field) || Field <- Fields]}.

-spec make_form_field(mod_event_pusher_push:form_field()) -> exml:element().
make_form_field({Name, Value}) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, Name}],
           children = [#xmlel{name = <<"value">>, children = [#xmlcdata{content = Value}]}]}.

read_unread_count(Acc) ->
    try mongoose_acc:get(inbox, unread_count, Acc) of
        Val when is_integer(Val) ->
            integer_to_binary(Val)
    catch
        _:_ ->
            <<"1">>
    end.

