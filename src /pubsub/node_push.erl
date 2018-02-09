%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C) 2017 Erlang Solutions Ltd.
%%% This software is released under the Apache License, Version 2.0
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Node implementation that proxies all published items to `push_notification` hook.
%%% @end
%%%-------------------------------------------------------------------
-module(node_push).
-author('rafal.slota@erlang-solutions.com').
-behaviour(gen_pubsub_node).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("pubsub.hrl").

-export([init/3, terminate/2, options/0, features/0,
         create_node_permission/6, create_node/2, delete_node/1,
         purge_node/2, subscribe_node/8, unsubscribe_node/4,
         publish_item/9, delete_item/4, remove_extra_items/3,
         get_entity_affiliations/2, get_node_affiliations/1,
         get_affiliation/2, set_affiliation/3,
         get_entity_subscriptions/2, get_node_subscriptions/1,
         get_subscriptions/2, set_subscriptions/4,
         get_pending_nodes/2, get_states/1, get_state/2,
         set_state/1, get_items/7, get_items/3, get_item/7,
         get_item/2, set_item/1, get_item_name/3, node_to_path/1,
         path_to_node/1]).

init(Host, ServerHost, Opts) ->
    node_flat:init(Host, ServerHost, Opts),
    ok.

terminate(Host, ServerHost) ->
    node_flat:terminate(Host, ServerHost),
    ok.

options() ->
    [{deliver_payloads, true},
     {notify_config, false},
     {notify_delete, false},
     {notify_retract, false},
     {purge_offline, false},
     {persist_items, false},
     {max_items, 1},
     {subscribe, true},
     {access_model, whitelist},
     {roster_groups_allowed, []},
     {publish_model, open},
     {notification_type, headline},
     {max_payload_size, ?MAX_PAYLOAD_SIZE},
     {send_last_published_item, on_sub_and_presence},
     {deliver_notifications, true},
     {presence_based_delivery, true}].

features() ->
    [
        <<"create-nodes">>,
        <<"delete-nodes">>,
        <<"modify-affiliations">>,
        <<"publish">>,
        <<"publish-options">>,
        <<"publish-only-affiliation">>,
        <<"purge-nodes">>,
        <<"retrieve-affiliations">>
    ].

create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access) ->
    node_flat:create_node_permission(Host, ServerHost, Node, ParentNode, Owner, Access).

create_node(Nidx, Owner) ->
    node_flat:create_node(Nidx, Owner).

delete_node(Nodes) ->
    node_flat:delete_node(Nodes).

subscribe_node(Nidx, Sender, Subscriber, AccessModel,
               SendLast, PresenceSubscription, RosterGroup, Options) ->
    node_flat:subscribe_node(Nidx, Sender, Subscriber, AccessModel, SendLast,
                             PresenceSubscription, RosterGroup, Options).

unsubscribe_node(Nidx, Sender, Subscriber, SubId) ->
    node_flat:unsubscribe_node(Nidx, Sender, Subscriber, SubId).

publish_item(ServerHost, Nidx, Publisher, Model, _MaxItems, _ItemId, _ItemPublisher, Payload,
             PublishOptions) ->
    SubKey = jid:to_lower(Publisher),
    GenKey = jid:to_bare(SubKey),
    GenState = get_state(Nidx, GenKey),
    SubState = case SubKey of
                   GenKey -> GenState;
                   _ -> get_state(Nidx, SubKey)
               end,
    Affiliation = SubState#pubsub_state.affiliation,
    ElPayload = [El || #xmlel{} = El <- Payload],

    case is_allowed_to_publish(Model, Affiliation) of
        true ->
            do_publish_item(ServerHost, PublishOptions, ElPayload);
        false ->
            {error, mongoose_xmpp_errors:forbidden()}
    end.

do_publish_item(ServerHost, PublishOptions,
                [#xmlel{name = <<"notification">>} | _] = Notifications) ->
    case catch parse_form(PublishOptions) of
        #{<<"device_id">> := _, <<"service">> := _} = OptionMap ->
            NotificationRawForms = [exml_query:subelement(El, <<"x">>) || El <- Notifications],
            NotificationForms = [parse_form(Form) || Form <- NotificationRawForms],
            ejabberd_hooks:run(push_notifications, ServerHost,
                               [ServerHost, NotificationForms, OptionMap]),
            {result, default};
        _ ->
            {error, mod_pubsub:extended_error(mongoose_xmpp_errors:conflict(), <<"precondition-not-met">>)}
    end;
do_publish_item(_ServerHost, _PublishOptions, _Payload) ->
    {error, mongoose_xmpp_errors:bad_request()}.

remove_extra_items(Nidx, MaxItems, ItemIds) ->
    node_flat:remove_extra_items(Nidx, MaxItems, ItemIds).

delete_item(Nidx, Publisher, PublishModel, ItemId) ->
    node_flat:delete_item(Nidx, Publisher, PublishModel, ItemId).

purge_node(Nidx, Owner) ->
    node_flat:purge_node(Nidx, Owner).

get_entity_affiliations(Host, Owner) ->
    node_flat:get_entity_affiliations(Host, Owner).


get_node_affiliations(Nidx) ->
    node_flat:get_node_affiliations(Nidx).

get_affiliation(Nidx, Owner) ->
    node_flat:get_affiliation(Nidx, Owner).

set_affiliation(Nidx, Owner, Affiliation) ->
    node_flat:set_affiliation(Nidx, Owner, Affiliation).

get_entity_subscriptions(Host, Owner) ->
    node_flat:get_entity_subscriptions(Host, Owner).

get_node_subscriptions(Nidx) ->
    node_flat:get_node_subscriptions(Nidx).

get_subscriptions(Nidx, Owner) ->
    node_flat:get_subscriptions(Nidx, Owner).

set_subscriptions(Nidx, Owner, Subscription, SubId) ->
    node_flat:set_subscriptions(Nidx, Owner, Subscription, SubId).

get_pending_nodes(Host, Owner) ->
    node_flat:get_pending_nodes(Host, Owner).

get_states(Nidx) ->
    node_flat:get_states(Nidx).

get_state(Nidx, JID) ->
    node_flat:get_state(Nidx, JID).

set_state(State) ->
    node_flat:set_state(State).

get_items(Nidx, From, RSM) ->
    node_flat:get_items(Nidx, From, RSM).

get_items(Nidx, JID, AccessModel, PresenceSubscription, RosterGroup, SubId, RSM) ->
    node_flat:get_items(Nidx, JID, AccessModel,
                        PresenceSubscription, RosterGroup, SubId, RSM).

get_item(Nidx, ItemId) ->
    node_flat:get_item(Nidx, ItemId).

get_item(Nidx, ItemId, JID, AccessModel, PresenceSubscription, RosterGroup, SubId) ->
    node_flat:get_item(Nidx, ItemId, JID, AccessModel,
                       PresenceSubscription, RosterGroup, SubId).

set_item(Item) ->
    node_flat:set_item(Item).

get_item_name(Host, Node, Id) ->
    node_flat:get_item_name(Host, Node, Id).

node_to_path(Node) ->
    node_flat:node_to_path(Node).

path_to_node(Path) ->
    node_flat:path_to_node(Path).

%%%
%%% Internal
%%%

is_allowed_to_publish(PublishModel, Affiliation) ->
    (PublishModel == open)
    or (PublishModel == publishers)
        and ((Affiliation == owner)
              or (Affiliation == publisher)
              or (Affiliation == publish_only)).


-spec parse_form(undefined | exml:element()) -> invalid_form | #{atom() => binary()}.
parse_form(undefined) ->
    #{};
parse_form(Form) ->
    IsForm = ?NS_XDATA == exml_query:attr(Form, <<"xmlns">>),
    IsSubmit = <<"submit">> == exml_query:attr(Form, <<"type">>, <<"submit">>),

    FieldsXML = exml_query:subelements(Form, <<"field">>),
    Fields = [{exml_query:attr(Field, <<"var">>),
               exml_query:path(Field, [{element, <<"value">>}, cdata])} || Field <- FieldsXML],
    {_, CustomFields} = lists:partition(
        fun({Name, _}) ->
            Name == <<"FORM_TYPE">>
        end, Fields),

    case IsForm andalso IsSubmit of
        true ->
            maps:from_list(CustomFields);
        false ->
            invalid_form
    end.
