%%%===================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd.
%%% @doc Suite for testing pubsub features as described in XEP-0060
%%% @Tools module - pubsub specific tools and high level
%%% @               wrappers for the escalus tool.
%%% @end
%%%===================================================================

-module(pubsub_tools).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").

-export([
         create_node/2, create_node/3,
         configure_node/3,
         delete_node/2,
         subscribe/2, subscribe/3,
         unsubscribe/2, unsubscribe/3,
         publish/3, publish/4, publish/5,
         receive_item_notification/3, receive_item_notification/4,
         receive_subscription_notification/4,
         receive_node_creation_notification/2,
         request_all_items/3,
         purge_all_items/2,
         fail_to_purge_all_items/3,
         retrieve_user_subscriptions/3,
         retrieve_node_subscriptions/3,
         fail_to_retrieve_node_subscriptions/3,
         modify_node_subscriptions/3,
         fail_to_modify_node_subscriptions/4,
         discover_nodes/3,
         fail_to_discover_nodes/3
        ]).

%%-----------------------------------------------------------------------------
%% API: pubsub tools
%%-----------------------------------------------------------------------------

create_node(User, {NodeAddr, NodeName}) ->
    create_node(User, {NodeAddr, NodeName}, []).

create_node(User, {NodeAddr, NodeName}, Config) ->
    Id = <<"create1">>,
    CreateNodeIq = escalus_pubsub_stanza:create_node_stanza(
                       User, Id, NodeAddr, NodeName, Config),
    escalus:send(User, CreateNodeIq),
    receive_response(User, Id).

configure_node(User, {NodeAddr, NodeName}, Config) ->
    Id = <<"config1">>,
    RequestIq = escalus_pubsub_stanza:configure_node_stanza(User, Id, NodeAddr, NodeName, Config),
    escalus:send(User, RequestIq),
    receive_response(User, Id).

delete_node(User, {NodeAddr, NodeName}) ->
    Id = <<"delete1">>,
    DeleteNode = escalus_pubsub_stanza:delete_node_stanza(NodeName),
    DeleteNodeIq = escalus_pubsub_stanza:iq_with_id(set, Id, NodeAddr, User, [DeleteNode]),
    escalus:send(User, DeleteNodeIq),
    receive_response(User, Id).

subscribe(User, {NodeAddr, NodeName}) ->
    subscribe(User, {NodeAddr, NodeName}, []).

subscribe(User, {NodeAddr, NodeName}, Options) ->
    UserName = escalus_utils:get_username(User),
    Jid = jid(User, proplists:get_value(jid_type, Options, full)),
    Id = <<UserName/binary, <<"binsuffix">>/binary>>,
    Config = proplists:get_value(config, Options, []),
    SubscribeIq = escalus_pubsub_stanza:subscribe_by_user_stanza(Jid, Id, NodeName, NodeAddr, Config),
    escalus:send(User, SubscribeIq),
    case proplists:get_value(expected_notification, Options) of
        undefined ->
            ok;
        ExpectedItemId ->
            Stanza = receive_item_notification(User, ExpectedItemId, {NodeAddr, NodeName}),
            true = exml_query:subelement(Stanza, <<"delay">>) =/= undefined
    end,
    ResultStanza = receive_response(User, Id),
    Subscription = exml_query:path(ResultStanza, [{element, <<"pubsub">>},
                                                  {element, <<"subscription">>}]),
    check_subscription(Subscription, Jid, NodeName).

unsubscribe(User, {NodeAddr, NodeName}) ->
    unsubscribe(User, {NodeAddr, NodeName}, full).

unsubscribe(User, {NodeAddr, NodeName}, JidType) ->
    UserName = escalus_utils:get_username(User),
    Jid = jid(User, JidType),
    Id = <<UserName/binary, <<"binsuffix">>/binary>>,
    UnsubscribeIq = escalus_pubsub_stanza:unsubscribe_by_user_stanza(Jid, Id, NodeName, NodeAddr),
    escalus:send(User, UnsubscribeIq),
    receive_response(User, Id).

publish(User, ItemId, Node) ->
    publish(User, ItemId, Node, true).

publish(User, ItemId, Node, WithItem) ->
    publish(User, ItemId, Node, WithItem, none).

publish(User, ItemId, {NodeAddr, NodeName}, WithItem, ExpectedErrorType) ->
    UserName = escalus_utils:get_username(User),
    Id = <<UserName/binary, <<"binsuffix">>/binary>>,
    Item = case WithItem of
               true -> item(ItemId, true);
               false -> []
           end,
    Publish = escalus_pubsub_stanza:publish_item_stanza(NodeName, Item),
    PublishIq = case NodeAddr of
                    pep -> escalus_pubsub_stanza:iq_with_id(set, Id, User, [Publish]);
                    _ -> escalus_pubsub_stanza:iq_with_id(set, Id, NodeAddr, User, [Publish])
                end,
    escalus:send(User, PublishIq),
    case ExpectedErrorType of
        none -> receive_response(User, Id);
        _ -> receive_error_response(User, Id, ExpectedErrorType)
    end.

receive_item_notification(User, ItemId, Node) ->
    receive_item_notification(User, ItemId, Node, true).

receive_item_notification(User, ItemId, {NodeAddr, NodeName}, WithPayload) ->
    Stanza = receive_notification(User, NodeAddr),
    true = escalus_pred:has_type(<<"headline">>, Stanza),
    Items = exml_query:path(Stanza, [{element, <<"event">>},
                                     {element, <<"items">>}]),
    check_items(Items, [ItemId], NodeName, WithPayload),
    Stanza.

receive_subscription_notification(User, JidType, Subscription, {NodeAddr, NodeName}) ->
    Stanza = receive_notification(User, NodeAddr),
    SubscriptionElem = exml_query:path(Stanza, [{element, <<"pubsub">>},
                                                {element, <<"subscription">>}]),
    Jid = jid(User, JidType),
    Jid = exml_query:attr(SubscriptionElem, <<"jid">>),
    Subscription = exml_query:attr(SubscriptionElem, <<"subscription">>),
    NodeName = exml_query:attr(SubscriptionElem, <<"node">>).

receive_node_creation_notification(User, {NodeAddr, NodeName}) ->
    Stanza = receive_notification(User, NodeAddr),
    NodeName = exml_query:path(Stanza, [{element, <<"event">>},
                                        {element, <<"create">>},
                                        {attr, <<"node">>}]).

request_all_items(User, ItemIds, {NodeAddr, NodeName}) ->
    Id = <<"items1">>,
    Request = escalus_pubsub_stanza:create_request_allitems_stanza(NodeName),
    RequestIq = escalus_pubsub_stanza:iq_with_id(get, Id, NodeAddr, User, [Request]),
    escalus:send(User, RequestIq),
    ResultStanza = receive_response(User, Id),
    Items = exml_query:path(ResultStanza, [{element, <<"pubsub">>},
                                           {element, <<"items">>}]),
    check_items(Items, ItemIds, NodeName, true).

purge_all_items(User, Node) ->
    Id = <<"purge1">>,
    send_purge_all_items_request(User, Id, Node),
    receive_response(User, Id).

fail_to_purge_all_items(User, ErrorType, Node) ->
    Id = <<"purge1">>,
    send_purge_all_items_request(User, Id, Node),
    receive_error_response(User, Id, ErrorType).

retrieve_user_subscriptions(User, ExpectedSubscriptions, NodeAddr) ->
    Id = <<"user_subs1">>,
    Request = escalus_pubsub_stanza:retrieve_user_subscriptions_stanza(),
    RequestIq = escalus_pubsub_stanza:iq_with_id(get, Id, NodeAddr, User, [Request]),
    escalus:send(User, RequestIq),
    ResultStanza = receive_response(User, Id),
    SubscriptionElems = exml_query:paths(ResultStanza, [{element, <<"pubsub">>},
                                                        {element, <<"subscriptions">>},
                                                        {element, <<"subscription">>}]),
    Jid = escalus_utils:get_jid(User),
    [Jid = exml_query:attr(Subscr, <<"jid">>) || Subscr <- SubscriptionElems],
    Subscriptions = [{exml_query:attr(Subscr, <<"node">>),
                      exml_query:attr(Subscr, <<"subscription">>)} || Subscr <- SubscriptionElems],
    ExpectedSubscriptions = lists:sort(Subscriptions).

fail_to_retrieve_node_subscriptions(User, ErrorType, {NodeAddr, NodeName}) ->
    Id = <<"node_subs_failed">>,
    send_node_subscriptions_request(User, Id, {NodeAddr, NodeName}),
    receive_error_response(User, Id, ErrorType).

retrieve_node_subscriptions(User, ExpectedSubscriptions, {NodeAddr, NodeName}) ->
    Id = <<"node_subs">>,
    send_node_subscriptions_request(User, Id, {NodeAddr, NodeName}),
    receive_node_subscriptions_response(User, Id, ExpectedSubscriptions, NodeName).

modify_node_subscriptions(User, ModifiedSubscriptions, {NodeAddr, NodeName}) ->
    Id = <<"modify_node_subs">>,
    send_modify_node_subscriptions_request(User, Id, ModifiedSubscriptions, {NodeAddr, NodeName}),
    receive_response(User, Id).

fail_to_modify_node_subscriptions(User, ModifiedSubscriptions, ErrorType, {NodeAddr, NodeName}) ->
    Id = <<"modify_node_subs_failed">>,
    send_modify_node_subscriptions_request(User, Id, ModifiedSubscriptions, {NodeAddr, NodeName}),
    receive_error_response(User, Id, ErrorType).

discover_nodes(User, {NodeAddr, NodeName}, ExpectedChildren) ->
    Id = <<"disco_children">>,
    send_child_node_discovery_request(User, Id, NodeAddr, NodeName),
    receive_node_discovery_response(User, Id, NodeAddr, NodeName, ExpectedChildren);
discover_nodes(User, NodeAddr, ExpectedNodes) ->
    Id = <<"disco_nodes">>,
    RequestIq = escalus_pubsub_stanza:discover_nodes_stanza(User, Id, NodeAddr),
    escalus:send(User, RequestIq),
    receive_node_discovery_response(User, Id, NodeAddr, undefined, ExpectedNodes).

fail_to_discover_nodes(User, {NodeAddr, NodeName}, ErrorType) ->
    Id = <<"disco_children_fail">>,
    send_child_node_discovery_request(User, Id, NodeAddr, NodeName),
    receive_error_response(User, Id, ErrorType).

%%-----------------------------------------------------------------------------
%% Specific request/response helper functions
%%-----------------------------------------------------------------------------

send_purge_all_items_request(User, Id, {NodeAddr, NodeName}) ->
    RequestIq = escalus_pubsub_stanza:purge_all_items_iq(User, Id, NodeAddr, NodeName),
    escalus:send(User, RequestIq).

send_node_subscriptions_request(User, Id, {NodeAddr, NodeName}) ->
    Request = escalus_pubsub_stanza:retrieve_subscriptions_stanza(NodeName),
    RequestIq = escalus_pubsub_stanza:iq_with_id(get, Id, NodeAddr, User, [Request]),
    escalus:send(User, RequestIq).

receive_node_subscriptions_response(User, Id, ExpectedSubscriptions, NodeName) ->
    ResultStanza = receive_response(User, Id),
    SubscriptionsElem = exml_query:path(ResultStanza, [{element, <<"pubsub">>},
                                                       {element, <<"subscriptions">>}]),
    NodeName = exml_query:attr(SubscriptionsElem, <<"node">>),
    SubscriptionElems = exml_query:subelements(SubscriptionsElem, <<"subscription">>),
    Subscriptions = [{exml_query:attr(Subscr, <<"jid">>),
                      exml_query:attr(Subscr, <<"subscription">>)} || Subscr <- SubscriptionElems],
    ExpectedSubscriptionsWithJids = fill_subscriptions_jids(ExpectedSubscriptions),
    ExpectedSubscriptionsWithJids = lists:sort(Subscriptions).

send_modify_node_subscriptions_request(User, Id, ModifiedSubscriptions, {NodeAddr, NodeName}) ->
    Changes = fill_subscriptions_jids(ModifiedSubscriptions),
    ChangeElems = escalus_pubsub_stanza:get_subscription_change_list_stanza(Changes),
    Request = escalus_pubsub_stanza:set_subscriptions_stanza(NodeName, ChangeElems),
    RequestIq = escalus_pubsub_stanza:iq_with_id(set, Id, NodeAddr, User, [Request]),
    escalus:send(User, RequestIq).

send_child_node_discovery_request(User, Id, NodeAddr, NodeName) ->
    RequestIq = escalus_pubsub_stanza:discover_nodes_stanza(User, Id, NodeAddr, NodeName),
    escalus:send(User, RequestIq).

receive_node_discovery_response(User, Id, NodeAddr, NodeName, ExpectedNodes) ->
    ResultStanza = receive_response(User, Id),
    Query = exml_query:subelement(ResultStanza, <<"query">>),
    NodeName = exml_query:attr(Query, <<"node">>),
    Items = exml_query:subelements(Query, <<"item">>),
    [NodeAddr = exml_query:attr(Item, <<"jid">>) || Item <- Items],
    ReceivedNodes = [exml_query:attr(Item, <<"node">>) || Item <- Items],
    ExpectedNodes = lists:sort(ReceivedNodes).

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------

receive_notification(User, NodeAddr) ->
    Stanza = escalus:wait_for_stanza(User),
    true = escalus_pred:is_stanza_from(NodeAddr, Stanza),
    true = escalus_pred:is_message(Stanza),
    Stanza.

receive_response(User, Id) ->
    ResultStanza = escalus:wait_for_stanza(User),
    true = escalus_pred:is_iq_result(ResultStanza),
    Id = exml_query:attr(ResultStanza, <<"id">>),
    ResultStanza.

receive_error_response(User, Id, Type) ->
    ErrorStanza = escalus:wait_for_stanza(User),
    true = escalus_pred:is_iq_error(ErrorStanza),
    Id = exml_query:attr(ErrorStanza, <<"id">>),
    ErrorElem = exml_query:subelement(ErrorStanza, <<"error">>),
    Type = exml_query:attr(ErrorElem, <<"type">>),
    ErrorStanza.

check_subscription(Subscr, Jid, NodeName) ->
    Jid = exml_query:attr(Subscr, <<"jid">>),
    NodeName = exml_query:attr(Subscr, <<"node">>),
    true = exml_query:attr(Subscr, <<"subid">>) =/= undefined,
    <<"subscribed">> = exml_query:attr(Subscr, <<"subscription">>).

check_items(ReceivedItemsElem, ExpectedItemIds, NodeName, WithPayload) ->
    NodeName = exml_query:attr(ReceivedItemsElem, <<"node">>),
    ReceivedItems = exml_query:subelements(ReceivedItemsElem, <<"item">>),
    [ReceivedItem = item(ExpectedItemId, WithPayload) ||
        {ReceivedItem, ExpectedItemId} <- lists:zip(ReceivedItems, ExpectedItemIds)].

item(ItemId, WithPayload) ->
    escalus_pubsub_stanza:publish_item(ItemId, payload(WithPayload)).

payload(false) -> [];
payload(true) -> escalus_pubsub_stanza:publish_entry([]).

fill_subscriptions_jids(Subscriptions) ->
    [{jid(User, JidType), Subscr} || {User, JidType, Subscr} <- Subscriptions].

jid(User, full) -> escalus_utils:get_jid(User);
jid(User, bare) -> escalus_utils:get_short_jid(User).
