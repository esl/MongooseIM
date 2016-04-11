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
         create_node/3,
         configure_node/4,
         delete_node/3,
         subscribe_with_notification/4,
         subscribe/3,
         check_subscription_response/4,
         unsubscribe/3,
         check_item_notification/4,
         publish/4,
         receive_item_notification/4,
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
         fail_to_discover_nodes/3,
         check_response/2
        ]).

%%-----------------------------------------------------------------------------
%% API: pubsub tools
%%-----------------------------------------------------------------------------

%% API: Complex interactions (more than request + response)

subscribe_with_notification(User, {NodeAddr, NodeName}, ExpectedItemId, Options) ->
    subscribe(User, {NodeAddr, NodeName}, [{receive_response, false}|Options]),
    Stanza = receive_item_notification(User, ExpectedItemId, {NodeAddr, NodeName}, Options),
    true = exml_query:subelement(Stanza, <<"delay">>) =/= undefined,
    ResultStanza = receive_response(User, id(User, Options, <<"subscribe">>)),
    check_subscription_response(ResultStanza, User, NodeName, Options).

%% API: Send request, receive (optional) response

create_node(User, {NodeAddr, NodeName}, Options) ->
    Id = id(User, Options, <<"create_node">>),
    Stanza = escalus_pubsub_stanza:create_node_stanza(
               User, Id, NodeAddr, NodeName, proplists:get_value(config, Options, [])),
    send_and_receive_response(User, Stanza, Id, Options).

configure_node(User, {NodeAddr, NodeName}, Config, Options) ->
    Id = id(User, Options, <<"configure_node">>),
    Stanza = escalus_pubsub_stanza:configure_node_stanza(
               User, Id, NodeAddr, NodeName, Config),
    send_and_receive_response(User, Stanza, Id, Options).

delete_node(User, {NodeAddr, NodeName}, Options) ->
    DeleteNodeElement = escalus_pubsub_stanza:delete_node_stanza(NodeName),
    Id = id(User, Options, <<"delete_node">>),
    Stanza = escalus_pubsub_stanza:iq_with_id(set, Id, NodeAddr, User, [DeleteNodeElement]),
    send_and_receive_response(User, Stanza, Id, Options).

subscribe(User, {NodeAddr, NodeName}, Options) ->
    Jid = jid(User, proplists:get_value(jid_type, Options, full)),
    Id = id(User, Options, <<"subscribe">>),
    Stanza = escalus_pubsub_stanza:subscribe_by_user_stanza(
               Jid, Id, NodeName, NodeAddr, proplists:get_value(config, Options, [])),
    send_and_receive_response(User, Stanza, Id, Options).

unsubscribe(User, {NodeAddr, NodeName}, Options) ->
    Jid = jid(User, proplists:get_value(jid_type, Options, full)),
    Id = id(User, Options, <<"unsubscribe">>),
    Stanza = escalus_pubsub_stanza:unsubscribe_by_user_stanza(
               Jid, Id, NodeName, NodeAddr),
    send_and_receive_response(User, Stanza, Id, Options).

publish(User, ItemId, {NodeAddr, NodeName}, Options) ->
    Item = case proplists:get_value(with_item, Options, true) of
               true -> item(ItemId, true);
               false -> []
           end,
    PublishElem = escalus_pubsub_stanza:publish_item_stanza(NodeName, Item),
    Id = id(User, Options, <<"publish">>),
    Stanza = case NodeAddr of
                    pep -> escalus_pubsub_stanza:iq_with_id(set, Id, User, [PublishElem]);
                    _ -> escalus_pubsub_stanza:iq_with_id(set, Id, NodeAddr, User, [PublishElem])
                end,
    send_and_receive_response(User, Stanza, Id, Options).

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

%% API: Receiving notifications

receive_item_notification(User, ItemId, Node, Options) ->
    Stanza = escalus:wait_for_stanza(User),
    check_item_notification(Stanza, ItemId, Node, Options).

receive_subscription_notification(User, JidType, Subscription, {NodeAddr, NodeName}) ->
    Stanza = escalus:wait_for_stanza(User),
    check_notification(Stanza, NodeAddr),
    SubscriptionElem = exml_query:path(Stanza, [{element, <<"pubsub">>},
                                                {element, <<"subscription">>}]),
    Jid = jid(User, JidType),
    Jid = exml_query:attr(SubscriptionElem, <<"jid">>),
    Subscription = exml_query:attr(SubscriptionElem, <<"subscription">>),
    NodeName = exml_query:attr(SubscriptionElem, <<"node">>).

receive_node_creation_notification(User, {NodeAddr, NodeName}) ->
    Stanza = escalus:wait_for_stanza(User),
    check_notification(Stanza, NodeAddr),
    NodeName = exml_query:path(Stanza, [{element, <<"event">>},
                                        {element, <<"create">>},
                                        {attr, <<"node">>}]).

%% API: Checking incoming stanzas (notifications and responses)

check_subscription_response(Stanza, User, NodeName, Options) ->
    Jid = jid(User, proplists:get_value(jid_type, Options, full)),
    Subscription = exml_query:path(Stanza, [{element, <<"pubsub">>},
                                            {element, <<"subscription">>}]),
    check_subscription(Subscription, Jid, NodeName).

check_item_notification(Stanza, ItemId, {NodeAddr, NodeName}, Options) ->
    check_notification(Stanza, NodeAddr),
    true = escalus_pred:has_type(<<"headline">>, Stanza),
    Items = exml_query:path(Stanza, [{element, <<"event">>},
                                     {element, <<"items">>}]),
    check_items(Items, [ItemId], NodeName, proplists:get_value(with_payload, Options, true)),
    Stanza.

check_response(ResultStanza, Id) ->
    true = escalus_pred:is_iq_result(ResultStanza),
    Id = exml_query:attr(ResultStanza, <<"id">>),
    ResultStanza.

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

check_notification(Stanza, NodeAddr) ->
    true = escalus_pred:is_stanza_from(NodeAddr, Stanza),
    true = escalus_pred:is_message(Stanza),
    Stanza.

receive_response(User, Id) ->
    ResultStanza = escalus:wait_for_stanza(User),
    check_response(ResultStanza, Id),
    ResultStanza.

receive_response(User, Id, Timeout) ->
    ResultStanza = escalus:wait_for_stanza(User, Timeout),
    check_response(ResultStanza, Id),
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

id(User, Options, DefaultSuffix) ->
    UserName = escalus_utils:get_username(User),
    proplists:get_value(id, Options, <<UserName/binary, "-", DefaultSuffix/binary>>).

send_and_receive_response(User, Stanza, Id, Options) ->
    escalus:send(User, Stanza),
    case {proplists:get_value(receive_response, Options, true),
          proplists:get_value(expected_error_type, Options, none)} of
        {false, _} -> ok;
        {true, none} -> receive_response(User, Id, proplists:get_value(response_timeout, Options, 1000));
        {true, ExpectedErrorType} -> receive_error_response(User, Id, ExpectedErrorType)
    end.
