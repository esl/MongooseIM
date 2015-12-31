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
         subscribe/2, subscribe/3, subscribe/4,
         unsubscribe/2, unsubscribe/3,
         publish/3, publish/4, publish/5,
         receive_notification/3, receive_notification/4,
         request_all_items/3,
         retrieve_subscriptions/3,
         discover_nodes/3
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
    log_stanza("REQUEST create node", CreateNodeIq),
    escalus:send(User, CreateNodeIq),
    receive_response(User, Id).

configure_node(User, {NodeAddr, NodeName}, Config) ->
    Id = <<"config1">>,
    RequestIq = escalus_pubsub_stanza:configure_node_stanza(User, Id, NodeAddr, NodeName, Config),
    log_stanza("REQUEST configure node", RequestIq),
    escalus:send(User, RequestIq),
    receive_response(User, Id).

delete_node(User, {NodeAddr, NodeName}) ->
    Id = <<"delete1">>,
    DeleteNode = escalus_pubsub_stanza:delete_node_stanza(NodeName),
    DeleteNodeIq = escalus_pubsub_stanza:iq_with_id(set, Id, NodeAddr, User, [DeleteNode]),
    log_stanza("REQUEST delete node", DeleteNodeIq),
    escalus:send(User, DeleteNodeIq),
    receive_response(User, Id).

subscribe(User, {NodeAddr, NodeName}) ->
    subscribe(User, {NodeAddr, NodeName}, none).

subscribe(User, {NodeAddr, NodeName}, ExpectedItemId) ->
    subscribe(User, {NodeAddr, NodeName}, ExpectedItemId, true).

subscribe(User, {NodeAddr, NodeName}, ExpectedItemId, WithResource) ->
    UserName = escalus_utils:get_username(User),
    Jid = jid(User, WithResource),
    Id = <<UserName/binary, <<"binsuffix">>/binary>>,
    SubscribeIq = escalus_pubsub_stanza:subscribe_by_user_stanza(Jid, Id, NodeName, NodeAddr),
    log_stanza("REQUEST subscribe", SubscribeIq),
    escalus:send(User, SubscribeIq),
    case ExpectedItemId of
        none ->
            ok;
        _ ->
            Stanza = receive_notification(User, ExpectedItemId, {NodeAddr, NodeName}),
            true = exml_query:subelement(Stanza, <<"delay">>) =/= undefined
    end,
    ResultStanza = receive_response(User, Id),
    Subscription = exml_query:path(ResultStanza, [{element, <<"pubsub">>},
                                                  {element, <<"subscription">>}]),
    check_subscription(Subscription, Jid, NodeName).

unsubscribe(User, {NodeAddr, NodeName}) ->
    unsubscribe(User, {NodeAddr, NodeName}, true).

unsubscribe(User, {NodeAddr, NodeName}, WithResource) ->
    UserName = escalus_utils:get_username(User),
    Jid = jid(User, WithResource),
    Id = <<UserName/binary, <<"binsuffix">>/binary>>,
    UnsubscribeIq = escalus_pubsub_stanza:unsubscribe_by_user_stanza(Jid, Id, NodeName, NodeAddr),
    log_stanza("REQUEST unsubscribe", UnsubscribeIq),
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
    PublishIq = escalus_pubsub_stanza:iq_with_id(set, Id, NodeAddr, User, [Publish]),
    log_stanza("REQUEST publish", PublishIq),
    escalus:send(User, PublishIq),
    case ExpectedErrorType of
        none -> receive_response(User, Id);
        _ -> receive_error_response(User, Id, ExpectedErrorType)
    end.

receive_notification(User, ItemId, Node) ->
    receive_notification(User, ItemId, Node, true).

receive_notification(User, ItemId, {NodeAddr, NodeName}, WithPayload) ->
    Stanza = escalus:wait_for_stanza(User),
    log_stanza("NOTIFICATION", Stanza),

    true = escalus_pred:is_message(Stanza) andalso escalus_pred:has_type(<<"headline">>, Stanza),
    true = escalus_pred:is_stanza_from(NodeAddr, Stanza),

    Items = exml_query:path(Stanza, [{element, <<"event">>},
                                     {element, <<"items">>}]),
    check_items(Items, [ItemId], NodeName, WithPayload),
    Stanza.

request_all_items(User, ItemIds, {NodeAddr, NodeName}) ->
    Id = <<"items1">>,
    Request = escalus_pubsub_stanza:create_request_allitems_stanza(NodeName),
    RequestIq = escalus_pubsub_stanza:iq_with_id(get, Id, NodeAddr, User, [Request]),
    log_stanza("REQUEST all items", RequestIq),
    escalus:send(User, RequestIq),
    ResultStanza = receive_response(User, Id),
    Items = exml_query:path(ResultStanza, [{element, <<"pubsub">>},
                                           {element, <<"items">>}]),
    check_items(Items, ItemIds, NodeName, true).

retrieve_subscriptions(User, ExpectedSubscriptions, NodeAddr) ->
    Id = <<"subs1">>,
    Request = escalus_pubsub_stanza:retrieve_user_subscriptions_stanza(),
    RequestIq = escalus_pubsub_stanza:iq_with_id(get, Id, NodeAddr, User, [Request]),
    log_stanza("REQUEST subscriptions", RequestIq),
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

discover_nodes(User, {NodeAddr, NodeName}, ExpectedChildren) ->
    Id = <<"disco1">>,
    RequestIq = escalus_pubsub_stanza:discover_nodes_stanza(User, Id, NodeAddr, NodeName),
    log_stanza("REQUEST nodes", RequestIq),
    escalus:send(User, RequestIq),
    ResultStanza = receive_response(User, Id),
    Query = exml_query:subelement(ResultStanza, <<"query">>),
    Items = exml_query:subelements(Query, <<"item">>),
    [NodeAddr = exml_query:attr(Item, <<"jid">>) || Item <- Items],
    ReceivedChildren = [exml_query:attr(Item, <<"node">>) || Item <- Items],
    ExpectedChildren = lists:sort(ReceivedChildren).

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------

receive_response(User, Id) ->
    ResultStanza = escalus:wait_for_stanza(User),
    log_stanza("RESPONSE", ResultStanza),
    true = escalus_pred:is_iq_result(ResultStanza),
    Id = exml_query:attr(ResultStanza, <<"id">>),
    ResultStanza.

receive_error_response(User, Id, Type) ->
    ErrorStanza = escalus:wait_for_stanza(User),
    log_stanza("RESPONSE (error expected)", ErrorStanza),
    true = escalus_pred:is_iq_error(ErrorStanza),
    Id = exml_query:attr(ErrorStanza, <<"id">>),
    ErrorElem = exml_query:subelement(ErrorStanza, <<"error">>),
    Type = exml_query:attr(ErrorElem, <<"type">>),
    ErrorStanza.

log_stanza(ReportString, Stanza) ->
    %ct:print("~p~n", [Stanza]),
    PrettyStanza = binary:list_to_bin(exml:to_pretty_iolist(Stanza)),
    ct:print("~s~n~s", [ReportString, PrettyStanza]),
    ct:log("~s~n~s", [ReportString, exml:escape_attr(PrettyStanza)]).

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

jid(User, true) -> escalus_utils:get_jid(User);
jid(User, false) -> escalus_utils:get_short_jid(User).
