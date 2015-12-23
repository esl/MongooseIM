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
         create_node/2,
         delete_node/2,
         subscribe/2, subscribe/3,
         unsubscribe/2,
         publish/3,
         receive_notification/3,
         request_all_items/3
        ]).

%%-----------------------------------------------------------------------------
%% API: pubsub tools
%%-----------------------------------------------------------------------------

create_node(User, {NodeAddr, NodeName}) ->
    Id = <<"create1">>,
    CreateNodeIq = escalus_pubsub_stanza:create_node_stanza(
                       User, Id, NodeAddr, NodeName),
    log_stanza("REQUEST create node", CreateNodeIq),
    escalus:send(User, CreateNodeIq),
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
    UserName = escalus_utils:get_username(User),
    Id = <<UserName/binary, <<"binsuffix">>/binary>>,
    SubscribeIq = escalus_pubsub_stanza:subscribe_by_user_stanza(User, Id, NodeName, NodeAddr),
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
    check_subscription(ResultStanza, User, NodeName).

unsubscribe(User, {NodeAddr, NodeName}) ->
    UserName = escalus_utils:get_username(User),
    Id = <<UserName/binary, <<"binsuffix">>/binary>>,
    UnsubscribeIq = escalus_pubsub_stanza:unsubscribe_by_user_stanza(User, Id, NodeName, NodeAddr),
    log_stanza("REQUEST unsubscribe", UnsubscribeIq),
    escalus:send(User, UnsubscribeIq),
    receive_response(User, Id).

publish(User, ItemId, {NodeAddr, NodeName}) ->
    UserName = escalus_utils:get_username(User),
    Id = <<UserName/binary, <<"binsuffix">>/binary>>,
    Publish = escalus_pubsub_stanza:create_publish_node_content_stanza(NodeName, ItemId),
    PublishIq = escalus_pubsub_stanza:iq_with_id(set, Id, NodeAddr, User, [Publish]),
    log_stanza("REQUEST publish", PublishIq),
    escalus:send(User, PublishIq),
    receive_response(User, Id).

receive_notification(User, ItemId, {NodeAddr, NodeName}) ->
    Stanza = escalus:wait_for_stanza(User),
    log_stanza("NOTIFICATION", Stanza),

    true = escalus_pred:is_message(Stanza) andalso escalus_pred:has_type(<<"headline">>, Stanza),
    true = escalus_pred:is_stanza_from(NodeAddr, Stanza),

    Items = exml_query:path(Stanza, [{element, <<"event">>},
                                     {element, <<"items">>}]),
    check_items(Items, [ItemId], NodeName),
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
    check_items(Items, ItemIds, NodeName).

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------

receive_response(User, Id) ->
    ResultStanza = escalus:wait_for_stanza(User),
    log_stanza("RESPONSE", ResultStanza),
    true = escalus_pred:is_iq_result(ResultStanza),
    Id = exml_query:attr(ResultStanza, <<"id">>),
    ResultStanza.

log_stanza(ReportString, Stanza) ->
    PrettyStanza = binary:list_to_bin(exml:to_pretty_iolist(Stanza)),
    %ct:print("~p~n", [Stanza]),
    ct:print("~s~n~s", [ReportString, PrettyStanza]),
    ct:log("~s~n~s", [ReportString, exml:escape_attr(PrettyStanza)]).

check_subscription(SubscrConfirmation, User, NodeName) ->
    Subscr = exml_query:path(SubscrConfirmation, [{element, <<"pubsub">>},
                                                  {element, <<"subscription">>}]),
    Jid = exml_query:attr(Subscr, <<"jid">>),
    Jid = escalus_utils:get_jid(User),
    NodeName = exml_query:attr(Subscr, <<"node">>),
    true = exml_query:attr(Subscr, <<"subid">>) =/= undefined,
    <<"subscribed">> = exml_query:attr(Subscr, <<"subscription">>).

check_items(ReceivedItemsElem, ExpectedItemIds, NodeName) ->
    NodeName = exml_query:attr(ReceivedItemsElem, <<"node">>),
    ReceivedItems = exml_query:subelements(ReceivedItemsElem, <<"item">>),
    [check_item(ReceivedItem, ExpectedItemId) ||
        {ReceivedItem, ExpectedItemId} <- lists:zip(ReceivedItems, ExpectedItemIds)].

check_item(ReceivedItem, ExpectedItemId) ->
    PublishEntry = escalus_pubsub_stanza:publish_entry([]),
    ReceivedItem = escalus_pubsub_stanza:publish_item(ExpectedItemId, PublishEntry).
