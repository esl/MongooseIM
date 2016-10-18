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

%% Send request, receive (optional) response
-export([create_node/3,
         configure_node/4,
         delete_node/3,
         subscribe/3,
         unsubscribe/3,
         publish/4,
         request_all_items/3,
         purge_all_items/3,
         retrieve_user_subscriptions/3,
         retrieve_node_subscriptions/3,
         modify_node_subscriptions/4,
         discover_nodes/3]).

%% Receive notification or response
-export([receive_item_notification/4,
         receive_subscription_notification/4,
         receive_node_creation_notification/3,
         receive_subscribe_response/3,
         receive_unsubscribe_response/3]).

%%-----------------------------------------------------------------------------
%% API: pubsub tools
%%-----------------------------------------------------------------------------

%% Send request, receive (optional) response

create_node(User, Node, Options) ->
    Id = id(User, Node, <<"create_node">>),
    Config = proplists:get_value(config, Options, []),
    Request = escalus_pubsub_stanza:create_node(User, Id, Node, Config),
    send_request_and_receive_response(User, Request, Id, Options).

configure_node(User, Node, Config, Options) ->
    Id = id(User, Node, <<"configure_node">>),
    Request = escalus_pubsub_stanza:configure_node(User, Id, Node, Config),
    send_request_and_receive_response(User, Request, Id, Options).

delete_node(User, Node, Options) ->
    Id = id(User, Node, <<"delete_node">>),
    Request = escalus_pubsub_stanza:delete_node(User, Id, Node),
    send_request_and_receive_response(User, Request, Id, Options).

subscribe(User, Node, Options) ->
    Jid = jid(User, proplists:get_value(jid_type, Options, full)),
    Id = id(User, Node, <<"subscribe">>),
    Config = proplists:get_value(config, Options, []),
    Request = escalus_pubsub_stanza:subscribe(Jid, Id, Node, Config),
    send_request_and_receive_response(
      User, Request, Id, [{expected_result, true} | Options],
      fun(Response) ->
              check_subscription_response(Response, User, Node, Options)
      end).

unsubscribe(User, Node, Options) ->
    Jid = jid(User, proplists:get_value(jid_type, Options, full)),
    Id = id(User, Node, <<"unsubscribe">>),
    Request = escalus_pubsub_stanza:unsubscribe(Jid, Id, Node),
    send_request_and_receive_response(User, Request, Id, Options).

publish(User, ItemId, Node, Options) ->
    Id = id(User, Node, <<"publish">>),
    Request = case proplists:get_value(with_item, Options, true) of
                  true -> escalus_pubsub_stanza:publish(User, ItemId, item_content(), Id, Node);
                  false -> escalus_pubsub_stanza:publish(User, Id, Node)
              end,
    send_request_and_receive_response(User, Request, Id, Options).

request_all_items(User, {_, NodeName} = Node, Options) ->
    Id = id(User, Node, <<"items">>),
    Request = escalus_pubsub_stanza:request_all_items(User, Id, Node),
    send_request_and_receive_response(
      User, Request, Id, Options,
      fun(Response, ExpectedResult) ->
              Items = exml_query:path(Response, [{element, <<"pubsub">>},
                                                 {element, <<"items">>}]),
              check_items(Items, ExpectedResult, NodeName, true)
      end).

purge_all_items(User, Node, Options) ->
    Id = id(User, Node, <<"purge">>),
    Request = escalus_pubsub_stanza:purge_all_items(User, Id, Node),
    send_request_and_receive_response(User, Request, Id, Options).

retrieve_user_subscriptions(User, NodeAddr, Options) ->
    Id = id(User, {NodeAddr, <<>>}, <<"user_subscriptions">>),
    Request = escalus_pubsub_stanza:retrieve_user_subscriptions(User, Id, NodeAddr),
    send_request_and_receive_response(
      User, Request, Id, Options,
      fun(Response, ExpectedResult) ->
              check_user_subscriptions_response(User, Response, ExpectedResult)
      end).

retrieve_node_subscriptions(User, Node, Options) ->
    Id = id(User, Node, <<"node_subscriptions">>),
    Request = escalus_pubsub_stanza:retrieve_node_subscriptions(User, Id, Node),
    send_request_and_receive_response(
      User, Request, Id, Options,
      fun(Response, ExpectedResult) ->
              check_node_subscriptions_response(Response, ExpectedResult, Node)
      end).

modify_node_subscriptions(User, ModifiedSubscriptions, Node, Options) ->
    Id = id(User, Node, <<"modify_node_subs">>),
    Subs = fill_subscriptions_jids(ModifiedSubscriptions),
    Request = escalus_pubsub_stanza:set_subscriptions(User, Id, Subs, Node),
    send_request_and_receive_response(User, Request, Id, Options).

discover_nodes(User, {NodeAddr, NodeName}, Options) ->
    %% discover child nodes
    Id = id(User, {NodeAddr, NodeName}, <<"disco_children">>),
    Request = escalus_pubsub_stanza:discover_nodes(User, Id, {NodeAddr, NodeName}),
    send_request_and_receive_response(
      User, Request, Id, Options,
      fun(Response, ExpectedResult) ->
              check_node_discovery_response(Response, {NodeAddr, NodeName}, ExpectedResult)
      end);
discover_nodes(User, NodeAddr, Options) ->
    %% discover top-level nodes
    Id = id(User, {NodeAddr, <<>>}, <<"disco_nodes">>),
    Request = escalus_pubsub_stanza:discover_nodes(User, Id, NodeAddr),
    send_request_and_receive_response(
      User, Request, Id, Options,
      fun(Response, ExpectedResult) ->
              check_node_discovery_response(Response, {NodeAddr, undefined}, ExpectedResult)
      end).

%% Receive notification or response

receive_item_notification(User, ItemId, {NodeAddr, NodeName}, Options) ->
    Stanza = receive_notification(User, NodeAddr, Options),
    check_item_notification(Stanza, ItemId, {NodeAddr, NodeName}, Options).

receive_subscription_notification(User, Subscription, {NodeAddr, NodeName}, Options) ->
    Stanza = receive_notification(User, NodeAddr, Options),
    check_subscription_notification(User, Stanza, Subscription, NodeName, Options).

receive_node_creation_notification(User, {NodeAddr, NodeName}, Options) ->
    Stanza = receive_notification(User, NodeAddr, Options),
    check_node_creation_notification(Stanza, NodeName).

receive_subscribe_response(User, Node, Options) ->
    Id = id(User, Node, <<"subscribe">>),
    Stanza = receive_response(User, Id, Options),
    check_subscription_response(Stanza, User, Node, Options).

receive_unsubscribe_response(User, Node, Options) ->
    Id = id(User, Node, <<"unsubscribe">>),
    Stanza = receive_response(User, Id, Options),
    check_response(Stanza, Id),
    Stanza.

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------

check_subscription_response(Response, User, {_, NodeName}, Options) ->
    Jid = jid(User, proplists:get_value(jid_type, Options, full)),
    Subscription = exml_query:path(Response, [{element, <<"pubsub">>},
                                              {element, <<"subscription">>}]),
    check_subscription(Subscription, Jid, NodeName),
    Response.

check_user_subscriptions_response(User, Response, ExpectedSubscriptions) ->
    SubscriptionElems = exml_query:paths(Response, [{element, <<"pubsub">>},
                                                    {element, <<"subscriptions">>},
                                                    {element, <<"subscription">>}]),
    Jid = escalus_utils:get_jid(User),
    [assert_ljid_equal(Jid, exml_query:attr(Subscr, <<"jid">>)) || Subscr <- SubscriptionElems],
    Subscriptions = [{exml_query:attr(Subscr, <<"node">>),
                      exml_query:attr(Subscr, <<"subscription">>)} || Subscr <- SubscriptionElems],
    ExpectedSorted = lists:sort(ExpectedSubscriptions),
    ExpectedSorted = lists:sort(Subscriptions),
    Response.

check_node_subscriptions_response(Response, ExpectedSubscriptions, {_, NodeName}) ->
    SubscriptionsElem = exml_query:path(Response, [{element, <<"pubsub">>},
                                                   {element, <<"subscriptions">>}]),
    NodeName = exml_query:attr(SubscriptionsElem, <<"node">>),
    SubscriptionElems = exml_query:subelements(SubscriptionsElem, <<"subscription">>),
    Subscriptions = [{exml_query:attr(Subscr, <<"jid">>),
                      exml_query:attr(Subscr, <<"subscription">>)} || Subscr <- SubscriptionElems],
    SubsWithLJids = convert_subscriptions_to_ljids(fill_subscriptions_jids(ExpectedSubscriptions)),
    SubsWithLJids = convert_subscriptions_to_ljids(lists:sort(Subscriptions)).

check_node_discovery_response(Response, {NodeAddr, NodeName}, ExpectedNodes) ->
    Query = exml_query:subelement(Response, <<"query">>),
    NodeName = exml_query:attr(Query, <<"node">>),
    Items = exml_query:subelements(Query, <<"item">>),
    [NodeAddr = exml_query:attr(Item, <<"jid">>) || Item <- Items],
    ReceivedNodes = [exml_query:attr(Item, <<"node">>) || Item <- Items],
    ReceivedSet = ordsets:from_list(ReceivedNodes),
    case ExpectedNodes of
        {no, NoNodeName} ->
            false = ordsets:is_element(NoNodeName, ReceivedSet);
        _ ->
            ExpectedSet = ordsets:from_list(ExpectedNodes),
            true = ordsets:is_subset(ExpectedSet, ReceivedSet)
    end,

    Response.

check_subscription_notification(User, Response, Subscription, NodeName, Options) ->
    SubscriptionElem = exml_query:path(Response, [{element, <<"pubsub">>},
                                                {element, <<"subscription">>}]),
    Jid = jid(User, proplists:get_value(jid_type, Options, full)),
    assert_ljid_equal(Jid, exml_query:attr(SubscriptionElem, <<"jid">>)),
    Subscription = exml_query:attr(SubscriptionElem, <<"subscription">>),
    NodeName = exml_query:attr(SubscriptionElem, <<"node">>),
    Response.

check_node_creation_notification(Response, NodeName) ->
    NodeName = exml_query:path(Response, [{element, <<"event">>},
                                          {element, <<"create">>},
                                          {attr, <<"node">>}]),
    Response.

check_item_notification(Response, ItemId, {NodeAddr, NodeName}, Options) ->
    check_notification(Response, NodeAddr),
    true = escalus_pred:has_type(<<"headline">>, Response),
    Items = exml_query:path(Response, [{element, <<"event">>},
                                       {element, <<"items">>}]),
    check_items(Items, [ItemId], NodeName, proplists:get_value(with_payload, Options, true)),
    Response.

send_request_and_receive_response(User, Request, Id, Options) ->
    send_request_and_receive_response(User, Request, Id, Options, fun(R) -> R end).

send_request_and_receive_response(User, Request, Id, Options, CheckResponseF) ->
    escalus:send(User, Request),
    case {proplists:get_value(receive_response, Options, true),
          proplists:get_value(expected_error_type, Options, none)} of
        {false, _} ->
            ok;
        {true, none} ->
            receive_and_check_response(User, Id, Options, CheckResponseF);
        {true, ExpectedErrorType} ->
            receive_error_response(User, Id, ExpectedErrorType, Options)
    end.

receive_and_check_response(User, Id, Options, CheckF) ->
    Response = receive_response(User, Id, Options),
    case proplists:get_value(expected_result, Options) of
        undefined -> Response;
        true -> CheckF(Response);
        ExpectedResult -> CheckF(Response, ExpectedResult)
    end.

receive_response(User, Id, Options) ->
    Stanza = receive_stanza(User, Options),
    check_response(Stanza, Id),
    Stanza.

check_response(Stanza, Id) ->
    true = escalus_pred:is_iq_result(Stanza),
    Id = exml_query:attr(Stanza, <<"id">>),
    Stanza.

receive_error_response(User, Id, Type, Options) ->
    ErrorStanza = receive_stanza(User, Options),
    true = escalus_pred:is_iq_error(ErrorStanza),
    Id = exml_query:attr(ErrorStanza, <<"id">>),
    ErrorElem = exml_query:subelement(ErrorStanza, <<"error">>),
    Type = exml_query:attr(ErrorElem, <<"type">>),
    ErrorStanza.

receive_notification(User, NodeAddr, Options) ->
    Stanza = receive_stanza(User, Options),
    check_notification(Stanza, NodeAddr),
    Stanza.

check_notification(Stanza, NodeAddr) ->
    true = escalus_pred:is_stanza_from(NodeAddr, Stanza),
    true = escalus_pred:is_message(Stanza),
    Stanza.

receive_stanza(User, Options) ->
    case proplists:get_value(stanza, Options) of
        undefined ->
            case proplists:get_value(response_timeout, Options) of
                undefined -> escalus:wait_for_stanza(User);
                Timeout -> escalus:wait_for_stanza(User, Timeout)
            end;
        Stanza ->
            Stanza
    end.

check_subscription(Subscr, Jid, NodeName) ->
    assert_ljid_equal(Jid, exml_query:attr(Subscr, <<"jid">>)),
    NodeName = exml_query:attr(Subscr, <<"node">>),
    true = exml_query:attr(Subscr, <<"subid">>) =/= undefined,
    <<"subscribed">> = exml_query:attr(Subscr, <<"subscription">>).

check_items(ReceivedItemsElem, ExpectedItemIds, NodeName, WithPayload) ->
    NodeName = exml_query:attr(ReceivedItemsElem, <<"node">>),
    ReceivedItems = exml_query:subelements(ReceivedItemsElem, <<"item">>),
    [check_item(ExpectedItemId, WithPayload, ReceivedItem) ||
        {ReceivedItem, ExpectedItemId} <- lists:zip(ReceivedItems, ExpectedItemIds)].

check_item(ExpectedItemId, WithPayload, ReceivedItem) ->
    ExpectedItemId = exml_query:attr(ReceivedItem, <<"id">>),
    Content = item_content(WithPayload),
    Content = exml_query:subelement(ReceivedItem, <<"entry">>).

item_content(false) -> undefined;
item_content(true) -> item_content().

convert_subscriptions_to_ljids(Subscriptions) ->
    [{escalus_utils:jid_to_lower(Jid), Sub} || {Jid, Sub} <- Subscriptions].

fill_subscriptions_jids(Subscriptions) ->
    [{jid(User, JidType), Subscr} || {User, JidType, Subscr} <- Subscriptions].

jid(User, full) -> escalus_utils:get_jid(User);
jid(User, bare) -> escalus_utils:get_short_jid(User).

assert_ljid_equal(ActualJid, ExpectedJid) ->
    LJid = escalus_utils:jid_to_lower(ExpectedJid),
    LJid = escalus_utils:jid_to_lower(ActualJid).

id(User, {NodeAddr, NodeName}, Suffix) ->
    UserName = escalus_utils:get_username(User),
    list_to_binary(io_lib:format("~s-~s-~s-~s", [UserName, NodeAddr, NodeName, Suffix])).

item_content() ->
    #xmlel{name = <<"entry">>,
           attrs = [{<<"xmlns">>, <<"http://www.w3.org/2005/Atom">>}]}.
