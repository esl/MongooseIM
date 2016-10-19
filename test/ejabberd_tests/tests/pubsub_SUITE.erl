%%%===================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd.
%%% @doc Suite for testing pubsub features as described in XEP-0060
%%% @end
%%%===================================================================

-module(pubsub_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").

-import(pubsub_tools,
        [create_node/3,
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
         discover_nodes/3,
         receive_item_notification/4,
         receive_subscription_notification/4,
         receive_node_creation_notification/3,
         receive_subscribe_response/3]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [
          {group, pubsub_tests},
          {group, node_config_tests},
          {group, manage_subscriptions_tests},
          {group, collection_tests},
          {group, collection_config_tests}
         ].

groups() -> [{pubsub_tests, [parallel],
              [
               create_delete_node_test,
               discover_nodes_test,
               subscribe_unsubscribe_test,
               publish_test,
               notify_test,
               request_all_items_test,
               purge_all_items_test,
               retrieve_subscriptions_test
              ]
             },
             {node_config_tests, [parallel],
              [
               disable_notifications_test,
               disable_payload_test,
               disable_persist_items_test,
               notify_only_available_users_test,
               send_last_published_item_test
              ]
             },
             {manage_subscriptions_tests, [parallel],
              [
               retrieve_node_subscriptions_test,
               modify_node_subscriptions_test
              ]
             },
             {collection_tests, [parallel],
              [
               create_delete_collection_test,
               subscribe_unsubscribe_collection_test,
               create_delete_leaf_test,
               notify_collection_test,
               notify_collection_leaf_and_item_test,
               notify_collection_bare_jid_test,
               notify_collection_and_leaf_test,
               notify_collection_and_leaf_same_user_test,
               retrieve_subscriptions_collection_test,
               discover_top_level_nodes_test,
               discover_child_nodes_test,
               request_all_items_leaf_test
              ]
             },
             {collection_config_tests, [parallel],
              [
               disable_notifications_leaf_test,
               disable_payload_leaf_test,
               disable_persist_items_leaf_test
              ]
             }
            ].

suite() ->
    escalus:suite().

domain() ->
    ct:get_config({hosts, mim, domain}).

node_addr() ->
    Domain = domain(),
    <<"pubsub.", Domain/binary>>.

rand_name(Prefix) ->
    Suffix = base64:encode(crypto:rand_bytes(5)),
    <<Prefix/binary, "_", Suffix/binary>>.

pubsub_node_name() ->
    rand_name(<<"princely_musings">>).

pubsub_node() ->
    {node_addr(), pubsub_node_name()}.

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config2 = dynamic_modules:save_modules(domain(), Config),
    dynamic_modules:ensure_modules(domain(), required_modules()),
    escalus:init_per_suite(Config2).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(domain(), Config),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestName, Config) ->
    escalus:init_per_testcase(_TestName, Config).

end_per_testcase(_TestName, Config) ->
    escalus:end_per_testcase(_TestName, Config).

%%--------------------------------------------------------------------
%% Test cases for XEP-0060
%% Comments in test cases refer to sections is the XEP
%%--------------------------------------------------------------------

create_delete_node_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              %% Request:  8.1.2 Ex.132 create node with (default) open access model
              %% Response:       Ex.134 success
              %%                        Note: contains node ID although XEP does not require this
              Node = pubsub_node(),
              create_node(Alice, Node, []),

              %% Request:  8.4.1 Ex.155 owner deletes a node
              %% Response:       Ex.157 success
              delete_node(Alice, Node, [])
      end).

discover_nodes_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% Request:  5.2 Ex.9  Entity asks service for all first-level nodes
              %% Response:     Ex.10 Service returns all first-level nodes
              %% it shouldn't contain the Node which will be created in a moment
              {_, NodeName} = Node = pubsub_node(),
              discover_nodes(Bob, node_addr(), [{expected_result, {no, NodeName}}]),

              create_node(Alice, Node, []),
              discover_nodes(Bob, node_addr(), [{expected_result, [NodeName]}]),

              {_, NodeName2} = Node2 = pubsub_node(),
              create_node(Alice, Node2, []),
              discover_nodes(Bob, node_addr(), [{expected_result, [NodeName, NodeName2]}]),

              delete_node(Alice, Node, []),
              delete_node(Alice, Node2, [])
      end).

subscribe_unsubscribe_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              create_node(Alice, Node, []),

              %% Request:  6.1.1 Ex.32 entity subscribes to a node
              %% Response: 6.1.2 Ex.33 success (with subscription ID)
              subscribe(Bob, Node, []),

              %% Request:  6.2.1 Ex.51 unsubscribe from a node
              %% Response: 6.2.2 Ex.52 success
              unsubscribe(Bob, Node, []),

              %% Check subscriptions without resources
              subscribe(Bob, Node, [{jid_type, bare}]),
              unsubscribe(Bob, Node, [{jid_type, bare}]),

              delete_node(Alice, Node, [])
      end).

publish_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              %% Auto-create enabled by default

              %% Request:  7.1.1 Ex.99  publish an item with an ItemID
              %% Response: 7.1.2 Ex.100 success
              Node = pubsub_node(),
              publish(Alice, <<"item1">>, Node, []),

              delete_node(Alice, Node, [])
      end).

notify_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 2}, {geralt, 2}],
      fun(Alice, Bob1, Bob2, Geralt1, Geralt2) ->
              Node = pubsub_node(),
              create_node(Alice, Node, []),
              subscribe(Bob1, Node, []),
              subscribe(Geralt1, Node, [{jid_type, bare}]),
              publish(Alice, <<"item1">>, Node, []),

              %% 7.1.2.1 Ex.101 notification with payload
              %%                Note: message has type 'headline' by default

              %% Bob subscribed with resource
              receive_item_notification(Bob1, <<"item1">>, Node, []),
              escalus_assert:has_no_stanzas(Bob2),

              %% Geralt subscribed without resource
              receive_item_notification(Geralt1, <<"item1">>, Node, []),
              receive_item_notification(Geralt2, <<"item1">>, Node, []),

              delete_node(Alice, Node, [])
      end).

request_all_items_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              create_node(Alice, Node, []),
              publish(Alice, <<"item1">>, Node, []),
              publish(Alice, <<"item2">>, Node, []),

              %% Request:  6.5.2 Ex.78 subscriber requests all items
              %% Response: 6.5.3 Ex.79 service returns all items
              request_all_items(Bob, Node, [{expected_result, [<<"item2">>, <<"item1">>]}]),
              %% TODO check ordering (although XEP does not specify this)

              delete_node(Alice, Node, [])
      end).

purge_all_items_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              create_node(Alice, Node, []),
              publish(Alice, <<"item1">>, Node, []),
              publish(Alice, <<"item2">>, Node, []),

              %% Response: 8.5.3.2 Ex.165 insufficient privileges
              purge_all_items(Bob, Node, [{expected_error_type, <<"auth">>}]),

              request_all_items(Bob, Node, [{expected_result, [<<"item2">>, <<"item1">>]}]),

              %% Request:  8.5.1 Ex.161 owner purges all items from node
              %% Response: 8.5.2 Ex.162 success
              purge_all_items(Alice, Node, []),

              request_all_items(Bob, Node, [{expected_result, []}]),

              delete_node(Alice, Node, [])
      end).

retrieve_subscriptions_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% Request:  5.6 Ex.20 Retrieve Subscriptions
              %% Response:     Ex.22 No Subscriptions
              retrieve_user_subscriptions(Bob, node_addr(), [{expected_result, []}]),

              {_, NodeName} = {_, NodeName} = Node = pubsub_node(),
              create_node(Alice, Node, []),
              subscribe(Bob, Node, []),

              %% Ex. 21 Service returns subscriptions
              Sub = [{NodeName, <<"subscribed">>}],
              retrieve_user_subscriptions(Bob, node_addr(), [{expected_result, Sub}]),

              {_, NodeName2} = Node2 = pubsub_node(),
              create_node(Alice, Node2, []),
              subscribe(Bob, Node2, []),

              %% Ex. 21 Service returns subscriptions
              Subs = [{NodeName, <<"subscribed">>}, {NodeName2, <<"subscribed">>}],
              retrieve_user_subscriptions(Bob, node_addr(), [{expected_result, Subs}]),

              %% Owner not subscribed automatically
              retrieve_user_subscriptions(Alice, node_addr(), [{expected_result, []}]),

              delete_node(Alice, Node, []),
              delete_node(Alice, Node2, [])
      end).

disable_notifications_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              NodeConfig = [{<<"pubsub#deliver_notifications">>, <<"false">>}],
              Node = pubsub_node(),
              create_node(Alice, Node, [{config, NodeConfig}]),

              subscribe(Bob, Node, []),
              publish(Alice, <<"item1">>, Node, []),

              %% Notifications disabled
              escalus_assert:has_no_stanzas(Bob),

              delete_node(Alice, Node, [])
      end).

disable_payload_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% Notification-Only Persistent Node, see 4.3, table 4
              NodeConfig = [{<<"pubsub#deliver_payloads">>, <<"false">>}],
              Node = pubsub_node(),
              create_node(Alice, Node, [{config, NodeConfig}]),

              subscribe(Bob, Node, []),
              publish(Alice, <<"item1">>, Node, []),

              %% Payloads disabled
              receive_item_notification(Bob, <<"item1">>, Node, [{with_payload, false}]),

              delete_node(Alice, Node, [])
      end).

disable_persist_items_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% Payload-Included Transient Node, see 4.3, table 4
              NodeConfig = [{<<"pubsub#persist_items">>, <<"false">>}],
              Node = pubsub_node(),
              create_node(Alice, Node, [{config, NodeConfig}]),

              subscribe(Bob, Node, []),
              publish(Alice, <<"item1">>, Node, []),

              %% Notifications should work
              receive_item_notification(Bob, <<"item1">>, Node, []),

              %% No items should be stored
              request_all_items(Bob, Node, [{expected_result, []}]),

              delete_node(Alice, Node, [])
      end).

notify_only_available_users_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% Second node notifies only available users
              Node = pubsub_node(),
              create_node(Alice, Node, []),
              NodeConfig = [{<<"pubsub#presence_based_delivery">>, <<"true">>}],
              Node2 = pubsub_node(),
              create_node(Alice, Node2, [{config, NodeConfig}]),

              subscribe(Bob, Node, [{jid_type, bare}]),
              subscribe(Bob, Node2, [{jid_type, bare}]),

              escalus:send(Bob, escalus_stanza:presence(<<"unavailable">>)),

              %% Receive item from node 1 (also make sure the presence is processed)
              publish(Alice, <<"item1">>, Node, []),
              receive_item_notification(Bob, <<"item1">>, Node, []),

              %% Item from node 2 not received (blocked by resource-based delivery)
              publish(Alice, <<"item2">>, Node2, []),
              escalus_assert:has_no_stanzas(Bob),

              delete_node(Alice, Node, [])
      end).

send_last_published_item_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% Request:  8.1.3 Ex.136 Request a new node with non-default configuration
              %% Response:       Ex.137 Service replies with success
              NodeConfig = [{<<"pubsub#send_last_published_item">>, <<"on_sub_and_presence">>}],
              Node = pubsub_node(),
              create_node(Alice, Node, [{config, NodeConfig}]),

              publish(Alice, <<"item1">>, Node, []),
              publish(Alice, <<"item2">>, Node, []),

              %% Note: when Bob subscribes, the last item (item2) is sent to him
              %%       6.1.7 Ex.50 service sends last published item
              %%       This is sent BEFORE the response iq stanza
              subscribe(Bob, Node, [{receive_response, false}]),
              receive_item_notification(Bob, <<"item2">>, Node, []),
              receive_subscribe_response(Bob, Node, []),

              delete_node(Alice, Node, [])
      end).

retrieve_node_subscriptions_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}, {geralt, 1}],
      fun(Alice, Bob, Geralt) ->
              Node = pubsub_node(),
              create_node(Alice, Node, []),

              %% Request:  8.8.1.1 Ex.182 Owner requests all subscriptions
              %% Response: 8.8.1.2 Ex.183 Service returns list of subscriptions (empty yet)
              retrieve_node_subscriptions(Alice, Node, [{expected_result, []}]),

              %% Response: 8.8.1.3 Ex.185 Entity is not an owner
              retrieve_node_subscriptions(Bob, Node, [{expected_error_type, <<"auth">>}]),

              subscribe(Bob, Node, []),
              subscribe(Geralt, Node, [{jid_type, bare}]),

              NodeSubs = [{Bob, full, <<"subscribed">>}, {Geralt, bare, <<"subscribed">>}],
              retrieve_node_subscriptions(Alice, Node, [{expected_result, NodeSubs}]),

              delete_node(Alice, Node, [])
      end).

modify_node_subscriptions_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}, {geralt, 1}],
      fun(Alice, Bob, Geralt) ->
              Node = pubsub_node(),
              create_node(Alice, Node, []),

              %% Request:  8.8.2.1 Ex.187 Owner modifies subscriptions
              %% Response: 8.8.2.2 Ex.183 Service responds with success
              modify_node_subscriptions(Alice, [{Bob, full, <<"subscribed">>},
                                                {Geralt, bare, <<"subscribed">>}], Node, []),

              %% 8.8.4 Ex.194 Notify subscribers
              receive_subscription_notification(Bob, <<"subscribed">>, Node, []),
              receive_subscription_notification(Geralt, <<"subscribed">>, Node, [{jid_type, bare}]),

              Subs = [{Bob, full, <<"subscribed">>}, {Geralt, bare, <<"subscribed">>}],
              retrieve_node_subscriptions(Alice, Node, [{expected_result, Subs}]),

              %% Response: 8.8.2.3 Ex.190 Entity is not an owner
              modify_node_subscriptions(Bob, [{Geralt, full, <<"subscribed">>}], Node,
                                        [{expected_error_type, <<"auth">>}]),

              %% Remove Bob, add Geralt's full JID
              modify_node_subscriptions(Alice, [{Bob, full, <<"none">>},
                                                {Geralt, full, <<"subscribed">>}], Node, []),

              receive_subscription_notification(Bob, <<"none">>, Node, []),
              receive_subscription_notification(Geralt, <<"subscribed">>, Node, []),

              ModSubs = [{Geralt, bare, <<"subscribed">>}, {Geralt, full, <<"subscribed">>}],
              retrieve_node_subscriptions(Alice, Node, [{expected_result, ModSubs}]),

              delete_node(Alice, Node, [])
      end).

%%--------------------------------------------------------------------
%% Test cases for XEP-0248
%% Comments in test cases refer to sections is the XEP
%%--------------------------------------------------------------------

pubsub_leaf_name() -> rand_name(<<"leaf">>).
pubsub_leaf() -> {node_addr(), pubsub_leaf_name()}.

create_delete_collection_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              %% Request:  7.1.1 Ex.18 create collection node
              %% Response:       Ex.19 success
              %%                        Note: contains node ID although XEP does not require this
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              Node = pubsub_node(),
              create_node(Alice, Node, [{config, CollectionConfig}]),

              %% Request:  7.3.1 Ex.30 delete collection node
              %% Response: 7.3.2 Ex.31 success
              delete_node(Alice, Node, [])
      end).

subscribe_unsubscribe_collection_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              Node = pubsub_node(),
              create_node(Alice, Node, [{config, CollectionConfig}]),

              %% Request:  6.1.1 Ex.10 subscribe (no configuration)
              %% Response: 6.1.2 Ex.12 success
              subscribe(Bob, Node, []),

              %% Same as XEP-0060
              unsubscribe(Bob, Node, []),

              delete_node(Alice, Node, [])
      end).

create_delete_leaf_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              create_node(Alice, Node, [{config, CollectionConfig}]),

              %% XEP-0060, 8.1.2, see 16.4.4 for config details
              NodeConfig = [{<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              create_node(Alice, Leaf, [{config, NodeConfig}]),

              delete_node(Alice, Leaf, []),
              delete_node(Alice, Node, [])
      end).

notify_collection_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              create_node(Alice, Node, [{config, CollectionConfig}]),

              NodeConfig = [{<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              create_node(Alice, Leaf, [{config, NodeConfig}]),
              Leaf2 = pubsub_leaf(),
              create_node(Alice, Leaf2, [{config, NodeConfig}]),
              subscribe(Bob, Node, []),

              %% Publish to leaf nodes, Bob should get notifications
              %% 5.3.1.1 Ex.5 Subscriber receives a publish notification from a collection
              publish(Alice, <<"item1">>, Leaf, []),
              receive_item_notification(Bob, <<"item1">>, Leaf, []),
              publish(Alice, <<"item2">>, Leaf2, []),
              receive_item_notification(Bob, <<"item2">>, Leaf2, []),

              delete_node(Alice, Leaf, []),
              delete_node(Alice, Leaf2, []),
              delete_node(Alice, Node, [])
      end).

notify_collection_leaf_and_item_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              create_node(Alice, Node, [{config, CollectionConfig}]),

              %% Subscribe before creating the leaf node
              subscribe(Bob, Node, []),
              NodeConfig = [{<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              create_node(Alice, Leaf, [{config, NodeConfig}]),

              %% Bob should get a notification for the leaf node creation
              %% 5.3.1.2 Ex.6 Subscriber receives a creation notification from a collection
              receive_node_creation_notification(Bob, Leaf, []),

              %% Publish to leaf node, Bob should get notified
              publish(Alice, <<"item1">>, Leaf, []),
              receive_item_notification(Bob, <<"item1">>, Leaf, []),

              delete_node(Alice, Leaf, []),
              delete_node(Alice, Node, [])
      end).

notify_collection_bare_jid_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 2}, {geralt, 2}],
      fun(Alice, Bob1, Bob2, Geralt1, Geralt2) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              create_node(Alice, Node, [{config, CollectionConfig}]),

              NodeConfig = [{<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              create_node(Alice, Leaf, [{config, NodeConfig}]),
              subscribe(Bob1, Node, []),
              subscribe(Geralt1, Node, [{jid_type, bare}]),
              publish(Alice, <<"item1">>, Leaf, []),

              %% Bob subscribed with resource
              receive_item_notification(Bob1, <<"item1">>, Leaf, []),
              escalus_assert:has_no_stanzas(Bob2),

              %% Geralt subscribed without resource
              receive_item_notification(Geralt1, <<"item1">>, Leaf, []),
              receive_item_notification(Geralt2, <<"item1">>, Leaf, []),

              delete_node(Alice, Leaf, []),
              delete_node(Alice, Node, [])
      end).

notify_collection_and_leaf_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}, {geralt, 1}],
      fun(Alice, Bob, Geralt) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              create_node(Alice, Node, [{config, CollectionConfig}]),

              NodeConfig = [{<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              create_node(Alice, Leaf, [{config, NodeConfig}]),
              subscribe(Bob, Node, []),
              subscribe(Geralt, Leaf, []),

              %% Publish to leaf nodes, Bob and Geralt should get notifications
              publish(Alice, <<"item1">>, Leaf, []),
              receive_item_notification(Bob, <<"item1">>, Leaf, []),
              receive_item_notification(Geralt, <<"item1">>, Leaf, []),

              delete_node(Alice, Leaf, []),
              delete_node(Alice, Node, [])
      end).

notify_collection_and_leaf_same_user_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              create_node(Alice, Node, [{config, CollectionConfig}]),

              NodeConfig = [{<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              create_node(Alice, Leaf, [{config, NodeConfig}]),
              subscribe(Bob, Node, []),
              subscribe(Bob, Leaf, []),

              %% Bob should get only one notification
              publish(Alice, <<"item1">>, Leaf, []),
              receive_item_notification(Bob, <<"item1">>, Leaf, []),
              escalus_assert:has_no_stanzas(Bob),

              delete_node(Alice, Leaf, []),
              delete_node(Alice, Node, [])
      end).

retrieve_subscriptions_collection_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              create_node(Alice, Node, [{config, CollectionConfig}]),

              NodeConfig = [{<<"pubsub#collection">>, NodeName}],
              {_, LeafName} = Leaf = pubsub_leaf(),
              create_node(Alice, Leaf, [{config, NodeConfig}]),
              Leaf2 = pubsub_leaf(),
              create_node(Alice, Leaf2, [{config, NodeConfig}]),
              subscribe(Bob, Node, []),
              subscribe(Bob, Leaf, []),

              % Only the nodes for which subscriptions were made should be returned
              Subs = [{LeafName, <<"subscribed">>}, {NodeName, <<"subscribed">>}],
              retrieve_user_subscriptions(Bob, node_addr(), [{expected_result, Subs}]),

              delete_node(Alice, Leaf, []),
              delete_node(Alice, Leaf2, []),
              delete_node(Alice, Node, [])
      end).

discover_top_level_nodes_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              create_node(Alice, Node, [{config, CollectionConfig}]),

              NodeConfig = [{<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              create_node(Alice, Leaf, [{config, NodeConfig}]),

              %% Discover top-level nodes, only the collection expected
              discover_nodes(Bob, node_addr(), [{expected_result, [NodeName]}]),

              delete_node(Alice, Leaf, []),
              delete_node(Alice, Node, [])
      end).

discover_child_nodes_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% Try to get children of a non-existing node
              {_, NodeName} = Node = pubsub_node(),
              discover_nodes(Bob, Node, [{expected_error_type, <<"cancel">>}]),

              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              create_node(Alice, Node, [{config, CollectionConfig}]),

              discover_nodes(Bob, Node, [{expected_result, {no, NodeName}}]),

              NodeConfig = [{<<"pubsub#collection">>, NodeName}],
              {_, LeafName} = Leaf = pubsub_leaf(),
              create_node(Alice, Leaf, [{config, NodeConfig}]),
              {_, LeafName2} = Leaf2 = pubsub_leaf(),
              create_node(Alice, Leaf2, [{config, NodeConfig}]),

              %% Request:  5.2.1 Ex.11 Entity requests child nodes
              %% Response: 5.2.2 Ex.12 Service returns child nodes
              discover_nodes(Bob, Node, [{expected_result, [LeafName, LeafName2]}]),

              delete_node(Alice, Leaf, []),
              delete_node(Alice, Leaf2, []),
              delete_node(Alice, Node, [])
      end).

request_all_items_leaf_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              create_node(Alice, Node, [{config, CollectionConfig}]),

              NodeConfig = [{<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              create_node(Alice, Leaf, [{config, NodeConfig}]),
              Leaf2 = pubsub_leaf(),
              create_node(Alice, Leaf2, [{config, NodeConfig}]),

              publish(Alice, <<"item1">>, Leaf, []),
              publish(Alice, <<"item2">>, Leaf2, []),

              %% Request items from leaf nodes - as described in XEP-0060
              request_all_items(Bob, Leaf, [{expected_result, [<<"item1">>]}]),
              request_all_items(Bob, Leaf2, [{expected_result, [<<"item2">>]}]),

              %% NOTE: This is not implemented yet
              %% Request:  6.2.1 Ex.15 Subscriber requests all items on a collection
              %% Response: 6.2.2 Ex.16 Service returns items on leaf nodes
              %%request_all_items(Bob, Node, [{expected_result, [<<"item2">>, <<"item1">>]}]),

              delete_node(Alice, Leaf, []),
              delete_node(Alice, Leaf2, []),
              delete_node(Alice, Node, [])
      end).

disable_notifications_leaf_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              create_node(Alice, Node, [{config, CollectionConfig}]),

              NodeConfig = [{<<"pubsub#deliver_notifications">>, <<"false">>},
                            {<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              create_node(Alice, Leaf, [{config, NodeConfig}]),

              subscribe(Bob, Node, []),
              publish(Alice, <<"item1">>, Leaf, []),

              %% Notifications disabled
              escalus_assert:has_no_stanzas(Bob),

              delete_node(Alice, Leaf, []),
              delete_node(Alice, Node, [])
      end).

disable_payload_leaf_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              create_node(Alice, Node, [{config, CollectionConfig}]),

              NodeConfig = [{<<"pubsub#deliver_payloads">>, <<"false">>},
                            {<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              create_node(Alice, Leaf, [{config, NodeConfig}]),

              subscribe(Bob, Node, []),
              publish(Alice, <<"item1">>, Leaf, []),

              %% Payloads disabled
              receive_item_notification(Bob, <<"item1">>, Leaf, [{with_payload, false}]),

              delete_node(Alice, Leaf, []),
              delete_node(Alice, Node, [])
      end).

disable_persist_items_leaf_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              create_node(Alice, Node, [{config, CollectionConfig}]),

              NodeConfig = [{<<"pubsub#persist_items">>, <<"false">>},
                            {<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              create_node(Alice, Leaf, [{config, NodeConfig}]),

              subscribe(Bob, Node, []),
              publish(Alice, <<"item1">>, Leaf, []),

              %% Notifications should work
              receive_item_notification(Bob, <<"item1">>, Leaf, []),

              %% No items should be stored
              request_all_items(Bob, Leaf, [{expected_result, []}]),

              delete_node(Alice, Leaf, []),
              delete_node(Alice, Node, [])
      end).

%%--------------------------------------------------------------------
%% Tests for unsupported features  - excluded from suite
%%--------------------------------------------------------------------

disable_payload_and_persist_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% Notification-Only Transient Node, see 4.3, table 4
              NodeConfig = [{<<"pubsub#deliver_payloads">>, <<"false">>},
                            {<<"pubsub#persist_items">>, <<"false">>}],
              Node = pubsub_node(),
              create_node(Alice, Node, [{config, NodeConfig}]),

              subscribe(Bob, Node, []),

              %% Response  7.1.3 Ex.112 attempt to publish payload to transient notification node
              %%                   Expected error of type 'modify'
              publish(Alice, <<"item1">>, Node, [{expected_error_type, <<"modify">>}]),

              %% Publish without payload should succeed
              publish(Alice, <<"item2">>, Node, [{with_payload, false}]),

              %% Notifications should work
              receive_item_notification(Bob, <<"item1">>, Node, []),

              %% No items should be stored
              request_all_items(Bob, Node, [{expected_result, []}]),

              %% No more notifications
              escalus_assert:has_no_stanzas(Bob),

              delete_node(Alice, Node, [])
      end).

disable_delivery_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              create_node(Alice, Node, []),

              %% Request: 6.3.7 Ex.71 Subscribe and configure
              %%                Ex.72 Success
              SubscrConfig = [{<<"pubsub#deliver">>, <<"false">>}],
              subscribe(Bob, Node, [{config, SubscrConfig}]),

              publish(Alice, <<"item1">>, Node, []),

              %% Notifications disabled
              escalus_assert:has_no_stanzas(Bob),

              delete_node(Alice, Node, [])
      end).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

required_modules() ->
    [{mod_pubsub, [
                   {plugins,[<<"dag">>]},
                   {nodetree,<<"dag">>}
                  ]}].
