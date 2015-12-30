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


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [
          {group, pubsub_tests},
          {group, config_tests},
          {group, collection_tests}
         ].

groups() -> [{pubsub_tests, [sequence],
              [
               create_delete_node_test,
               subscribe_unsubscribe_test,
               publish_test,
               notify_test,
               request_all_items_test,
               send_last_published_item_test,
               retrieve_subscriptions_test
              ]
             },
             {config_tests, [sequence],
              [
               disable_notifications_test,
               disable_payload_test,
               disable_persist_items_test
               %% Unsupported by ejabberd
               %% disable_payload_and_persist_test
              ]
             },
             {collection_tests, [sequence],
              [
               create_delete_collection_test,
               subscribe_unsubscribe_collection_test,
               create_delete_leaf_test,
               notify_collection_test,
               notify_collection_and_leaf_test,
               notify_collection_and_leaf_same_user_test,
               retrieve_subscriptions_collection_test,
               discover_child_nodes_test,
               request_all_items_leaf_test
              ]
             }
            ].

suite() ->
    escalus:suite().

-define(NODE_ADDR, <<"pubsub.localhost">>).
-define(NODE_NAME, <<"princely_musings">>).
-define(NODE, {?NODE_ADDR, ?NODE_NAME}).

-define(NODE_NAME_2, <<"subpub">>).
-define(NODE_2, {?NODE_ADDR, ?NODE_NAME_2}).

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config,{by_name, [alice, bob, geralt, carol]}),
    ok.

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config,{by_name, [alice, bob, geralt, carol]}),
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
    escalus:story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              %% Request:  8.1.2 Ex.132 create node with (default) open access model
              %% Response:       Ex.134 success
              %%                        Note: contains node ID although XEP does not require this
              pubsub_tools:create_node(Alice, ?NODE),

              %% Request:  8.4.1 Ex.155 owner deletes a node
              %% Response:       Ex.157 success
              pubsub_tools:delete_node(Alice, ?NODE)
      end).

subscribe_unsubscribe_test(Config) ->
    escalus:story(
      Config,
      [{alice,1}, {bob,1}],
      fun(Alice, Bob) ->
              pubsub_tools:create_node(Alice, ?NODE),

              %% Request:  6.1.1 Ex.32 entity subscribes to a node
              %% Response: 6.1.2 Ex.33 success (with subscription ID)
              pubsub_tools:subscribe(Bob, ?NODE),

              %% Request:  6.2.1 Ex.51 unsubscribe from a node
              %% Response: 6.2.2 Ex.52 success
              pubsub_tools:unsubscribe(Bob, ?NODE),

              pubsub_tools:delete_node(Alice, ?NODE)
      end).

publish_test(Config) ->
    escalus:story(
      Config,
      [{alice,1}],
      fun(Alice) ->
              %% Auto-create enabled by default

              %% Request:  7.1.1 Ex.99  publish an item with an ItemID
              %% Response: 7.1.2 Ex.100 success
              pubsub_tools:publish(Alice, <<"item1">>, ?NODE),

              pubsub_tools:delete_node(Alice, ?NODE)
      end).

notify_test(Config) ->
    escalus:story(
      Config,
      [{alice,1}, {bob,1}, {geralt,1}],
      fun(Alice, Bob, Geralt) ->
              pubsub_tools:create_node(Alice, ?NODE),
              pubsub_tools:subscribe(Bob, ?NODE),
              pubsub_tools:subscribe(Geralt, ?NODE),
              pubsub_tools:publish(Alice, <<"item1">>, ?NODE),

              %% 7.1.2.1 Ex.101 notification with payload
              %%                Note: message has type 'headline' by default
              pubsub_tools:receive_notification(Bob, <<"item1">>, ?NODE),
              pubsub_tools:receive_notification(Geralt, <<"item1">>, ?NODE),

              pubsub_tools:delete_node(Alice, ?NODE)
      end).

send_last_published_item_test(Config) ->
    escalus:story(
      Config,
      [{alice,1}, {bob,1}],
      fun(Alice, Bob) ->
              %% Request:  8.1.3 Ex.136 Request a new node with non-default configuration
              %% Response:       Ex.137 Service replies with success
              NodeConfig = [{<<"pubsub#send_last_published_item">>, <<"on_sub_and_presence">>}],
              pubsub_tools:create_node(Alice, ?NODE, NodeConfig),

              pubsub_tools:publish(Alice, <<"item1">>, ?NODE),

              %% Note: when Bob subscribes, the last item (item2) is sent to him
              %%       6.1.7 Ex.50 service sends last published item
              %%       This is sent BEFORE the response iq stanza
              pubsub_tools:subscribe(Bob, ?NODE, <<"item1">>),

              pubsub_tools:delete_node(Alice, ?NODE)
      end).

request_all_items_test(Config) ->
    escalus:story(
      Config,
      [{alice,1}, {bob,1}],
      fun(Alice, Bob) ->
              pubsub_tools:create_node(Alice, ?NODE),
              pubsub_tools:publish(Alice, <<"item1">>, ?NODE),
              pubsub_tools:publish(Alice, <<"item2">>, ?NODE),

              %% Request:  6.5.2 Ex.78 subscriber requests all items
              %% Response: 6.5.3 Ex.79 service returns all items
              pubsub_tools:request_all_items(Bob, [<<"item2">>, <<"item1">>], ?NODE),
              %% TODO check ordering (although XEP does not specify this)

              pubsub_tools:delete_node(Alice, ?NODE)
      end).

retrieve_subscriptions_test(Config) ->
    escalus:story(
      Config,
      [{alice,1}, {bob,1}],
      fun(Alice, Bob) ->
              %% Request:  5.6 Ex.20 Retrieve Subscriptions
              %% Response:     Ex.22 No Subscriptions
              pubsub_tools:retrieve_subscriptions(Bob, [], ?NODE_ADDR),

              pubsub_tools:create_node(Alice, ?NODE),
              pubsub_tools:subscribe(Bob, ?NODE),

              %% Ex. 21 Service returns subscriptions
              pubsub_tools:retrieve_subscriptions(Bob, [{?NODE_NAME, <<"subscribed">>}], ?NODE_ADDR),

              pubsub_tools:create_node(Alice, ?NODE_2),
              pubsub_tools:subscribe(Bob, ?NODE_2),

              %% Ex. 21 Service returns subscriptions
              pubsub_tools:retrieve_subscriptions(Bob, [{?NODE_NAME, <<"subscribed">>},
                                                        {?NODE_NAME_2, <<"subscribed">>}], ?NODE_ADDR),

              %% Owner not subscribed automatically
              pubsub_tools:retrieve_subscriptions(Alice, [], ?NODE_ADDR),

              pubsub_tools:delete_node(Alice, ?NODE),
              pubsub_tools:delete_node(Alice, ?NODE_2)
      end).

disable_notifications_config_test(Config) ->
    escalus:story(
      Config,
      [{alice,1}, {bob,1}],
      fun(Alice, Bob) ->
              NodeConfig = [{<<"pubsub#deliver_notifications">>, <<"false">>}],
              pubsub_tools:create_node(Alice, ?NODE, NodeConfig),

              pubsub_tools:subscribe(Bob, ?NODE),
              pubsub_tools:publish(Alice, <<"item1">>, ?NODE),

              %% Notifications disabled
              escalus_assert:has_no_stanzas(Bob),

              pubsub_tools:delete_node(Alice, ?NODE)
      end).

disable_payload_config_test(Config) ->
    escalus:story(
      Config,
      [{alice,1}, {bob,1}],
      fun(Alice, Bob) ->
              %% Notifiacation-Only Persistent Node, see 4.3, table 4
              NodeConfig = [{<<"pubsub#deliver_payloads">>, <<"false">>}],
              pubsub_tools:create_node(Alice, ?NODE, NodeConfig),

              pubsub_tools:subscribe(Bob, ?NODE),
              pubsub_tools:publish(Alice, <<"item1">>, ?NODE),

              %% Payloads disabled
              pubsub_tools:receive_notification(Bob, <<"item1">>, ?NODE, false),

              pubsub_tools:delete_node(Alice, ?NODE)
      end).

disable_persist_items_test(Config) ->
    escalus:story(
      Config,
      [{alice,1}, {bob,1}],
      fun(Alice, Bob) ->
              %% Payload-Included Transient Node, see 4.3, table 4
              NodeConfig = [{<<"pubsub#persist_items">>, <<"false">>}],
              pubsub_tools:create_node(Alice, ?NODE, NodeConfig),

              pubsub_tools:subscribe(Bob, ?NODE),
              pubsub_tools:publish(Alice, <<"item1">>, ?NODE),

              %% Notifications should work
              pubsub_tools:receive_notification(Bob, <<"item1">>, ?NODE),

              %% No items should be stored
              pubsub_tools:request_all_items(Bob, [], ?NODE),

              pubsub_tools:delete_node(Alice, ?NODE)
      end).

%% disable_payload_and_persist_test(Config) ->
%%     escalus:story(
%%       Config,
%%       [{alice,1}, {bob,1}],
%%       fun(Alice, Bob) ->
%%               %% Notification-Only Transient Node, see 4.3, table 4
%%               NodeConfig = [{<<"pubsub#deliver_payloads">>, <<"false">>},
%%                             {<<"pubsub#persist_items">>, <<"false">>}],
%%               pubsub_tools:create_node(Alice, ?NODE, NodeConfig),

%%               pubsub_tools:subscribe(Bob, ?NODE),

%%               %% Response  7.1.3 Ex.112 attempt to publish payload to transient notification node
%%               %%                   Expected error of type 'modify'
%%               pubsub_tools:publish(Alice, <<"item1">>, ?NODE, true, <<"modify">>),

%%               %% Publish without payload should succeed
%%               pubsub_tools:publish(Alice, <<"item2">>, ?NODE, false),

%%               %% Notifications should work
%%               pubsub_tools:receive_notification(Bob, <<"item1">>, ?NODE),

%%               %% No items should be stored
%%               pubsub_tools:request_all_items(Bob, [], ?NODE),

%%               %% No more notifications
%%               escalus_assert:has_no_stanzas(Bob),

%%               pubsub_tools:delete_node(Alice, ?NODE)
%%       end).

%%--------------------------------------------------------------------
%% Test cases for XEP-0248
%% Comments in test cases refer to sections is the XEP
%%--------------------------------------------------------------------

-define(LEAF_NAME, <<"leaf">>).
-define(LEAF, {?NODE_ADDR, ?LEAF_NAME}).

-define(LEAF_NAME_2, <<"leaf2">>).
-define(LEAF_2, {?NODE_ADDR, ?LEAF_NAME_2}).

create_delete_collection_test(Config) ->
    escalus:story(
      Config,
      [{alice,1}],
      fun(Alice) ->
              %% Request:  7.1.1 Ex.18 create collection node
              %% Response:       Ex.19 success
              %%                        Note: contains node ID although XEP does not require this
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              pubsub_tools:create_node(Alice, ?NODE, CollectionConfig),

              %% Request:  7.3.1 Ex.30 delete collection node
              %% Response: 7.3.2 Ex.31 success
              pubsub_tools:delete_node(Alice, ?NODE)
      end).

subscribe_unsubscribe_collection_test(Config) ->
    escalus:story(
      Config,
      [{alice,1}, {bob,1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              pubsub_tools:create_node(Alice, ?NODE, CollectionConfig),

              %% Request:  6.1.1 Ex.10 subscribe (no configuration)
              %% Response: 6.1.2 Ex.12 success
              pubsub_tools:subscribe(Bob, ?NODE),

              %% Same as XEP-0060
              pubsub_tools:unsubscribe(Bob, ?NODE),

              pubsub_tools:delete_node(Alice, ?NODE)
      end).

create_delete_leaf_test(Config) ->
    escalus:story(
      Config,
      [{alice,1}],
      fun(Alice) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              pubsub_tools:create_node(Alice, ?NODE, CollectionConfig),

              %% XEP-0060, 8.1.2, see 16.4.4 for config details
              NodeConfig = [{<<"pubsub#collection">>, ?NODE_NAME}],
              pubsub_tools:create_node(Alice, ?LEAF, NodeConfig),

              pubsub_tools:delete_node(Alice, ?LEAF),
              pubsub_tools:delete_node(Alice, ?NODE)
      end).

notify_collection_test(Config) ->
    escalus:story(
      Config,
      [{alice,1}, {bob,1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              pubsub_tools:create_node(Alice, ?NODE, CollectionConfig),

              NodeConfig = [{<<"pubsub#collection">>, ?NODE_NAME}],
              pubsub_tools:create_node(Alice, ?LEAF, NodeConfig),
              pubsub_tools:create_node(Alice, ?LEAF_2, NodeConfig),
              pubsub_tools:subscribe(Bob, ?NODE),

              %% Publish to leaf nodes, Bob should get notifications
              pubsub_tools:publish(Alice, <<"item1">>, ?LEAF),
              pubsub_tools:receive_notification(Bob, <<"item1">>, ?LEAF),
              pubsub_tools:publish(Alice, <<"item2">>, ?LEAF_2),
              pubsub_tools:receive_notification(Bob, <<"item2">>, ?LEAF_2),

              pubsub_tools:delete_node(Alice, ?LEAF),
              pubsub_tools:delete_node(Alice, ?LEAF_2),
              pubsub_tools:delete_node(Alice, ?NODE)
      end).

notify_collection_and_leaf_test(Config) ->
    escalus:story(
      Config,
      [{alice,1}, {bob,1}, {geralt,1}],
      fun(Alice, Bob, Geralt) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              pubsub_tools:create_node(Alice, ?NODE, CollectionConfig),

              NodeConfig = [{<<"pubsub#collection">>, ?NODE_NAME}],
              pubsub_tools:create_node(Alice, ?LEAF, NodeConfig),
              pubsub_tools:subscribe(Bob, ?NODE),
              pubsub_tools:subscribe(Geralt, ?LEAF),

              %% Publish to leaf nodes, Bob and Geralt should get notifications
              pubsub_tools:publish(Alice, <<"item1">>, ?LEAF),
              pubsub_tools:receive_notification(Bob, <<"item1">>, ?LEAF),
              pubsub_tools:receive_notification(Geralt, <<"item1">>, ?LEAF),

              pubsub_tools:delete_node(Alice, ?LEAF),
              pubsub_tools:delete_node(Alice, ?NODE)
      end).

notify_collection_and_leaf_same_user_test(Config) ->
    escalus:story(
      Config,
      [{alice,1}, {bob,1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              pubsub_tools:create_node(Alice, ?NODE, CollectionConfig),

              NodeConfig = [{<<"pubsub#collection">>, ?NODE_NAME}],
              pubsub_tools:create_node(Alice, ?LEAF, NodeConfig),
              pubsub_tools:subscribe(Bob, ?NODE),
              pubsub_tools:subscribe(Bob, ?LEAF),

              %% Bob should get only one notification
              pubsub_tools:publish(Alice, <<"item1">>, ?LEAF),
              pubsub_tools:receive_notification(Bob, <<"item1">>, ?LEAF),
              escalus_assert:has_no_stanzas(Bob),

              pubsub_tools:delete_node(Alice, ?LEAF),
              pubsub_tools:delete_node(Alice, ?NODE)
      end).

retrieve_subscriptions_collection_test(Config) ->
    escalus:story(
      Config,
      [{alice,1}, {bob,1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              pubsub_tools:create_node(Alice, ?NODE, CollectionConfig),

              NodeConfig = [{<<"pubsub#collection">>, ?NODE_NAME}],
              pubsub_tools:create_node(Alice, ?LEAF, NodeConfig),
              pubsub_tools:create_node(Alice, ?LEAF_2, NodeConfig),
              pubsub_tools:subscribe(Bob, ?NODE),
              pubsub_tools:subscribe(Bob, ?LEAF),

              % Only the nodes for which subscriptions were made should be returned
              pubsub_tools:retrieve_subscriptions(Bob, [{?LEAF_NAME, <<"subscribed">>},
                                                        {?NODE_NAME, <<"subscribed">>}], ?NODE_ADDR),

              pubsub_tools:delete_node(Alice, ?LEAF),
              pubsub_tools:delete_node(Alice, ?LEAF_2),
              pubsub_tools:delete_node(Alice, ?NODE)
      end).

discover_child_nodes_test(Config) ->
    escalus:story(
      Config,
      [{alice,1}, {bob,1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              pubsub_tools:create_node(Alice, ?NODE, CollectionConfig),

              NodeConfig = [{<<"pubsub#collection">>, ?NODE_NAME}],
              pubsub_tools:create_node(Alice, ?LEAF, NodeConfig),
              pubsub_tools:create_node(Alice, ?LEAF_2, NodeConfig),

              pubsub_tools:discover_nodes(Bob, ?NODE, [?LEAF_NAME, ?LEAF_NAME_2]),

              pubsub_tools:delete_node(Alice, ?LEAF),
              pubsub_tools:delete_node(Alice, ?LEAF_2),
              pubsub_tools:delete_node(Alice, ?NODE)
      end).

request_all_items_leaf_test(Config) ->
    escalus:story(
      Config,
      [{alice,1}, {bob,1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              pubsub_tools:create_node(Alice, ?NODE, CollectionConfig),

              NodeConfig = [{<<"pubsub#collection">>, ?NODE_NAME}],
              pubsub_tools:create_node(Alice, ?LEAF, NodeConfig),
              pubsub_tools:create_node(Alice, ?LEAF_2, NodeConfig),

              pubsub_tools:publish(Alice, <<"item1">>, ?LEAF),
              pubsub_tools:publish(Alice, <<"item2">>, ?LEAF_2),

              %% Request items from leaf nodes - as described in XEP-0060
              pubsub_tools:request_all_items(Bob, [<<"item1">>], ?LEAF),
              pubsub_tools:request_all_items(Bob, [<<"item2">>], ?LEAF_2),

              %% NOTE: This is not implemented yet
              %% Request:  6.2.1 Ex.15 Subscriber requests all items on a collection
              %% Response: 6.2.2 Ex.16 Service returns items on leaf nodes
              %%pubsub_tools:request_all_items(Bob, [<<"item2">>, <<"item1">>], ?NODE),

              pubsub_tools:delete_node(Alice, ?LEAF),
              pubsub_tools:delete_node(Alice, ?LEAF_2),
              pubsub_tools:delete_node(Alice, ?NODE)
      end).

