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
          {group, pubsub_cycle_tests}
         ].

groups() -> [{pubsub_cycle_tests, [sequence],
              [
               create_delete_node_test,
               subscribe_unsubscribe_test,
               publish_test,
               notify_test,
               request_all_items_test,
               retrieve_subscriptions_test
              ]
             }].

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
%% Test cases for XEP 0060
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
              pubsub_tools:create_node(Alice, ?NODE),

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

request_all_items_test(Config) ->
    escalus:story(
      Config,
      [{alice,1}, {bob,1}],
      fun(Alice, Bob) ->
              pubsub_tools:create_node(Alice, ?NODE),
              pubsub_tools:publish(Alice, <<"item1">>, ?NODE),
              pubsub_tools:publish(Alice, <<"item2">>, ?NODE),

              %% Note: when Bob subscribes, the last item (item2) is sent to him
              %%       6.1.7 Ex.50 service sends last published item
              %%       This is sent BEFORE the response iq stanza
              pubsub_tools:subscribe(Bob, ?NODE, <<"item2">>),

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
