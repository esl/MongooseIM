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
         delete_node/3,
         request_configuration/3,
         configure_node/4,
         get_affiliations/3,
         set_affiliations/4,
         subscribe/3,
         unsubscribe/3,
         publish/4,
         retract_item/3,
         request_all_items/3,
         purge_all_items/3,
         retrieve_user_subscriptions/3,
         retrieve_node_subscriptions/3,
         submit_subscription_response/5,
         request_pending_subscriptions/3,
         request_pending_subscriptions/4,
         modify_node_subscriptions/4,
         discover_nodes/3,
         receive_item_notification/4,
         receive_subscription_notification/4,
         receive_subscription_request/4,
         receive_subscription_requests/4,
         receive_node_creation_notification/3,
         receive_subscribe_response/3]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [
          {group, pubsub_tests},
          {group, node_config_tests},
          {group, service_config_tests},
          {group, node_affiliations_tests},
          {group, manage_subscriptions_tests},
          {group, collection_tests},
          {group, collection_config_tests},
          {group, debug_calls_tests}
         ].

groups() -> [{pubsub_tests, [parallel],
              [
               create_delete_node_test,
               discover_nodes_test,
               subscribe_unsubscribe_test,
               publish_test,
               publish_with_max_items_test,
               notify_test,
               request_all_items_test,
               retract_test,
               retract_when_user_goes_offline_test,
               purge_all_items_test,
               retrieve_subscriptions_test
              ]
             },
             {node_config_tests, [parallel],
              [
               retrieve_configuration_test,
               set_configuration_test,
               notify_config_test,
               disable_notifications_test,
               disable_payload_test,
               disable_persist_items_test,
               notify_only_available_users_test,
               notify_unavailable_user_test,
               send_last_published_item_test
              ]
             },
             {service_config_tests, [parallel],
              [
               max_subscriptions_test
              ]
             },
             {node_affiliations_tests, [parallel],
              [
               get_affiliations_test,
               add_publisher_and_member_test,
               swap_owners_test,
               deny_no_owner_test
              ]
             },
             {manage_subscriptions_tests, [parallel],
              [
               retrieve_node_subscriptions_test,
               modify_node_subscriptions_test,
               process_subscription_requests_test,
               retrieve_pending_subscription_requests_test
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
             },
             {debug_calls_tests, [parallel],
              [
               debug_get_items_test
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
    Suffix = base64:encode(crypto:strong_rand_bytes(5)),
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

init_per_testcase(notify_unavailable_user_test, _Config) ->
    {skip, "mod_offline does not store events"};
init_per_testcase(max_subscriptions_test, Config) ->
    MaxSubs = lookup_service_option(domain(), max_subscriptions_node),
    set_service_option(domain(), max_subscriptions_node, 1),
    init_per_testcase(generic, [{max_subscriptions_node, MaxSubs} | Config]);
init_per_testcase(_TestName, Config) ->
    escalus:init_per_testcase(_TestName, Config).

end_per_testcase(max_subscriptions_test, Config1) ->
    {value, {_, OldMaxSubs}, Config2} = lists:keytake(max_subscriptions_node, 1, Config1),
    set_service_option(domain(), max_subscriptions_node, OldMaxSubs),
    end_per_testcase(generic, Config2);
end_per_testcase(TestName, Config) ->
    escalus:end_per_testcase(TestName, Config).

%%--------------------------------------------------------------------
%% Test cases for XEP-0060
%% Comments in test cases refer to sections is the XEP
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Main PubSub cases
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

publish_with_max_items_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              NodeConfig = [{<<"pubsub#max_items">>, <<"1">>},
                            {<<"pubsub#notify_retract">>, <<"1">>}],
              create_node(Alice, Node, [{config, NodeConfig}]),
              
              publish(Alice, <<"item1">>, Node, []),
              
              subscribe(Bob, Node, []),

              publish(Alice, <<"item2">>, Node, []),
              receive_item_notification(Bob, <<"item2">>, Node, []),
              verify_item_retract(Node, <<"item1">>, escalus:wait_for_stanza(Bob)),

              delete_node(Alice, Node, [])
      end).

notify_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 2}, {geralt, 2}],
      fun(Alice, Bob1, Bob2, Geralt1, Geralt2) ->
              Node = pubsub_node(),

              % It's a quick win for confirming happy path for this option
              % TODO: Extract into separate test case
              NodeConfig = [{<<"pubsub#presence_based_delivery">>, <<"1">>}],

              create_node(Alice, Node, [{config, NodeConfig}]),
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

retract_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              create_node(Alice, Node, []),
              publish(Alice, <<"item1">>, Node, []),
              publish(Alice, <<"item2">>, Node, []),

              %% Request:  7.2.1 Ex.115 Entity deletes an item from a node
              %% Response: 7.2.2 Ex.116 Service replies with success
              retract_item(Alice, Node, <<"item1">>),
              request_all_items(Bob, Node, [{expected_result, [<<"item2">>]}]),

              %% Request:  7.2.1 Ex.115 Entity deletes an item from a node
              %% Response: 7.2.2 Ex.116 Service replies with success
              %% Notification: 7.2.2.1 Ex.117 Subscribers are notified of deletion
              configure_node(Alice, Node, [{<<"pubsub#notify_retract">>, <<"1">>}], []),
              subscribe(Bob, Node, []),
              retract_item(Alice, Node, <<"item2">>),
              verify_item_retract(Node, <<"item2">>, escalus:wait_for_stanza(Bob)),
              request_all_items(Bob, Node, [{expected_result, []}]),

              delete_node(Alice, Node, [])
      end).

retract_when_user_goes_offline_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              NodeConfig = [{<<"pubsub#purge_offline">>, <<"1">>},
                            {<<"pubsub#publish_model">>, <<"open">>}],
              create_node(Alice, Node, [{config, NodeConfig}]),
              
              publish(Alice, <<"item1">>, Node, []),
              publish(Bob, <<"item2">>, Node, []),
              request_all_items(Alice, Node, [{expected_result, [<<"item2">>, <<"item1">>]}]),

              escalus_client:stop(Bob),
              request_all_items(Alice, Node, [{expected_result, [<<"item1">>]}]),

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

              {_, NodeName} = Node = pubsub_node(),
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

%%--------------------------------------------------------------------
%% Node configuration
%%--------------------------------------------------------------------

retrieve_configuration_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              Node = pubsub_node(),
              create_node(Alice, Node, []),

              NodeConfig = request_configuration(Alice, Node, []),
              verify_config_fields(NodeConfig),

              delete_node(Alice, Node, [])
      end).

set_configuration_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              Node = pubsub_node(),
              create_node(Alice, Node, []),

              ValidNodeConfig = node_config_for_test(),
              %% TODO: Investigate why this request may take more than 1s
              pubsub_tools:configure_node(Alice, Node, ValidNodeConfig,
                                          [{response_timeout, 10000}]),
              NodeConfig = request_configuration(Alice, Node, []),
              verify_config_values(NodeConfig, ValidNodeConfig),

              delete_node(Alice, Node, [])
      end).

notify_config_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              create_node(Alice, Node, [{config, [{<<"pubsub#notify_config">>, <<"1">>}]}]),
              subscribe(Bob, Node, []),

              ConfigChange = [{<<"pubsub#title">>, <<"newtitle">>}],
              configure_node(Alice, Node, ConfigChange, []),
              verify_config_event(Node, ConfigChange, escalus:wait_for_stanza(Bob)),

              delete_node(Alice, Node, [])
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
              %% Node notifies only available users
              NodeConfig = [{<<"pubsub#presence_based_delivery">>, <<"true">>}],
              Node = pubsub_node(),
              create_node(Alice, Node, [{config, NodeConfig}]),

              subscribe(Bob, Node, [{jid_type, bare}]),

              escalus:send(Bob, escalus_stanza:presence(<<"unavailable">>)),

              %% Item from node 2 not received (blocked by resource-based delivery)
              publish(Alice, <<"item2">>, Node, []),
              escalus_assert:has_no_stanzas(Bob),

              delete_node(Alice, Node, [])
      end).

notify_unavailable_user_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              create_node(Alice, Node, []),

              subscribe(Bob, Node, [{jid_type, bare}]),

              escalus:send(Bob, escalus_stanza:presence(<<"unavailable">>)),

              %% Receive item from node 1 (also make sure the presence is processed)
              publish(Alice, <<"item1">>, Node, []),

              escalus_assert:has_no_stanzas(Bob),
              escalus:send(Bob, escalus_stanza:presence(<<"available">>)),
              receive_item_notification(Bob, <<"item1">>, Node, []),

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

%%--------------------------------------------------------------------
%% Service config
%%--------------------------------------------------------------------

max_subscriptions_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              create_node(Alice, Node, []),

              subscribe(Alice, Node, []),
              IQError = subscribe(Bob, Node, [{expected_error_type, <<"cancel">>}]),
              is_not_allowed_and_closed(IQError),

              delete_node(Alice, Node, [])
      end).

%%--------------------------------------------------------------------
%% Node affiliations management
%%--------------------------------------------------------------------

get_affiliations_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              Node = pubsub_node(),
              create_node(Alice, Node, []),

              verify_affiliations(get_affiliations(Alice, Node, []), [{Alice, <<"owner">>}]),

              delete_node(Alice, Node, [])
      end).

add_publisher_and_member_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}, {kate, 1}],
      fun(Alice, Bob, Kate) ->
              Node = pubsub_node(),
              NodeConfig = [{<<"pubsub#access_model">>, <<"whitelist">>},
                            {<<"pubsub#publish_model">>, <<"publishers">>}],
              create_node(Alice, Node, [{config, NodeConfig}]),

              publish(Bob, <<"item1">>, Node, [{expected_error_type, <<"auth">>}]),
              IQError = subscribe(Kate, Node, [{expected_error_type, <<"cancel">>}]),
              is_not_allowed_and_closed(IQError),

              AffChange = [{Bob, <<"publisher">>}, {Kate, <<"member">>}],
              set_affiliations(Alice, Node, AffChange, []),

              publish(Kate, <<"nope">>, Node, [{expected_error_type, <<"auth">>}]),
              subscribe(Kate, Node, []),
              publish(Bob, <<"item1">>, Node, []),
              receive_item_notification(Kate, <<"item1">>, Node, []),

              delete_node(Alice, Node, [])
      end).

swap_owners_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              create_node(Alice, Node, []),

              AffChange = [{Bob, <<"owner">>}, {Alice, <<"none">>}],
              set_affiliations(Alice, Node, AffChange, []),

              get_affiliations(Alice, Node, [{expected_error_type, <<"auth">>}]),
              verify_affiliations(get_affiliations(Bob, Node, []), [{Bob, <<"owner">>}]),

              delete_node(Bob, Node, [])
      end).

deny_no_owner_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              Node = pubsub_node(),
              create_node(Alice, Node, []),

              AffChange = [{Alice, <<"member">>}],
              IQError = set_affiliations(Alice, Node, AffChange,
                                         [{expected_error_type, <<"modify">>}]),
              verify_returned_affiliation(IQError, Alice, <<"owner">>),

              verify_affiliations(get_affiliations(Alice, Node, []), [{Alice, <<"owner">>}]),

              delete_node(Alice, Node, [])
      end).

%%--------------------------------------------------------------------
%% Subscriptions management
%%--------------------------------------------------------------------

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

process_subscription_requests_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}, {kate, 1}],
      fun(Alice, Bob, Kate) ->
              Node = pubsub_node(),
              NodeConfig = [{<<"pubsub#access_model">>, <<"authorize">>}],
              create_node(Alice, Node, [{config, NodeConfig}]),

              subscribe(Bob, Node, [{subscription, <<"pending">>}]),
              BobsRequest = receive_subscription_request(Alice, Bob, Node, []),
              subscribe(Kate, Node, [{subscription, <<"pending">>}]),
              KatesRequest = receive_subscription_request(Alice, Kate, Node, []),

              submit_subscription_response(Alice, BobsRequest, Node, true, []),
              receive_subscription_notification(Bob, <<"subscribed">>, Node, []),
              submit_subscription_response(Alice, KatesRequest, Node, false, []),
              receive_subscription_notification(Kate, <<"none">>, Node, []),

              publish(Alice, <<"item1">>, Node, []),
              receive_item_notification(Bob, <<"item1">>, Node, []),
              [] = escalus:peek_stanzas(Kate),

              delete_node(Alice, Node, [])
      end).

retrieve_pending_subscription_requests_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}, {kate, 1}],
      fun(Alice, Bob, Kate) ->
              {NodeAddr, NodeName} = Node = pubsub_node(),
              NodeConfig = [{<<"pubsub#access_model">>, <<"authorize">>}],
              create_node(Alice, Node, [{config, NodeConfig}]),

              subscribe(Bob, Node, [{subscription, <<"pending">>}]),
              receive_subscription_request(Alice, Bob, Node, []),
              subscribe(Kate, Node, [{subscription, <<"pending">>}]),
              receive_subscription_request(Alice, Kate, Node, []),

              request_pending_subscriptions(Alice, NodeAddr, [NodeName], []),

              %% TODO: XEP requires IQ result to come before the requests
              Request = request_pending_subscriptions(Alice, Node, [{receive_response, false}]),
              receive_subscription_requests(Alice, [Bob, Kate], Node, []),
              IQRes = escalus:wait_for_stanza(Alice),
              escalus:assert(is_iq_result, [Request], IQRes),

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

%%--------------------------------------------------------------------
%% Collections config
%%--------------------------------------------------------------------

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
%% Debug calls tests
%%--------------------------------------------------------------------

debug_get_items_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              {NodeAddr, NodeName} = Node = pubsub_node(),
              create_node(Alice, Node, []),
              publish(Alice, <<"item1">>, Node, []),
              publish(Alice, <<"item2">>, Node, []),

              Items = escalus_ejabberd:rpc(mod_pubsub, get_items, [NodeAddr, NodeName]),
              % We won't bother with importing records etc...
              2 = length(Items),

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
                   {plugins, [<<"dag">>]},
                   {nodetree, <<"dag">>},
                   {host, "pubsub.@HOST@"}
                  ]}].

verify_config_fields(NodeConfig) ->
    ValidFields = [
                   {<<"FORM_TYPE">>, <<"hidden">>},
                   {<<"pubsub#title">>, <<"text-single">>}, 
                   {<<"pubsub#deliver_notifications">>, <<"boolean">>},
                   {<<"pubsub#deliver_payloads">>, <<"boolean">>},
                   {<<"pubsub#notify_config">>, <<"boolean">>},
                   {<<"pubsub#notify_delete">>, <<"boolean">>},
                   {<<"pubsub#notify_retract">>, <<"boolean">>},
% not supported yet                   {<<"pubsub#notify_sub">>, <<"boolean">>},
                   {<<"pubsub#persist_items">>, <<"boolean">>},
                   {<<"pubsub#max_items">>, <<"text-single">>},
% not supported yet                   {<<"pubsub#item_expire">>, <<"text-single">>},
                   {<<"pubsub#subscribe">>, <<"boolean">>},
                   {<<"pubsub#access_model">>, <<"list-single">>},
                   {<<"pubsub#roster_groups_allowed">>, <<"list-multi">>},
                   {<<"pubsub#publish_model">>, <<"list-single">>},
                   {<<"pubsub#purge_offline">>, <<"boolean">>},
                   {<<"pubsub#max_payload_size">>, <<"text-single">>},
                   {<<"pubsub#send_last_published_item">>, <<"list-single">>},
                   {<<"pubsub#presence_based_delivery">>, <<"boolean">>},
                   {<<"pubsub#notification_type">>, <<"list-single">>},
                   {<<"pubsub#type">>, <<"text-single">>},
% not supported yet                   {<<"pubsub#dataform_xslt">>, <<"text-single">>}
% not supported yet                   {<<"pubsub#node_type">>, undef},
% not supported yet                   {<<"pubsub#children">>, undef},
                   {<<"pubsub#collection">>, <<"text-multi">>}
                  ],
    [] =
    lists:foldl(fun({Var, Type}, Fields) ->
                        {{value, {_, Type, _}, NewFields}, _}
                        = {lists:keytake(Var, 1, Fields), Var},
                        NewFields
                end, NodeConfig, ValidFields).

node_config_for_test() ->
    [
     {<<"pubsub#title">>, <<"TARDIS">>}, 
     {<<"pubsub#deliver_notifications">>, <<"1">>},
     {<<"pubsub#deliver_payloads">>, <<"1">>},
     {<<"pubsub#notify_config">>, <<"0">>},
     {<<"pubsub#notify_delete">>, <<"1">>},
     {<<"pubsub#notify_retract">>, <<"1">>},
     % Not supported yet                   {<<"pubsub#notify_sub">>, <<"boolean">>},
     {<<"pubsub#persist_items">>, <<"0">>},
     {<<"pubsub#max_items">>, <<"10">>},
     % Not supported yet: {<<"pubsub#item_expire">>, <<"text-single">>},
     {<<"pubsub#subscribe">>, <<"0">>},
     {<<"pubsub#access_model">>, <<"presence">>},
     % TODO: Verify with test case: {<<"pubsub#roster_groups_allowed">>, <<"list-multi">>},
     {<<"pubsub#publish_model">>, <<"publishers">>},
     {<<"pubsub#purge_offline">>, <<"1">>},
     {<<"pubsub#max_payload_size">>, <<"24601">>},
     {<<"pubsub#send_last_published_item">>, <<"on_sub">>},
     {<<"pubsub#presence_based_delivery">>, <<"1">>},
     {<<"pubsub#notification_type">>, <<"normal">>},
     {<<"pubsub#type">>, <<"urn:mim">>}
     % Not supported yet: {<<"pubsub#dataform_xslt">>, <<"text-single">>}
     % Not supported yet: {<<"pubsub#node_type">>, undef},
     % Not supported yet: {<<"pubsub#children">>, undef},
     % Covered by collection tests: {<<"pubsub#collection">>, <<"text-multi">>}
    ].

verify_item_retract({NodeAddr, NodeName}, ItemId, Stanza) ->
    escalus:assert(is_message, Stanza),
    NodeAddr == exml_query:attr(Stanza, <<"from">>),

    [#xmlel{ attrs = [{<<"xmlns">>, ?NS_PUBSUB_EVENT}] } = Event]
    = exml_query:subelements(Stanza, <<"event">>),

    [#xmlel{ attrs = [{<<"node">>, NodeName}] } = Items]
    = exml_query:subelements(Event, <<"items">>),

    [#xmlel{ attrs = [{<<"id">>, ItemId}] }] = exml_query:subelements(Items, <<"retract">>).

verify_config_event({NodeAddr, NodeName}, ConfigChange, Stanza) ->
    escalus:assert(is_message, Stanza),
    NodeAddr == exml_query:attr(Stanza, <<"from">>),

    [#xmlel{ attrs = [{<<"xmlns">>, ?NS_PUBSUB_EVENT}] } = Event]
    = exml_query:subelements(Stanza, <<"event">>),

    [#xmlel{ attrs = [{<<"node">>, NodeName}] } = ConfigEl]
    = exml_query:subelements(Event, <<"configuration">>),

    Fields = exml_query:paths(ConfigEl, [{element, <<"x">>},
                                         {element, <<"field">>}]),

    Opts = [ {exml_query:attr(F, <<"var">>),
              exml_query:path(F, [{element, <<"value">>}, cdata])} || F <- Fields ],

    true = lists:all(fun({K, V}) ->
                             {K, V} =:= lists:keyfind(K, 1, Opts)
                     end, ConfigChange).

verify_config_values(NodeConfig, ValidConfig) ->
    lists:foreach(fun({Var, Val}) ->
                          {{_, _, Val}, _} = {lists:keyfind(Var, 1, NodeConfig), Var}
                  end, ValidConfig).

verify_affiliations(Affiliations, ValidAffiliations) ->
    NormalisedValidAffiliations
    = lists:sort([ {escalus_utils:jid_to_lower(escalus_client:short_jid(Client)), Aff}
                   || {Client, Aff} <- ValidAffiliations ]),
    NormalisedValidAffiliations = lists:sort(Affiliations).

verify_returned_affiliation(IQError, User, Aff) ->
    UserJid = escalus_utils:jid_to_lower(escalus_utils:get_short_jid(User)),
    QPath = [{element, <<"pubsub">>},
             {element, <<"affiliations">>},
             {element, <<"affiliation">>}],
    [AffEl] = exml_query:paths(IQError, QPath),
    UserJid = exml_query:attr(AffEl, <<"jid">>),
    Aff = exml_query:attr(AffEl, <<"affiliation">>).

is_not_allowed_and_closed(IQError) ->
    ?NS_STANZA_ERRORS = exml_query:path(IQError, [{element, <<"error">>},
                                                  {element, <<"not-allowed">>},
                                                  {attr, <<"xmlns">>}]),
    ?NS_PUBSUB_ERRORS = exml_query:path(IQError, [{element, <<"error">>},
                                                  {element, <<"closed-node">>},
                                                  {attr, <<"xmlns">>}]).

%% TODO: Functions below will most probably fail when mod_pubsub gets some nice refactoring!

set_service_option(Host, Key, Val) ->
    true = escalus_ejabberd:rpc(ets, insert, [service_tab_name(Host), {Key, Val}]).

lookup_service_option(Host, Key) ->
    [{_, Val}] = escalus_ejabberd:rpc(ets, lookup, [service_tab_name(Host), Key]),
    Val.

service_tab_name(Host) ->
    escalus_ejabberd:rpc(gen_mod, get_module_proc, [Host, config]).

