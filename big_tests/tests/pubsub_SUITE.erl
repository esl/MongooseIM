%%%===================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd.
%%% @doc Suite for testing pubsub features as described in XEP-0060
%%% @end
%%%===================================================================

-module(pubsub_SUITE).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").

-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

-export([
         discover_nodes_test/1,
         create_delete_node_test/1,
         subscribe_unsubscribe_test/1,
         subscribe_options_test/1,
         subscribe_options_deliver_option_test/1,
         subscribe_options_separate_request_test/1,
         publish_test/1,
         publish_with_max_items_test/1,
         publish_with_existing_id_test/1,
         notify_test/1,
         request_all_items_test/1,
         request_particular_item_test/1,
         retract_test/1,
         retract_when_user_goes_offline_test/1,
         purge_all_items_test/1,
         publish_only_retract_items_scope_test/1
        ]).

-export([
         max_subscriptions_test/1
        ]).

-export([
         retrieve_configuration_test/1,
         set_configuration_test/1,
         notify_config_test/1,
         disable_notifications_test/1,
         disable_payload_test/1,
         disable_persist_items_test/1,
         notify_only_available_users_test/1,
         notify_unavailable_user_test/1,
         send_last_published_item_test/1,
         send_last_published_item_no_items_test/1
        ]).

-export([
         get_affiliations_test/1,
         add_publisher_and_member_test/1,
         swap_owners_test/1,
         deny_no_owner_test/1
        ]).

-export([
         retrieve_user_subscriptions_test/1,
         retrieve_node_subscriptions_test/1,
         modify_node_subscriptions_test/1,
         process_subscription_requests_test/1,
         retrieve_pending_subscription_requests_test/1
        ]).

-export([
         create_delete_collection_test/1,
         subscribe_unsubscribe_collection_test/1,
         collection_delete_makes_leaf_parentless/1,
         notify_collection_test/1,
         notify_collection_leaf_and_item_test/1,
         notify_collection_bare_jid_test/1,
         notify_collection_and_leaf_test/1,
         notify_collection_and_leaf_same_user_test/1,
         notify_collections_with_same_leaf_test/1,
         notify_nested_collections_test/1,
         retrieve_subscriptions_collection_test/1,
         discover_top_level_nodes_test/1,
         discover_child_nodes_test/1,
         request_all_items_leaf_test/1
        ]).

-export([
         disable_notifications_leaf_test/1,
         disable_payload_leaf_test/1,
         disable_persist_items_leaf_test/1
        ]).

-export([
         debug_get_items_test/1,
         debug_get_item_test/1
        ]).

-export([
         can_create_node_with_existing_parent_path/1,
         cant_create_node_with_missing_parent_path/1,
         disco_node_children_by_path_prefix/1,
         deleting_parent_path_deletes_children/1
        ]).

%% Disabled tests - broken support in mod_pubsub
-export([
         disable_payload_and_persist_test/1,
         disable_delivery_test/1
        ]).

-export([
         get_item_with_publisher_option_test/1,
         receive_item_notification_with_publisher_option_test/1
        ]).
-import(pubsub_tools, [pubsub_node/0,
                       domain/0,
                       node_addr/0,
                       encode_group_name/2,
                       decode_group_name/1]).
-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             rpc/4]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, GN} || {GN, _, _} <- groups()].

groups() ->
    lists:flatmap(
      fun(NodeTree) ->
              [ {encode_group_name(BaseGroup, NodeTree), Opts, Cases}
                || {BaseGroup, Opts, Cases} <- base_groups(),
                   group_is_compatible(BaseGroup, NodeTree) ]
      end, [<<"dag">>, <<"tree">>]).

% nodetree_tree doesn't support collections by XEP
% It uses implicit collections by path in nodes' names
group_is_compatible(collection, <<"tree">>) -> false;
group_is_compatible(collection_config, <<"tree">>) -> false;
group_is_compatible(hometree_specific, OnlyNodetreeTree) -> OnlyNodetreeTree =:= <<"tree">>;
group_is_compatible(_, _) -> true.

base_groups() ->
    %% NOTE: basic and collection are placed in sequence because they run bigger
    %% sets of tests, which in parallel might cause deadlocks in the
    %% transactions for MNESIA and MYSQL.
    %% TODO: Optimise pubsub backends (MYSQL! Mnesia?)
    G = [{basic, [sequence], basic_tests()},
         {service_config, [parallel], service_config_tests()},
         {node_config, [parallel], node_config_tests()},
         {node_affiliations, [parallel], node_affiliations_tests()},
         {manage_subscriptions, [parallel], manage_subscriptions_tests()},
         {collection, [sequence], collection_tests()},
         {collection_config, [parallel], collection_config_tests()},
         {debug_calls, [parallel], debug_calls_tests()},
         {pubsub_item_publisher_option, [parallel], pubsub_item_publisher_option_tests()},
         {hometree_specific, [sequence], hometree_specific_tests()},
         {last_item_cache, [parallel], last_item_cache_tests()}
        ],
    ct_helper:repeat_all_until_all_ok(G).

basic_tests() ->
    [
     discover_nodes_test,
     create_delete_node_test,
     subscribe_unsubscribe_test,
     subscribe_options_test,
     subscribe_options_deliver_option_test,
     subscribe_options_separate_request_test,
     publish_test,
     publish_with_max_items_test,
     publish_with_existing_id_test,
     notify_test,
     request_all_items_test,
     request_particular_item_test,
     retract_test,
     retract_when_user_goes_offline_test,
     purge_all_items_test,
     publish_only_retract_items_scope_test
    ].

service_config_tests() ->
    [
     max_subscriptions_test
    ].

node_config_tests() ->
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
    ].

node_affiliations_tests() ->
    [
     get_affiliations_test,
     add_publisher_and_member_test,
     swap_owners_test,
     deny_no_owner_test
    ].

manage_subscriptions_tests() ->
    [
     retrieve_user_subscriptions_test,
     retrieve_node_subscriptions_test,
     modify_node_subscriptions_test,
     process_subscription_requests_test,
     retrieve_pending_subscription_requests_test
    ].

collection_tests() ->
    [
     create_delete_collection_test,
     subscribe_unsubscribe_collection_test,
     collection_delete_makes_leaf_parentless,
     notify_collection_test,
     notify_collection_leaf_and_item_test,
     notify_collection_bare_jid_test,
     notify_collection_and_leaf_test,
     notify_collection_and_leaf_same_user_test,
     notify_collections_with_same_leaf_test,
     notify_nested_collections_test,
     retrieve_subscriptions_collection_test,
     discover_top_level_nodes_test,
     discover_child_nodes_test,
     request_all_items_leaf_test
    ].

collection_config_tests() ->
    [
     disable_notifications_leaf_test,
     disable_payload_leaf_test,
     disable_persist_items_leaf_test
    ].

debug_calls_tests() ->
    [
     debug_get_items_test,
     debug_get_item_test
    ].

pubsub_item_publisher_option_tests() ->
    [
     get_item_with_publisher_option_test,
     receive_item_notification_with_publisher_option_test
    ].

hometree_specific_tests() ->
    [
     can_create_node_with_existing_parent_path,
     cant_create_node_with_missing_parent_path,
     disco_node_children_by_path_prefix,
     deleting_parent_path_deletes_children
    ].

last_item_cache_tests() ->
    [
     send_last_published_item_test,
     send_last_published_item_no_items_test,
     purge_all_items_test
    ].
%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(ComplexName, Config) ->
    DecodedGroupName = decode_group_name(ComplexName),
    ExtraOptions = extra_options_by_group_name(DecodedGroupName),
    Config2 = dynamic_modules:save_modules(domain(), Config),
    dynamic_modules:ensure_modules(domain(), required_modules(ExtraOptions)),
    Config2.

extra_options_by_group_name(#{ node_tree := NodeTree,
                               base_name := pubsub_item_publisher_option }) ->
    [{nodetree, NodeTree},
     {plugins, [plugin_by_nodetree(NodeTree)]},
     {item_publisher, true}];
extra_options_by_group_name(#{ node_tree := NodeTree,
                               base_name := hometree_specific }) ->
    [{nodetree, NodeTree},
     {plugins, [<<"hometree">>]}];
extra_options_by_group_name(#{ node_tree := NodeTree,
                               base_name := last_item_cache}) ->
    [{nodetree, NodeTree},
     {plugins, [plugin_by_nodetree(NodeTree)]},
     {last_item_cache, mongoose_helper:mnesia_or_rdbms_backend()}];
extra_options_by_group_name(#{ node_tree := NodeTree }) ->
    [{nodetree, NodeTree},
     {plugins, [plugin_by_nodetree(NodeTree)]}].

plugin_by_nodetree(<<"dag">>) -> <<"dag">>;
plugin_by_nodetree(<<"tree">>) -> <<"flat">>.

end_per_group(_GroupName, Config) ->
    dynamic_modules:restore_modules(domain(), Config).

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

discover_nodes_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% Request:  5.2 Ex.9  Entity asks service for all first-level nodes
              %% Response:     Ex.10 Service returns all first-level nodes
              %% it shouldn't contain the Node which will be created in a moment
              {_, NodeName} = Node = pubsub_node(),
              pubsub_tools:discover_nodes(Bob, node_addr(), [{expected_result, [{no, NodeName}]}]),

              pubsub_tools:create_node(Alice, Node, []),
              pubsub_tools:discover_nodes(Bob, node_addr(), [{expected_result, [NodeName]}]),

              {_, NodeName2} = Node2 = pubsub_node(),
              pubsub_tools:create_node(Alice, Node2, []),
              pubsub_tools:discover_nodes(
                Bob, node_addr(), [{expected_result, [NodeName, NodeName2]}]),

              pubsub_tools:delete_node(Alice, Node, []),
              pubsub_tools:delete_node(Alice, Node2, [])
      end).

create_delete_node_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              %% Request:  8.1.2 Ex.132 create node with (default) open access model
              %% Response:       Ex.134 success
              %%                        Note: contains node ID although XEP does not require this
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),

              %% Request:  8.4.1 Ex.155 owner deletes a node
              %% Response:       Ex.157 success
              pubsub_tools:delete_node(Alice, Node, [])
      end).

subscribe_unsubscribe_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),

              %% Request:  6.1.1 Ex.32 entity subscribes to a node
              %% Response: 6.1.2 Ex.33 success (with subscription ID)
              pubsub_tools:subscribe(Bob, Node, []),

              %% Request:  6.2.1 Ex.51 unsubscribe from a node
              %% Response: 6.2.2 Ex.52 success
              pubsub_tools:unsubscribe(Bob, Node, []),

              %% Check subscriptions without resources
              pubsub_tools:subscribe(Bob, Node, [{jid_type, bare}]),
              pubsub_tools:unsubscribe(Bob, Node, [{jid_type, bare}]),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

subscribe_options_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}, {geralt, 1}],
      fun(Alice, Bob, Geralt) ->
              {_, NodeName} = Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),

              %% 6.3.4.2 Example 62. No such subscriber
              [ pubsub_tools:get_subscription_options(Client, {node_addr(), NodeName},
                                                      [{expected_error_type, <<"modify">>}])
                || Client <- [Alice, Bob, Geralt] ],

              GeraltOpts = [{<<"pubsub#deliver">>, <<"true">>}],
              BobOpts = [{<<"pubsub#deliver">>, <<"false">>}],
              pubsub_tools:subscribe(Geralt, Node, [{config, GeraltOpts}]),
              pubsub_tools:subscribe(Bob, Node, [{config, BobOpts}]),

              %% 6.3.2 Example 59. Subscriber requests subscription options form
              pubsub_tools:get_subscription_options(Geralt, {node_addr(), NodeName},
                                                    [{expected_result, GeraltOpts}]),
              pubsub_tools:get_subscription_options(Bob, {node_addr(), NodeName},
                                                    [{expected_result, BobOpts}]),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

subscribe_options_deliver_option_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}, {geralt, 1}],
      fun(Alice, Bob, Geralt) ->
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),

              pubsub_tools:subscribe(Geralt, Node, [{config, [{<<"pubsub#deliver">>, <<"true">>}]}]),
              pubsub_tools:subscribe(Bob, Node, [{config, [{<<"pubsub#deliver">>, <<"false">>}]}]),

              pubsub_tools:publish(Alice, <<"item1">>, Node, []),

              %% Geralt should recive a notification
              pubsub_tools:receive_item_notification(Geralt, <<"item1">>, Node, [{expected_result, true}]),
              %% Bob should not recive a notification
              [] = escalus:wait_for_stanzas(Bob, 1, 5000),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

subscribe_options_separate_request_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Clients = [Alice, Bob],
              OptionAfterUpdate = {<<"pubsub#deliver">>, <<"false">>},
              {_, NodeName} = Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),

              pubsub_tools:subscribe(Bob, Node, [{config, [{<<"pubsub#deliver">>, <<"true">>}]}]),
              pubsub_tools:subscribe(Alice, Node, []),

              %% 6.3.5 Example 68. Subscriber submits completed options form
              [ pubsub_tools:upsert_subscription_options(
                Client,
                {node_addr(), NodeName},
                [{subscription_options,[OptionAfterUpdate]}, {receive_response, true}])
              || Client <- Clients ],

              %% 6.3.2 Example 59. Subscriber requests subscription options form
              [ pubsub_tools:get_subscription_options(Client, {node_addr(), NodeName},
                                                    [{expected_result, [OptionAfterUpdate]}])
              || Client <- Clients ],

              pubsub_tools:delete_node(Alice, Node, [])
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
              pubsub_tools:publish(Alice, <<"item1">>, Node, []),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

publish_with_max_items_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              NodeConfig = [{<<"pubsub#max_items">>, <<"1">>},
                            {<<"pubsub#notify_retract">>, <<"1">>}],
              pubsub_tools:create_node(Alice, Node, [{config, NodeConfig}]),

              pubsub_tools:publish(Alice, <<"item1">>, Node, []),

              pubsub_tools:subscribe(Bob, Node, []),

              %% mod_pubsub:broadcast_step/1 ensures that a publish notification for a new item
              %% would always arrive before a retraction notification for an old item
              pubsub_tools:publish(Alice, <<"item2">>, Node, []),
              pubsub_tools:receive_item_notification(Bob, <<"item2">>, Node, []),
              verify_item_retract(Node, <<"item1">>, escalus:wait_for_stanza(Bob)),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

publish_with_existing_id_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              %% Auto-create enabled by default

              %% Request:  7.1.1 Ex.99  publish an item with an ItemID
              %% Response: 7.1.2 Ex.100 success
              Node = pubsub_node(),
              pubsub_tools:publish(Alice, <<"item1">>, Node, []),

              pubsub_tools:get_item(Alice, Node, <<"item1">>, [{expected_result, [<<"item1">>]}]),

              %% Publish an item with the same id in order to update it
              NewEl = #xmlel{name = <<"entry">>, children = [#xmlel{name = <<"new_entry">>}]},
              pubsub_tools:publish(Alice, <<"item1">>, Node, [{with_payload, NewEl}]),
              pubsub_tools:get_item(Alice, Node, <<"item1">>, [{expected_result,
                                                                [#{id => <<"item1">>,
                                                                   entry => NewEl}]}]),


              pubsub_tools:delete_node(Alice, Node, [])

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

              pubsub_tools:create_node(Alice, Node, [{config, NodeConfig}]),
              pubsub_tools:subscribe(Bob1, Node, []),
              pubsub_tools:subscribe(Geralt1, Node, [{jid_type, bare}]),
              pubsub_tools:publish(Alice, <<"item1">>, Node, []),

              %% 7.1.2.1 Ex.101 notification with payload
              %%                Note: message has type 'headline' by default

              %% Bob subscribed with resource
              pubsub_tools:receive_item_notification(Bob1, <<"item1">>, Node, []),
              escalus_assert:has_no_stanzas(Bob2),

              %% Geralt subscribed without resource
              pubsub_tools:receive_item_notification(Geralt1, <<"item1">>, Node, []),
              pubsub_tools:receive_item_notification(Geralt2, <<"item1">>, Node, []),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

request_all_items_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),
              pubsub_tools:publish(Alice, <<"item1">>, Node, []),
              pubsub_tools:publish(Alice, <<"item2">>, Node, []),

              %% Request:  6.5.2 Ex.78 subscriber requests all items
              %% Response: 6.5.3 Ex.79 service returns all items
              pubsub_tools:get_all_items(Bob, Node,
                                         [{expected_result, [<<"item2">>, <<"item1">>]}]),
              %% TODO check ordering (although XEP does not specify this)

              pubsub_tools:delete_node(Alice, Node, [])
      end).

request_particular_item_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),
              pubsub_tools:publish(Alice, <<"item1">>, Node, []),
              pubsub_tools:publish(Alice, <<"item2">>, Node, []),

              %% Request:  6.5.8 Ex.78 subscriber requests a particular items
              pubsub_tools:get_item(Bob, Node, <<"item1">>, [{expected_result, [<<"item1">>]}]),

              pubsub_tools:delete_node(Alice, Node, [])

      end).

retract_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),
              pubsub_tools:publish(Alice, <<"item1">>, Node, []),
              pubsub_tools:publish(Alice, <<"item2">>, Node, []),

              %% Request:  7.2.1 Ex.115 Entity deletes an item from a node
              %% Response: 7.2.2 Ex.116 Service replies with success
              pubsub_tools:retract_item(Alice, Node, <<"item1">>, []),
              pubsub_tools:get_all_items(Bob, Node, [{expected_result, [<<"item2">>]}]),

              %% Request:  7.2.1 Ex.115 Entity deletes an item from a node
              %% Response: 7.2.2 Ex.116 Service replies with success
              %% Notification: 7.2.2.1 Ex.117 Subscribers are notified of deletion
              pubsub_tools:set_configuration(Alice, Node,
                                             [{<<"pubsub#notify_retract">>, <<"1">>}], []),
              pubsub_tools:subscribe(Bob, Node, []),
              pubsub_tools:retract_item(Alice, Node, <<"item2">>, []),
              verify_item_retract(Node, <<"item2">>, escalus:wait_for_stanza(Bob)),
              pubsub_tools:get_all_items(Bob, Node, [{expected_result, []}]),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

retract_when_user_goes_offline_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              NodeConfig = [{<<"pubsub#purge_offline">>, <<"1">>},
                            {<<"pubsub#publish_model">>, <<"open">>}],
              pubsub_tools:create_node(Alice, Node, [{config, NodeConfig}]),

              pubsub_tools:publish(Alice, <<"item1">>, Node, []),
              pubsub_tools:publish(Bob, <<"item2">>, Node, []),
              pubsub_tools:get_all_items(Alice, Node,
                                         [{expected_result, [<<"item2">>, <<"item1">>]}]),

              mongoose_helper:logout_user(Config, Bob),

              pubsub_tools:get_all_items(Alice, Node, [{expected_result, [<<"item1">>]}]),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

purge_all_items_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),
              pubsub_tools:publish(Alice, <<"item1">>, Node, []),
              pubsub_tools:publish(Alice, <<"item2">>, Node, []),

              %% Response: 8.5.3.2 Ex.165 insufficient privileges
              pubsub_tools:purge_all_items(Bob, Node, [{expected_error_type, <<"auth">>}]),

              pubsub_tools:get_all_items(Bob, Node,
                                         [{expected_result, [<<"item2">>, <<"item1">>]}]),

              %% Request:  8.5.1 Ex.161 owner purges all items from node
              %% Response: 8.5.2 Ex.162 success
              pubsub_tools:purge_all_items(Alice, Node, []),

              pubsub_tools:get_all_items(Bob, Node, [{expected_result, []}]),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

publish_only_retract_items_scope_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
                Node = pubsub_node(),
                pubsub_tools:create_node(Alice, Node, []),
                AffChange = [{Bob, <<"publish-only">>}],
                pubsub_tools:set_affiliations(Alice, Node, AffChange, []),


                pubsub_tools:publish(Bob, <<"item1">>, Node, []),
                pubsub_tools:publish(Alice, <<"item2">>, Node, []),

                %% Request:  7.2.1 Ex.116 publish-only sends a retract request for his own item
                %% Response: 7.2.2 Ex.117 success
                pubsub_tools:retract_item(Bob, Node, <<"item1">>, []),

                %% Request:  7.2.1 Ex.116 publish-only sends a retract request for someone's else item
                %% Response: 7.2.3.1 Ex.120 insufficient privileges
                pubsub_tools:retract_item(Bob, Node, <<"item2">>, [{expected_error_type, <<"auth">>}]),
                pubsub_tools:get_all_items(Alice, Node, [{expected_result, [<<"item2">>]}]),

                pubsub_tools:delete_node(Alice, Node, [])
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
              pubsub_tools:create_node(Alice, Node, []),

              pubsub_tools:subscribe(Alice, Node, []),
              IQError = pubsub_tools:subscribe(Bob, Node, [{expected_error_type, <<"cancel">>}]),
              is_not_allowed_and_closed(IQError),

              pubsub_tools:delete_node(Alice, Node, [])
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
              pubsub_tools:create_node(Alice, Node, []),

              NodeConfig = pubsub_tools:get_configuration(Alice, Node, []),
              verify_config_fields(NodeConfig),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

set_configuration_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),

              ValidNodeConfig = node_config_for_test(),
              pubsub_tools:set_configuration(Alice, Node, ValidNodeConfig,
                                             [{response_timeout, 10000}]),
              pubsub_tools:get_configuration(Alice, Node, [{expected_result, ValidNodeConfig}]),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

notify_config_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              pubsub_tools:create_node(
                Alice, Node, [{config, [{<<"pubsub#notify_config">>, <<"1">>}]}]),
              pubsub_tools:subscribe(Bob, Node, []),

              ConfigChange = [{<<"pubsub#title">>, <<"newtitle">>}],
              pubsub_tools:set_configuration(Alice, Node, ConfigChange, []),
              verify_config_event(Node, ConfigChange, escalus:wait_for_stanza(Bob)),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

disable_notifications_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              NodeConfig = [{<<"pubsub#deliver_notifications">>, <<"false">>}],
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, [{config, NodeConfig}]),

              pubsub_tools:subscribe(Bob, Node, []),
              pubsub_tools:publish(Alice, <<"item1">>, Node, []),

              %% Notifications disabled
              escalus_assert:has_no_stanzas(Bob),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

disable_payload_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% Notification-Only Persistent Node, see 4.3, table 4
              NodeConfig = [{<<"pubsub#deliver_payloads">>, <<"false">>}],
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, [{config, NodeConfig}]),

              pubsub_tools:subscribe(Bob, Node, []),
              pubsub_tools:publish(Alice, <<"item1">>, Node, []),

              %% Payloads disabled
              pubsub_tools:receive_item_notification(Bob, <<"item1">>,
                                                     Node, [{with_payload, false}]),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

disable_persist_items_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% Payload-Included Transient Node, see 4.3, table 4
              NodeConfig = [{<<"pubsub#persist_items">>, <<"false">>}],
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, [{config, NodeConfig}]),

              pubsub_tools:subscribe(Bob, Node, []),
              pubsub_tools:publish(Alice, <<"item1">>, Node, []),

              %% Notifications should work
              pubsub_tools:receive_item_notification(Bob, <<"item1">>, Node, []),

              %% No items should be stored
              pubsub_tools:get_all_items(Bob, Node, [{expected_result, []}]),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

notify_only_available_users_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% Node notifies only available users
              NodeConfig = [{<<"pubsub#presence_based_delivery">>, <<"true">>}],
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, [{config, NodeConfig}]),

              pubsub_tools:subscribe(Bob, Node, [{jid_type, bare}]),

              push_helper:become_unavailable(Bob),

              %% Item from node 2 not received (blocked by resource-based delivery)
              pubsub_tools:publish(Alice, <<"item2">>, Node, []),
              escalus_assert:has_no_stanzas(Bob),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

notify_unavailable_user_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),

              pubsub_tools:subscribe(Bob, Node, [{jid_type, bare}]),

              push_helper:become_unavailable(Bob),

              %% Receive item from node 1 (also make sure the presence is processed)
              pubsub_tools:publish(Alice, <<"item1">>, Node, []),

              escalus_assert:has_no_stanzas(Bob),
              escalus:send(Bob, escalus_stanza:presence(<<"available">>)),
              pubsub_tools:receive_item_notification(Bob, <<"item1">>, Node, []),

              pubsub_tools:delete_node(Alice, Node, [])
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
              pubsub_tools:create_node(Alice, Node, [{config, NodeConfig}]),

              pubsub_tools:publish(Alice, <<"item1">>, Node, []),
              pubsub_tools:publish(Alice, <<"item2">>, Node, []),

              %% Note: when Bob subscribes, the last item (item2) is sent to him
              %%       6.1.7 Ex.50 service sends last published item
              %%       This is sent BEFORE the response iq stanza
              pubsub_tools:subscribe(Bob, Node, [{receive_response, false}]),
              pubsub_tools:receive_item_notification(Bob, <<"item2">>, Node, []),
              pubsub_tools:receive_subscribe_response(Bob, Node, []),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

send_last_published_item_no_items_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              NodeConfig = [{<<"pubsub#send_last_published_item">>, <<"on_sub_and_presence">>}],
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, [{config, NodeConfig}]),

              %% Note: when Bob subscribes, the last item would is sent to him
              pubsub_tools:subscribe(Bob, Node, [{receive_response, false}]),
              escalus_assert:has_no_stanzas(Bob),
              pubsub_tools:delete_node(Alice, Node, [])
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
              pubsub_tools:create_node(Alice, Node, []),

              verify_affiliations(pubsub_tools:get_affiliations(Alice, Node, []),
                                  [{Alice, <<"owner">>}]),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

add_publisher_and_member_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}, {kate, 1}],
      fun(Alice, Bob, Kate) ->
              Node = pubsub_node(),
              NodeConfig = [{<<"pubsub#access_model">>, <<"whitelist">>},
                            {<<"pubsub#publish_model">>, <<"publishers">>}],
              pubsub_tools:create_node(Alice, Node, [{config, NodeConfig}]),

              pubsub_tools:publish(Bob, <<"item1">>, Node, [{expected_error_type, <<"auth">>}]),
              IQError = pubsub_tools:subscribe(Kate, Node, [{expected_error_type, <<"cancel">>}]),
              is_not_allowed_and_closed(IQError),

              AffChange = [{Bob, <<"publisher">>}, {Kate, <<"member">>}],
              pubsub_tools:set_affiliations(Alice, Node, AffChange, []),

              pubsub_tools:publish(Kate, <<"nope">>, Node, [{expected_error_type, <<"auth">>}]),
              pubsub_tools:subscribe(Kate, Node, []),
              pubsub_tools:publish(Bob, <<"item1">>, Node, []),
              pubsub_tools:receive_item_notification(Kate, <<"item1">>, Node, []),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

swap_owners_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),

              AffChange = [{Bob, <<"owner">>}, {Alice, <<"none">>}],
              pubsub_tools:set_affiliations(Alice, Node, AffChange, []),

              pubsub_tools:get_affiliations(Alice, Node, [{expected_error_type, <<"auth">>}]),
              verify_affiliations(pubsub_tools:get_affiliations(Bob, Node, []),
                                  [{Bob, <<"owner">>}]),

              pubsub_tools:delete_node(Bob, Node, [])
      end).

deny_no_owner_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),

              AffChange = [{Alice, <<"member">>}],
              IQError = pubsub_tools:set_affiliations(Alice, Node, AffChange,
                                                      [{expected_error_type, <<"modify">>}]),
              verify_returned_affiliation(IQError, Alice, <<"owner">>),

              verify_affiliations(pubsub_tools:get_affiliations(Alice, Node, []),
                                  [{Alice, <<"owner">>}]),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

%%--------------------------------------------------------------------
%% Subscriptions management
%%--------------------------------------------------------------------

retrieve_user_subscriptions_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% Request:  5.6 Ex.20 Retrieve Subscriptions
              %% Response:     Ex.22 No Subscriptions
              pubsub_tools:get_user_subscriptions(Bob, node_addr(), [{expected_result, []}]),

              {_, NodeName} = Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),
              pubsub_tools:subscribe(Bob, Node, []),

              %% Ex. 21 Service returns subscriptions
              Sub = [{NodeName, <<"subscribed">>}],
              pubsub_tools:get_user_subscriptions(Bob, node_addr(), [{expected_result, Sub}]),

              {_, NodeName2} = Node2 = pubsub_node(),
              pubsub_tools:create_node(Alice, Node2, []),
              pubsub_tools:subscribe(Bob, Node2, []),

              %% Ex. 21 Service returns subscriptions
              Subs = [{NodeName, <<"subscribed">>}, {NodeName2, <<"subscribed">>}],
              pubsub_tools:get_user_subscriptions(Bob, node_addr(), [{expected_result, Subs}]),

              %% Owner not subscribed automatically
              pubsub_tools:get_user_subscriptions(Alice, node_addr(), [{expected_result, []}]),

              pubsub_tools:delete_node(Alice, Node, []),
              pubsub_tools:delete_node(Alice, Node2, [])
      end).

retrieve_node_subscriptions_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}, {geralt, 1}],
      fun(Alice, Bob, Geralt) ->
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),

              %% Request:  8.8.1.1 Ex.182 Owner requests all subscriptions
              %% Response: 8.8.1.2 Ex.183 Service returns list of subscriptions (empty yet)
              pubsub_tools:get_node_subscriptions(Alice, Node, [{expected_result, []}]),

              %% Response: 8.8.1.3 Ex.185 Entity is not an owner
              pubsub_tools:get_node_subscriptions(Bob, Node, [{expected_error_type, <<"auth">>}]),

              pubsub_tools:subscribe(Bob, Node, []),
              pubsub_tools:subscribe(Geralt, Node, [{jid_type, bare}]),

              NodeSubs = [{Bob, full, <<"subscribed">>}, {Geralt, bare, <<"subscribed">>}],
              pubsub_tools:get_node_subscriptions(Alice, Node, [{expected_result, NodeSubs}]),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

modify_node_subscriptions_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}, {geralt, 1}],
      fun(Alice, Bob, Geralt) ->
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),

              %% Request:  8.8.2.1 Ex.187 Owner modifies subscriptions
              %% Response: 8.8.2.2 Ex.183 Service responds with success
              pubsub_tools:modify_node_subscriptions(
                Alice, [{Bob, full, <<"subscribed">>},
                        {Geralt, bare, <<"subscribed">>}], Node, []),

              %% 8.8.4 Ex.194 Notify subscribers
              pubsub_tools:receive_subscription_notification(Bob, <<"subscribed">>, Node, []),
              pubsub_tools:receive_subscription_notification(Geralt, <<"subscribed">>,
                                                             Node, [{jid_type, bare}]),

              Subs = [{Bob, full, <<"subscribed">>}, {Geralt, bare, <<"subscribed">>}],
              pubsub_tools:get_node_subscriptions(Alice, Node, [{expected_result, Subs}]),

              %% Response: 8.8.2.3 Ex.190 Entity is not an owner
              pubsub_tools:modify_node_subscriptions(Bob, [{Geralt, full, <<"subscribed">>}], Node,
                                                     [{expected_error_type, <<"auth">>}]),

              %% Remove Bob, add Geralt's full JID
              pubsub_tools:modify_node_subscriptions(
                Alice, [{Bob, full, <<"none">>},
                        {Geralt, full, <<"subscribed">>}], Node, []),

              pubsub_tools:receive_subscription_notification(Bob, <<"none">>, Node, []),
              pubsub_tools:receive_subscription_notification(Geralt, <<"subscribed">>, Node, []),

              ModSubs = [{Geralt, bare, <<"subscribed">>}, {Geralt, full, <<"subscribed">>}],
              pubsub_tools:get_node_subscriptions(Alice, Node, [{expected_result, ModSubs}]),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

process_subscription_requests_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}, {kate, 1}],
      fun(Alice, Bob, Kate) ->
              Node = pubsub_node(),
              NodeConfig = [{<<"pubsub#access_model">>, <<"authorize">>}],
              pubsub_tools:create_node(Alice, Node, [{config, NodeConfig}]),

              pubsub_tools:subscribe(Bob, Node, [{subscription, <<"pending">>}]),
              BobsRequest = pubsub_tools:receive_subscription_request(Alice, Bob, Node, []),
              pubsub_tools:subscribe(Kate, Node, [{subscription, <<"pending">>}]),
              KatesRequest = pubsub_tools:receive_subscription_request(Alice, Kate, Node, []),

              pubsub_tools:submit_subscription_response(Alice, BobsRequest, Node, true, []),
              pubsub_tools:receive_subscription_notification(Bob, <<"subscribed">>, Node, []),
              pubsub_tools:submit_subscription_response(Alice, KatesRequest, Node, false, []),
              pubsub_tools:receive_subscription_notification(Kate, <<"none">>, Node, []),

              pubsub_tools:publish(Alice, <<"item1">>, Node, []),
              pubsub_tools:receive_item_notification(Bob, <<"item1">>, Node, []),
              [] = escalus:peek_stanzas(Kate),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

retrieve_pending_subscription_requests_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}, {kate, 1}],
      fun(Alice, Bob, Kate) ->
              {NodeAddr, NodeName} = Node = pubsub_node(),
              NodeConfig = [{<<"pubsub#access_model">>, <<"authorize">>}],
              pubsub_tools:create_node(Alice, Node, [{config, NodeConfig}]),

              pubsub_tools:subscribe(Bob, Node, [{subscription, <<"pending">>}]),
              pubsub_tools:receive_subscription_request(Alice, Bob, Node, []),
              pubsub_tools:subscribe(Kate, Node, [{subscription, <<"pending">>}]),
              pubsub_tools:receive_subscription_request(Alice, Kate, Node, []),

              pubsub_tools:get_pending_subscriptions(Alice, NodeAddr, [NodeName], []),

              %% TODO: XEP requires IQ result to come before the requests
              Request = pubsub_tools:get_pending_subscriptions(Alice, Node,
                                                               [{receive_response, false}]),
              pubsub_tools:receive_subscription_requests(Alice, [Bob, Kate], Node, []),
              IQRes = escalus:wait_for_stanza(Alice),
              escalus:assert(is_iq_result, [Request], IQRes),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

%%--------------------------------------------------------------------
%% Test cases for XEP-0248
%% Comments in test cases refer to sections is the XEP
%%--------------------------------------------------------------------

pubsub_leaf_name() -> pubsub_tools:rand_name(<<"leaf">>).
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
              pubsub_tools:create_node(Alice, Node, [{config, CollectionConfig}]),

              %% Request:  7.3.1 Ex.30 delete collection node
              %% Response: 7.3.2 Ex.31 success
              pubsub_tools:delete_node(Alice, Node, [])
      end).

subscribe_unsubscribe_collection_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, [{config, CollectionConfig}]),

              %% Request:  6.1.1 Ex.10 subscribe (no configuration)
              %% Response: 6.1.2 Ex.12 success
              pubsub_tools:subscribe(Bob, Node, []),

              %% Same as XEP-0060
              pubsub_tools:unsubscribe(Bob, Node, []),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

collection_delete_makes_leaf_parentless(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, [{config, CollectionConfig}]),

              %% XEP-0060, 8.1.2, see 16.4.4 for config details
              NodeConfig = [{<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              pubsub_tools:create_node(Alice, Leaf, [{config, NodeConfig}]),

              pubsub_tools:delete_node(Alice, Node, []),

              % Leaf becomes an orphan
              NewNodeConfig = pubsub_tools:get_configuration(Alice, Leaf, []),
              {_, _, []} = lists:keyfind(<<"pubsub#collection">>, 1, NewNodeConfig)
      end).

notify_collection_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, [{config, CollectionConfig}]),

              NodeConfig = [{<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              pubsub_tools:create_node(Alice, Leaf, [{config, NodeConfig}]),
              Leaf2 = pubsub_leaf(),
              pubsub_tools:create_node(Alice, Leaf2, [{config, NodeConfig}]),
              pubsub_tools:subscribe(Bob, Node, []),

              %% Publish to leaf nodes, Bob should get notifications
              %% 5.3.1.1 Ex.5 Subscriber receives a publish notification from a collection
              pubsub_tools:publish(Alice, <<"item1">>, Leaf, []),
              pubsub_tools:receive_item_notification(Bob, <<"item1">>, Leaf, [{collection, Node}]),
              pubsub_tools:publish(Alice, <<"item2">>, Leaf2, []),
              pubsub_tools:receive_item_notification(Bob, <<"item2">>, Leaf2, [{collection, Node}]),

              pubsub_tools:delete_node(Alice, Leaf, []),
              pubsub_tools:delete_node(Alice, Leaf2, []),
              pubsub_tools:delete_node(Alice, Node, [])
      end).

notify_collection_leaf_and_item_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, [{config, CollectionConfig}]),

              %% Subscribe before creating the leaf node
              pubsub_tools:subscribe(Bob, Node, []),
              NodeConfig = [{<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              pubsub_tools:create_node(Alice, Leaf, [{config, NodeConfig}]),

              %% Bob should get a notification for the leaf node creation
              %% 5.3.1.2 Ex.6 Subscriber receives a creation notification from a collection
              pubsub_tools:receive_node_creation_notification(Bob, Leaf, []),

              %% Publish to leaf node, Bob should get notified
              pubsub_tools:publish(Alice, <<"item1">>, Leaf, []),
              pubsub_tools:receive_item_notification(Bob, <<"item1">>, Leaf, [{collection, Node}]),

              pubsub_tools:delete_node(Alice, Leaf, []),
              pubsub_tools:delete_node(Alice, Node, [])
      end).

notify_collection_bare_jid_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 2}, {geralt, 2}],
      fun(Alice, Bob1, Bob2, Geralt1, Geralt2) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, [{config, CollectionConfig}]),

              NodeConfig = [{<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              pubsub_tools:create_node(Alice, Leaf, [{config, NodeConfig}]),
              pubsub_tools:subscribe(Bob1, Node, []),
              pubsub_tools:subscribe(Geralt1, Node, [{jid_type, bare}]),
              pubsub_tools:publish(Alice, <<"item1">>, Leaf, []),

              %% Bob subscribed with resource
              pubsub_tools:receive_item_notification(Bob1, <<"item1">>, Leaf, [{collection, Node}]),
              escalus_assert:has_no_stanzas(Bob2),

              %% Geralt subscribed without resource
              pubsub_tools:receive_item_notification(Geralt1, <<"item1">>, Leaf,
                                                     [{collection, Node}]),
              pubsub_tools:receive_item_notification(Geralt2, <<"item1">>, Leaf,
                                                     [{collection, Node}]),

              pubsub_tools:delete_node(Alice, Leaf, []),
              pubsub_tools:delete_node(Alice, Node, [])
      end).

notify_collection_and_leaf_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}, {geralt, 1}],
      fun(Alice, Bob, Geralt) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, [{config, CollectionConfig}]),

              NodeConfig = [{<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              pubsub_tools:create_node(Alice, Leaf, [{config, NodeConfig}]),
              pubsub_tools:subscribe(Bob, Node, []),
              pubsub_tools:subscribe(Geralt, Leaf, []),

              %% Publish to leaf nodes, Bob and Geralt should get notifications
              pubsub_tools:publish(Alice, <<"item1">>, Leaf, []),
              pubsub_tools:receive_item_notification(Bob, <<"item1">>, Leaf,
                                                     [{collection, Node}]),
              pubsub_tools:receive_item_notification(Geralt, <<"item1">>, Leaf,
                                                     [no_collection_shim]),

              pubsub_tools:delete_node(Alice, Leaf, []),
              pubsub_tools:delete_node(Alice, Node, [])
      end).

notify_collection_and_leaf_same_user_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, [{config, CollectionConfig}]),

              NodeConfig = [{<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              pubsub_tools:create_node(Alice, Leaf, [{config, NodeConfig}]),
              pubsub_tools:subscribe(Bob, Node, []),
              pubsub_tools:subscribe(Bob, Leaf, []),

              %% Bob should get only one notification
              pubsub_tools:publish(Alice, <<"item1">>, Leaf, []),
              pubsub_tools:receive_item_notification(Bob, <<"item1">>, Leaf, [{collection, Node}]),
              escalus_assert:has_no_stanzas(Bob),

              pubsub_tools:delete_node(Alice, Leaf, []),
              pubsub_tools:delete_node(Alice, Node, [])
      end).

notify_collections_with_same_leaf_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}, {geralt, 1}],
      fun(Alice, Bob, Geralt) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, CollectionName1} = Collection1 = pubsub_node(),
              pubsub_tools:create_node(Alice, Collection1, [{config, CollectionConfig}]),
              {_, CollectionName2} = Collection2 = pubsub_node(),
              pubsub_tools:create_node(Alice, Collection2, [{config, CollectionConfig}]),

              LeafConfig = [{<<"pubsub#collection">>, <<"text-multi">>,
                             [CollectionName1, CollectionName2]}],
              Leaf = pubsub_leaf(),
              pubsub_tools:create_node(Alice, Leaf, [{config, LeafConfig}]),
              pubsub_tools:subscribe(Bob, Collection1, []),
              pubsub_tools:subscribe(Geralt, Collection2, []),

              %% Publish to leaf node, Bob and Geralt should get notifications
              pubsub_tools:publish(Alice, <<"item1">>, Leaf, []),
              pubsub_tools:receive_item_notification(Bob, <<"item1">>, Leaf,
                                                     [{collection, Collection1}]),
              pubsub_tools:receive_item_notification(Geralt, <<"item1">>, Leaf,
                                                     [{collection, Collection2}]),

              pubsub_tools:delete_node(Alice, Leaf, []),
              pubsub_tools:delete_node(Alice, Collection1, []),
              pubsub_tools:delete_node(Alice, Collection2, [])
      end).

notify_nested_collections_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}, {geralt, 1}],
      fun(Alice, Bob, Geralt) ->
              TopCollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, TopCollectionName} = TopCollection = pubsub_node(),
              pubsub_tools:create_node(Alice, TopCollection, [{config, TopCollectionConfig}]),

              MiddleCollectionConfig = [
                                        {<<"pubsub#node_type">>, <<"collection">>},
                                        {<<"pubsub#collection">>, TopCollectionName}
                                       ],
              {_, MiddleCollectionName} = MiddleCollection = pubsub_node(),
              pubsub_tools:create_node(Alice, MiddleCollection, [{config, MiddleCollectionConfig}]),

              LeafConfig = [{<<"pubsub#collection">>, MiddleCollectionName}],
              Leaf = pubsub_leaf(),
              pubsub_tools:create_node(Alice, Leaf, [{config, LeafConfig}]),
              pubsub_tools:subscribe(Bob, MiddleCollection, []),
              pubsub_tools:subscribe(Geralt, TopCollection, []),

              %% Publish to leaf node, Bob and Geralt should get notifications
              pubsub_tools:publish(Alice, <<"item1">>, Leaf, []),
              pubsub_tools:receive_item_notification(Bob, <<"item1">>, Leaf,
                                                     [{collection, MiddleCollection}]),
              pubsub_tools:receive_item_notification(Geralt, <<"item1">>, Leaf,
                                                     [{collection, TopCollection}]),

              pubsub_tools:delete_node(Alice, Leaf, []),
              pubsub_tools:delete_node(Alice, MiddleCollection, []),
              pubsub_tools:delete_node(Alice, TopCollection, [])
      end).
 
retrieve_subscriptions_collection_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, [{config, CollectionConfig}]),

              NodeConfig = [{<<"pubsub#collection">>, NodeName}],
              {_, LeafName} = Leaf = pubsub_leaf(),
              pubsub_tools:create_node(Alice, Leaf, [{config, NodeConfig}]),
              Leaf2 = pubsub_leaf(),
              pubsub_tools:create_node(Alice, Leaf2, [{config, NodeConfig}]),
              pubsub_tools:subscribe(Bob, Node, []),
              pubsub_tools:subscribe(Bob, Leaf, []),

              % Only the nodes for which subscriptions were made should be returned
              Subs = [{LeafName, <<"subscribed">>}, {NodeName, <<"subscribed">>}],
              pubsub_tools:get_user_subscriptions(Bob, node_addr(), [{expected_result, Subs}]),

              pubsub_tools:delete_node(Alice, Leaf, []),
              pubsub_tools:delete_node(Alice, Leaf2, []),
              pubsub_tools:delete_node(Alice, Node, [])
      end).

discover_top_level_nodes_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              % This one is visible at top level

              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, [{config, CollectionConfig}]),

              % This one is not

              LeafConfig = [{<<"pubsub#collection">>, NodeName}],
              {_, LeafName} = Leaf = pubsub_leaf(),
              pubsub_tools:create_node(Alice, Leaf, [{config, LeafConfig}]),

              % This one is visible, as it is not associated with any collection
              {_, CollectionlessName} = Collectionless = pubsub_node(),
              pubsub_tools:create_node(Alice, Collectionless, []),

              %% Discover top-level nodes, only the collection expected
              pubsub_tools:discover_nodes(Bob, node_addr(),
                                          [{expected_result, [NodeName, CollectionlessName,
                                                              {no, LeafName}]}]),

              pubsub_tools:delete_node(Alice, Leaf, []),
              pubsub_tools:delete_node(Alice, Node, []),
              pubsub_tools:delete_node(Alice, Collectionless, [])
      end).

discover_child_nodes_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% Try to get children of a non-existing node
              {_, NodeName} = Node = pubsub_node(),
              pubsub_tools:discover_nodes(Bob, Node, [{expected_error_type, <<"cancel">>}]),

              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              pubsub_tools:create_node(Alice, Node, [{config, CollectionConfig}]),

              pubsub_tools:discover_nodes(Bob, Node, [{expected_result, [{no, NodeName}]}]),

              NodeConfig = [{<<"pubsub#collection">>, NodeName}],
              {_, LeafName} = Leaf = pubsub_leaf(),
              pubsub_tools:create_node(Alice, Leaf, [{config, NodeConfig}]),
              {_, LeafName2} = Leaf2 = pubsub_leaf(),
              pubsub_tools:create_node(Alice, Leaf2, [{config, NodeConfig}]),

              %% Request:  5.2.1 Ex.11 Entity requests child nodes
              %% Response: 5.2.2 Ex.12 Service returns child nodes
              pubsub_tools:discover_nodes(Bob, Node, [{expected_result, [LeafName, LeafName2]}]),

              pubsub_tools:delete_node(Alice, Leaf, []),
              pubsub_tools:delete_node(Alice, Leaf2, []),
              pubsub_tools:delete_node(Alice, Node, [])
      end).

request_all_items_leaf_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, [{config, CollectionConfig}]),

              NodeConfig = [{<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              pubsub_tools:create_node(Alice, Leaf, [{config, NodeConfig}]),
              Leaf2 = pubsub_leaf(),
              pubsub_tools:create_node(Alice, Leaf2, [{config, NodeConfig}]),

              pubsub_tools:publish(Alice, <<"item1">>, Leaf, []),
              pubsub_tools:publish(Alice, <<"item2">>, Leaf2, []),

              %% Request items from leaf nodes - as described in XEP-0060
              pubsub_tools:get_all_items(Bob, Leaf, [{expected_result, [<<"item1">>]}]),
              pubsub_tools:get_all_items(Bob, Leaf2, [{expected_result, [<<"item2">>]}]),

              %% NOTE: This is not implemented yet
              %% Request:  6.2.1 Ex.15 Subscriber requests all items on a collection
              %% Response: 6.2.2 Ex.16 Service returns items on leaf nodes
              %%pubsub_tools:get_all_items(Bob, Node,
              %%                           [{expected_result, [<<"item2">>, <<"item1">>]}]),

              pubsub_tools:delete_node(Alice, Leaf, []),
              pubsub_tools:delete_node(Alice, Leaf2, []),
              pubsub_tools:delete_node(Alice, Node, [])
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
              pubsub_tools:create_node(Alice, Node, [{config, CollectionConfig}]),

              NodeConfig = [{<<"pubsub#deliver_notifications">>, <<"false">>},
                            {<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              pubsub_tools:create_node(Alice, Leaf, [{config, NodeConfig}]),

              pubsub_tools:subscribe(Bob, Node, []),
              pubsub_tools:publish(Alice, <<"item1">>, Leaf, []),

              %% Notifications disabled
              escalus_assert:has_no_stanzas(Bob),

              pubsub_tools:delete_node(Alice, Leaf, []),
              pubsub_tools:delete_node(Alice, Node, [])
      end).

disable_payload_leaf_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, [{config, CollectionConfig}]),

              NodeConfig = [{<<"pubsub#deliver_payloads">>, <<"false">>},
                            {<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              pubsub_tools:create_node(Alice, Leaf, [{config, NodeConfig}]),

              pubsub_tools:subscribe(Bob, Node, []),
              pubsub_tools:publish(Alice, <<"item1">>, Leaf, []),

              %% Payloads disabled
              pubsub_tools:receive_item_notification(Bob, <<"item1">>, Leaf,
                                                     [{with_payload, false}, {collection, Node}]),

              pubsub_tools:delete_node(Alice, Leaf, []),
              pubsub_tools:delete_node(Alice, Node, [])
      end).

disable_persist_items_leaf_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              CollectionConfig = [{<<"pubsub#node_type">>, <<"collection">>}],
              {_, NodeName} = Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, [{config, CollectionConfig}]),

              NodeConfig = [{<<"pubsub#persist_items">>, <<"false">>},
                            {<<"pubsub#collection">>, NodeName}],
              Leaf = pubsub_leaf(),
              pubsub_tools:create_node(Alice, Leaf, [{config, NodeConfig}]),

              pubsub_tools:subscribe(Bob, Node, []),
              pubsub_tools:publish(Alice, <<"item1">>, Leaf, []),

              %% Notifications should work
              pubsub_tools:receive_item_notification(Bob, <<"item1">>, Leaf, [{collection, Node}]),

              %% No items should be stored
              pubsub_tools:get_all_items(Bob, Leaf, [{expected_result, []}]),

              pubsub_tools:delete_node(Alice, Leaf, []),
              pubsub_tools:delete_node(Alice, Node, [])
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
              pubsub_tools:create_node(Alice, Node, []),
              pubsub_tools:publish(Alice, <<"item1">>, Node, []),
              pubsub_tools:publish(Alice, <<"item2">>, Node, []),

              Items = rpc(mim(), mod_pubsub, get_items, [NodeAddr, NodeName]),
              % We won't bother with importing records etc...
              2 = length(Items),

              {error, _} = rpc(mim(), mod_pubsub, get_items, [NodeAddr, <<"no_such_node_here">>]),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

debug_get_item_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              {NodeAddr, NodeName} = Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),
              pubsub_tools:publish(Alice, <<"item1">>, Node, []),
              pubsub_tools:publish(Alice, <<"item2">>, Node, []),

              Item = rpc(mim(), mod_pubsub, get_item, [NodeAddr, NodeName, <<"item2">>]),
              % We won't bother with importing records etc...
              {<<"item2">>, _} = element(2, Item),

              {error, _} = rpc(mim(), mod_pubsub, get_item, [NodeAddr, NodeName, <<"itemX">>]),

              pubsub_tools:delete_node(Alice, Node, [])
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
              pubsub_tools:create_node(Alice, Node, [{config, NodeConfig}]),

              pubsub_tools:subscribe(Bob, Node, []),

              %% Response  7.1.3 Ex.112 attempt to publish payload to transient notification node
              %%                   Expected error of type 'modify'
              pubsub_tools:publish(Alice, <<"item1">>, Node,
                                   [{expected_error_type, <<"modify">>}]),

              %% Publish without payload should succeed
              pubsub_tools:publish(Alice, <<"item2">>, Node, [{with_payload, false}]),

              %% Notifications should work
              pubsub_tools:receive_item_notification(Bob, <<"item1">>, Node, []),

              %% No items should be stored
              pubsub_tools:get_all_items(Bob, Node, [{expected_result, []}]),

              %% No more notifications
              escalus_assert:has_no_stanzas(Bob),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

disable_delivery_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),

              %% Request: 6.3.7 Ex.71 Subscribe and configure
              %%                Ex.72 Success
              SubscrConfig = [{<<"pubsub#deliver">>, <<"false">>}],
              pubsub_tools:subscribe(Bob, Node, [{config, SubscrConfig}]),

              pubsub_tools:publish(Alice, <<"item1">>, Node, []),

              %% Notifications disabled
              escalus_assert:has_no_stanzas(Bob),

              pubsub_tools:delete_node(Alice, Node, [])
      end).
%%-----------------------------------------------------------------
%% pubsub_item_publisher_option
%%-----------------------------------------------------------------

get_item_with_publisher_option_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),

              pubsub_tools:publish(Alice, <<"item1">>, Node, []),

              PublisherJID =  escalus_utils:jid_to_lower(escalus_client:full_jid(Alice)),
              pubsub_tools:get_item(Alice, Node, <<"item1">>,
                                    [{expected_result, [#{id => <<"item1">>,
                                                          publisher => PublisherJID}]}]),
              pubsub_tools:delete_node(Alice, Node, [])
      end).

receive_item_notification_with_publisher_option_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Node = pubsub_node(),
              pubsub_tools:create_node(Alice, Node, []),

              pubsub_tools:subscribe(Bob, Node, []),
              pubsub_tools:publish(Alice, <<"item1">>, Node, []),


              PublisherJID =  escalus_utils:jid_to_lower(escalus_client:full_jid(Alice)),
              pubsub_tools:receive_item_notification(Bob, #{id => <<"item1">>,
                                                            publisher => PublisherJID}, Node, []),

              pubsub_tools:delete_node(Alice, Node, [])
      end).

%%-----------------------------------------------------------------
%% hometree - specific
%%-----------------------------------------------------------------

can_create_node_with_existing_parent_path(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              {Parent, Node} = path_node_and_parent(Alice, pubsub_node()),
              pubsub_tools:create_node(Alice, Parent, []),
              pubsub_tools:create_node(Alice, Node, []),
              
              pubsub_tools:delete_node(Alice, Node, []),
              pubsub_tools:delete_node(Alice, Parent, [])
      end).

cant_create_node_with_missing_parent_path(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              {_Parent, Node} = path_node_and_parent(Alice, pubsub_node()),
              pubsub_tools:create_node(Alice, Node, [{expected_error_type, <<"auth">>}])
      end).

disco_node_children_by_path_prefix(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% Try to get children of a non-existing node
              {Parent, {_, NodeName} = Node} = path_node_and_parent(Alice, pubsub_node()),
              pubsub_tools:discover_nodes(Bob, Parent, [{expected_error_type, <<"cancel">>}]),

              pubsub_tools:create_node(Alice, Parent, []),
              
              pubsub_tools:discover_nodes(Bob, Parent, [{expected_result, []}]),
              
              pubsub_tools:create_node(Alice, Node, []),

              %% Request:  5.2.1 Ex.11 Entity requests child nodes
              %% Response: 5.2.2 Ex.12 Service returns child nodes
              pubsub_tools:discover_nodes(Bob, Parent, [{expected_result, [NodeName]}]),

              pubsub_tools:delete_node(Alice, Node, []),
              pubsub_tools:delete_node(Alice, Parent, [])
      end).

deleting_parent_path_deletes_children(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              {{_, ParentName} = Parent, {_, NodeName} = Node}
              = path_node_and_parent(Alice, pubsub_node()),

              pubsub_tools:create_node(Alice, Parent, []),
              pubsub_tools:create_node(Alice, Node, []),
              
              pubsub_tools:delete_node(Alice, Parent, []),

              pubsub_tools:discover_nodes(Alice, node_addr(),
                                          [{expected_result, [{no, ParentName}, {no, NodeName}]}]),
              pubsub_tools:discover_nodes(Alice, Node, [{expected_error_type, <<"cancel">>}])
      end).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

path_node_and_parent(Client, {NodeAddr, NodeName}) ->
    %% TODO: Add proper JID stringprepping to escalus!!!
    JID = escalus_ejabberd:rpc(jid, from_binary, [escalus_client:short_jid(Client)]),
    {LUser, LServer, _} = escalus_ejabberd:rpc(jid, to_lower, [JID]),
    Prefix = <<"/home/", LServer/binary, "/", LUser/binary>>,
    {{NodeAddr, Prefix}, {NodeAddr, <<Prefix/binary, "/", NodeName/binary>>}}.

required_modules(ExtraOpts) ->
    [{mod_pubsub, [
                   {backend, mongoose_helper:mnesia_or_rdbms_backend()},
                   {host, "pubsub.@HOST@"}
                   | ExtraOpts
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
    NodeAddr = exml_query:attr(Stanza, <<"from">>),

    [#xmlel{ attrs = [{<<"xmlns">>, ?NS_PUBSUB_EVENT}] } = Event]
    = exml_query:subelements(Stanza, <<"event">>),

    [#xmlel{ attrs = [{<<"node">>, NodeName}] } = Items]
    = exml_query:subelements(Event, <<"items">>),

    [#xmlel{ attrs = [{<<"id">>, ItemId}] }] = exml_query:subelements(Items, <<"retract">>).

verify_config_event({NodeAddr, NodeName}, ConfigChange, Stanza) ->
    escalus:assert(is_message, Stanza),
    NodeAddr = exml_query:attr(Stanza, <<"from">>),

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
    true = rpc(mim(), ets, insert, [service_tab_name(Host), {Key, Val}]).

lookup_service_option(Host, Key) ->
    [{_, Val}] = rpc(mim(), ets, lookup, [service_tab_name(Host), Key]),
    Val.

service_tab_name(Host) ->
    rpc(mim(), gen_mod, get_module_proc, [Host, config]).

