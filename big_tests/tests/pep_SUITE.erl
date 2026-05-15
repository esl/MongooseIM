-module(pep_SUITE).
-moduledoc "PEP (XEP-0163) tests for mod_pubsub".

-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile([export_all, nowarn_export_all]).

-import(config_parser_helper, [default_mod_config/1]).
-import(distributed_helper, [require_rpc_nodes/1]).
-import(domain_helper, [host_type/0]).
-import(pubsub_tools, [check_item_notification/4,
                       create_node/3, delete_node/3,
                       get_all_items/3, get_configuration/3, get_item/4, get_items/4,
                       item_content/0, item_el/2,
                       publish/4, publish_raw/4, publish_with_options/5, published_item_id/2,
                       receive_item_notification/4, receive_node_deletion_notification/3,
                       send_generic_request/6,
                       set_configuration/4,
                       subscribe/3, unsubscribe/3]).

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

init_per_suite(Config) ->
    maybe
        ok ?= caps_helper:check_backend(),
        escalus:init_per_suite(dynamic_modules:save_modules(host_type(), Config))
    end.

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(GroupName, Config) ->
    Config1 = dynamic_modules:save_modules(host_type(), Config),
    dynamic_modules:ensure_modules(host_type(), required_modules(GroupName)),
    Config1.

end_per_group(_GroupName, Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config).

init_per_testcase(TestName, Config) ->
    escalus:init_per_testcase(TestName, Config).

end_per_testcase(TestName, Config) ->
    escalus:end_per_testcase(TestName, Config).

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

all() ->
    [{group, pep},
     {group, pep_no_caps}].

groups() ->
    [{pep, [parallel], pep_tests()},
     {pep_no_caps, [parallel], pep_no_caps_tests()}].

pep_tests() ->
    disco_tests() ++ protocol_tests() ++ protocol_tests_with_sm().

pep_no_caps_tests() ->
    [disco_info_no_caps,
     create_presence_and_publish_implicit_sub_no_caps].

-doc "Tests for XEP-0030 Service Discovery with rules from XEP-0163 / XEP-0060".
disco_tests() ->
    [disco_info,
     disco_info_node,
     disco_items,
     disco_items_node,
     disco_items_open].

-doc "PEP tests for the XEP-0163 / XEP-0060 protocol".
protocol_tests() ->
    basic_tests() ++ negative_tests().

basic_tests() ->
    [create_and_delete,
     create_with_empty_config,
     create_and_configure,
     create_open_and_publish_both_subs,
     create_open_and_publish_explicit_sub,
     create_open_and_publish_implicit_sub,
     create_open_and_publish_no_sub,
     create_presence_and_publish_explicit_sub,
     create_presence_and_publish_implicit_sub,
     create_presence_and_publish_no_sub,
     publish_and_get_items,
     publish_implicit_sub,
     publish_open_explicit_sub,
     publish_self_notify,
     send_last_item_on_caps,
     send_last_item_on_implicit_sub].

negative_tests() ->
    [bad_request_fails,
     configure_with_invalid_form_fails,
     create_at_foreign_jid_fails,
     create_duplicate_node_fails,
     create_with_invalid_config_fails,
     delete_nonexistent_node_fails,
     get_item_without_id_fails,
     manage_nonexistent_node_fails,
     manage_foreign_node_fails,
     publish_to_foreign_node_fails,
     publish_with_invalid_items_fails,
     publish_options_invalid_config_fails,
     request_without_nodeid_fails,
     subscribe_presence_required_fails,
     subscribe_to_nonexistent_node_fails,
     subscribe_with_invalid_jid_fails,
     unsubscribe_implicit_fails,
     unsubscribe_foreign_jid_fails].

-doc "Tests ensuring that PEP works with XEP-0198 Stream Management enabled".
protocol_tests_with_sm() ->
    [send_last_item_on_implicit_sub_with_sm]. % check for regressions in a typical scenario

%% Disco

disco_info(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun disco_info_story/1).

disco_info_node(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun disco_info_node_story/3).

disco_items(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun disco_items_story/3).

disco_items_node(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun disco_items_node_story/3).

disco_items_open(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun disco_items_open_story/3).

%% Basic cases

create_and_delete(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun create_and_delete_story/2).

create_open_and_publish_both_subs(Config) ->
    escalus:fresh_story_with_config(set_caps(Config), [{alice, 1}, {bob, 1}],
                                    fun create_open_and_publish_both_subs_story/3).

create_open_and_publish_explicit_sub(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}],
                        fun create_open_and_publish_explicit_sub_story/2).

create_open_and_publish_implicit_sub(Config) ->
    escalus:fresh_story_with_config(set_caps(Config), [{alice, 1}, {bob, 1}],
                                    fun create_open_and_publish_implicit_sub_story/3).

create_open_and_publish_no_sub(Config) ->
    escalus:fresh_story_with_config(set_caps(Config), [{alice, 1}, {bob, 1}, {mike, 1}],
                                    fun create_open_and_publish_no_sub_story/4).

create_presence_and_publish_explicit_sub(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}],
                        fun create_presence_and_publish_explicit_sub_story/2).

create_presence_and_publish_implicit_sub(Config) ->
    escalus:fresh_story_with_config(set_caps(Config), [{alice, 1}, {bob, 1}],
                                    fun create_presence_and_publish_implicit_sub_story/3).

create_presence_and_publish_no_sub(Config) ->
    escalus:fresh_story_with_config(set_caps(Config), [{alice, 1}, {bob, 1}, {mike, 1}],
                                    fun create_presence_and_publish_no_sub_story/4).

create_with_empty_config(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun create_with_empty_config_story/1).

create_and_configure(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun create_and_configure_story/1).

publish_and_get_items(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun publish_and_get_items_story/1).

publish_implicit_sub(Config) ->
    escalus:fresh_story_with_config(set_caps(Config), [{alice, 1}, {bob, 1}],
                                    fun publish_implicit_sub_story/3).

publish_open_explicit_sub(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun publish_open_explicit_sub_story/2).

publish_self_notify(Config) ->
    escalus:fresh_story_with_config(set_caps(Config), [{bob, 1}], fun publish_self_notify_story/2).

send_last_item_on_caps(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun send_last_item_on_caps_story/2).

send_last_item_on_implicit_sub(Config) ->
    escalus:fresh_story_with_config(set_caps(Config), [{alice, 1}, {bob, 1}],
                                    fun send_last_item_on_implicit_sub_story/3).

%% Negative cases

bad_request_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun bad_request_fails_story/1).

configure_with_invalid_form_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun configure_with_invalid_form_fails_story/1).

create_at_foreign_jid_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun create_at_foreign_jid_fails_story/2).

create_duplicate_node_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun create_duplicate_node_fails_story/1).

create_with_invalid_config_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun create_with_invalid_config_fails_story/1).

delete_nonexistent_node_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun delete_nonexistent_node_fails_story/1).

get_item_without_id_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun get_item_without_id_fails_story/1).

manage_nonexistent_node_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun manage_nonexistent_node_fails_story/1).

manage_foreign_node_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun manage_foreign_node_fails_story/2).

publish_to_foreign_node_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun publish_to_foreign_node_fails_story/2).

publish_with_invalid_items_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun publish_with_invalid_items_fails_story/1).

publish_options_invalid_config_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun publish_options_invalid_config_fails_story/1).

request_without_nodeid_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun request_without_nodeid_fails_story/1).

subscribe_presence_required_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}],
                        fun subscribe_presence_required_fails_story/2).

subscribe_to_nonexistent_node_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun subscribe_to_nonexistent_node_fails_story/1).

subscribe_with_invalid_jid_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}],
                        fun subscribe_with_invalid_jid_fails_story/2).

unsubscribe_implicit_fails(Config) ->
    escalus:fresh_story_with_config(set_caps(Config), [{alice, 1}, {bob, 1}],
                                    fun unsubscribe_implicit_fails_story/3).

unsubscribe_foreign_jid_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun unsubscribe_foreign_jid_fails_story/2).

%% Stream management

send_last_item_on_implicit_sub_with_sm(ConfigIn) ->
    Config0 = escalus_users:update_userspec(ConfigIn, alice, stream_management, true),
    Config1 = escalus_users:update_userspec(Config0, bob, stream_management, true),
    Config2 = set_caps(Config1),
    escalus:fresh_story_with_config(Config2, [{alice, 1}, {bob, 1}],
                                    fun send_last_item_on_implicit_sub_story/3).

%% No mod_caps

disco_info_no_caps(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun disco_info_no_caps_story/1).

create_presence_and_publish_implicit_sub_no_caps(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}],
                        fun create_presence_and_publish_implicit_sub_no_caps_story/2).

%%--------------------------------------------------------------------
%% Stories
%%--------------------------------------------------------------------

%% Disco

disco_info_story(Alice) ->
    %% XEP-0163 6.1 Account Owner Service Discovery
    assert_disco_info(Alice, escalus_client:short_jid(Alice), true),

    %% XEP-0030 8 Security Considerations
    assert_no_disco_info(Alice, nonexistent_bare_jid()).

disco_info_node_story(Alice, Bob, Kate) ->
    NodeNS = random_node_ns(),
    AliceJid = escalus_client:short_jid(Alice),
    publish(Alice, ~"item1", pep_node(Alice, NodeNS), []),
    set_up_presence_subscription(Bob, Alice, 0),

    %% XEP-0163 6.2 Contact Service Discovery
    assert_disco_info_node(Alice, AliceJid, NodeNS),
    assert_disco_info_node(Bob, AliceJid, NodeNS),

    %% XEP-0030 7 Error Conditions
    assert_no_disco_info_node(Alice, AliceJid, random_node_ns(), ~"item-not-found"),

    %% XEP-0030 8 Security Considerations
    assert_no_disco_info_node(Kate, AliceJid, NodeNS, ~"service-unavailable").

disco_items_story(Alice, Bob, Kate) ->
    NodeNS = random_node_ns(),
    AliceJid = escalus_client:short_jid(Alice),
    publish(Alice, ~"item1", pep_node(Alice, NodeNS), []),
    set_up_presence_subscription(Bob, Alice, 0),

    %% XEP-0163 6.2 Contact Service Discovery
    assert_disco_items(Alice, AliceJid, NodeNS),
    assert_disco_items(Bob, AliceJid, NodeNS),
    assert_no_disco_items(Kate, AliceJid),

    %% XEP-0030 8 Security Considerations
    assert_no_disco_items(Alice, nonexistent_bare_jid()).

disco_items_node_story(Alice, Bob, Kate) ->
    NodeNS = random_node_ns(),
    AliceJid = escalus_client:short_jid(Alice),
    publish(Alice, ~"item1", pep_node(Alice, NodeNS), []),
    set_up_presence_subscription(Bob, Alice, 0),

    %% XEP-0030 7 Error Conditions
    assert_no_disco_items_node(Alice, AliceJid, NodeNS, ~"item-not-found"),
    assert_no_disco_items_node(Bob, AliceJid, NodeNS, ~"item-not-found"),

    %% XEP-0030 8 Security Considerations
    assert_no_disco_items_node(Kate, AliceJid, NodeNS, ~"service-unavailable").

disco_items_open_story(Alice, Bob, Kate) ->
    NodeNS = random_node_ns(),
    AliceJid = escalus_client:short_jid(Alice),
    PepNode = pep_node(Alice, NodeNS),
    create_node(Alice, PepNode, [{config, [{~"pubsub#access_model", ~"open"}]}]),
    publish(Alice, ~"item1", PepNode, []),
    set_up_presence_subscription(Bob, Alice, 0),

    %% XEP-0163 6.2 Contact Service Discovery
    assert_disco_items(Alice, AliceJid, NodeNS),
    assert_disco_items(Bob, AliceJid, NodeNS),
    assert_disco_items(Kate, AliceJid, NodeNS).

%% Basic cases

create_and_delete_story(Alice, Bob) ->
    PepNode = pep_node(Alice),
    create_node(Alice, PepNode, [{config, [{~"pubsub#access_model", ~"open"}]}]),
    subscribe(Bob, PepNode, []),

    %% XEP-0060 6.5 Retrieve items
    get_all_items(Alice, PepNode, [{expected_result, []}]),
    get_item(Alice, PepNode, ~"item1", [{expected_result, []}]),

    %% XEP-0060 8.2 Delete a Node
    delete_node(Alice, PepNode, []),
    receive_node_deletion_notification(Bob, PepNode, []),

    %% Create + delete without subscribers - to exercise the no-subscribers path in the code
    PepNodeNoSubs = pep_node(Alice),
    create_node(Alice, PepNodeNoSubs, []),
    delete_node(Alice, PepNodeNoSubs, []).

create_with_empty_config_story(Alice) ->
    PepNode = pep_node(Alice),
    create_node(Alice, PepNode, [{config, []}]),
    get_configuration(Alice, PepNode, [{expected_result, [{~"pubsub#access_model", ~"presence"}]}]).

create_and_configure_story(Alice) ->
    PepNode = pep_node(Alice),
    DefaultConfig = [{~"pubsub#access_model", ~"presence"}],
    UpdatedConfig = [{~"pubsub#access_model", ~"open"}],
    create_node(Alice, PepNode, []),

    %% XEP-0060 8.2.5.1 Success (no-op)
    set_configuration(Alice, PepNode, DefaultConfig, []),
    get_configuration(Alice, PepNode, [{expected_result, DefaultConfig}]),

    %% XEP-0060 8.2.5.1 Success
    set_configuration(Alice, PepNode, UpdatedConfig, []),
    get_configuration(Alice, PepNode, [{expected_result, UpdatedConfig}]).

create_open_and_publish_both_subs_story(Config, Alice, Bob) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = pep_node(Alice, NodeNS),
    create_node(Alice, PepNode, [{config, [{~"pubsub#access_model", ~"open"}]}]),

    %% Set up implicit subscription
    set_up_presence_subscription(Bob, Alice, 0),

    %% Set up explicit subscription with bare and full JID (to exercise all deduplication paths)
    subscribe(Bob, PepNode, []),
    subscribe(Bob, PepNode, [{jid_type, bare}]),

    %% XEP-0163 4.3.2 Avoid duplicated notifications for overlapping recipients
    publish(Alice, ~"item1", PepNode, []),
    receive_item_notification(Bob, ~"item1", PepNode, []),
    [] = escalus:wait_for_stanzas(Bob, 1, 500),

    %% XEP-0060 6.5 Retrieve items
    get_all_items(Bob, PepNode, [{expected_result, [~"item1"]}]),
    get_item(Bob, PepNode, ~"item1", [{expected_result, [~"item1"]}]).

create_open_and_publish_explicit_sub_story(Alice, Bob) ->
    NodeNS = random_node_ns(),
    PepNode = pep_node(Alice, NodeNS),
    create_node(Alice, PepNode, [{config, [{~"pubsub#access_model", ~"open"}]}]),

    %% XEP-0163 4 Open access model with explicit subscription
    subscribe(Bob, PepNode, []),
    publish(Alice, ~"item1", PepNode, []),
    receive_item_notification(Bob, ~"item1", PepNode, []),

    %% XEP-0060 6.5 Retrieve items
    get_all_items(Bob, PepNode, [{expected_result, [~"item1"]}]),
    get_item(Bob, PepNode, ~"item1", [{expected_result, [~"item1"]}]),

    %% XEP-0060 6.2.2 Success Case
    unsubscribe(Bob, PepNode, []),
    publish(Alice, ~"item2", PepNode, []),
    [] = escalus:wait_for_stanzas(Bob, 1, 500).

create_open_and_publish_implicit_sub_story(Config, Alice, Bob) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = pep_node(Alice, NodeNS),
    create_node(Alice, PepNode, [{config, [{~"pubsub#access_model", ~"open"}]}]),
    set_up_presence_subscription(Bob, Alice, 0),

    %% XEP-0163 4 Open access model with implicit subscription
    publish(Alice, ~"item1", PepNode, []),
    receive_item_notification(Bob, ~"item1", PepNode, []),

    %% XEP-0060 6.5 Retrieve items
    get_all_items(Bob, PepNode, [{expected_result, [~"item1"]}]),
    get_item(Bob, PepNode, ~"item1", [{expected_result, [~"item1"]}]).

create_open_and_publish_no_sub_story(Config, Alice, Bob, Mike) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = pep_node(Alice, NodeNS),
    create_node(Alice, PepNode, [{config, [{~"pubsub#access_model", ~"open"}]}]),
    set_up_presence_subscription(Mike, Alice, 0),
    publish(Alice, ~"item1", PepNode, []),

    %% XEP-0163 4 Notification filtering
    [] = escalus:wait_for_stanzas(Alice, 1, 500), % account owner without caps
    escalus_assert:has_no_stanzas(Bob), % caps but no presence subscription
    escalus_assert:has_no_stanzas(Mike), % presence subscription but no caps

    %% XEP-0060 6.5 Retrieve items
    get_all_items(Alice, PepNode, [{expected_result, [~"item1"]}]),
    get_item(Alice, PepNode, ~"item1", [{expected_result, [~"item1"]}]),
    get_all_items(Bob, PepNode, [{expected_result, [~"item1"]}]),
    get_item(Bob, PepNode, ~"item1", [{expected_result, [~"item1"]}]),
    get_all_items(Mike, PepNode, [{expected_result, [~"item1"]}]),
    get_item(Mike, PepNode, ~"item1", [{expected_result, [~"item1"]}]).

create_presence_and_publish_explicit_sub_story(Alice, Bob) ->
    NodeNS = random_node_ns(),
    PepNode = pep_node(Alice, NodeNS),
    create_node(Alice, PepNode, []),

    set_up_presence_subscription(Bob, Alice, 0),

    %% XEP-0163 4 (point 1) requires explicit subscriptions only for the 'open' model.
    %% However, here we follow XEP-0060: 4.2 (table 3) says a 'subscribed' entity MUST receive
    %% event notifications, and 12.2 (point 2) lists this trigger without access-model restrictions.
    subscribe(Bob, PepNode, []),
    publish(Alice, ~"item1", PepNode, []),
    receive_item_notification(Bob, ~"item1", PepNode, []),

    %% XEP-0060 6.2.2 Success Case
    unsubscribe(Bob, PepNode, []),
    publish(Alice, ~"item2", PepNode, []),
    [] = escalus:wait_for_stanzas(Bob, 1, 500).

create_presence_and_publish_implicit_sub_story(Config, Alice, Bob) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = pep_node(Alice, NodeNS),
    create_node(Alice, PepNode, []),
    set_up_presence_subscription(Bob, Alice, 0),

    %% XEP-0163 4 Automatic subscription plus notification filtering
    publish(Alice, ~"item1", PepNode, []),
    receive_item_notification(Bob, ~"item1", PepNode, []),

    %% XEP-0060 6.5 Retrieve items
    get_all_items(Bob, PepNode, [{expected_result, [~"item1"]}]),
    get_item(Bob, PepNode, ~"item1", [{expected_result, [~"item1"]}]).

create_presence_and_publish_no_sub_story(Config, Alice, Bob, Mike) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = pep_node(Alice, NodeNS),
    create_node(Alice, PepNode, []),
    set_up_presence_subscription(Mike, Alice, 0),
    publish(Alice, ~"item1", PepNode, []),

    %% XEP-0163 4 Notification filtering
    [] = escalus:wait_for_stanzas(Alice, 1, 500), % account owner without caps
    escalus_assert:has_no_stanzas(Bob), % caps but no presence subscription
    escalus_assert:has_no_stanzas(Mike), % presence subscription but no caps

    %% XEP-0060 6.5 Retrieve items
    get_all_items(Alice, PepNode, [{expected_result, [~"item1"]}]),
    get_item(Alice, PepNode, ~"item1", [{expected_result, [~"item1"]}]),
    get_all_items(Bob, PepNode, [{expected_error_type, ~"auth"}]),
    get_item(Bob, PepNode, ~"item1", [{expected_error_type, ~"auth"}]),
    get_all_items(Mike, PepNode, [{expected_result, [~"item1"]}]),
    get_item(Mike, PepNode, ~"item1", [{expected_result, [~"item1"]}]).

publish_and_get_items_story(Alice) ->
    PepNode = pep_node(Alice, random_node_ns()),
    publish(Alice, ~"item0", PepNode, []),
    publish(Alice, ~"item1", PepNode, []),
    publish(Alice, ~"item2", PepNode, []),

    %% XEP-0060 6.5 Request items
    get_all_items(Alice, PepNode, [{expected_result, [~"item0", ~"item1", ~"item2"]}]),
    get_item(Alice, PepNode, ~"item1", [{expected_result, [~"item1"]}]),
    get_items(Alice, PepNode, [~"item1", ~"item404", ~"item0"],
              [{expected_result, [~"item1", ~"item0"]}]).

publish_implicit_sub_story(Config, Alice, Bob) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = pep_node(Alice, NodeNS),
    set_up_presence_subscription(Bob, Alice, 0),

    %% XEP-0163 3 Auto-create and publish an item
    publish(Alice, ~"item1", PepNode, []),

    %% XEP-0163 4.3.3 Publish generates a notification
    receive_item_notification(Bob, ~"item1", PepNode, []),

    %% XEP-0060 6.5 Retrieve items
    get_all_items(Bob, PepNode, [{expected_result, [~"item1"]}]).

publish_open_explicit_sub_story(Alice, Bob) ->
    NodeNS = random_node_ns(),
    PepNode = pep_node(Alice, NodeNS),
    PublishOptions = [{~"pubsub#access_model", ~"open"}],
    publish_with_options(Alice, ~"item0", PepNode, [], PublishOptions),
    publish(Alice, ~"item1", PepNode, []),

    %% XEP-0163 4.3.4 Bob can subscribe and receive the last item
    subscribe(Bob, PepNode, [{expected_notification, {PepNode, ~"item1"}}]),

    %% XEP-0060 6.1.6 Subsequent 'subscribe' returns the current subscription state
    subscribe(Bob, PepNode, []),
    [] = escalus:wait_for_stanzas(Bob, 1, 500),

    %% XEP-0163 4.3.3 'publish' by Alice results in a notification to Bob
    publish(Alice, ~"item2", PepNode, []),
    receive_item_notification(Bob, ~"item2", PepNode, []),

    %% XEP-0060 6.5 Bob can request all items
    get_all_items(Bob, PepNode, [{expected_result, [~"item0", ~"item1", ~"item2"]}]).

publish_self_notify_story(Config, Bob) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = pep_node(Bob, NodeNS),

    %% XEP-0163 4, Case 3 Entity is the account owner itself
    publish(Bob, ~"item1", PepNode, [{expected_notification, {PepNode, ~"item1"}}]),

    %% XEP-0060 6.5 Retrieve items
    get_all_items(Bob, PepNode, [{expected_result, [~"item1"]}]),
    get_item(Bob, PepNode, ~"item1", [{expected_result, [~"item1"]}]),

    %% XEP-0060 7.1.2 Generated item ID
    {Response, Msg} = publish(Bob, undefined, PepNode, [{expected_notification, PepNode}]),
    GeneratedItemId = published_item_id(Response, PepNode),
    check_item_notification(Msg, GeneratedItemId, PepNode, []),
    get_item(Bob, PepNode, GeneratedItemId, [{expected_result, [GeneratedItemId]}]).

send_last_item_on_caps_story(Alice, Bob) ->
    NodeNS = random_node_ns(),
    PepNode = pep_node(Alice, NodeNS),
    publish(Alice, ~"item2", PepNode, []),

    escalus_story:make_all_clients_friends([Alice, Bob]),

    Features = features(NodeNS),
    Caps = caps_helper:enable_new_caps(Bob, Features, v1),
    caps_helper:receive_presence_with_caps(Alice, Bob, Caps),

    receive_item_notification(Bob, ~"item2", PepNode, []),

    %% Unchanged caps shouldn't trigger notifications
    caps_helper:enable_caps(Bob, Features, v1),
    ct:sleep(200),
    escalus_assert:has_no_stanzas(Bob).

send_last_item_on_implicit_sub_story(Config, Alice, Bob) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = pep_node(Alice, NodeNS),
    publish(Alice, ~"item1", PepNode, []), % previous item - not delivered
    publish(Alice, ~"item2", PepNode, []), % last item
    [Message] = set_up_presence_subscription(Bob, Alice, 1),
    check_item_notification(Message, ~"item2", PepNode, []),
    assert_delayed_notification(Message),

    %% Bob can receive further notifications
    publish(Alice, ~"item3", PepNode, []),
    receive_item_notification(Bob, ~"item3", PepNode, []).

%% Negative cases

bad_request_fails_story(Alice) ->
    AliceJid = escalus_utils:get_short_jid(Alice),
    Opts = [{expected_error_type, {~"modify", ~"bad-request"}}],
    send_generic_request(Alice, AliceJid, ~"set", ?NS_PUBSUB, ~"unknown-action", Opts),
    send_generic_request(Alice, AliceJid, ~"get", ?NS_PUBSUB, ~"unknown-action", Opts),
    send_generic_request(Alice, AliceJid, ~"set", ?NS_PUBSUB_OWNER, ~"unknown-action", Opts),
    send_generic_request(Alice, AliceJid, ~"get", ?NS_PUBSUB_OWNER, ~"unknown-action", Opts),
    send_generic_request(Alice, AliceJid, ~"set", ?NS_PUBSUB, {raw, ~"pubstub"}, Opts).

configure_with_invalid_form_fails_story(Alice) ->
    PepNode = pep_node(Alice),
    create_node(Alice, PepNode, []),

    %% Malformed configuration form
    UpdatedConfig = [{~"pubsub#access_model", ~"open"}],
    Opts = [{expected_error_type, {~"modify", ~"bad-request"}},
            {modify_request, fun form_helper:remove_form_types/1}],
    set_configuration(Alice, PepNode, UpdatedConfig, Opts).

create_at_foreign_jid_fails_story(Alice, Bob) ->
    Opts = [{expected_error_type, {~"auth", ~"forbidden"}}],

    %% XEP-0060 8.1 Create a Node, Example 128
    create_node(Bob, pep_node(Alice), Opts),

    %% XEP-0060 7.1.3.1 Insufficient Privileges (auto-create attempt)
    publish(Bob, ~"item2", pep_node(Alice), Opts).

create_duplicate_node_fails_story(Alice) ->
    PepNode = pep_node(Alice),
    create_node(Alice, PepNode, []),

    %% XEP-0060 8.1.3.2 Node Already Exists
    Opts = [{expected_error_type, {~"cancel", ~"conflict"}}],
    create_node(Alice, PepNode, Opts).

create_with_invalid_config_fails_story(Alice) ->
    InvalidOptionOpts = [{config, [{~"pubsub#definitely_invalid_option", ~"1"}]},
                         {expected_error_type, {~"modify", ~"bad-request"}}],
    create_node(Alice, pep_node(Alice), InvalidOptionOpts),

    InvalidModelOpts = [{config, [{~"pubsub#access_model", ~"not-a-real-model"}]},
                        {expected_error_type, {~"modify", ~"bad-request"}}],
    create_node(Alice, pep_node(Alice), InvalidModelOpts),

    %% Malformed create request with unexpected extra child
    ExtraChildOpts = [{expected_error_type, {~"modify", ~"bad-request"}},
                      {modify_request, fun pubsub_tools:add_unexpected_pubsub_child/1}],
    create_node(Alice, pep_node(Alice), ExtraChildOpts),

    %% Malformed create configuration form
    PepNode = pep_node(Alice),
    MalformedConfigOpts = [{config, [{~"pubsub#access_model", ~"open"}]},
                           {expected_error_type, {~"modify", ~"bad-request"}},
                           {modify_request, fun form_helper:remove_form_types/1}],
    create_node(Alice, PepNode, MalformedConfigOpts),

    UnsupportedModelOpts = [{config, [{~"pubsub#access_model", ~"authorize"}]},
                            {expected_error_type,
                             {~"modify", ~"not-acceptable", ~"unsupported-access-model"}}],
    create_node(Alice, pep_node(Alice), UnsupportedModelOpts).

delete_nonexistent_node_fails_story(Alice) ->
    %% XEP-0060 8.2.4 Node Does Not Exist
    Opts = [{expected_error_type, {~"cancel", ~"item-not-found"}}],
    delete_node(Alice, pep_node(Alice), Opts).

get_item_without_id_fails_story(Alice) ->
    PepNode = pep_node(Alice),
    create_node(Alice, PepNode, []),

    %% XEP-0060 6.5 Request an item: missing item ID
    Opts = [{expected_error_type, {~"modify", ~"bad-request"}}],
    get_items(Alice, PepNode, [undefined], Opts).

manage_nonexistent_node_fails_story(Alice) ->
    PepNode = pep_node(Alice),
    Opts = [{expected_error_type, {~"cancel", ~"item-not-found"}}],

    %% XEP-0060 8.2.3.5 Node Does Not Exist
    get_configuration(Alice, PepNode, Opts),

    %% XEP-0060 8.2.3.5 Node Does Not Exist: same missing-node condition applies to 'set'
    set_configuration(Alice, PepNode, [{~"pubsub#access_model", ~"open"}], Opts),

    %% XEP-0060 8.2.4 Node Does Not Exist
    delete_node(Alice, PepNode, Opts).

manage_foreign_node_fails_story(Alice, Bob) ->
    PepNode = pep_node(Alice),
    create_node(Alice, PepNode, []),
    Opts = [{expected_error_type, {~"auth", ~"forbidden"}}],

    %% XEP-0060 8.5.3.1 Insufficient Privileges
    get_configuration(Bob, PepNode, Opts),

    %% XEP-0060 8.5.6.1 Insufficient Privileges
    set_configuration(Bob, PepNode, [{~"pubsub#access_model", ~"open"}], Opts),

    %% XEP-0060 8.4.3.1 Insufficient Privileges (existing node)
    delete_node(Bob, PepNode, Opts),

    %% XEP-0060 8.4.3.1 Insufficient Privileges (non-existent node)
    delete_node(Bob, pep_node(Alice), Opts).

publish_to_foreign_node_fails_story(Alice, Bob) ->
    PepNode = pep_node(Alice),
    create_node(Alice, PepNode, []),

    %% XEP-0060 7.1.3.1 Insufficient Privileges
    publish(Bob, ~"item1", PepNode, [{expected_error_type, {~"auth", ~"forbidden"}}]).

publish_with_invalid_items_fails_story(Alice) ->
    PepNode = pep_node(Alice),

    %% XEP-0060 7.1.3.4 Item Required
    ItemRequiredOpts = [{with_payload, false},
                        {expected_error_type, {~"modify", ~"bad-request", ~"item-required"}}],
    publish(Alice, ~"item1", PepNode, ItemRequiredOpts),

    %% XEP-0060 7.1.3.6 Payload Required
    EmptyItem = item_el(~"item2", []),
    PayloadRequiredOpts = [{expected_error_type, {~"modify", ~"bad-request", ~"payload-required"}}],
    publish_raw(Alice, PepNode, [EmptyItem], PayloadRequiredOpts),

    %% XEP-0060 7.1.3.6 Invalid Payload
    Content = item_content(),
    InvalidItem = item_el(~"item3", [Content, Content]),
    InvalidPayloadOpts = [{expected_error_type, {~"modify", ~"bad-request", ~"invalid-payload"}}],
    publish_raw(Alice, PepNode, [InvalidItem], InvalidPayloadOpts),

    %% XEP-0060 7.1.3.5 Too Many Items
    ItemEl = item_el(~"item4", [Content]),
    BadRequestOpts = [{expected_error_type, {~"modify", ~"bad-request"}}],
    publish_raw(Alice, PepNode, [ItemEl, ItemEl], BadRequestOpts),

    %% Malformed publish request: direct child is not <item/>.
    publish_raw(Alice, PepNode, [Content], BadRequestOpts).

publish_options_invalid_config_fails_story(Alice) ->
    PreconditionOpts = [{expected_error_type, {~"cancel", ~"conflict", ~"precondition-not-met"}}],

    %% XEP-0060 7.1.5 Publishing Options: unmet publish-options precondition
    UnknownOptionFields = [{~"pubsub#definitely_invalid_option", ~"1"}],
    publish_with_options(Alice, ~"item1", pep_node(Alice), PreconditionOpts, UnknownOptionFields),

    %% XEP-0060 7.1.5 Publishing Options: unmet publish-options precondition
    InvalidAccessModelFields = [{~"pubsub#access_model", ~"not-a-real-model"}],
    publish_with_options(Alice, ~"item1", pep_node(Alice),
                         PreconditionOpts, InvalidAccessModelFields),

    %% XEP-0060 7.1.5 Publishing Options: unmet publish-options precondition
    PepNode = pep_node(Alice),
    create_node(Alice, PepNode, []),
    Fields = [{~"pubsub#access_model", ~"open"}],
    publish_with_options(Alice, ~"item1", PepNode, PreconditionOpts, Fields),

    %% Malformed publish-options request with unexpected extra child
    MalformedOpts = [{expected_error_type, {~"modify", ~"bad-request"}},
                     {modify_request, fun pubsub_tools:add_unexpected_pubsub_child/1}],
    publish_with_options(Alice, ~"item1", pep_node(Alice), MalformedOpts, Fields),

    %% Malformed publish-options form
    MalformedFormOpts = [{expected_error_type, {~"modify", ~"bad-request"}},
                         {modify_request, fun form_helper:remove_form_types/1}],
    publish_with_options(Alice, ~"item1", pep_node(Alice), MalformedFormOpts, Fields).

request_without_nodeid_fails_story(Alice) ->
    MissingNode = pep_node(Alice, undefined),

    %% XEP-0060 8.1.2.3 NodeID Required
    CreateOpts = [{expected_error_type, {~"modify", ~"not-acceptable", ~"nodeid-required"}}],
    create_node(Alice, MissingNode, CreateOpts),

    %% XEP-0060 8.5.3.2 NodeID Required for 'get'
    GetOpts = [{expected_error_type, {~"modify", ~"bad-request", ~"nodeid-required"}}],
    get_configuration(Alice, MissingNode, GetOpts),

    %% XEP-0060 8.5.3.2 NodeID Required for 'set'
    SetOpts = [{expected_error_type, {~"modify", ~"bad-request", ~"nodeid-required"}}],
    set_configuration(Alice, MissingNode, [{~"pubsub#access_model", ~"open"}], SetOpts).

subscribe_presence_required_fails_story(Alice, Bob) ->
    PepNode = pep_node(Alice),
    create_node(Alice, PepNode, []),

    %% XEP-0060 6.1.3.2 Presence Subscription Required
    Opts = [{expected_error_type, {~"auth", ~"not-authorized", ~"presence-subscription-required"}}],
    subscribe(Bob, PepNode, Opts).

subscribe_to_nonexistent_node_fails_story(Alice) ->
    PepNode = pep_node(Alice),
    Opts = [{expected_error_type, {~"cancel", ~"item-not-found"}}],

    %% XEP-0060 6.1.3.4 Node Does Not Exist
    subscribe(Alice, PepNode, Opts),

    %% XEP-0060 6.2.3.4 Node Does Not Exist
    unsubscribe(Alice, PepNode, Opts).

subscribe_with_invalid_jid_fails_story(Alice, Bob) ->
    PepNode = pep_node(Alice),
    create_node(Alice, PepNode, [{config, [{~"pubsub#access_model", ~"open"}]}]),

    %% XEP-0060 6.1.3.3 Invalid JID
    InvalidJid = escalus_utils:get_short_jid(Alice),
    Opts = [{subscriber_jid, InvalidJid},
            {expected_error_type, {~"modify", ~"bad-request", ~"invalid-jid"}}],
    subscribe(Bob, PepNode, Opts).

unsubscribe_implicit_fails_story(Config, Alice, Bob) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = pep_node(Alice, NodeNS),
    create_node(Alice, PepNode, []),
    set_up_presence_subscription(Bob, Alice, 0),

    %% XEP-0060 6.2.3.2 No Such Subscriber
    Opts = [{expected_error_type, {~"cancel", ~"unexpected-request", ~"not-subscribed"}}],
    unsubscribe(Bob, PepNode, Opts).

unsubscribe_foreign_jid_fails_story(Alice, Bob) ->
    PepNode = pep_node(Alice),
    create_node(Alice, PepNode, [{config, [{~"pubsub#access_model", ~"open"}]}]),
    subscribe(Bob, PepNode, []),

    %% XEP-0060 6.2.3.1 Insufficient Privileges
    Opts = [{subscriber_jid, escalus_utils:get_short_jid(Alice)},
            {expected_error_type, {~"auth", ~"forbidden"}}],
    unsubscribe(Bob, PepNode, Opts).

%% No mod_caps

disco_info_no_caps_story(Alice) ->
    %% XEP-0163 6.1 Account Owner Service Discovery
    assert_disco_info(Alice, escalus_client:short_jid(Alice), false).

create_presence_and_publish_implicit_sub_no_caps_story(Alice, Bob) ->
    PepNode = pep_node(Alice, random_node_ns()),
    create_node(Alice, PepNode, []),
    set_up_presence_subscription(Bob, Alice, 0),

    %% Without mod_caps, caps-based implicit notifications are not delivered
    publish(Alice, ~"item1", PepNode, []),
    [] = escalus:wait_for_stanzas(Bob, 1, 500).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

%% Setup

required_modules(pep) ->
    [{mod_caps, default_mod_config(mod_caps)},
     {mod_pubsub, default_mod_config(mod_pubsub)}];
required_modules(pep_no_caps) ->
    [{mod_pubsub, default_mod_config(mod_pubsub)}].

set_caps(Config) ->
    set_caps(Config, random_node_ns()).

set_caps(Config, NS) ->
    [{escalus_overrides, [{start_ready_clients, {?MODULE, start_caps_clients}}]},
     {node_ns, NS} | Config].

%% Implemented only for one resource per client, because it is enough
start_caps_clients(Config, [{UserSpec, Resource}]) ->
    {ok, Client} = escalus_client:start(Config, UserSpec, Resource),
    exchange_initial_presence(Config, Client, is_caps_client(Client)),
    [Client].

exchange_initial_presence(Config, Client, true) ->
    Features = features(proplists:get_value(node_ns, Config)),
    caps_helper:enable_new_caps(Client, Features, v1);
exchange_initial_presence(_Config, Client, false) ->
    escalus_story:send_initial_presence(Client),
    escalus:assert(is_presence, escalus:wait_for_stanza(Client)).

is_caps_client(Client) ->
    case escalus_client:username(Client) of
        <<"bob", _/binary>> -> true;
        _ -> false
    end.

set_up_presence_subscription(Subscriber, Owner, ExpectedMessageCount) ->
    send_presence(Subscriber, ~"subscribe", Owner),
    escalus:assert(is_iq, escalus_client:wait_for_stanza(Subscriber)),
    escalus:assert(is_presence, escalus_client:wait_for_stanza(Owner)),
    send_presence(Owner, ~"subscribed", Subscriber),
    escalus:assert(is_iq, escalus_client:wait_for_stanza(Owner)),
    Preds = [is_iq] ++ lists:duplicate(ExpectedMessageCount, is_message)
        ++ lists:duplicate(2, is_presence),
    Stanzas = escalus_client:wait_for_stanzas(Subscriber, length(Preds)),
    escalus:assert_many(Preds, Stanzas),
    lists:filter(fun escalus_pred:is_message/1, Stanzas). % return only messages

send_presence(From, Type, To) ->
    ToJid = escalus_client:short_jid(To),
    Stanza = escalus_stanza:presence_direct(ToJid, Type),
    escalus_client:send(From, Stanza).

%% Disco helpers and assertions

assert_no_disco_info(Client, OwnerJid) ->
    Request = escalus_stanza:disco_info(OwnerJid),
    escalus:send(Client, Request),
    Stanza = escalus:wait_for_stanza(Client),
    escalus:assert(is_iq_error, [Request], Stanza),
    escalus:assert(is_error, [~"cancel", ~"service-unavailable"], Stanza),
    escalus:assert(is_stanza_from, [OwnerJid], Stanza).

assert_disco_info(Client, OwnerJid, HasCapsModule) ->
    Request = escalus_stanza:disco_info(OwnerJid),
    escalus:send(Client, Request),
    Stanza = escalus:wait_for_stanza(Client),
    escalus:assert(is_iq_result, [Request], Stanza),
    ?assertNot(escalus_pred:has_identity(~"pubsub", ~"service", Stanza)),
    escalus:assert(has_identity, [~"pubsub", ~"pep"], Stanza),
    lists:foreach(fun(Feature) ->
                          escalus:assert(has_feature, [Feature], Stanza)
                  end, pep_disco_features(HasCapsModule)),
    escalus:assert(is_stanza_from, [OwnerJid], Stanza).

assert_no_disco_items(Client, OwnerJid) ->
    ResultQuery = make_disco_items_query(Client, OwnerJid),
    ?assertEqual([], ResultQuery#xmlel.children).

assert_disco_items(Client, OwnerJid, NodeNS) ->
    ResultQuery = make_disco_items_query(Client, OwnerJid),
    Item = exml_query:subelement_with_attr(ResultQuery, ~"node", NodeNS),
    ?assertEqual(jid:str_tolower(OwnerJid), exml_query:attr(Item, ~"jid")).

assert_no_disco_items_node(Client, OwnerJid, NodeNS, ErrorCondition) ->
    Request = escalus_stanza:disco_items(OwnerJid, NodeNS),
    escalus:send(Client, Request),
    Stanza = escalus:wait_for_stanza(Client),
    escalus:assert(is_iq_error, [Request], Stanza),
    escalus:assert(is_error, [~"cancel", ErrorCondition], Stanza),
    escalus:assert(is_stanza_from, [OwnerJid], Stanza).

assert_disco_info_node(Client, OwnerJid, NodeNS) ->
    Request = escalus_stanza:disco_info(OwnerJid, NodeNS),
    escalus:send(Client, Request),
    Stanza = escalus:wait_for_stanza(Client),
    escalus:assert(is_stanza_from, [OwnerJid], Stanza),
    escalus:assert(is_iq_result, [Request], Stanza),
    Query = exml_query:subelement(Stanza, ~"query"),
    ?assertEqual(NodeNS, exml_query:attr(Query, ~"node")),
    escalus:assert(has_identity, [~"pubsub", ~"leaf"], Stanza),
    escalus:assert(has_feature, [?NS_PUBSUB], Stanza).

assert_no_disco_info_node(Client, OwnerJid, NodeNS, ErrorCondition) ->
    Request = escalus_stanza:disco_info(OwnerJid, NodeNS),
    escalus:send(Client, Request),
    Stanza = escalus:wait_for_stanza(Client),
    escalus:assert(is_stanza_from, [OwnerJid], Stanza),
    escalus:assert(is_iq_error, [Request], Stanza),
    escalus:assert(is_error, [~"cancel", ErrorCondition], Stanza).

assert_delayed_notification(Stanza) ->
    DelayEl = exml_query:subelement(Stanza, ~"delay"),
    escalus:assert(has_ns, [?NS_DELAY], DelayEl),
    ?assertMatch(Stamp when is_binary(Stamp), exml_query:attr(DelayEl, ~"stamp")).

make_disco_items_query(Client, OwnerJid) ->
    Request = escalus_stanza:disco_items(OwnerJid),
    escalus:send(Client, Request),
    Stanza = escalus:wait_for_stanza(Client),
    escalus:assert(is_stanza_from, [OwnerJid], Stanza),
    escalus:assert(is_iq_result, [Request], Stanza),
    exml_query:subelement(Stanza, ~"query").

%% Nodes, features, jids

pep_node(Client) ->
    pep_node(Client, random_node_ns()).

pep_node(Client, NodeName) ->
    {escalus_utils:jid_to_lower(escalus_utils:get_short_jid(Client)), NodeName}.

pep_disco_features(HasCapsModule) ->
    [?NS_PUBSUB,
     <<?NS_PUBSUB/binary, "#access-open">>,
     <<?NS_PUBSUB/binary, "#access-presence">>,
     <<?NS_PUBSUB/binary, "#auto-create">>,
     <<?NS_PUBSUB/binary, "#config-node">>,
     <<?NS_PUBSUB/binary, "#create-and-configure">>,
     <<?NS_PUBSUB/binary, "#create-nodes">>,
     <<?NS_PUBSUB/binary, "#delete-nodes">>,
     <<?NS_PUBSUB/binary, "#item-ids">>,
     <<?NS_PUBSUB/binary, "#last-published">>,
     <<?NS_PUBSUB/binary, "#persistent-items">>,
     <<?NS_PUBSUB/binary, "#publish">>,
     <<?NS_PUBSUB/binary, "#publish-options">>,
     <<?NS_PUBSUB/binary, "#retrieve-items">>,
     <<?NS_PUBSUB/binary, "#subscribe">>] ++
    case HasCapsModule of
        true -> [<<?NS_PUBSUB/binary, "#filtered-notifications">>];
        false -> []
    end.

features(NodeNS) ->
    [NodeNS, ns_notify(NodeNS)].

ns_notify(NS) ->
    <<NS/binary, "+notify">>.

random_node_ns() ->
    base64:encode(crypto:strong_rand_bytes(16)).

nonexistent_bare_jid() ->
    <<"unknown@", (domain_helper:domain())/binary>>.
