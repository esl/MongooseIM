-module(pep_SUITE).
-moduledoc "PEP (XEP-0163) tests for mod_pubsub".

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [require_rpc_nodes/1]).
-import(config_parser_helper, [default_mod_config/1]).
-import(domain_helper, [host_type/0]).

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
    Config0 = dynamic_modules:save_modules(host_type(), Config),
    NewConfig = required_modules(GroupName),
    dynamic_modules:ensure_modules(host_type(), NewConfig),
    Config0.

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
    [{group, pep}].

groups() ->
    [{pep, [parallel], pep_tests()}].

pep_tests() ->
    disco_tests() ++ protocol_tests() ++ protocol_tests_with_sm().

-doc "Tests for XEP-0030 Service Discovery with rules from XEP-0163 / XEP-0060".
disco_tests() ->
    [disco_info,
     disco_info_node,
     disco_items,
     disco_items_node,
     disco_items_open].

-doc "PEP tests for the XEP-0163 / XEP-0060 protocol".
protocol_tests() ->
    [auto_create_and_publish_implicit_sub,
     auto_create_and_publish_self_notify,
     auto_create_open_and_publish_explicit_sub,
     create_presence_and_publish_no_sub,
     create_open_and_publish_no_sub,
     create_presence_and_publish_implicit_sub,
     create_open_and_publish_implicit_sub,
     create_presence_and_publish_explicit_sub,
     create_open_and_publish_explicit_sub,
     create_open_and_publish_both_subs,
     create_and_delete_node,
     create_node_with_empty_config,
     create_and_configure_node,
     send_last_item_on_caps,
     send_last_item_on_implicit_sub,
     request_without_nodeid_fails,
     request_with_unknown_action_fails,
     create_node_at_foreign_jid_fails,
     create_node_invalid_config_fails,
     manage_nonexistent_node_fails,
     manage_foreign_node_fails,
     subscribe_to_nonexistent_node_fails,
     subscribe_with_invalid_jid_fails,
     publish_to_foreign_node_fails,
     publish_with_invalid_items_fails,
     publish_options_invalid_config_fails].

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

%% Auto-create and publish

auto_create_and_publish_implicit_sub(Config) ->
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {bob, 1}],
                                    fun auto_create_and_publish_implicit_sub_story/3).

auto_create_and_publish_self_notify(Config) ->
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{bob, 1}],
                                    fun auto_create_and_publish_self_notify_story/2).

auto_create_open_and_publish_explicit_sub(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun auto_create_open_and_publish_explicit_sub_story/3).

%% Create and publish

create_presence_and_publish_no_sub(Config) ->
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {bob, 1}, {mike, 1}],
                                    fun create_presence_and_publish_no_sub_story/4).

create_open_and_publish_no_sub(Config) ->
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {bob, 1}, {mike, 1}],
                                    fun create_open_and_publish_no_sub_story/4).

create_presence_and_publish_implicit_sub(Config) ->
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {bob, 1}],
                                    fun create_presence_and_publish_implicit_sub_story/3).

create_open_and_publish_implicit_sub(Config) ->
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {bob, 1}],
                                    fun create_open_and_publish_implicit_sub_story/3).

create_presence_and_publish_explicit_sub(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun create_presence_and_publish_explicit_sub_story/3).

create_open_and_publish_explicit_sub(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun create_open_and_publish_explicit_sub_story/3).

create_open_and_publish_both_subs(Config) ->
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {bob, 1}],
                                    fun create_open_and_publish_both_subs_story/3).

%% Node lifecycle and configuration

create_and_delete_node(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}],
                        fun create_and_delete_node_story/2).

create_node_with_empty_config(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun create_node_with_empty_config_story/1).

create_and_configure_node(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun create_and_configure_node_story/1).

%% Last item delivery

send_last_item_on_caps(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun send_last_item_on_caps_story/2).

send_last_item_on_implicit_sub(Config) ->
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {bob, 1}],
                                    fun send_last_item_on_implicit_sub_story/3).

%% Negative cases

request_without_nodeid_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun request_without_nodeid_fails_story/1).

request_with_unknown_action_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun request_with_unknown_action_fails_story/1).

create_node_at_foreign_jid_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}],
                        fun create_node_at_foreign_jid_fails_story/2).

create_node_invalid_config_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun create_node_invalid_config_fails_story/1).

manage_nonexistent_node_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun manage_nonexistent_node_fails_story/1).

manage_foreign_node_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}],
                        fun manage_foreign_node_fails_story/2).

subscribe_to_nonexistent_node_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun subscribe_to_nonexistent_node_fails_story/1).

subscribe_with_invalid_jid_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}],
                        fun subscribe_with_invalid_jid_fails_story/2).

publish_to_foreign_node_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}],
                        fun publish_to_foreign_node_fails_story/2).

publish_with_invalid_items_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun publish_with_invalid_items_fails_story/1).

publish_options_invalid_config_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun publish_options_invalid_config_fails_story/1).

%% Stream management

send_last_item_on_implicit_sub_with_sm(ConfigIn) ->
    Config0 = escalus_users:update_userspec(ConfigIn, alice, stream_management, true),
    Config1 = escalus_users:update_userspec(Config0, bob, stream_management, true),
    Config2 = set_caps(Config1),
    escalus:fresh_story_with_config(Config2, [{alice, 1}, {bob, 1}],
                                    fun send_last_item_on_implicit_sub_story/3).

%%--------------------------------------------------------------------
%% Stories
%%--------------------------------------------------------------------

%% Disco

disco_info_story(Alice) ->
    %% XEP-0163 6.1 Account Owner Service Discovery
    assert_disco_info(Alice, escalus_client:short_jid(Alice)),

    %% XEP-0030 8 Security Considerations
    assert_no_disco_info(Alice, nonexistent_bare_jid()).

disco_info_node_story(Alice, Bob, Kate) ->
    NodeNS = random_node_ns(),
    AliceJid = escalus_client:short_jid(Alice),
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),
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
    NonexistentJid = nonexistent_bare_jid(),
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),
    set_up_presence_subscription(Bob, Alice, 0),

    %% XEP-0163 6.2 Contact Service Discovery
    assert_disco_items(Alice, AliceJid, NodeNS),
    assert_disco_items(Bob, AliceJid, NodeNS),
    assert_no_disco_items(Kate, AliceJid, NodeNS),

    %% XEP-0030 8 Security Considerations
    assert_no_disco_items(Alice, NonexistentJid).

disco_items_node_story(Alice, Bob, Kate) ->
    NodeNS = random_node_ns(),
    AliceJid = escalus_client:short_jid(Alice),
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),
    set_up_presence_subscription(Bob, Alice, 0),

    %% XEP-0030 7 Error Conditions
    assert_no_disco_items_node(Alice, AliceJid, NodeNS, ~"item-not-found"),
    assert_no_disco_items_node(Bob, AliceJid, NodeNS, ~"item-not-found"),

    %% XEP-0030 8 Security Considerations
    assert_no_disco_items_node(Kate, AliceJid, NodeNS, ~"service-unavailable").

disco_items_open_story(Alice, Bob, Kate) ->
    NodeNS = random_node_ns(),
    AliceJid = escalus_client:short_jid(Alice),
    PepNode = make_pep_node_info(Alice, NodeNS),
    create_node_with_config(Alice, PepNode, [{~"pubsub#access_model", ~"open"}]),
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),
    set_up_presence_subscription(Bob, Alice, 0),

    %% XEP-0163 6.2 Contact Service Discovery
    assert_disco_items(Alice, AliceJid, NodeNS),
    assert_disco_items(Bob, AliceJid, NodeNS),
    assert_disco_items(Kate, AliceJid, NodeNS).

%% Auto-create and publish

auto_create_and_publish_implicit_sub_story(Config, Alice, Bob) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = make_pep_node_info(Alice, NodeNS),
    set_up_presence_subscription(Bob, Alice, 0),

    %% XEP-0163 3 Auto-create and publish an item
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),

    %% XEP-0163 4.3.3 Publish generates a notification
    pubsub_tools:receive_item_notification(Bob, ~"item1", PepNode, []),

    %% XEP-0060 6.5 Retrieve items
    pubsub_tools:get_all_items(Bob, PepNode, [{expected_result, [~"item1"]}]).

auto_create_and_publish_self_notify_story(Config, Bob) ->
    NodeNS = ?config(node_ns, Config),
    BobJid = escalus_utils:get_short_jid(Bob),

    %% XEP-0163 4, Case 3 Entity is the account owner itself
    pubsub_tools:publish(Bob, ~"item1", {pep, NodeNS},
                         [{expected_notification, {BobJid, NodeNS, ~"item1"}}]),

    %% XEP-0060 6.5 Retrieve items
    pubsub_tools:get_all_items(Bob, {pep, NodeNS}, [{expected_result, [~"item1"]}]),
    pubsub_tools:get_item(Bob, {pep, NodeNS}, ~"item1", [{expected_result, [~"item1"]}]).

auto_create_open_and_publish_explicit_sub_story(_Config, Alice, Bob) ->
    NodeNS = random_node_ns(),
    PepNode = make_pep_node_info(Alice, NodeNS),
    PublishOptions = [{~"pubsub#access_model", ~"open"}],
    pubsub_tools:publish_with_options(Alice, ~"item0", {pep, NodeNS}, [], PublishOptions),
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),

    %% XEP-0163 4.3.4 Bob can subscribe and receive the last item
    AliceJid = escalus_utils:get_short_jid(Alice),
    pubsub_tools:subscribe(Bob, PepNode, [{expected_notification, {AliceJid, NodeNS, ~"item1"}}]),

    %% XEP-0060 6.1.6 Subsequent 'subscribe' returns the current subscription state
    pubsub_tools:subscribe(Bob, PepNode, []),
    [] = escalus:wait_for_stanzas(Bob, 1, 500),

    %% XEP-0163 4.3.3 'publish' by Alice results in a notification to Bob
    pubsub_tools:publish(Alice, ~"item2", {pep, NodeNS}, []),
    pubsub_tools:receive_item_notification(Bob, ~"item2", PepNode, []),

    %% XEP-0060 6.5 Bob can request items
    pubsub_tools:get_all_items(Bob, PepNode, [{expected_result, [~"item0", ~"item1", ~"item2"]}]),
    pubsub_tools:get_item(Bob, PepNode, ~"item1", [{expected_result, [~"item1"]}]),
    pubsub_tools:get_items(Bob, PepNode, [~"item1", ~"item404", ~"item0"],
                           [{expected_result, [~"item1", ~"item0"]}]).

%% Create and publish

create_presence_and_publish_no_sub_story(Config, Alice, Bob, Mike) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = make_pep_node_info(Alice, NodeNS),
    pubsub_tools:create_node(Alice, PepNode, []),
    set_up_presence_subscription(Mike, Alice, 0),
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),

    %% XEP-0163 4 Notification filtering
    [] = escalus:wait_for_stanzas(Alice, 1, 500), % account owner without caps
    escalus_assert:has_no_stanzas(Bob), % caps but no presence subscription
    escalus_assert:has_no_stanzas(Mike), % presence subscription but no caps

    %% XEP-0060 6.5 Retrieve items
    pubsub_tools:get_all_items(Alice, PepNode, [{expected_result, [~"item1"]}]),
    pubsub_tools:get_item(Alice, PepNode, ~"item1", [{expected_result, [~"item1"]}]),
    pubsub_tools:get_all_items(Bob, PepNode, [{expected_error_type, ~"auth"}]),
    pubsub_tools:get_item(Bob, PepNode, ~"item1", [{expected_error_type, ~"auth"}]),
    pubsub_tools:get_all_items(Mike, PepNode, [{expected_result, [~"item1"]}]),
    pubsub_tools:get_item(Mike, PepNode, ~"item1", [{expected_result, [~"item1"]}]).

create_open_and_publish_no_sub_story(Config, Alice, Bob, Mike) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = make_pep_node_info(Alice, NodeNS),
    create_node_with_config(Alice, PepNode, [{~"pubsub#access_model", ~"open"}]),
    set_up_presence_subscription(Mike, Alice, 0),
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),

    %% XEP-0163 4 Notification filtering
    [] = escalus:wait_for_stanzas(Alice, 1, 500), % account owner without caps
    escalus_assert:has_no_stanzas(Bob), % caps but no presence subscription
    escalus_assert:has_no_stanzas(Mike), % presence subscription but no caps

    %% XEP-0060 6.5 Retrieve items
    pubsub_tools:get_all_items(Alice, PepNode, [{expected_result, [~"item1"]}]),
    pubsub_tools:get_item(Alice, PepNode, ~"item1", [{expected_result, [~"item1"]}]),
    pubsub_tools:get_all_items(Bob, PepNode, [{expected_result, [~"item1"]}]),
    pubsub_tools:get_item(Bob, PepNode, ~"item1", [{expected_result, [~"item1"]}]),
    pubsub_tools:get_all_items(Mike, PepNode, [{expected_result, [~"item1"]}]),
    pubsub_tools:get_item(Mike, PepNode, ~"item1", [{expected_result, [~"item1"]}]).

create_presence_and_publish_implicit_sub_story(Config, Alice, Bob) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = make_pep_node_info(Alice, NodeNS),
    pubsub_tools:create_node(Alice, PepNode, []),
    set_up_presence_subscription(Bob, Alice, 0),

    %% XEP-0163 4 Automatic subscription plus notification filtering
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),
    pubsub_tools:receive_item_notification(
      Bob, ~"item1", {escalus_utils:get_short_jid(Alice), NodeNS}, []),

    %% XEP-0060 6.5 Retrieve items
    pubsub_tools:get_all_items(Bob, PepNode, [{expected_result, [~"item1"]}]),
    pubsub_tools:get_item(Bob, PepNode, ~"item1", [{expected_result, [~"item1"]}]).

create_open_and_publish_implicit_sub_story(Config, Alice, Bob) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = make_pep_node_info(Alice, NodeNS),
    create_node_with_config(Alice, PepNode, [{~"pubsub#access_model", ~"open"}]),
    set_up_presence_subscription(Bob, Alice, 0),

    %% XEP-0163 4 Open access model with implicit subscription
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),
    pubsub_tools:receive_item_notification(Bob, ~"item1", PepNode, []),

    %% XEP-0060 6.5 Retrieve items
    pubsub_tools:get_all_items(Bob, PepNode, [{expected_result, [~"item1"]}]),
    pubsub_tools:get_item(Bob, PepNode, ~"item1", [{expected_result, [~"item1"]}]),

    %% XEP-0060 6.2.3.2 No Such Subscriber
    Opts = [{expected_error_type, {~"cancel", ~"unexpected-request", ~"not-subscribed"}}],
    pubsub_tools:unsubscribe(Bob, PepNode, Opts).

create_presence_and_publish_explicit_sub_story(_Config, Alice, Bob) ->
    NodeNS = random_node_ns(),
    PepNode = make_pep_node_info(Alice, NodeNS),
    pubsub_tools:create_node(Alice, PepNode, []),

    %% XEP-0060 6.1.3.2 Presence Subscription Required
    Opts = [{expected_error_type, {~"auth", ~"not-authorized", ~"presence-subscription-required"}}],
    pubsub_tools:subscribe(Bob, PepNode, Opts).

create_open_and_publish_explicit_sub_story(_Config, Alice, Bob) ->
    NodeNS = random_node_ns(),
    PepNode = make_pep_node_info(Alice, NodeNS),
    create_node_with_config(Alice, PepNode, [{~"pubsub#access_model", ~"open"}]),

    %% XEP-0163 4 Open access model with explicit subscription
    pubsub_tools:subscribe(Bob, PepNode, []),
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),
    pubsub_tools:receive_item_notification(Bob, ~"item1", PepNode, []),

    %% XEP-0060 6.5 Retrieve items
    pubsub_tools:get_all_items(Bob, PepNode, [{expected_result, [~"item1"]}]),
    pubsub_tools:get_item(Bob, PepNode, ~"item1", [{expected_result, [~"item1"]}]),

    %% XEP-0060 6.2.3.1 Insufficient Privileges
    InvalidJid = escalus_utils:get_short_jid(Alice),
    InvalidUnsubOpts = [{subscriber_jid, InvalidJid},
                        {expected_error_type, {~"auth", ~"forbidden"}}],
    pubsub_tools:unsubscribe(Bob, PepNode, InvalidUnsubOpts),

    %% XEP-0060 6.2.2 Success Case
    pubsub_tools:unsubscribe(Bob, PepNode, []),
    pubsub_tools:publish(Alice, ~"item2", {pep, NodeNS}, []),
    [] = escalus:wait_for_stanzas(Bob, 1, 500).

create_open_and_publish_both_subs_story(Config, Alice, Bob) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = make_pep_node_info(Alice, NodeNS),
    create_node_with_config(Alice, PepNode, [{~"pubsub#access_model", ~"open"}]),
    set_up_presence_subscription(Bob, Alice, 0),
    pubsub_tools:subscribe(Bob, PepNode, []),

    %% XEP-0163 4.3.2 Avoid duplicated notifications for overlapping recipients
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),
    pubsub_tools:receive_item_notification(Bob, ~"item1", PepNode, []),

    %% XEP-0060 6.5 Retrieve items
    pubsub_tools:get_all_items(Bob, PepNode, [{expected_result, [~"item1"]}]),
    pubsub_tools:get_item(Bob, PepNode, ~"item1", [{expected_result, [~"item1"]}]),
    [] = escalus:wait_for_stanzas(Bob, 1, 500).

%% Node lifecycle and configuration

create_and_delete_node_story(Alice, Bob) ->
    PepNode = make_pep_node_info(Alice, random_node_ns()),
    create_node_with_config(Alice, PepNode, [{~"pubsub#access_model", ~"open"}]),
    pubsub_tools:subscribe(Bob, PepNode, []),

    %% XEP-0060 8.1.3.2 Node Already Exists
    DuplicateCreateOpts = [{expected_error_type, {~"cancel", ~"conflict"}}],
    pubsub_tools:create_node(Alice, PepNode, DuplicateCreateOpts),

    %% XEP-0060 6.5 Retrieve items
    pubsub_tools:get_all_items(Alice, PepNode, [{expected_result, []}]),
    pubsub_tools:get_item(Alice, PepNode, ~"item1", [{expected_result, []}]),

    %% XEP-0060 8.2 Delete a Node
    pubsub_tools:delete_node(Alice, PepNode, []),
    pubsub_tools:receive_node_deletion_notification(Bob, PepNode, []),

    %% XEP-0060 6.5.9.11 Node Does Not Exist
    MissingNodeOpts = [{expected_error_type, {~"cancel", ~"item-not-found"}}],
    pubsub_tools:get_all_items(Alice, PepNode, MissingNodeOpts),
    pubsub_tools:get_item(Alice, PepNode, ~"item1", MissingNodeOpts).

create_node_with_empty_config_story(Alice) ->
    PepNode = make_pep_node_info(Alice, random_node_ns()),
    create_node_with_config(Alice, PepNode, []),
    pubsub_tools:get_configuration(Alice, PepNode, [{expected_result,
                                                     [{~"pubsub#access_model", ~"presence"}]}]).

create_and_configure_node_story(Alice) ->
    PepNode = make_pep_node_info(Alice, random_node_ns()),
    pubsub_tools:create_node(Alice, PepNode, []),
    pubsub_tools:get_configuration(Alice, PepNode, [{expected_result,
                                                     [{~"pubsub#access_model", ~"presence"}]}]),

    %% Malformed configuration form
    MalformedConfigOpts = [{expected_error_type, {~"modify", ~"bad-request"}},
                           {modify_request, fun form_helper:remove_form_types/1}],
    pubsub_tools:set_configuration(Alice, PepNode, [{~"pubsub#access_model", ~"open"}],
                                   MalformedConfigOpts),

    %% XEP-0060 8.2.5.1 Success
    pubsub_tools:set_configuration(Alice, PepNode, [{~"pubsub#access_model", ~"open"}], []),
    pubsub_tools:get_configuration(Alice, PepNode, [{expected_result,
                                                     [{~"pubsub#access_model", ~"open"}]}]).

%% Last item delivery

send_last_item_on_caps_story(Alice, Bob) ->
    NodeNS = random_node_ns(),
    pubsub_tools:publish(Alice, ~"item2", {pep, NodeNS}, []),

    escalus_story:make_all_clients_friends([Alice, Bob]),

    Features = features(NodeNS),
    Caps = caps_helper:enable_new_caps(Bob, Features, v1),
    caps_helper:receive_presence_with_caps(Alice, Bob, Caps),

    Node = {escalus_utils:get_short_jid(Alice), NodeNS},
    pubsub_tools:receive_item_notification(Bob, ~"item2", Node, []),

    %% Unchanged caps shouldn't trigger notifications
    caps_helper:enable_caps(Bob, Features, v1),
    ct:sleep(200),
    escalus_assert:has_no_stanzas(Bob).

send_last_item_on_implicit_sub_story(Config, Alice, Bob) ->
    %% if alice publishes an item and then bob subscribes successfully to her presence
    %% then bob will receive the item right after final subscription stanzas
    NodeNS = ?config(node_ns, Config),
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []), % previous item - not delivered
    pubsub_tools:publish(Alice, ~"item2", {pep, NodeNS}, []), % expected last item
    [Message] = set_up_presence_subscription(Bob, Alice, 1),
    Node = {escalus_utils:get_short_jid(Alice), NodeNS},
    pubsub_tools:check_item_notification(Message, ~"item2", Node, []),
    assert_delayed_notification(Message),

    %% Bob can receive further notifications
    pubsub_tools:publish(Alice, ~"item3", {pep, NodeNS}, []),
    pubsub_tools:receive_item_notification(Bob, ~"item3", Node, []).

%% Negative cases

request_without_nodeid_fails_story(Alice) ->
    PepService = escalus_utils:get_short_jid(Alice),

    %% XEP-0060 8.1.2.3 NodeID Required
    CreateOpts = [{expected_error_type, {~"modify", ~"not-acceptable", ~"nodeid-required"}}],
    pubsub_tools:create_node(Alice, PepService, CreateOpts),

    %% XEP-0060 8.5.3.2 NodeID Required for 'get'
    GetOpts = [{expected_error_type, {~"modify", ~"bad-request", ~"nodeid-required"}}],
    pubsub_tools:get_configuration(Alice, PepService, GetOpts),

    %% XEP-0060 8.5.3.2 NodeID Required for 'set'
    SetOpts = [{expected_error_type, {~"modify", ~"bad-request", ~"nodeid-required"}}],
    pubsub_tools:set_configuration(Alice, PepService, [{~"pubsub#access_model", ~"open"}], SetOpts).

request_with_unknown_action_fails_story(Alice) ->
    UnknownAction = #xmlel{name = ~"unknown-action"},
    Opts = [{expected_error_type, {~"modify", ~"bad-request"}}],

    PubsubSetId = ~"pubsub-set-unknown-action",
    PubsubSetRequest = escalus_pubsub_stanza:pubsub_iq(
                         ~"set", Alice, PubsubSetId, pep, [UnknownAction]),
    pubsub_tools:send_request_and_receive_response(Alice, PubsubSetRequest, PubsubSetId, Opts),

    PubsubGetId = ~"pubsub-get-unknown-action",
    PubsubGetRequest = escalus_pubsub_stanza:pubsub_iq(
                         ~"get", Alice, PubsubGetId, pep, [UnknownAction]),
    pubsub_tools:send_request_and_receive_response(Alice, PubsubGetRequest, PubsubGetId, Opts),

    OwnerSetId = ~"owner-set-unknown-action",
    OwnerSetRequest = escalus_pubsub_stanza:pubsub_owner_iq(
                        ~"set", Alice, OwnerSetId, pep, [UnknownAction]),
    pubsub_tools:send_request_and_receive_response(Alice, OwnerSetRequest, OwnerSetId, Opts),

    OwnerGetId = ~"owner-get-unknown-action",
    OwnerGetRequest = escalus_pubsub_stanza:pubsub_owner_iq(
                        ~"get", Alice, OwnerGetId, pep, [UnknownAction]),
    pubsub_tools:send_request_and_receive_response(Alice, OwnerGetRequest, OwnerGetId, Opts).

create_node_at_foreign_jid_fails_story(Alice, Bob) ->
    Opts = [{expected_error_type, {~"auth", ~"forbidden"}}],

    %% XEP-0060 8.1 Create a Node, Example 128
    pubsub_tools:create_node(Bob, make_pep_node_info(Alice, random_node_ns()), Opts),

    %% XEP-0060 7.1.3.1 Insufficient Privileges (auto-create attempt)
    pubsub_tools:publish(Bob, ~"item2", make_pep_node_info(Alice, random_node_ns()), Opts).

create_node_invalid_config_fails_story(Alice) ->
    create_node_with_invalid_config(Alice, [{~"pubsub#definitely_invalid_option", ~"1"}],
                                    {~"modify", ~"bad-request"}),

    create_node_with_invalid_config(Alice, [{~"pubsub#access_model", ~"not-a-real-model"}],
                                    {~"modify", ~"bad-request"}),

    %% Malformed create configuration form
    PepNode = make_pep_node_info(Alice, random_node_ns()),
    MalformedConfigOpts = [{config, [{~"pubsub#access_model", ~"open"}]},
                           {expected_error_type, {~"modify", ~"bad-request"}},
                           {modify_request, fun form_helper:remove_form_types/1}],
    pubsub_tools:create_node(Alice, PepNode, MalformedConfigOpts),

    create_node_with_invalid_config(Alice, [{~"pubsub#access_model", ~"authorize"}],
                                    {~"modify", ~"not-acceptable", ~"unsupported-access-model"}).

manage_nonexistent_node_fails_story(Alice) ->
    PepNode = make_pep_node_info(Alice, random_node_ns()),
    Opts = [{expected_error_type, {~"cancel", ~"item-not-found"}}],

    %% XEP-0060 8.2.3.5 Node Does Not Exist
    pubsub_tools:get_configuration(Alice, PepNode, Opts),

    %% XEP-0060 8.2.3.5 Node Does Not Exist: same missing-node condition applies to 'set'
    pubsub_tools:set_configuration(Alice, PepNode, [{~"pubsub#access_model", ~"open"}], Opts),

    %% XEP-0060 8.2.4 Node Does Not Exist
    pubsub_tools:delete_node(Alice, PepNode, Opts).

manage_foreign_node_fails_story(Alice, Bob) ->
    PepNode = make_pep_node_info(Alice, random_node_ns()),
    pubsub_tools:create_node(Alice, PepNode, []),
    Opts = [{expected_error_type, {~"auth", ~"forbidden"}}],

    %% XEP-0060 8.5.3.1 Insufficient Privileges
    pubsub_tools:get_configuration(Bob, PepNode, Opts),

    %% XEP-0060 8.5.6.1 Insufficient Privileges
    pubsub_tools:set_configuration(Bob, PepNode, [{~"pubsub#access_model", ~"open"}], Opts),

    %% XEP-0060 8.4.3.1 Insufficient Privileges (existing node)
    pubsub_tools:delete_node(Bob, PepNode, Opts),

    %% XEP-0060 8.4.3.1 Insufficient Privileges (non-existent node)
    pubsub_tools:delete_node(Bob, make_pep_node_info(Alice, random_node_ns()), Opts).

subscribe_to_nonexistent_node_fails_story(Alice) ->
    NodeNS = random_node_ns(),
    PepNode = make_pep_node_info(Alice, NodeNS),
    Opts = [{expected_error_type, {~"cancel", ~"item-not-found"}}],

    %% XEP-0060 6.1.3.4 Node Does Not Exist
    pubsub_tools:subscribe(Alice, PepNode, Opts),

    %% XEP-0060 6.2.3.4 Node Does Not Exist
    pubsub_tools:unsubscribe(Alice, PepNode, Opts).

subscribe_with_invalid_jid_fails_story(Alice, Bob) ->
    NodeNS = random_node_ns(),
    PepNode = make_pep_node_info(Alice, NodeNS),
    create_node_with_config(Alice, PepNode, [{~"pubsub#access_model", ~"open"}]),

    %% XEP-0060 6.1.3.3 Invalid JID
    InvalidJid = escalus_utils:get_short_jid(Alice),
    Opts = [{subscriber_jid, InvalidJid},
            {expected_error_type, {~"modify", ~"bad-request", ~"invalid-jid"}}],
    pubsub_tools:subscribe(Bob, PepNode, Opts).

publish_to_foreign_node_fails_story(Alice, Bob) ->
    NodeNS = random_node_ns(),
    PepNode = make_pep_node_info(Alice, NodeNS),
    pubsub_tools:create_node(Alice, PepNode, []),

    %% XEP-0060 7.1.3.1 Insufficient Privileges
    Opts = [{expected_error_type, {~"auth", ~"forbidden"}}],
    pubsub_tools:publish(Bob, ~"item1", PepNode, Opts).

publish_with_invalid_items_fails_story(Alice) ->
    Node = {pep, random_node_ns()},

    %% XEP-0060 7.1.3.4 Item Required
    ItemRequiredOpts = [{with_payload, false},
                        {expected_error_type, {~"modify", ~"bad-request", ~"item-required"}}],
    pubsub_tools:publish(Alice, ~"item1", Node, ItemRequiredOpts),

    %% XEP-0060 7.1.3.6 Payload Required
    PayloadRequiredOpts = [{publish_shape, item_without_payload},
                           {expected_error_type, {~"modify", ~"bad-request", ~"payload-required"}}],
    pubsub_tools:publish(Alice, ~"item2", Node, PayloadRequiredOpts),

    %% XEP-0060 7.1.3.6 Invalid Payload
    InvalidPayloadOpts = [{publish_shape, item_with_multiple_payloads},
                          {expected_error_type, {~"modify", ~"bad-request", ~"invalid-payload"}}],
    pubsub_tools:publish(Alice, ~"item3", Node, InvalidPayloadOpts),

    %% XEP-0060 7.1.3.5 Too Many Items
    BadRequestOpts = [{expected_error_type, {~"modify", ~"bad-request"}}],
    TooManyItemsOpts = [{publish_shape, multiple_items} | BadRequestOpts],
    pubsub_tools:publish(Alice, ~"item4", Node, TooManyItemsOpts),

    %% Malformed publish request: direct child is not <item/>.
    InvalidChildOpts = [{publish_shape, invalid_child} | BadRequestOpts],
    pubsub_tools:publish(Alice, ~"item5", Node, InvalidChildOpts).

publish_options_invalid_config_fails_story(Alice) ->
    PreconditionOpts = [{expected_error_type, {~"cancel", ~"conflict", ~"precondition-not-met"}}],

    %% XEP-0060 7.1.5 Publishing Options: unmet publish-options precondition
    UnknownOptionFields = [{~"pubsub#definitely_invalid_option", ~"1"}],
    pubsub_tools:publish_with_options(Alice, ~"item1", {pep, random_node_ns()},
                                      PreconditionOpts, UnknownOptionFields),

    %% XEP-0060 7.1.5 Publishing Options: unmet publish-options precondition
    InvalidAccessModelFields = [{~"pubsub#access_model", ~"not-a-real-model"}],
    pubsub_tools:publish_with_options(Alice, ~"item1", {pep, random_node_ns()},
                                      PreconditionOpts, InvalidAccessModelFields),

    %% XEP-0060 7.1.5 Publishing Options: unmet publish-options precondition
    NodeNS = random_node_ns(),
    PepNode = make_pep_node_info(Alice, NodeNS),
    pubsub_tools:create_node(Alice, PepNode, []),
    Fields = [{~"pubsub#access_model", ~"open"}],
    pubsub_tools:publish_with_options(Alice, ~"item1", {pep, NodeNS}, PreconditionOpts, Fields),

    %% Malformed publish-options form
    MalformedFormOpts = [{expected_error_type, {~"modify", ~"bad-request"}},
                         {publish_options_form_type, ~"WRONG_NS"}],
    pubsub_tools:publish_with_options(Alice, ~"item1", {pep, random_node_ns()},
                                      MalformedFormOpts, Fields).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

create_node_with_config(Client, PepNode, Fields) ->
    pubsub_tools:create_node(Client, PepNode, [{config, Fields}]).

create_node_with_invalid_config(Client, Config, ExpectedError) ->
    PepNode = make_pep_node_info(Client, random_node_ns()),
    Opts = [{config, Config}, {expected_error_type, ExpectedError}],
    pubsub_tools:create_node(Client, PepNode, Opts).

required_modules(pep) ->
    [{mod_caps, default_mod_config(mod_caps)},
     {mod_pubsub, default_mod_config(mod_pubsub)}].

make_pep_node_info(Client, NodeName) ->
    {escalus_utils:jid_to_lower(escalus_utils:get_short_jid(Client)), NodeName}.

set_caps(Config) ->
    set_caps(Config, random_name()).

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
        <<"alice", _/binary>> -> false;
        <<"mike", _/binary>> -> false;
        _ -> true
    end.

pep_disco_features() ->
    [?NS_PUBSUB,
     <<?NS_PUBSUB/binary, "#access-open">>,
     <<?NS_PUBSUB/binary, "#access-presence">>,
     <<?NS_PUBSUB/binary, "#auto-create">>,
     <<?NS_PUBSUB/binary, "#config-node">>,
     <<?NS_PUBSUB/binary, "#create-and-configure">>,
     <<?NS_PUBSUB/binary, "#create-nodes">>,
     <<?NS_PUBSUB/binary, "#delete-nodes">>,
     <<?NS_PUBSUB/binary, "#filtered-notifications">>,
     <<?NS_PUBSUB/binary, "#item-ids">>,
     <<?NS_PUBSUB/binary, "#last-published">>,
     <<?NS_PUBSUB/binary, "#persistent-items">>,
     <<?NS_PUBSUB/binary, "#publish">>,
     <<?NS_PUBSUB/binary, "#publish-options">>,
     <<?NS_PUBSUB/binary, "#retrieve-items">>,
     <<?NS_PUBSUB/binary, "#subscribe">>].

features(NodeNS) ->
    [NodeNS, ns_notify(NodeNS)].

ns_notify(NS) ->
    <<NS/binary, "+notify">>.

random_node_ns() ->
    random_name().

random_name() ->
    base64:encode(crypto:strong_rand_bytes(16)).

nonexistent_bare_jid() ->
    <<"unknown@", (domain_helper:domain())/binary>>.

assert_no_disco_info(Client, OwnerJid) ->
    Request = escalus_stanza:disco_info(OwnerJid),
    escalus:send(Client, Request),
    Stanza = escalus:wait_for_stanza(Client),
    escalus:assert(is_iq_error, [Request], Stanza),
    escalus:assert(is_error, [~"cancel", ~"service-unavailable"], Stanza),
    escalus:assert(is_stanza_from, [OwnerJid], Stanza).

assert_disco_info(Client, OwnerJid) ->
    escalus:send(Client, escalus_stanza:disco_info(OwnerJid)),
    Stanza = escalus:wait_for_stanza(Client),
    ?assertNot(escalus_pred:has_identity(~"pubsub", ~"service", Stanza)),
    escalus:assert(has_identity, [~"pubsub", ~"pep"], Stanza),
    lists:foreach(fun(Feature) ->
                          escalus:assert(has_feature, [Feature], Stanza)
                  end, pep_disco_features()),
    escalus:assert(is_stanza_from, [OwnerJid], Stanza).

assert_no_disco_items(Client, OwnerJid) ->
    Query = disco_items_query(Client, OwnerJid),
    ?assertEqual([], exml_query:subelements(Query, ~"item")).

assert_disco_items(Client, OwnerJid, NodeNS) ->
    Query = disco_items_query(Client, OwnerJid),
    Item = exml_query:subelement_with_attr(Query, ~"node", NodeNS),
    ?assertEqual(jid:str_tolower(OwnerJid), exml_query:attr(Item, ~"jid")).

assert_no_disco_items(Client, OwnerJid, NodeNS) ->
    Query = disco_items_query(Client, OwnerJid),
    ?assertEqual(undefined, exml_query:subelement_with_attr(Query, ~"node", NodeNS)).

assert_no_disco_items_node(Client, OwnerJid, NodeNS, ErrorCondition) ->
    Request = escalus_stanza:disco_items(OwnerJid, NodeNS),
    escalus:send(Client, Request),
    Stanza = escalus:wait_for_stanza(Client),
    escalus:assert(is_iq_error, [Request], Stanza),
    escalus:assert(is_error, [~"cancel", ErrorCondition], Stanza),
    escalus:assert(is_stanza_from, [OwnerJid], Stanza).

assert_disco_info_node(Client, OwnerJid, NodeNS) ->
    escalus:send(Client, escalus_stanza:disco_info(OwnerJid, NodeNS)),
    Stanza = escalus:wait_for_stanza(Client),
    escalus:assert(is_stanza_from, [OwnerJid], Stanza),
    escalus:assert(is_iq_result, Stanza),
    Query = exml_query:subelement(Stanza, ~"query"),
    ?assertEqual(NodeNS, exml_query:attr(Query, ~"node")),
    escalus:assert(has_identity, [~"pubsub", ~"leaf"], Stanza),
    escalus:assert(has_feature, [?NS_PUBSUB], Stanza).

assert_no_disco_info_node(Client, OwnerJid, NodeNS, ErrorCondition) ->
    escalus:send(Client, escalus_stanza:disco_info(OwnerJid, NodeNS)),
    Stanza = escalus:wait_for_stanza(Client),
    escalus:assert(is_stanza_from, [OwnerJid], Stanza),
    escalus:assert(is_error, [~"cancel", ErrorCondition], Stanza).

assert_publish_result(Stanza, NodeNS, ItemId) ->
    Pubsub = exml_query:subelement(Stanza, ~"pubsub"),
    Publish = exml_query:subelement(Pubsub, ~"publish"),
    ?assertEqual(NodeNS, exml_query:attr(Publish, ~"node")),
    Item = exml_query:subelement(Publish, ~"item"),
    ?assertEqual(ItemId, exml_query:attr(Item, ~"id")).

assert_delayed_notification(Stanza) ->
    DelayEl = exml_query:subelement(Stanza, ~"delay"),
    escalus:assert(has_ns, [?NS_DELAY], DelayEl),
    ?assertMatch(Stamp when is_binary(Stamp), exml_query:attr(DelayEl, ~"stamp")).

disco_items_query(Client, OwnerJid) ->
    escalus:send(Client, escalus_stanza:disco_items(OwnerJid)),
    Stanza = escalus:wait_for_stanza(Client),
    escalus:assert(is_stanza_from, [OwnerJid], Stanza),
    exml_query:subelement(Stanza, ~"query").

send_presence(From, Type, To) ->
    ToJid = escalus_client:short_jid(To),
    Stanza = escalus_stanza:presence_direct(ToJid, Type),
    escalus_client:send(From, Stanza).

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
