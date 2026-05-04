%%%===================================================================
%%% @copyright (C) 2016, Erlang Solutions Ltd.
%%% @doc Suite for testing Personal Eventing Protocol features
%%%      as described in XEP-0163
%%% @end
%%%===================================================================

-module(pep_SUITE).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [require_rpc_nodes/1]).
-import(config_parser_helper, [default_mod_config/1]).
-import(domain_helper, [host_type/0]).

-define(NS_PUBSUB_PUB_OPTIONS,  <<"http://jabber.org/protocol/pubsub#publish-options">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, pep}
    ].

groups() ->
    [
     {pep, [parallel], pep_tests()}
    ].

pep_tests() ->
    [disco_info_sm_bare_jid,
     disco_info_sm_node,
     disco_items_sm,
     disco_items_sm_open,
     auto_create_and_publish_implicit_sub,
     auto_create_and_publish_self_notify,
     create_presence_and_publish_implicit_sub,
     create_presence_and_publish_explicit_sub,
     create_presence_and_publish_no_sub,
     create_and_delete_node,
     create_node_with_empty_config,
     create_and_configure_node,
     request_without_nodeid_fails,
     request_with_unknown_action_fails,
     manage_nonexistent_node,
     manage_other_user_node,
     subscribe_to_nonexistent_node,
     subscribe_with_invalid_jid_fails,
     publish_to_other_user_node_fails,
     publish_with_invalid_items_fails,
     create_node_at_other_user_jid_fails,
     auto_create_open_and_publish_explicit_sub,
     publish_options_fail_invalid_config,
     create_node_fail_invalid_config,
     create_open_and_publish_explicit_sub,
     create_open_and_publish_implicit_sub,
     create_open_and_publish_both_subs,
     create_open_and_publish_no_sub,
     send_caps_after_login_test,
     send_last_item_on_implicit_sub,
     send_last_item_on_implicit_sub_with_sm,
     h_ok_after_notify_test
    ].

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

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
%% Test cases for XEP-0163
%% Comments in test cases refer to sections is the XEP
%%--------------------------------------------------------------------

%% Group: pep_tests (sequence)

disco_info_sm_bare_jid(Config) ->
    escalus:fresh_story(
        Config,
        [{alice, 1}],
        fun(Alice) ->
            AliceJid = escalus_client:short_jid(Alice),
            escalus:send(Alice, escalus_stanza:disco_info(AliceJid)),
            Stanza = escalus:wait_for_stanza(Alice),
            ?assertNot(escalus_pred:has_identity(~"pubsub", ~"service", Stanza)),
            escalus:assert(has_identity, [~"pubsub", ~"pep"], Stanza),
            lists:foreach(fun(Feature) ->
                                  escalus:assert(has_feature, [Feature], Stanza)
                          end, pep_disco_features()),
            escalus:assert(is_stanza_from, [AliceJid], Stanza)
        end).

disco_info_sm_node(Config) ->
    NodeNS = random_node_ns(),
    escalus:fresh_story(
        Config,
        [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, Kate) ->
            AliceJid = escalus_client:short_jid(Alice),
            pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),
            make_friends(Bob, Alice, 0),
            assert_disco_info_node(Alice, AliceJid, NodeNS),
            assert_disco_info_node(Bob, AliceJid, NodeNS),
            assert_no_disco_info_node(Kate, AliceJid, NodeNS)
        end).

disco_items_sm(Config) ->
    NodeNS = random_node_ns(),
    escalus:fresh_story(
        Config,
        [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, Kate) ->
            AliceJid = escalus_client:short_jid(Alice),
            pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),
            make_friends(Bob, Alice, 0),
            assert_disco_items_node(Alice, AliceJid, NodeNS),
            assert_disco_items_node(Bob, AliceJid, NodeNS),
            assert_no_disco_items_node(Kate, AliceJid, NodeNS)
        end).

disco_items_sm_open(Config) ->
    NodeNS = random_node_ns(),
    escalus:fresh_story(
        Config,
        [{alice, 1}, {bob, 1}, {kate, 1}],
        fun(Alice, Bob, Kate) ->
            AliceJid = escalus_client:short_jid(Alice),
            PepNode = make_pep_node_info(Alice, NodeNS),
            pubsub_tools:create_node(Alice, PepNode, [{config, [{~"pubsub#access_model", ~"open"}]}]),
            pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),
            make_friends(Bob, Alice, 0),
            assert_disco_items_node(Alice, AliceJid, NodeNS),
            assert_disco_items_node(Bob, AliceJid, NodeNS),
            assert_disco_items_node(Kate, AliceJid, NodeNS)
        end).

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
     <<?NS_PUBSUB/binary, "#persistent-items">>,
     <<?NS_PUBSUB/binary, "#publish">>,
     <<?NS_PUBSUB/binary, "#publish-options">>,
     <<?NS_PUBSUB/binary, "#retrieve-items">>,
     <<?NS_PUBSUB/binary, "#subscribe">>].

auto_create_and_publish_implicit_sub(Config) ->
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {bob, 1}],
                                    fun auto_create_and_publish_implicit_sub_story/3).

auto_create_and_publish_self_notify(Config) ->
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{bob, 1}],
                                    fun auto_create_and_publish_self_notify_story/2).

create_presence_and_publish_implicit_sub(Config) ->
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {bob, 1}],
                                    fun create_presence_and_publish_implicit_sub_story/3).

create_presence_and_publish_explicit_sub(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun create_presence_and_publish_explicit_sub_story/3).

create_presence_and_publish_no_sub(Config) ->
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {bob, 1}, {mike, 1}],
                                    fun create_presence_and_publish_no_sub_story/4).

create_and_delete_node(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}],
                        fun create_and_delete_node_story/2).

create_node_with_empty_config(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun create_node_with_empty_config_story/1).

create_and_configure_node(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun create_and_configure_node_story/1).

request_without_nodeid_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun request_without_nodeid_fails_story/1).

request_with_unknown_action_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun request_with_unknown_action_fails_story/1).

manage_nonexistent_node(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun manage_nonexistent_node_story/1).

manage_other_user_node(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}],
                        fun manage_other_user_node_story/2).

subscribe_to_nonexistent_node(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun subscribe_to_nonexistent_node_story/1).

subscribe_with_invalid_jid_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}],
                        fun subscribe_with_invalid_jid_fails_story/2).

publish_to_other_user_node_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}],
                        fun publish_to_other_user_node_fails_story/2).

publish_with_invalid_items_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun publish_with_invalid_items_fails_story/1).

create_node_at_other_user_jid_fails(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}],
                        fun create_node_at_other_user_jid_fails_story/2).

auto_create_open_and_publish_explicit_sub(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun auto_create_open_and_publish_explicit_sub_story/3).

publish_options_fail_invalid_config(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun publish_options_fail_invalid_config_story/1).

create_node_fail_invalid_config(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun create_node_fail_invalid_config_story/1).

create_open_and_publish_explicit_sub(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun create_open_and_publish_explicit_sub_story/3).

create_open_and_publish_implicit_sub(Config) ->
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {bob, 1}],
                                    fun create_open_and_publish_implicit_sub_story/3).

create_open_and_publish_both_subs(Config) ->
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {bob, 1}],
                                    fun create_open_and_publish_both_subs_story/3).

create_open_and_publish_no_sub(Config) ->
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {bob, 1}, {mike, 1}],
                                    fun create_open_and_publish_no_sub_story/4).

auto_create_and_publish_implicit_sub_story(Config, Alice, Bob) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = make_pep_node_info(Alice, NodeNS),
    make_friends(Bob, Alice, 0),
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),
    pubsub_tools:receive_item_notification(
      Bob, ~"item1", {escalus_utils:get_short_jid(Alice), NodeNS}, []),
    pubsub_tools:get_all_items(Bob, PepNode, [{expected_result, [~"item1"]}]).

auto_create_and_publish_self_notify_story(Config, Bob) ->
    NodeNS = ?config(node_ns, Config),
    BobJid = escalus_utils:get_short_jid(Bob),
    {Resp, Msg} = pubsub_tools:publish(Bob, ~"item1", {pep, NodeNS},
                                       [{expected_notification, BobJid}]),
    assert_publish_result(Resp, NodeNS, ~"item1"),
    pubsub_tools:check_item_notification(Msg, ~"item1", {BobJid, NodeNS}, []),
    pubsub_tools:get_all_items(Bob, {pep, NodeNS}, [{expected_result, [~"item1"]}]),
    pubsub_tools:get_item(Bob, {pep, NodeNS}, ~"item1", [{expected_result, [~"item1"]}]).

create_presence_and_publish_implicit_sub_story(Config, Alice, Bob) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = make_pep_node_info(Alice, NodeNS),
    pubsub_tools:create_node(Alice, PepNode, []),
    make_friends(Bob, Alice, 0),
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),
    pubsub_tools:receive_item_notification(
      Bob, ~"item1", {escalus_utils:get_short_jid(Alice), NodeNS}, []),
    pubsub_tools:get_all_items(Bob, PepNode, [{expected_result, [~"item1"]}]),
    pubsub_tools:get_item(Bob, PepNode, ~"item1", [{expected_result, [~"item1"]}]).

create_presence_and_publish_explicit_sub_story(_Config, Alice, Bob) ->
    NodeNS = random_node_ns(),
    PepNode = make_pep_node_info(Alice, NodeNS),
    pubsub_tools:create_node(Alice, PepNode, []),

    %% XEP-0060 6.1.3.2 Presence Subscription Required
    Result = pubsub_tools:subscribe(Bob, PepNode, [{expected_error_type, ~"auth"}]),
    #xmlel{} = exml_query:subelement(Result, ~"presence-subscription-required").

create_presence_and_publish_no_sub_story(Config, Alice, Bob, Mike) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = make_pep_node_info(Alice, NodeNS),
    pubsub_tools:create_node(Alice, PepNode, []),
    make_friends(Mike, Alice, 0),
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),
    [] = escalus:wait_for_stanzas(Alice, 1, 500), % account owner without caps
    [] = escalus:wait_for_stanzas(Bob, 1, 500), % caps but no presence subscription
    [] = escalus:wait_for_stanzas(Mike, 1, 500), % presence subscription but no caps
    pubsub_tools:get_all_items(Alice, PepNode, [{expected_result, [~"item1"]}]),
    pubsub_tools:get_item(Alice, PepNode, ~"item1", [{expected_result, [~"item1"]}]),
    pubsub_tools:get_all_items(Bob, PepNode, [{expected_error_type, ~"auth"}]),
    pubsub_tools:get_item(Bob, PepNode, ~"item1", [{expected_error_type, ~"auth"}]),
    pubsub_tools:get_all_items(Mike, PepNode, [{expected_result, [~"item1"]}]),
    pubsub_tools:get_item(Mike, PepNode, ~"item1", [{expected_result, [~"item1"]}]).

create_and_delete_node_story(Alice, Bob) ->
    PepNode = make_pep_node_info(Alice, random_node_ns()),
    pubsub_tools:create_node(Alice, PepNode, [{config, [{~"pubsub#access_model", ~"open"}]}]),
    pubsub_tools:subscribe(Bob, PepNode, []),

    %% XEP-0060 8.1.3.2 Node Already Exists
    DuplicateCreateResult = pubsub_tools:create_node(Alice, PepNode, [{expected_error_type, ~"cancel"}]),
    escalus:assert(is_error, [~"cancel", ~"conflict"], DuplicateCreateResult),

    pubsub_tools:get_all_items(Alice, PepNode, [{expected_result, []}]),
    pubsub_tools:get_item(Alice, PepNode, ~"item1", [{expected_result, []}]),

    %% XEP-0060 8.2 Delete a Node
    pubsub_tools:delete_node(Alice, PepNode, []),
    pubsub_tools:receive_node_deletion_notification(Bob, PepNode, []),
    Result = pubsub_tools:get_all_items(Alice, PepNode, [{expected_error_type, ~"cancel"}]),
    escalus:assert(is_error, [~"cancel", ~"item-not-found"], Result),
    Result2 = pubsub_tools:get_item(Alice, PepNode, ~"item1", [{expected_error_type, ~"cancel"}]),
    escalus:assert(is_error, [~"cancel", ~"item-not-found"], Result2).

create_node_with_empty_config_story(Alice) ->
    PepNode = make_pep_node_info(Alice, random_node_ns()),
    pubsub_tools:create_node(Alice, PepNode,
                             [{modify_request, fun add_empty_config_to_create_node_request/1}]),
    pubsub_tools:get_configuration(Alice, PepNode, [{expected_result,
                                                     [{~"pubsub#access_model", ~"presence"}]}]).

create_and_configure_node_story(Alice) ->
    PepNode = make_pep_node_info(Alice, random_node_ns()),
    pubsub_tools:create_node(Alice, PepNode, []),
    pubsub_tools:get_configuration(Alice, PepNode, [{expected_result,
                                                     [{~"pubsub#access_model", ~"presence"}]}]),

    %% Malformed configuration form
    MalformedSetConfigResult = pubsub_tools:set_configuration(
                                 Alice, PepNode, [{~"pubsub#access_model", ~"open"}],
                                 [{expected_error_type, ~"modify"},
                                  {modify_request, fun form_helper:remove_form_types/1}]),
    escalus:assert(is_error, [~"modify", ~"bad-request"], MalformedSetConfigResult),

    %% XEP-0060 8.2.5.1 Success
    pubsub_tools:set_configuration(Alice, PepNode, [{~"pubsub#access_model", ~"open"}], []),
    pubsub_tools:get_configuration(Alice, PepNode, [{expected_result,
                                                     [{~"pubsub#access_model", ~"open"}]}]).

request_without_nodeid_fails_story(Alice) ->
    NodeName = random_node_ns(),
    PepNode = make_pep_node_info(Alice, NodeName),

    %% XEP-0060 8.1.2.3 NodeID Required
    CreateWithoutNodeIdResult = pubsub_tools:create_node(
                                  Alice, PepNode,
                                  [{expected_error_type, ~"modify"},
                                   {modify_request, fun remove_node_attr/1}]),
    escalus:assert(is_error, [~"modify", ~"not-acceptable"], CreateWithoutNodeIdResult),
    #xmlel{} = exml_query:subelement(CreateWithoutNodeIdResult, ~"nodeid-required"),

    %% XEP-0060 8.5.3.2 NodeID Required
    GetConfigWithoutNodeId = remove_node_attr(
        escalus_pubsub_stanza:get_configuration(Alice, <<"get-config-without-nodeid">>, PepNode)),
    escalus:send(Alice, GetConfigWithoutNodeId),
    GetConfigResult = escalus:wait_for_stanza(Alice),
    escalus:assert(is_error, [~"modify", ~"bad-request"], GetConfigResult),
    #xmlel{} = exml_query:subelement(GetConfigResult, ~"nodeid-required"),

    %% XEP-0060 8.5.3.2 NodeID Required
    SetConfigResult = pubsub_tools:set_configuration(
                        Alice, PepNode, [{~"pubsub#access_model", ~"open"}],
                        [{expected_error_type, ~"modify"},
                         {modify_request, fun remove_node_attr/1}]),
    escalus:assert(is_error, [~"modify", ~"bad-request"], SetConfigResult),
    #xmlel{} = exml_query:subelement(SetConfigResult, ~"nodeid-required").

request_with_unknown_action_fails_story(Alice) ->
    UnknownAction = #xmlel{name = ~"unknown-action"},

    PubsubSetId = ~"pubsub-set-unknown-action",
    PubsubSetRequest = escalus_pubsub_stanza:pubsub_iq(
                         ~"set", Alice, PubsubSetId, pep, [UnknownAction]),
    PubsubSetResult = pubsub_tools:send_request_and_receive_response(
                        Alice, PubsubSetRequest, PubsubSetId, [{expected_error_type, ~"modify"}]),
    escalus:assert(is_error, [~"modify", ~"bad-request"], PubsubSetResult),

    PubsubGetId = ~"pubsub-get-unknown-action",
    PubsubGetRequest = escalus_pubsub_stanza:pubsub_iq(
                         ~"get", Alice, PubsubGetId, pep, [UnknownAction]),
    PubsubGetResult = pubsub_tools:send_request_and_receive_response(
                        Alice, PubsubGetRequest, PubsubGetId, [{expected_error_type, ~"modify"}]),
    escalus:assert(is_error, [~"modify", ~"bad-request"], PubsubGetResult),

    OwnerSetId = ~"owner-set-unknown-action",
    OwnerSetRequest = escalus_pubsub_stanza:pubsub_owner_iq(
                        ~"set", Alice, OwnerSetId, pep, [UnknownAction]),
    OwnerSetResult = pubsub_tools:send_request_and_receive_response(
                       Alice, OwnerSetRequest, OwnerSetId, [{expected_error_type, ~"modify"}]),
    escalus:assert(is_error, [~"modify", ~"bad-request"], OwnerSetResult),

    OwnerGetId = ~"owner-get-unknown-action",
    OwnerGetRequest = escalus_pubsub_stanza:pubsub_owner_iq(
                        ~"get", Alice, OwnerGetId, pep, [UnknownAction]),
    OwnerGetResult = pubsub_tools:send_request_and_receive_response(
                       Alice, OwnerGetRequest, OwnerGetId, [{expected_error_type, ~"modify"}]),
    escalus:assert(is_error, [~"modify", ~"bad-request"], OwnerGetResult).

manage_nonexistent_node_story(Alice) ->
    PepNode = make_pep_node_info(Alice, random_node_ns()),

    %% XEP-0060 8.2.3.5 Node Does Not Exist
    Result0 = pubsub_tools:get_configuration(Alice, PepNode, [{expected_error_type, ~"cancel"}]),
    escalus:assert(is_error, [~"cancel", ~"item-not-found"], Result0),

    %% XEP-0060 8.2.3.5 Node Does Not Exist: same missing-node condition applies to 'set'
    Result00 = pubsub_tools:set_configuration(
                 Alice, PepNode, [{~"pubsub#access_model", ~"open"}], [{expected_error_type, ~"cancel"}]),
    escalus:assert(is_error, [~"cancel", ~"item-not-found"], Result00),

    %% XEP-0060 8.2.4 Node Does Not Exist
    Result = pubsub_tools:delete_node(Alice, PepNode, [{expected_error_type, ~"cancel"}]),
    escalus:assert(is_error, [~"cancel", ~"item-not-found"], Result).

manage_other_user_node_story(Alice, Bob) ->
    PepNode = make_pep_node_info(Alice, random_node_ns()),
    pubsub_tools:create_node(Alice, PepNode, []),

    %% XEP-0060 8.5.3.1 Insufficient Privileges
    Result0 = pubsub_tools:get_configuration(Bob, PepNode, [{expected_error_type, ~"auth"}]),
    escalus:assert(is_error, [~"auth", ~"forbidden"], Result0),

    %% XEP-0060 8.5.6.1 Insufficient Privileges
    Result00 = pubsub_tools:set_configuration(
                 Bob, PepNode, [{~"pubsub#access_model", ~"open"}], [{expected_error_type, ~"auth"}]),
    escalus:assert(is_error, [~"auth", ~"forbidden"], Result00),

    %% XEP-0060 8.4.3.1 Insufficient Privileges (existing node)
    Result = pubsub_tools:delete_node(Bob, PepNode, [{expected_error_type, ~"auth"}]),
    escalus:assert(is_error, [~"auth", ~"forbidden"], Result),

    %% XEP-0060 8.4.3.1 Insufficient Privileges (non-existent node)
    Result2 = pubsub_tools:delete_node(Bob, make_pep_node_info(Alice, random_node_ns()),
                                       [{expected_error_type, ~"auth"}]),
    escalus:assert(is_error, [~"auth", ~"forbidden"], Result2).

subscribe_to_nonexistent_node_story(Alice) ->
    NodeNS = random_node_ns(),
    PepNode = make_pep_node_info(Alice, NodeNS),

    %% XEP-0060 6.1.3.4 Node Does Not Exist
    Result = pubsub_tools:subscribe(Alice, PepNode, [{expected_error_type, ~"cancel"}]),
    escalus:assert(is_error, [~"cancel", ~"item-not-found"], Result),

    %% XEP-0060 6.2.3.4 Node Does Not Exist
    Result2 = pubsub_tools:unsubscribe(Alice, PepNode, [{expected_error_type, ~"cancel"}]),
    escalus:assert(is_error, [~"cancel", ~"item-not-found"], Result2).

subscribe_with_invalid_jid_fails_story(Alice, Bob) ->
    NodeNS = random_node_ns(),
    PepNode = make_pep_node_info(Alice, NodeNS),
    pubsub_tools:create_node(Alice, PepNode,
                             [{modify_request, fun add_open_access_model_to_create_node_request/1}]),

    %% XEP-0060 6.1.3.3 Invalid JID
    InvalidJid = escalus_utils:get_short_jid(Alice),
    Id = <<"invalid-jid-subscribe">>,
    Request0 = escalus_pubsub_stanza:subscribe(Bob, Id, PepNode, []),
    Request = set_subscribe_jid(Request0, InvalidJid),
    escalus:send(Bob, Request),
    Result = escalus:wait_for_stanza(Bob),
    escalus:assert(is_error, [~"modify", ~"bad-request"], Result),
    #xmlel{} = exml_query:subelement(Result, ~"invalid-jid").

publish_to_other_user_node_fails_story(Alice, Bob) ->
    NodeNS = random_node_ns(),
    PepNode = make_pep_node_info(Alice, NodeNS),
    pubsub_tools:create_node(Alice, PepNode, []),

    %% XEP-0060 7.1.3.1 Insufficient Privileges
    Result = pubsub_tools:publish(Bob, ~"item1", PepNode, [{expected_error_type, ~"auth"}]),
    escalus:assert(is_error, [~"auth", ~"forbidden"], Result).

publish_with_invalid_items_fails_story(Alice) ->
    Node = {pep, random_node_ns()},

    %% XEP-0060 7.1.3.4 Item Required
    Result = pubsub_tools:publish(Alice, ~"item1", Node,
                                  [{with_payload, false}, {expected_error_type, ~"modify"}]),
    escalus:assert(is_error, [~"modify", ~"bad-request"], Result),
    #xmlel{} = exml_query:subelement(Result, ~"item-required"),

    %% XEP-0060 7.1.3.6 Payload Required
    Id2 = pubsub_tools:id(Alice, Node, <<"publish-item-without-payload">>),
    Request2 = set_publish_item_children(
                 pubsub_tools:publish_request(Id2, Alice, ~"item2", Node, []), []),
    escalus:send(Alice, Request2),
    Result2 = escalus:wait_for_stanza(Alice),
    escalus:assert(is_error, [~"modify", ~"bad-request"], Result2),
    #xmlel{} = exml_query:subelement(Result2, ~"payload-required"),

    %% XEP-0060 7.1.3.6 Invalid Payload
    Id3 = pubsub_tools:id(Alice, Node, <<"publish-item-with-invalid-payload">>),
    Request3 = set_publish_item_children(
                 pubsub_tools:publish_request(Id3, Alice, ~"item3", Node, []),
                 [pubsub_tools:item_content(), pubsub_tools:item_content()]),
    escalus:send(Alice, Request3),
    Result3 = escalus:wait_for_stanza(Alice),
    escalus:assert(is_error, [~"modify", ~"bad-request"], Result3),
    #xmlel{} = exml_query:subelement(Result3, ~"invalid-payload"),

    %% XEP-0060 7.1.3.5 Too Many Items
    Id4 = pubsub_tools:id(Alice, Node, <<"publish-with-two-items">>),
    Request4 = duplicate_publish_item(pubsub_tools:publish_request(Id4, Alice, ~"item4", Node, [])),
    escalus:send(Alice, Request4),
    Result4 = escalus:wait_for_stanza(Alice),
    escalus:assert(is_error, [~"modify", ~"bad-request"], Result4),

    %% Malformed publish request: direct child is not <item/>.
    Id5 = pubsub_tools:id(Alice, Node, <<"publish-with-invalid-child">>),
    Request5 = set_publish_children(pubsub_tools:publish_request(Id5, Alice, ~"item5", Node, []),
                                    [pubsub_tools:item_content()]),
    escalus:send(Alice, Request5),
    Result5 = escalus:wait_for_stanza(Alice),
    escalus:assert(is_error, [~"modify", ~"bad-request"], Result5).

create_node_at_other_user_jid_fails_story(Alice, Bob) ->
    %% XEP-0060 8.1 Create a Node, Example 128
    Result = pubsub_tools:create_node(Bob, make_pep_node_info(Alice, random_node_ns()),
                                      [{expected_error_type, ~"auth"}]),
    escalus:assert(is_error, [~"auth", ~"forbidden"], Result),

    %% XEP-0060 7.1.3.1 Insufficient Privileges (auto-create attempt)
    Result2 = pubsub_tools:publish(Bob, ~"item2", make_pep_node_info(Alice, random_node_ns()),
                                   [{expected_error_type, ~"auth"}]),
    escalus:assert(is_error, [~"auth", ~"forbidden"], Result2).

auto_create_open_and_publish_explicit_sub_story(_Config, Alice, Bob) ->
    NodeNS = random_node_ns(),
    PepNode = make_pep_node_info(Alice, NodeNS),
    PublishOptions = [{~"pubsub#access_model", ~"open"}],
    pubsub_tools:publish_with_options(Alice, ~"item0", {pep, NodeNS}, [], PublishOptions),
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),
    {_Resp, LastItemMsg} = pubsub_tools:subscribe(
                             Bob, PepNode,
                             [{expected_notification, escalus_utils:get_short_jid(Alice)}]),
    pubsub_tools:check_item_notification(LastItemMsg, ~"item1",
                                         {escalus_utils:get_short_jid(Alice), NodeNS}, []),
    pubsub_tools:publish(Alice, ~"item2", {pep, NodeNS}, []),
    pubsub_tools:receive_item_notification(
      Bob, ~"item2", {escalus_utils:get_short_jid(Alice), NodeNS}, []),
    pubsub_tools:get_all_items(Bob, PepNode, [{expected_result, [~"item0", ~"item1", ~"item2"]}]),
    pubsub_tools:get_item(Bob, PepNode, ~"item1", [{expected_result, [~"item1"]}]),
    pubsub_tools:get_items(Bob, PepNode, [~"item1", ~"item404", ~"item0"],
                           [{expected_result, [~"item1", ~"item0"]}]).

publish_options_fail_invalid_config_story(Alice) ->
    %% XEP-0060 7.1.5 Publishing Options: unmet publish-options precondition
    Result = publish_with_publish_options(Alice, {pep, random_node_ns()}, ~"item1",
                                          [{~"pubsub#definitely_invalid_option", ~"1"}]),
    escalus:assert(is_error, [~"cancel", ~"conflict"], Result),
    #xmlel{} = exml_query:subelement(Result, ~"precondition-not-met"),

    %% XEP-0060 7.1.5 Publishing Options: unmet publish-options precondition
    Result2 = publish_with_publish_options(Alice, {pep, random_node_ns()}, ~"item1",
                                           [{~"pubsub#access_model", ~"not-a-real-model"}]),
    escalus:assert(is_error, [~"cancel", ~"conflict"], Result2),
    #xmlel{} = exml_query:subelement(Result2, ~"precondition-not-met"),

    %% XEP-0060 7.1.5 Publishing Options: unmet publish-options precondition
    NodeNS = random_node_ns(),
    PepNode = make_pep_node_info(Alice, NodeNS),
    pubsub_tools:create_node(Alice, PepNode, []),
    ResultExisting = publish_with_publish_options(Alice, {pep, NodeNS}, ~"item1",
                                                  [{~"pubsub#access_model", ~"open"}]),
    escalus:assert(is_error, [~"cancel", ~"conflict"], ResultExisting),
    #xmlel{} = exml_query:subelement(ResultExisting, ~"precondition-not-met"),

    %% Malformed publish-options form
    Result3 = publish_with_publish_options(Alice, {pep, random_node_ns()}, ~"item1",
                                           [{~"pubsub#access_model", ~"open"}], ~"WRONG_NS"),
    escalus:assert(is_error, [~"modify", ~"bad-request"], Result3).

create_node_fail_invalid_config_story(Alice) ->
    Result = create_node_with_config(Alice, [{~"pubsub#definitely_invalid_option", ~"1"}]),
    escalus:assert(is_error, [~"modify", ~"bad-request"], Result),

    Result2 = create_node_with_config(Alice, [{~"pubsub#access_model", ~"not-a-real-model"}]),
    escalus:assert(is_error, [~"modify", ~"bad-request"], Result2),

    %% Malformed create configuration form
    PepNode = make_pep_node_info(Alice, random_node_ns()),
    ResultMalformed = pubsub_tools:create_node(
                        Alice, PepNode,
                        [{config, [{~"pubsub#access_model", ~"open"}]},
                         {expected_error_type, ~"modify"},
                         {modify_request, fun form_helper:remove_form_types/1}]),
    escalus:assert(is_error, [~"modify", ~"bad-request"], ResultMalformed),

    Result3 = create_node_with_config(Alice, [{~"pubsub#access_model", ~"authorize"}]),
    escalus:assert(is_error, [~"modify", ~"not-acceptable"], Result3),
    #xmlel{} = exml_query:subelement(Result3, ~"unsupported-access-model").

create_open_and_publish_explicit_sub_story(_Config, Alice, Bob) ->
    NodeNS = random_node_ns(),
    PepNode = make_pep_node_info(Alice, NodeNS),
    pubsub_tools:create_node(Alice, PepNode,
                             [{modify_request, fun add_open_access_model_to_create_node_request/1}]),
    pubsub_tools:subscribe(Bob, PepNode, []),
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),
    pubsub_tools:receive_item_notification(
      Bob, ~"item1", {escalus_utils:get_short_jid(Alice), NodeNS}, []),
    pubsub_tools:get_all_items(Bob, PepNode, [{expected_result, [~"item1"]}]),
    pubsub_tools:get_item(Bob, PepNode, ~"item1", [{expected_result, [~"item1"]}]),

    %% XEP-0060 6.2.3.1 Insufficient Privileges
    InvalidJid = escalus_utils:get_short_jid(Alice),
    InvalidUnsubId = <<"invalid-jid-unsubscribe">>,
    InvalidUnsubRequest0 = escalus_pubsub_stanza:unsubscribe(
                             escalus_utils:get_short_jid(Bob), InvalidUnsubId, PepNode),
    InvalidUnsubRequest = set_subscribe_jid(InvalidUnsubRequest0, InvalidJid),
    escalus:send(Bob, InvalidUnsubRequest),
    InvalidUnsubResult = escalus:wait_for_stanza(Bob),
    escalus:assert(is_error, [~"auth", ~"forbidden"], InvalidUnsubResult),

    %% XEP-0060 6.2.2 Success Case
    pubsub_tools:unsubscribe(Bob, PepNode, []),
    pubsub_tools:publish(Alice, ~"item2", {pep, NodeNS}, []),
    [] = escalus:wait_for_stanzas(Bob, 1, 500).

create_open_and_publish_implicit_sub_story(Config, Alice, Bob) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = make_pep_node_info(Alice, NodeNS),
    pubsub_tools:create_node(Alice, PepNode,
                             [{modify_request, fun add_open_access_model_to_create_node_request/1}]),
    make_friends(Bob, Alice, 0),
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),
    pubsub_tools:receive_item_notification(
      Bob, ~"item1", {escalus_utils:get_short_jid(Alice), NodeNS}, []),
    pubsub_tools:get_all_items(Bob, PepNode, [{expected_result, [~"item1"]}]),
    pubsub_tools:get_item(Bob, PepNode, ~"item1", [{expected_result, [~"item1"]}]),

    %% XEP-0060 6.2.3.2 No Such Subscriber
    Result = pubsub_tools:unsubscribe(Bob, PepNode, [{expected_error_type, ~"cancel"}]),
    escalus:assert(is_error, [~"cancel", ~"unexpected-request"], Result),
    #xmlel{} = exml_query:subelement(Result, ~"not-subscribed").

create_open_and_publish_both_subs_story(Config, Alice, Bob) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = make_pep_node_info(Alice, NodeNS),
    pubsub_tools:create_node(Alice, PepNode,
                             [{modify_request, fun add_open_access_model_to_create_node_request/1}]),
    make_friends(Bob, Alice, 0),
    pubsub_tools:subscribe(Bob, PepNode, []),
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),
    pubsub_tools:receive_item_notification(
      Bob, ~"item1", {escalus_utils:get_short_jid(Alice), NodeNS}, []),
    pubsub_tools:get_all_items(Bob, PepNode, [{expected_result, [~"item1"]}]),
    pubsub_tools:get_item(Bob, PepNode, ~"item1", [{expected_result, [~"item1"]}]),
    [] = escalus:wait_for_stanzas(Bob, 1, 500).

create_open_and_publish_no_sub_story(Config, Alice, Bob, Mike) ->
    NodeNS = ?config(node_ns, Config),
    PepNode = make_pep_node_info(Alice, NodeNS),
    pubsub_tools:create_node(Alice, PepNode,
                             [{modify_request, fun add_open_access_model_to_create_node_request/1}]),
    make_friends(Mike, Alice, 0),
    pubsub_tools:publish(Alice, ~"item1", {pep, NodeNS}, []),
    [] = escalus:wait_for_stanzas(Alice, 1, 500), % account owner without caps
    [] = escalus:wait_for_stanzas(Bob, 1, 500), % caps but no presence subscription
    [] = escalus:wait_for_stanzas(Mike, 1, 500), % presence subscription but no caps
    pubsub_tools:get_all_items(Alice, PepNode, [{expected_result, [~"item1"]}]),
    pubsub_tools:get_item(Alice, PepNode, ~"item1", [{expected_result, [~"item1"]}]),
    pubsub_tools:get_all_items(Bob, PepNode, [{expected_result, [~"item1"]}]),
    pubsub_tools:get_item(Bob, PepNode, ~"item1", [{expected_result, [~"item1"]}]),
    pubsub_tools:get_all_items(Mike, PepNode, [{expected_result, [~"item1"]}]),
    pubsub_tools:get_item(Mike, PepNode, ~"item1", [{expected_result, [~"item1"]}]).

send_caps_after_login_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              NodeNS = random_node_ns(),
              pubsub_tools:publish(Alice, <<"item2">>, {pep, NodeNS}, []),

              escalus_story:make_all_clients_friends([Alice, Bob]),

              Features = features(NodeNS),
              Caps = caps_helper:enable_new_caps(Bob, Features, v1),
              caps_helper:receive_presence_with_caps(Alice, Bob, Caps),

              Node = {escalus_utils:get_short_jid(Alice), NodeNS},
              pubsub_tools:receive_item_notification(Bob, <<"item2">>, Node, []),

              %% Unchanged caps shouldn't trigger notifications
              caps_helper:enable_caps(Bob, Features, v1),
              ct:sleep(200),
              escalus_assert:has_no_stanzas(Bob)
        end).

send_last_item_on_implicit_sub(Config) ->
    %% if alice publishes an item and then bob subscribes successfully to her presence
    %% then bob will receive the item right after final subscription stanzas
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {bob, 1}],
                                    fun send_last_item_on_implicit_sub_story/3).

send_last_item_on_implicit_sub_story(Config, Alice, Bob) ->
    NodeNS = ?config(node_ns, Config),
    pubsub_tools:publish(Alice, <<"item1">>, {pep, NodeNS}, []), % previous item - not delivered
    pubsub_tools:publish(Alice, <<"item2">>, {pep, NodeNS}, []), % expected last item
    [Message] = make_friends(Bob, Alice),
    Node = {escalus_utils:get_short_jid(Alice), NodeNS},
    pubsub_tools:check_item_notification(Message, <<"item2">>, Node, []),

    %% Bob can receive further notifications
    pubsub_tools:publish(Alice, <<"item3">>, {pep, NodeNS}, []),
    pubsub_tools:receive_item_notification(Bob, <<"item3">>, Node, []).

send_last_item_on_implicit_sub_with_sm(Config) ->
    %% Same as send_last_item_on_implicit_sub but with stream management turned on
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {bob, 1}],
                                    fun send_last_item_on_implicit_sub_with_sm_story/3).

send_last_item_on_implicit_sub_with_sm_story(Config, Alice, Bob) ->
    NodeNS = ?config(node_ns, Config),
    enable_sm(Alice),
    enable_sm(Bob),
    publish_with_sm(Alice, <<"item2">>, {pep, NodeNS}, []),
    [Message] = make_friends_sm(Bob, Alice),
    Node = {escalus_utils:get_short_jid(Alice), NodeNS},
    pubsub_tools:check_item_notification(Message, <<"item2">>, Node, []).

h_ok_after_notify_test(ConfigIn) ->
    Config = escalus_users:update_userspec(ConfigIn, kate, stream_management, true),
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {kate, 1}],
                                    fun h_ok_after_notify_story/3).

h_ok_after_notify_story(Config, Alice, Kate) ->
    NodeNS = ?config(node_ns, Config),
    escalus_story:make_all_clients_friends([Alice, Kate]),

    pubsub_tools:publish(Alice, <<"item2">>, {pep, NodeNS}, []),
    Node = {escalus_utils:get_short_jid(Alice), NodeNS},
    Check = fun(Message) ->
                    pubsub_tools:check_item_notification(Message, <<"item2">>, Node, [])
            end,
    Check(escalus_connection:get_stanza(Kate, item2)),

    H = escalus_tcp:get_sm_h(Kate#client.rcv_pid),
    escalus:send(Kate, escalus_stanza:sm_ack(H)),

    escalus_connection:send(Kate, escalus_stanza:sm_request()),

    % Presence exchange triggers asynchronous sending of the last published item.
    % If this happens after item2 is published, Kate will receive it twice.
    Stanza = escalus_connection:get_stanza(Kate, stream_mgmt_ack_or_item2),
    case escalus_pred:is_sm_ack(Stanza) of
        true ->
            ok;
        false ->
            Check(Stanza),
            escalus:assert(is_sm_ack, escalus_connection:get_stanza(Kate, stream_mgmt_ack))
    end.

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

add_open_access_model_to_create_node_request(#xmlel{children = [PubsubEl]} = Request) ->
    Fields = [#{var => <<"pubsub#access_model">>, values => [<<"open">>]}],
    Form = form_helper:form(#{ns => <<"http://jabber.org/protocol/pubsub#node_config">>, fields => Fields}),
    ConfigureEl = #xmlel{name = <<"configure">>, children = [Form]},
    PubsubEl2 = PubsubEl#xmlel{children = PubsubEl#xmlel.children ++ [ConfigureEl]},
    Request#xmlel{children = [PubsubEl2]}.

add_empty_config_to_create_node_request(#xmlel{children = [PubsubEl]} = Request) ->
    Form = form_helper:form(#{ns => <<"http://jabber.org/protocol/pubsub#node_config">>, fields => []}),
    ConfigureEl = #xmlel{name = <<"configure">>, children = [Form]},
    PubsubEl2 = PubsubEl#xmlel{children = PubsubEl#xmlel.children ++ [ConfigureEl]},
    Request#xmlel{children = [PubsubEl2]}.

set_subscribe_jid(#xmlel{children = [PubsubEl = #xmlel{children = [SubscribeEl | Els]}]} = Request,
                  Jid) ->
    Attrs = SubscribeEl#xmlel.attrs,
    SubscribeEl2 = SubscribeEl#xmlel{attrs = Attrs#{<<"jid">> => Jid}},
    PubsubEl2 = PubsubEl#xmlel{children = [SubscribeEl2 | Els]},
    Request#xmlel{children = [PubsubEl2]}.

remove_node_attr(
    #xmlel{children = [PubsubEl = #xmlel{children = [ActionEl | Els]}]} = Request) ->
    Attrs = ActionEl#xmlel.attrs,
    ActionEl2 = ActionEl#xmlel{attrs = maps:remove(<<"node">>, Attrs)},
    PubsubEl2 = PubsubEl#xmlel{children = [ActionEl2 | Els]},
    Request#xmlel{children = [PubsubEl2]}.

set_publish_item_children(
    #xmlel{children = [PubsubEl = #xmlel{children = [PublishEl = #xmlel{children = [ItemEl]}]}]} = Request,
    Children) ->
    ItemEl2 = ItemEl#xmlel{children = Children},
    PublishEl2 = PublishEl#xmlel{children = [ItemEl2]},
    PubsubEl2 = PubsubEl#xmlel{children = [PublishEl2]},
    Request#xmlel{children = [PubsubEl2]}.

set_publish_children(
    #xmlel{children = [PubsubEl = #xmlel{children = [PublishEl = #xmlel{}]}]} = Request,
    Children) ->
    PublishEl2 = PublishEl#xmlel{children = Children},
    PubsubEl2 = PubsubEl#xmlel{children = [PublishEl2]},
    Request#xmlel{children = [PubsubEl2]}.

duplicate_publish_item(
    #xmlel{children = [PubsubEl = #xmlel{children = [PublishEl = #xmlel{children = [ItemEl]}]}]} = Request) ->
    PublishEl2 = PublishEl#xmlel{children = [ItemEl, ItemEl]},
    PubsubEl2 = PubsubEl#xmlel{children = [PublishEl2]},
    Request#xmlel{children = [PubsubEl2]}.

publish_with_publish_options(Client, Node, Content, Options) ->
    publish_with_publish_options(Client, Node, Content, Options, ?NS_PUBSUB_PUB_OPTIONS).

publish_with_publish_options(Client, Node, Content, Options, FormType) ->
    OptionsEl = #xmlel{name = <<"publish-options">>,
                       children = form(Options, FormType)},

    Id = pubsub_tools:id(Client, Node, <<"publish">>),
    Publish = pubsub_tools:publish_request(Id, Client, Content, Node, Options),
    #xmlel{children = [#xmlel{} = PubsubEl]} = Publish,
    NewPubsubEl = PubsubEl#xmlel{children = PubsubEl#xmlel.children ++ [OptionsEl]},
    escalus:send(Client, Publish#xmlel{children = [NewPubsubEl]}),
    escalus:wait_for_stanza(Client).

create_node_with_config(Client, Config) ->
    PepNode = make_pep_node_info(Client, random_node_ns()),
    pubsub_tools:create_node(Client, PepNode, [{config, Config}, {expected_error_type, ~"modify"}]).

form(FormFields, FormType) ->
    FieldSpecs = lists:map(fun field_spec/1, FormFields),
    [form_helper:form(#{fields => FieldSpecs, ns => FormType})].

field_spec({Var, Value}) when is_list(Value) -> #{var => Var, values => Value};
field_spec({Var, Value}) -> #{var => Var, values => [Value]}.

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

%%-----------------------------------------------------------------
%% XML helpers
%%-----------------------------------------------------------------

features(NodeNS) ->
    [NodeNS, ns_notify(NodeNS)].

ns_notify(NS) ->
    <<NS/binary, "+notify">>.

random_node_ns() ->
    random_name().

random_name() ->
    base64:encode(crypto:strong_rand_bytes(16)).

assert_disco_items_node(Client, OwnerJid, NodeNS) ->
    Query = disco_items_query(Client, OwnerJid),
    Item = exml_query:subelement_with_attr(Query, ~"node", NodeNS),
    ?assertEqual(jid:str_tolower(OwnerJid), exml_query:attr(Item, ~"jid")).

assert_no_disco_items_node(Client, OwnerJid, NodeNS) ->
    Query = disco_items_query(Client, OwnerJid),
    ?assertEqual(undefined, exml_query:subelement_with_attr(Query, ~"node", NodeNS)).

assert_disco_info_node(Client, OwnerJid, NodeNS) ->
    escalus:send(Client, escalus_stanza:disco_info(OwnerJid, NodeNS)),
    Stanza = escalus:wait_for_stanza(Client),
    escalus:assert(is_stanza_from, [OwnerJid], Stanza),
    escalus:assert(is_iq_result, Stanza),
    Query = exml_query:subelement(Stanza, ~"query"),
    ?assertEqual(NodeNS, exml_query:attr(Query, ~"node")),
    escalus:assert(has_identity, [~"pubsub", ~"leaf"], Stanza),
    escalus:assert(has_feature, [?NS_PUBSUB], Stanza).

assert_no_disco_info_node(Client, OwnerJid, NodeNS) ->
    escalus:send(Client, escalus_stanza:disco_info(OwnerJid, NodeNS)),
    Stanza = escalus:wait_for_stanza(Client),
    escalus:assert(is_stanza_from, [OwnerJid], Stanza),
    escalus:assert(is_error, [~"cancel", ~"service-unavailable"], Stanza).

assert_publish_result(Stanza, NodeNS, ItemId) ->
    Pubsub = exml_query:subelement(Stanza, ~"pubsub"),
    Publish = exml_query:subelement(Pubsub, ~"publish"),
    ?assertEqual(NodeNS, exml_query:attr(Publish, ~"node")),
    Item = exml_query:subelement(Publish, ~"item"),
    ?assertEqual(ItemId, exml_query:attr(Item, ~"id")).

disco_items_query(Client, OwnerJid) ->
    escalus:send(Client, escalus_stanza:disco_items(OwnerJid)),
    Stanza = escalus:wait_for_stanza(Client),
    escalus:assert(is_stanza_from, [OwnerJid], Stanza),
    exml_query:subelement(Stanza, ~"query").

send_presence(From, Type, To) ->
    ToJid = escalus_client:short_jid(To),
    Stanza = escalus_stanza:presence_direct(ToJid, Type),
    escalus_client:send(From, Stanza).

make_friends(Bob, Alice) ->
    make_friends(Bob, Alice, 1).

make_friends(Bob, Alice, ExpectedMessageCount) ->
    % makes uni-directional presence subscriptions while SM is disabled
    % returns stanzas received finally by the inviter
    send_presence(Bob, <<"subscribe">>, Alice),
    escalus:assert(is_iq, escalus_client:wait_for_stanza(Bob)),
    escalus:assert(is_presence, escalus_client:wait_for_stanza(Alice)),
    send_presence(Alice, <<"subscribed">>, Bob),
    escalus:assert(is_iq, escalus_client:wait_for_stanza(Alice)),
    Preds = [is_iq] ++ lists:duplicate(ExpectedMessageCount, is_message)
        ++ lists:duplicate(2, is_presence),
    escalus:assert_many(Preds, BobStanzas = escalus_client:wait_for_stanzas(Bob, length(Preds))),
    lists:filter(fun escalus_pred:is_message/1, BobStanzas).

make_friends_sm(Bob, Alice) ->
    % makes uni-directional presence subscriptions while SM is enabled
    % returns stanzas received finally by the inviter
    send_presence(Bob, <<"subscribe">>, Alice),
    escalus:assert_many([is_iq, is_sm_ack_request],
                        escalus_client:wait_for_stanzas(Bob, 2)),
    escalus:assert_many([is_presence, is_sm_ack_request],
                        escalus_client:wait_for_stanzas(Alice, 2)),
    send_presence(Alice, <<"subscribed">>, Bob),
    escalus:assert_many([is_iq, is_sm_ack_request],
                        escalus_client:wait_for_stanzas(Alice, 2)),
    escalus:assert_many([is_message, is_iq]
                        ++ lists:duplicate(2, is_presence)
                        ++ lists:duplicate(4, is_sm_ack_request),
                        BobStanzas = escalus_client:wait_for_stanzas(Bob, 8)),
    lists:filter(fun escalus_pred:is_message/1, BobStanzas).

publish_with_sm(User, ItemId, Node, Options) ->
    Id = id(User, Node, <<"publish">>),
    Request = case proplists:get_value(with_payload, Options, true) of
                  true -> escalus_pubsub_stanza:publish(User, ItemId, item_content(), Id, Node);
                  false -> escalus_pubsub_stanza:publish(User, Id, Node)
              end,
    escalus_client:send(User, Request),
    escalus:wait_for_stanzas(User, 2).

id(User, {NodeAddr, NodeName}, Suffix) ->
    UserName = escalus_utils:get_username(User),
    list_to_binary(io_lib:format("~s-~s-~s-~s", [UserName, NodeAddr, NodeName, Suffix])).

item_content() ->
    #xmlel{name = <<"entry">>,
        attrs = #{<<"xmlns">> => <<"http://www.w3.org/2005/Atom">>}}.

enable_sm(User) ->
    escalus_client:send(User, escalus_stanza:enable_sm()),
    #xmlel{name = <<"enabled">>} = escalus:wait_for_stanza(User).
