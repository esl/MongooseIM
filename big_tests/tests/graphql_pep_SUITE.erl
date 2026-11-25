-module(graphql_pep_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(domain_helper, [host_type/0]).
-import(graphql_helper, [execute_command/4, execute_user_command/5, get_ok_value/2,
                         get_err_code/1, get_coercion_err_msg/1,
                         get_unauthorized/1, get_not_loaded/1, get_bad_request/1]).

-include_lib("stdlib/include/assert.hrl").
-include("../../include/mod_pubsub.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user},
     {group, admin_http},
     {group, admin_cli},
     {group, domain_admin}].

groups() ->
    [{user, [], [{group, user_pep}, {group, pep_caps}, {group, pep_not_configured}]},
     {admin_http, [], [{group, admin_pep}, {group, pep_caps}, {group, pep_not_configured}]},
     {admin_cli, [], [{group, admin_pep_cli}, {group, pep_caps}, {group, pep_not_configured_cli}]},
     {domain_admin, [], [{group, domain_admin_pep}, {group, pep_caps}, {group, pep_not_configured}]},
     {user_pep, [parallel], pep_tests()},
     {admin_pep, [parallel], admin_tests()},
     {admin_pep_cli, [], admin_tests()},
     {domain_admin_pep, [parallel], domain_admin_tests()},
     {pep_caps, [], [presence_subscriber_notifications]},
     {pep_not_configured, [parallel], not_configured_tests()},
     {pep_not_configured_cli, [], not_configured_tests()}].

pep_tests() ->
    node_tests() ++ item_tests().

node_tests() ->
    [create_node,
     create_node_with_config,
     create_node_errors,
     get_nodes,
     get_configuration,
     get_configuration_errors,
     configure,
     configure_errors,
     delete_node,
     delete_node_errors].

item_tests() ->
    [publish_item_auto_create,
     publish_item,
     publish_item_with_options,
     publish_item_errors,
     get_items,
     get_items_errors,
     retract_item,
     retract_item_errors].

admin_tests() ->
    pep_tests() ++ [get_nodes_nonexistent_owner,
                    get_nodes_unknown_owner_domain].

domain_admin_tests() ->
    pep_tests() ++ no_permission_tests().

no_permission_tests() ->
    [get_nodes_no_permission,
     create_node_no_permission,
     get_configuration_no_permission,
     configure_no_permission,
     delete_node_no_permission,
     publish_item_no_permission,
     get_items_no_permission,
     retract_item_no_permission].

not_configured_tests() ->
    [get_nodes_not_configured,
     create_node_not_configured,
     get_configuration_not_configured,
     configure_not_configured,
     delete_node_not_configured,
     publish_item_not_configured,
     get_items_not_configured,
     retract_item_not_configured].

%% Setup and teardown

init_per_suite(Config) ->
    case mongoose_helper:is_rdbms_enabled(host_type()) of
        true ->
            Config1 = ejabberd_node_utils:init(mim(), Config),
            escalus:init_per_suite(Config1);
        false ->
            {skip, "RDBMS not enabled"}
    end.

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(user, Config) ->
    graphql_helper:init_user(Config);
init_per_group(admin_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_cli, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(domain_admin, Config) ->
    graphql_helper:init_domain_admin_handler(Config);
init_per_group(pep_caps = Group, Config) ->
    maybe
        ok ?= caps_helper:check_backend(),
        init_group(Group, Config)
    end;
init_per_group(Group, Config) ->
    init_group(Group, Config).

end_per_group(domain_admin, Config) ->
    graphql_helper:end_domain_admin_handler(Config),
    graphql_helper:clean();
end_per_group(Group, _Config) when Group =:= user;
                                   Group =:= admin_http;
                                   Group =:= admin_cli ->
    graphql_helper:clean();
end_per_group(_Group, Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config).

init_per_testcase(TestCase, Config) ->
    Config1 = escalus:init_per_testcase(TestCase, Config),
    escalus_fresh:create_users(Config1, required_users(TestCase)).

end_per_testcase(TestCase, Config) ->
    escalus:end_per_testcase(TestCase, Config).

init_group(Group, Config) ->
    Config1 = dynamic_modules:save_modules(host_type(), Config),
    dynamic_modules:ensure_modules(host_type(), required_modules(Group)),
    Config1.

required_users(TestCase) when TestCase =:= get_nodes;
                              TestCase =:= delete_node;
                              TestCase =:= publish_item;
                              TestCase =:= retract_item;
                              TestCase =:= presence_subscriber_notifications ->
    [{alice, 1}, {bob, 1}];
required_users(TestCase) ->
    case lists:member(TestCase, no_permission_tests()) of
        true -> [{alice_bis, 1}];
        false -> [{alice, 1}]
    end.

required_modules(Group) when Group =:= pep_not_configured;
                             Group =:= pep_not_configured_cli ->
    [{mod_pubsub, stopped}];
required_modules(pep_caps) ->
    [{mod_caps, config_parser_helper:default_mod_config(mod_caps)},
     {mod_pubsub, config_parser_helper:default_mod_config(mod_pubsub)}];
required_modules(_Group) ->
    [{mod_pubsub, config_parser_helper:default_mod_config(mod_pubsub)}].

%% Node management

create_node(Config) ->
    Node = ~"urn:test:pep:default",
    Res = command(~"createNode", alice, #{node => Node, config => null}, Config),
    ?assertEqual(true, get_ok_value([data, pep, createNode], Res)),
    ?assertMatch(#pubsub_node{config = #{access_model := presence, max_items := infinity}},
                 get_node(alice, Node, Config)).

create_node_with_config(Config) ->
    Node = ~"urn:test:pep:configured",
    NodeConfig = #{accessModel => ~"OPEN", maxItems => 5},
    Res = command(~"createNode", alice, #{node => Node, config => NodeConfig}, Config),
    ?assertEqual(true, get_ok_value([data, pep, createNode], Res)),
    ?assertMatch(#pubsub_node{config = #{access_model := open, max_items := 5}},
                 get_node(alice, Node, Config)).

create_node_errors(Config) ->
    Node = ~"urn:test:pep:existing",
    BadNode = ~"urn:test:pep:invalid",
    create_node(alice, Node, #{}, Config),

    %% Node already exists
    Res = command(~"createNode", alice, #{node => Node, config => null}, Config),
    ?assertEqual(~"conflict", get_err_code(Res)),

    %% Invalid config
    Res2 = command(~"createNode", alice, #{node => BadNode, config => #{maxItems => -1}}, Config),
    ?assertNotEqual(nomatch, binary:match(get_coercion_err_msg(Res2), ~"PepMaxItems")),
    ?assertEqual(undefined, get_node(alice, BadNode, Config)),
    Res3 = command(~"createNode", alice,
                   #{node => BadNode, config => #{accessModel => ~"NONEXISTENT"}}, Config),
    get_bad_request(Res3),
    ?assertMatch({_, #{~"errors" := [#{~"extensions" := #{~"code" := ~"unknown_enum"}}]}}, Res3),
    ?assertEqual(undefined, get_node(alice, BadNode, Config)).

get_nodes(Config) ->
    Res = command(~"getNodes", alice, #{}, Config),
    ?assertEqual([], get_ok_value([data, pep, getNodes], Res)),
    Node1 = ~"urn:test:pep:first",
    Node2 = ~"urn:test:pep:second",
    create_node(alice, Node1, #{}, Config),
    create_node(alice, Node2, #{}, Config),
    create_node(bob, ~"urn:test:pep:other-owner", #{}, Config),
    Res2 = command(~"getNodes", alice, #{}, Config),
    ?assertEqual([Node1, Node2], lists:sort(get_ok_value([data, pep, getNodes], Res2))).

get_configuration(Config) ->
    Node = ~"urn:test:pep:config",
    create_node(alice, Node, #{access_model => open, max_items => 7}, Config),

    %% Get custom config
    Res = command(~"getConfiguration", alice, #{node => Node}, Config),
    ?assertEqual(#{~"accessModel" => ~"OPEN", ~"maxItems" => 7},
                 get_ok_value([data, pep, getConfiguration], Res)),

    DefaultNode = ~"urn:test:pep:defaults",
    create_node(alice, DefaultNode, #{}, Config),

    %% Get default config
    Res2 = command(~"getConfiguration", alice, #{node => DefaultNode}, Config),
    ?assertEqual(#{~"accessModel" => ~"PRESENCE", ~"maxItems" => ~"max"},
                 get_ok_value([data, pep, getConfiguration], Res2)).

get_configuration_errors(Config) ->
    Res = command(~"getConfiguration", alice, #{node => ~"urn:test:pep:missing"}, Config),
    ?assertEqual(~"item_not_found", get_err_code(Res)).

configure(Config) ->
    Node = ~"urn:test:pep:config",
    create_node(alice, Node, #{access_model => presence, max_items => 7}, Config),

    %% Change accessModel
    Res = command(~"configure", alice,
                  #{node => Node, config => #{accessModel => ~"OPEN"}}, Config),
    ?assertEqual(true, get_ok_value([data, pep, configure], Res)),
    ?assertMatch(#pubsub_node{config = #{access_model := open, max_items := 7}},
                 get_node(alice, Node, Config)),

    %% Change maxItems to 0
    Res2 = command(~"configure", alice,
                   #{node => Node, config => #{accessModel => null, maxItems => 0}}, Config),
    ?assertEqual(true, get_ok_value([data, pep, configure], Res2)),
    ?assertMatch(#pubsub_node{config = #{access_model := open, max_items := 0}},
                 get_node(alice, Node, Config)),

    %% Change maxItems to 'max'
    Res3 = command(~"configure", alice,
                   #{node => Node, config => #{maxItems => ~"max"}}, Config),
    ?assertEqual(true, get_ok_value([data, pep, configure], Res3)),
    ?assertMatch(#pubsub_node{config = #{access_model := open, max_items := infinity}},
                 get_node(alice, Node, Config)).

configure_errors(Config) ->
    Res = command(~"configure", alice,
                  #{node => ~"urn:test:pep:missing", config => #{maxItems => 1}}, Config),
    ?assertEqual(~"item_not_found", get_err_code(Res)),
    Node = ~"urn:test:pep:config",
    create_node(alice, Node, #{}, Config),

    %% 'maxItems' cannot be negative
    Res2 = command(~"configure", alice, #{node => Node, config => #{maxItems => -1}}, Config),
    ?assertNotEqual(nomatch, binary:match(get_coercion_err_msg(Res2), ~"PepMaxItems")),
    ?assertMatch(#pubsub_node{config = #{access_model := presence, max_items := infinity}},
                 get_node(alice, Node, Config)),

    %% Invalid access model
    Res3 = command(~"configure", alice,
                   #{node => Node, config => #{accessModel => ~"NONEXISTENT"}}, Config),
    get_bad_request(Res3),
    ?assertMatch({_, #{~"errors" := [#{~"extensions" := #{~"code" := ~"unknown_enum"}}]}}, Res3),
    ?assertMatch(#pubsub_node{config = #{access_model := presence, max_items := infinity}},
                 get_node(alice, Node, Config)).

delete_node(Config) ->
    Node = ~"urn:test:pep:delete",
    create_node(alice, Node, #{access_model => open}, Config),
    PepNode = {escalus_utils:jid_to_lower(owner_jid(alice, Config)), Node},
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        pubsub_tools:subscribe(Bob, PepNode, []),
        Res = command(~"deleteNode", alice, #{node => Node}, Config),
        ?assertEqual(true, get_ok_value([data, pep, deleteNode], Res)),
        pubsub_tools:receive_node_deletion_notification(Bob, PepNode, []),
        ?assertEqual(undefined, get_node(alice, Node, Config))
    end).

delete_node_errors(Config) ->
    Res = command(~"deleteNode", alice, #{node => ~"urn:test:pep:missing"}, Config),
    ?assertEqual(~"item_not_found", get_err_code(Res)).

%% Item management

publish_item_auto_create(Config) ->
    Node = ~"urn:test:pep:auto",
    Payload = item_payload(~"first"),
    Args = #{node => Node, itemId => ~"item1", payload => Payload},
    Res = command(~"publishItem", alice, Args, Config),
    ?assertEqual(~"item1", get_ok_value([data, pep, publishItem], Res)),
    {ok, Element} = exml:parse(Payload),
    ?assertMatch([#item{id = ~"item1", payload = Element}], get_items(alice, Node, Config)),
    ?assertMatch(#pubsub_node{config = #{access_model := presence, max_items := infinity}},
                 get_node(alice, Node, Config)).

publish_item(Config) ->
    Node = ~"urn:test:pep:items",
    Payload = item_payload(~"first"),
    {ok, Element} = exml:parse(Payload),
    Args = #{node => Node, itemId => ~"item1", payload => Payload},
    create_node(alice, Node, #{access_model => open}, Config),
    PepNode = {escalus_utils:jid_to_lower(owner_jid(alice, Config)), Node},
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        pubsub_tools:subscribe(Bob, PepNode, []),

        %% Publish a new item and notify the subscriber
        Res = command(~"publishItem", alice, Args, Config),
        ?assertEqual(~"item1", get_ok_value([data, pep, publishItem], Res)),
        pubsub_tools:receive_item_notification(Bob, ~"item1", PepNode, []),
        ?assertMatch([#item{id = ~"item1", payload = Element}], get_items(alice, Node, Config)),

        %% Replace the item and notify the subscriber
        UpdatedPayload = item_payload(~"updated"),
        Res2 = command(~"publishItem", alice, Args#{payload => UpdatedPayload}, Config),
        ?assertEqual(~"item1", get_ok_value([data, pep, publishItem], Res2)),
        {ok, UpdatedElement} = exml:parse(UpdatedPayload),
        pubsub_tools:receive_item_notification(Bob, ~"item1", PepNode, []),
        ?assertMatch([#item{id = ~"item1", payload = UpdatedElement}], get_items(alice, Node, Config))
    end).

publish_item_with_options(Config) ->
    Node = ~"urn:test:pep:options",

    %% Publish an item with options, and with node auto-creation
    Payload = item_payload(~"first"),
    Options = #{accessModel => ~"OPEN", maxItems => 2},
    Args = #{node => Node, itemId => ~"item1", payload => Payload, publishOptions => Options},
    Res = command(~"publishItem", alice, Args, Config),
    ?assertEqual(~"item1", get_ok_value([data, pep, publishItem], Res)),
    ?assertMatch(#pubsub_node{config = #{access_model := open, max_items := 2}},
                 get_node(alice, Node, Config)),

    %% Publish another item with a matching option
    Res2 = command(~"publishItem", alice,
                   Args#{itemId => ~"item2", publishOptions => #{accessModel => ~"OPEN"}}, Config),
    ?assertEqual(~"item2", get_ok_value([data, pep, publishItem], Res2)),
    {ok, Element} = exml:parse(Payload),
    Items = [{Id, El} || #item{id = Id, payload = El} <- get_items(alice, Node, Config)],
    ?assertEqual([{~"item1", Element}, {~"item2", Element}], Items),
    ?assertMatch(#pubsub_node{config = #{access_model := open, max_items := 2}},
                 get_node(alice, Node, Config)).

publish_item_errors(Config) ->
    Node = ~"urn:test:pep:items",
    Payload = item_payload(~"original"),
    create_node(alice, Node, #{access_model => open, max_items => 2}, Config),
    publish_item(alice, Node, ~"item1", Payload, Config),

    %% Fail to create an item with conflicting options
    Args = #{node => Node, itemId => ~"item2", payload => item_payload(~"replacement")},
    Res = command(~"publishItem", alice,
                  Args#{publishOptions => #{accessModel => ~"PRESENCE"}}, Config),
    ?assertEqual(~"conflict", get_err_code(Res)),
    Res2 = command(~"publishItem", alice, Args#{publishOptions => #{maxItems => 3}}, Config),
    ?assertEqual(~"conflict", get_err_code(Res2)),

    %% Items and nodes are unchanged
    {ok, Element} = exml:parse(Payload),
    ?assertMatch([#item{id = ~"item1", payload = Element}], get_items(alice, Node, Config)),
    ?assertMatch(#pubsub_node{config = #{access_model := open, max_items := 2}},
                 get_node(alice, Node, Config)),

    %% Fail to create an item with invalid XML payload
    BadNode = ~"urn:test:pep:invalid",
    Res3 = command(~"publishItem", alice, Args#{node => BadNode, payload => ~"<broken"}, Config),
    ?assertNotEqual(nomatch, binary:match(get_coercion_err_msg(Res3), ~"XmlElement")),
    ?assertEqual(undefined, get_node(alice, BadNode, Config)).

get_items(Config) ->
    Node = ~"urn:test:pep:items",
    create_node(alice, Node, #{}, Config),

    %% Get items from an empty node
    Res = command(~"getItems", alice, #{node => Node}, Config),
    ?assertEqual([], get_ok_value([data, pep, getItems], Res)),

    ItemIds = [~"item1", ~"item2", ~"item3"],
    [publish_item(alice, Node, ItemId, item_payload(ItemId), Config) || ItemId <- ItemIds],
    Items = [Item1, Item2, Item3] =
        [#{~"itemId" => ItemId, ~"payload" => item_payload(ItemId)} || ItemId <- ItemIds],

    %% Get three items
    Res2 = command(~"getItems", alice, #{node => Node}, Config),
    ?assertEqual(Items, get_ok_value([data, pep, getItems], Res2)),

    %% Get two existing items by id (plus a non-existing one)
    Res3 = command(~"getItems", alice,
                   #{node => Node, itemIds => [~"item3", ~"missing", ~"item1"]}, Config),
    ?assertEqual([Item3, Item1], get_ok_value([data, pep, getItems], Res3)),

    %% Get two latest items
    Res4 = command(~"getItems", alice, #{node => Node, maxItems => 2}, Config),
    ?assertEqual([Item2, Item3], get_ok_value([data, pep, getItems], Res4)).

get_items_errors(Config) ->
    %% Node does not exist
    Res = command(~"getItems", alice, #{node => ~"urn:test:pep:missing"}, Config),
    ?assertEqual(~"item_not_found", get_err_code(Res)),

    %% 'itemIds' and 'maxItems' can't be used together
    Node = ~"urn:test:pep:items",
    create_node(alice, Node, #{}, Config),
    Res2 = command(~"getItems", alice,
                   #{node => Node, itemIds => [~"item1"], maxItems => 1}, Config),
    ?assertEqual(~"bad_request", get_err_code(Res2)),

    %% 'maxItems' has to be positive
    Res4 = command(~"getItems", alice, #{node => Node, maxItems => 0}, Config),
    ?assertNotEqual(nomatch, binary:match(get_coercion_err_msg(Res4), ~"PosInt")).

retract_item(Config) ->
    Node = ~"urn:test:pep:items",
    create_node(alice, Node, #{access_model => open}, Config),
    Payload = item_payload(~"value"),
    [publish_item(alice, Node, ItemId, Payload, Config)
     || ItemId <- [~"item1", ~"item2", ~"item3", ~"item4"]],
    PepNode = {escalus_utils:jid_to_lower(owner_jid(alice, Config)), Node},
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        pubsub_tools:subscribe(Bob, PepNode, [{expected_notification, {PepNode, ~"item4"}}]),

        %% Omitted and false notify do not send a retraction event
        Res = command(~"retractItem", alice, #{node => Node, itemId => ~"item1"}, Config),
        ?assertEqual(true, get_ok_value([data, pep, retractItem], Res)),
        Res2 = command(~"retractItem", alice,
                       #{node => Node, itemId => ~"item2", notify => false}, Config),
        ?assertEqual(true, get_ok_value([data, pep, retractItem], Res2)),
        ?assertEqual([], escalus:wait_for_stanzas(Bob, 1, 500)),

        %% Explicitly request a retraction event
        Res3 = command(~"retractItem", alice,
                       #{node => Node, itemId => ~"item3", notify => true}, Config),
        ?assertEqual(true, get_ok_value([data, pep, retractItem], Res3)),
        pubsub_tools:receive_retract_notification(Bob, ~"item3", PepNode, []),
        ?assertMatch([#item{id = ~"item4"}], get_items(alice, Node, Config))
    end).

retract_item_errors(Config) ->
    %% Node does not exist
    Args = #{node => ~"urn:test:pep:missing", itemId => ~"item1"},
    Res = command(~"retractItem", alice, Args, Config),
    ?assertEqual(~"item_not_found", get_err_code(Res)),
    Node = ~"urn:test:pep:items",

    %% Item does not exist
    publish_item(alice, Node, ~"kept", item_payload(~"value"), Config),
    Res2 = command(~"retractItem", alice, Args#{node => Node}, Config),
    ?assertEqual(~"item_not_found", get_err_code(Res2)),
    ?assertMatch([#item{id = ~"kept"}], get_items(alice, Node, Config)).

%% Presence subscriber notifications

presence_subscriber_notifications(Config) ->
    Node = pubsub_tools:rand_name(~"urn:test:pep:presence"),
    PepNode = {escalus_utils:jid_to_lower(owner_jid(alice, Config)), Node},
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        Features = [Node, <<Node/binary, "+notify">>],
        caps_helper:enable_new_caps(Bob, Features, v1),
        caps_helper:wait_for_caps(host_type(), Bob, Features),
        pubsub_tools:set_up_presence_subscription(Bob, Alice, 0),

        Payload = item_payload(~"value"),
        Args = #{node => Node, itemId => ~"item1", payload => Payload},
        Res = command(~"publishItem", alice, Args, Config),
        ?assertEqual(~"item1", get_ok_value([data, pep, publishItem], Res)),
        pubsub_tools:receive_item_notification(Bob, ~"item1", PepNode, []),

        Res2 = command(~"retractItem", alice,
                       #{node => Node, itemId => ~"item1", notify => true}, Config),
        ?assertEqual(true, get_ok_value([data, pep, retractItem], Res2)),
        pubsub_tools:receive_retract_notification(Bob, ~"item1", PepNode, []),

        Res3 = command(~"deleteNode", alice, #{node => Node}, Config),
        ?assertEqual(true, get_ok_value([data, pep, deleteNode], Res3)),
        pubsub_tools:receive_node_deletion_notification(Bob, PepNode, [])
    end).

%% Admin owner errors

get_nodes_nonexistent_owner(Config) ->
    Owner = owner_jid(alice, Config),
    Res = command(~"getNodes", <<"missing-", Owner/binary>>, #{}, Config),
    ?assertEqual(~"user_not_found", get_err_code(Res)).

get_nodes_unknown_owner_domain(Config) ->
    Res = command(~"getNodes", ~"alice@nonexistent.invalid", #{}, Config),
    ?assertEqual(~"domain_not_found", get_err_code(Res)).

%% Domain admin permissions

get_nodes_no_permission(Config) ->
    no_permission(~"getNodes", #{}, Config).

create_node_no_permission(Config) ->
    no_permission(~"createNode", #{node => ~"urn:test:pep:node", config => null}, Config).

get_configuration_no_permission(Config) ->
    no_permission(~"getConfiguration", #{node => ~"urn:test:pep:node"}, Config).

configure_no_permission(Config) ->
    no_permission(~"configure", #{node => ~"urn:test:pep:node", config => #{maxItems => 1}}, Config).

delete_node_no_permission(Config) ->
    no_permission(~"deleteNode", #{node => ~"urn:test:pep:node"}, Config).

publish_item_no_permission(Config) ->
    Args = #{node => ~"urn:test:pep:node", itemId => ~"item1", payload => item_payload(~"value")},
    no_permission(~"publishItem", Args, Config).

get_items_no_permission(Config) ->
    no_permission(~"getItems", #{node => ~"urn:test:pep:node"}, Config).

retract_item_no_permission(Config) ->
    no_permission(~"retractItem", #{node => ~"urn:test:pep:node", itemId => ~"item1"}, Config).

no_permission(Command, Args, Config) ->
    get_unauthorized(command(Command, alice_bis, Args, Config)).

%% Module unavailable

get_nodes_not_configured(Config) ->
    get_not_loaded(command(~"getNodes", alice, #{}, Config)).

create_node_not_configured(Config) ->
    Args = #{node => ~"urn:test:pep:node", config => null},
    get_not_loaded(command(~"createNode", alice, Args, Config)).

get_configuration_not_configured(Config) ->
    get_not_loaded(command(~"getConfiguration", alice, #{node => ~"urn:test:pep:node"}, Config)).

configure_not_configured(Config) ->
    Args = #{node => ~"urn:test:pep:node", config => #{maxItems => 1}},
    get_not_loaded(command(~"configure", alice, Args, Config)).

delete_node_not_configured(Config) ->
    get_not_loaded(command(~"deleteNode", alice, #{node => ~"urn:test:pep:node"}, Config)).

publish_item_not_configured(Config) ->
    Args = #{node => ~"urn:test:pep:node", itemId => ~"item1", payload => item_payload(~"value")},
    get_not_loaded(command(~"publishItem", alice, Args, Config)).

get_items_not_configured(Config) ->
    get_not_loaded(command(~"getItems", alice, #{node => ~"urn:test:pep:node"}, Config)).

retract_item_not_configured(Config) ->
    Args = #{node => ~"urn:test:pep:node", itemId => ~"item1"},
    get_not_loaded(command(~"retractItem", alice, Args, Config)).

%% Helpers

command(Command, Owner, Args, Config) ->
    case proplists:get_value(schema_endpoint, Config) of
        user -> execute_user_command(~"pep", Command, Owner, Args, Config);
        _ -> execute_command(~"pep", Command, Args#{owner => owner_jid(Owner, Config)}, Config)
    end.

owner_jid(Owner, Config) when is_atom(Owner) ->
    escalus_users:get_jid(Config, Owner);
owner_jid(Owner, _Config) when is_binary(Owner) ->
    Owner.

create_node(Owner, Node, NodeConfig, Config) ->
    Jid = jid:from_binary(owner_jid(Owner, Config)),
    {ok, true} = rpc(mim(), mod_pubsub_api, create_node, [Jid, Node, NodeConfig]).

get_node(Owner, Node, Config) ->
    Jid = jid:to_bare(jid:from_binary(owner_jid(Owner, Config))),
    rpc(mim(), mod_pubsub_backend, get_node, [host_type(), {Jid, Node}]).

publish_item(Owner, Node, ItemId, Payload, Config) ->
    Jid = jid:from_binary(owner_jid(Owner, Config)),
    {ok, Element} = exml:parse(Payload),
    {ok, ItemId} = rpc(mim(), mod_pubsub_api, publish_item, [Jid, Node, ItemId, Element, #{}]).

get_items(Owner, Node, Config) ->
    Jid = jid:to_bare(jid:from_binary(owner_jid(Owner, Config))),
    rpc(mim(), mod_pubsub_backend, get_items, [host_type(), {Jid, Node}, #{}]).

item_payload(Value) ->
    <<"<value xmlns='urn:test:pep'>", Value/binary, "</value>">>.
