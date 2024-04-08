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

-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

-export([
         disco_test/1,
         disco_sm_test/1,
         disco_sm_items_test/1,
         pep_caps_test/1,
         publish_and_notify_test/1,
         auto_create_with_publish_options_test/1,
         publish_options_success_test/1,
         publish_options_fail_unknown_option_story/1,
         publish_options_fail_wrong_value_story/1,
         publish_options_fail_wrong_form/1,
         send_caps_after_login_test/1,
         delayed_receive/1,
         delayed_receive_with_sm/1,
         h_ok_after_notify_test/1,
         authorize_access_model/1,
         unsubscribe_after_presence_unsubscription/1
        ]).

-export([
         start_caps_clients/2,
         send_initial_presence_with_caps/2,
         add_config_to_create_node_request/1
        ]).

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             subhost_pattern/1,
                             rpc/4]).
-import(config_parser_helper, [mod_config/2]).
-import(domain_helper, [domain/0]).

-define(NS_PUBSUB_PUB_OPTIONS,  <<"http://jabber.org/protocol/pubsub#publish-options">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, pep_tests},
     {group, cache_tests}
    ].

groups() ->
    [
         {pep_tests, [parallel],
          [
           disco_test,
           disco_sm_test,
           disco_sm_items_test,
           pep_caps_test,
           publish_and_notify_test,
           auto_create_with_publish_options_test,
           publish_options_success_test,
           publish_options_fail_unknown_option_story,
           publish_options_fail_wrong_value_story,
           publish_options_fail_wrong_form,
           send_caps_after_login_test,
           delayed_receive,
           delayed_receive_with_sm,
           h_ok_after_notify_test,
           authorize_access_model,
           unsubscribe_after_presence_unsubscription
          ]
         },
         {cache_tests, [parallel],
          [
           send_caps_after_login_test,
           delayed_receive,
           delayed_receive_with_sm,
           unsubscribe_after_presence_unsubscription
          ]
         }
    ].

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(dynamic_modules:save_modules(domain(), Config)).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(cache_tests, Config) ->
    Config0 = dynamic_modules:save_modules(domain(), Config),
    NewConfig = required_modules(cache_tests),
    dynamic_modules:ensure_modules(domain(), NewConfig),
    Config0;

init_per_group(_GroupName, Config) ->
    dynamic_modules:ensure_modules(domain(), required_modules()),
    Config.

end_per_group(_GroupName, Config) ->
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

disco_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              escalus:send(Alice, escalus_stanza:disco_info(pubsub_tools:node_addr())),
              Stanza = escalus:wait_for_stanza(Alice),
              escalus:assert(has_identity, [<<"pubsub">>, <<"service">>], Stanza),
              escalus:assert(has_identity, [<<"pubsub">>, <<"pep">>], Stanza),
              escalus:assert(has_feature, [?NS_PUBSUB], Stanza)
      end).

disco_sm_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              AliceJid = escalus_client:short_jid(Alice),
              escalus:send(Alice, escalus_stanza:disco_info(AliceJid)),
              Stanza = escalus:wait_for_stanza(Alice),
              ?assertNot(escalus_pred:has_identity(<<"pubsub">>, <<"service">>, Stanza)),
              escalus:assert(has_identity, [<<"pubsub">>, <<"pep">>], Stanza),
              escalus:assert(has_feature, [?NS_PUBSUB], Stanza),
              escalus:assert(is_stanza_from, [AliceJid], Stanza)
      end).

disco_sm_items_test(Config) ->
    NodeNS = random_node_ns(),
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              AliceJid = escalus_client:short_jid(Alice),

              %% Node not present yet
              escalus:send(Alice, escalus_stanza:disco_items(AliceJid)),
              Stanza1 = escalus:wait_for_stanza(Alice),
              Query1 = exml_query:subelement(Stanza1, <<"query">>),
              ?assertEqual(undefined, exml_query:subelement_with_attr(Query1, <<"node">>, NodeNS)),
              escalus:assert(is_stanza_from, [AliceJid], Stanza1),

              %% Publish an item to trigger node creation
              pubsub_tools:publish(Alice, <<"item1">>, {pep, NodeNS}, []),

              %% Node present
              escalus:send(Alice, escalus_stanza:disco_items(AliceJid)),
              Stanza2 = escalus:wait_for_stanza(Alice),
              Query2 = exml_query:subelement(Stanza2, <<"query">>),
              Item = exml_query:subelement_with_attr(Query2, <<"node">>, NodeNS),
              ?assertEqual(jid:str_tolower(AliceJid), exml_query:attr(Item, <<"jid">>)),
              escalus:assert(is_stanza_from, [AliceJid], Stanza2)
      end).

pep_caps_test(Config) ->
    escalus:fresh_story(
      Config,
      [{bob, 1}],
      fun(Bob) ->
              NodeNS = random_node_ns(),
              Caps = caps(NodeNS),

              %% Send presence with capabilities (chap. 1 ex. 4)
              send_presence_with_caps(Bob, Caps),
              receive_presence_with_caps(Bob, Bob, Caps),

              %% Server does not know the version string, so it requests feature list
              DiscoRequest = escalus:wait_for_stanza(Bob),
              %% Client responds with a list of supported features (chap. 1 ex. 5)
              send_caps_disco_result(Bob, DiscoRequest, NodeNS)
      end).

publish_and_notify_test(Config) ->
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {bob, 1}], fun publish_and_notify_story/3).

publish_and_notify_story(Config, Alice, Bob) ->
    NodeNS = ?config(node_ns, Config),
    escalus_story:make_all_clients_friends([Alice, Bob]),
    pubsub_tools:publish(Alice, <<"item1">>, {pep, NodeNS}, []),
    pubsub_tools:receive_item_notification(
      Bob, <<"item1">>, {escalus_utils:get_short_jid(Alice), NodeNS}, []).

auto_create_with_publish_options_test(Config) ->
    % Given pubsub is configured with pep plugin
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->

            % When publishing an item with publish-options
            Node = {pep, random_node_ns()},
            PublishOptions = [{<<"pubsub#access_model">>, <<"open">>}],
            pubsub_tools:publish_with_options(Alice, <<"item1">>, Node, [], PublishOptions),

            % Then node configuration contains specified publish-options
            NodeConfig = pubsub_tools:get_configuration(Alice, Node, []),
            verify_publish_options(NodeConfig, PublishOptions)
      end).

publish_options_success_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
            NodeNS = random_node_ns(),
            PepNode = make_pep_node_info(Alice, NodeNS),
            pubsub_tools:create_node(Alice, PepNode,
                                     [{modify_request,fun add_config_to_create_node_request/1}]),
            escalus_story:make_all_clients_friends([Alice, Bob]),
            PublishOptions = [{<<"pubsub#deliver_payloads">>, <<"1">>},
                              {<<"pubsub#notify_config">>, <<"0">>},
                              {<<"pubsub#notify_delete">>, <<"0">>},
                              {<<"pubsub#purge_offline">>, <<"0">>},
                              {<<"pubsub#notify_retract">>, <<"0">>},
                              {<<"pubsub#persist_items">>, <<"1">>},
                              {<<"pubsub#roster_groups_allowed">>, [<<"friends">>, <<"enemies">>]},
                              {<<"pubsub#max_items">>, <<"1">>},
                              {<<"pubsub#subscribe">>, <<"1">>},
                              {<<"pubsub#access_model">>, <<"presence">>},
                              {<<"pubsub#publish_model">>, <<"publishers">>},
                              {<<"pubsub#notification_type">>, <<"headline">>},
                              {<<"pubsub#max_payload_size">>, <<"60000">>},
                              {<<"pubsub#send_last_published_item">>, <<"on_sub_and_presence">>},
                              {<<"pubsub#deliver_notifications">>, <<"1">>},
                              {<<"pubsub#presence_based_delivery">>, <<"1">>}],
            Result = publish_with_publish_options(Alice, {pep, NodeNS}, <<"item1">>, PublishOptions),
            escalus:assert(is_iq_result, Result)
      end).

publish_options_fail_unknown_option_story(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
            NodeNS = random_node_ns(),
            PepNode = make_pep_node_info(Alice, NodeNS),
            pubsub_tools:create_node(Alice, PepNode, []),

            PublishOptions = [{<<"deliver_payloads">>, <<"1">>}],
            Result = publish_with_publish_options(Alice, {pep, NodeNS}, <<"item1">>, PublishOptions),
            escalus:assert(is_error, [<<"cancel">>, <<"conflict">>], Result),

            PublishOptions2 = [{<<"pubsub#not_existing_option">>, <<"1">>}],
            Result2 = publish_with_publish_options(Alice, {pep, NodeNS}, <<"item1">>, PublishOptions2),
            escalus:assert(is_error, [<<"cancel">>, <<"conflict">>], Result2)
      end).

publish_options_fail_wrong_value_story(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
            NodeNS = random_node_ns(),
            PepNode = make_pep_node_info(Alice, NodeNS),
            pubsub_tools:create_node(Alice, PepNode,
                                     [{modify_request,fun add_config_to_create_node_request/1}]),

            PublishOptions = [{<<"pubsub#deliver_payloads">>, <<"0">>}],
            Result = publish_with_publish_options(Alice, {pep, NodeNS}, <<"item1">>, PublishOptions),
            escalus:assert(is_error, [<<"cancel">>, <<"conflict">>], Result),

            PublishOptions2 = [{<<"pubsub#roster_groups_allowed">>, <<"friends">>}],
            Result2 = publish_with_publish_options(Alice, {pep, NodeNS}, <<"item1">>, PublishOptions2),
            escalus:assert(is_error, [<<"cancel">>, <<"conflict">>], Result2)
      end).

publish_options_fail_wrong_form(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
            NodeNS = random_node_ns(),
            PepNode = make_pep_node_info(Alice, NodeNS),
            pubsub_tools:create_node(Alice, PepNode, []),
            PublishOptions = [{<<"deliver_payloads">>, <<"0">>}],
            Result = publish_with_publish_options(Alice, {pep, NodeNS}, <<"item1">>, PublishOptions, <<"WRONG_NS">>),
            escalus:assert(is_error, [<<"cancel">>, <<"conflict">>], Result)
      end).

send_caps_after_login_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              NodeNS = random_node_ns(),
              pubsub_tools:publish(Alice, <<"item2">>, {pep, NodeNS}, []),

              escalus_story:make_all_clients_friends([Alice, Bob]),

              Caps = caps(NodeNS),
              send_presence_with_caps(Bob, Caps),
              receive_presence_with_caps(Bob, Bob, Caps),
              receive_presence_with_caps(Alice, Bob, Caps),

              handle_requested_caps(NodeNS, Bob),

              Node = {escalus_utils:get_short_jid(Alice), NodeNS},
              Check = fun(Message) ->
                              pubsub_tools:check_item_notification(Message, <<"item2">>, Node, [])
                      end,

              %% Presence subscription triggers PEP last item sending
              %% and sometimes this async process takes place after caps
              %% are updated, leading to duplicated notification
              Check(escalus_client:wait_for_stanza(Bob)),
              case escalus_client:peek_stanzas(Bob) of
                  [Message2] ->
                      Check(Message2);
                  [] ->
                      ok
              end
        end).

delayed_receive(Config) ->
    %% if alice publishes an item and then bob subscribes successfully to her presence
    %% then bob will receive the item right after final subscription stanzas
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {bob, 1}], fun delayed_receive_story/3).

delayed_receive_story(Config, Alice, Bob) ->
    NodeNS = ?config(node_ns, Config),
    pubsub_tools:publish(Alice, <<"item2">>, {pep, NodeNS}, []),
    [Message] = make_friends(Bob, Alice),
    Node = {escalus_utils:get_short_jid(Alice), NodeNS},
    pubsub_tools:check_item_notification(Message, <<"item2">>, Node, []),

    %% Known issue: without a mutual presence subscription Bob will not receive further items
    pubsub_tools:publish(Alice, <<"item3">>, {pep, NodeNS}, []),
    [] = escalus_client:wait_for_stanzas(Bob, 1, 500),
    ok.

delayed_receive_with_sm(Config) ->
    %% Same as delayed_receive but with stream management turned on
    Config1 = set_caps(Config),
    escalus:fresh_story_with_config(Config1, [{alice, 1}, {bob, 1}],
                                    fun delayed_receive_with_sm_story/3).

delayed_receive_with_sm_story(Config, Alice, Bob) ->
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

authorize_access_model(Config) ->
    escalus:fresh_story(Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              NodeNS = random_node_ns(),
              {NodeAddr, _} = PepNode = make_pep_node_info(Alice, NodeNS),
              AccessModel = {<<"pubsub#access_model">>, <<"authorize">>},
              pubsub_tools:create_node(Alice, PepNode, [{config, [AccessModel]}]),

              pubsub_tools:subscribe(Bob, PepNode, [{subscription, <<"pending">>}]),
              BobsRequest = pubsub_tools:receive_subscription_request(Alice, Bob, PepNode, []),

              %% FIXME: Only one item should be here but node_pep is based on node_flat, which
              %% is node_dag's ancestor, so this entry gets duplicated because every plugin
              %% is queried for subscriptions. Nasty fix involves deduplicating entries
              %% in mod_pubsub:get_subscriptions. The proper fix means not hacking node plugins
              %% into serving PEP but it's definitely a major change...
              Subs = [{NodeNS, <<"pending">>}, {NodeNS, <<"pending">>}],
              pubsub_tools:get_user_subscriptions(Bob, NodeAddr, [{expected_result, Subs}]),

              pubsub_tools:submit_subscription_response(Alice, BobsRequest, PepNode, true, []),
              pubsub_tools:receive_subscription_notification(Bob, <<"subscribed">>, PepNode, []),

              pubsub_tools:publish(Alice, <<"fish">>, {pep, NodeNS}, []),
              pubsub_tools:receive_item_notification(
                Bob, <<"fish">>, {escalus_utils:get_short_jid(Alice), NodeNS}, []),

              pubsub_tools:delete_node(Alice, PepNode, [])
      end).

unsubscribe_after_presence_unsubscription(Config) ->
    escalus:fresh_story(Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              escalus_story:make_all_clients_friends([Alice, Bob]),

              NodeNS = random_node_ns(),
              PepNode = make_pep_node_info(Alice, NodeNS),
              pubsub_tools:create_node(Alice, PepNode, []),
              pubsub_tools:subscribe(Bob, PepNode, []),
              pubsub_tools:publish(Alice, <<"fish">>, {pep, NodeNS}, []),
              pubsub_tools:receive_item_notification(
                Bob, <<"fish">>, {escalus_utils:get_short_jid(Alice), NodeNS}, []),

              BobJid = escalus_utils:get_short_jid(Bob),
              escalus:send(Alice, escalus_stanza:presence_direct(BobJid, <<"unsubscribed">>)),
              %% Bob & Alice get roster update, Bob gets presence unsubscribed & unavailable
              [_, _, _] = escalus:wait_for_stanzas(Bob, 3),
              _ = escalus:wait_for_stanza(Alice),

              %% Unsubscription from PEP nodes is implicit
              pubsub_tools:publish(Alice, <<"salmon">>, {pep, NodeNS}, []),
              [] = escalus:wait_for_stanzas(Bob, 1, 500),

              pubsub_tools:delete_node(Alice, PepNode, [])
      end).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

add_config_to_create_node_request(#xmlel{children = [PubsubEl]} = Request) ->
    Fields = [#{values => [<<"friends">>, <<"enemies">>], var => <<"pubsub#roster_groups_allowed">>}],
    Form = form_helper:form(#{ns => <<"http://jabber.org/protocol/pubsub#node_config">>, fields => Fields}),
    ConfigureEl = #xmlel{name = <<"configure">>, children = [Form]},
    PubsubEl2 = PubsubEl#xmlel{children = PubsubEl#xmlel.children ++ [ConfigureEl]},
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

form(FormFields, FormType) ->
    FieldSpecs = lists:map(fun field_spec/1, FormFields),
    [form_helper:form(#{fields => FieldSpecs, ns => FormType})].

field_spec({Var, Value}) when is_list(Value) -> #{var => Var, values => Value};
field_spec({Var, Value}) -> #{var => Var, values => [Value]}.

required_modules() ->
    [{mod_caps, config_parser_helper:mod_config_with_auto_backend(mod_caps)},
     {mod_pubsub, mod_config(mod_pubsub, #{plugins => [<<"dag">>, <<"pep">>],
                                           nodetree => nodetree_dag,
                                           backend => mongoose_helper:mnesia_or_rdbms_backend(),
                                           pep_mapping => #{},
                                           host => subhost_pattern("pubsub.@HOST@")})}].
required_modules(cache_tests) ->
    HostType = domain_helper:host_type(),
    [{mod_caps, config_parser_helper:mod_config_with_auto_backend(mod_caps)},
     {mod_pubsub, mod_config(mod_pubsub, #{plugins => [<<"dag">>, <<"pep">>],
                                           nodetree => nodetree_dag,
                                           backend => mongoose_helper:mnesia_or_rdbms_backend(),
                                           pep_mapping => #{},
                                           host => subhost_pattern("pubsub.@HOST@"),
                                           last_item_cache => mongoose_helper:mnesia_or_rdbms_backend()
     })}].

send_initial_presence_with_caps(NodeNS, Client) ->
    case is_caps_client(Client) of
        false -> escalus_story:send_initial_presence(Client);
        true -> send_presence_with_caps(Client, caps(NodeNS))
    end.

handle_requested_caps(NodeNS, User) ->
    case is_caps_client(User) of
        false -> ok;
        true -> DiscoRequest = escalus:wait_for_stanza(User),
                send_caps_disco_result(User, DiscoRequest, NodeNS)
    end.

is_caps_client(Client) ->
    case escalus_client:username(Client) of
        <<"alice", _/binary>> -> false;
        _ -> true
    end.

send_presence_with_caps(User, Caps) ->
    Presence = escalus_stanza:presence(<<"available">>, [Caps]),
    escalus:send(User, Presence).

send_caps_disco_result(User, DiscoRequest, NodeNS) ->
    QueryEl = escalus_stanza:query_el(?NS_DISCO_INFO, feature_elems(NodeNS)),
    DiscoResult = escalus_stanza:iq_result(DiscoRequest, [QueryEl]),
    escalus:send(User, DiscoResult).

receive_presence_with_caps(User1, User2, Caps) ->
    PresenceNotification = escalus:wait_for_stanza(User1),
    escalus:assert(is_presence, PresenceNotification),
    escalus:assert(is_stanza_from, [User2], PresenceNotification),
    Caps = exml_query:subelement(PresenceNotification, <<"c">>).

make_pep_node_info(Client, NodeName) ->
    {escalus_utils:jid_to_lower(escalus_utils:get_short_jid(Client)), NodeName}.

verify_publish_options(FullNodeConfig, Options) ->
    NodeConfig = [{Opt, Value} || {Opt, _, Value} <- FullNodeConfig],
    Options = lists:filter(fun(Option) ->
                               lists:member(Option, NodeConfig)
                           end, Options).

set_caps(Config) ->
    [{escalus_overrides, [{start_ready_clients, {?MODULE, start_caps_clients}}]},
     {node_ns, random_node_ns()} | Config].

%% Implemented only for one resource per client, because it is enough
start_caps_clients(Config, [{UserSpec, Resource}]) ->
    NodeNS = ?config(node_ns, Config),
    {ok, Client} = escalus_client:start(Config, UserSpec, Resource),
    send_initial_presence_with_caps(NodeNS, Client),
    escalus:assert(is_presence, escalus:wait_for_stanza(Client)),
    handle_requested_caps(NodeNS, Client),
    [Client].

%%-----------------------------------------------------------------
%% XML helpers
%%-----------------------------------------------------------------

feature_elems(PEPNodeNS) ->
    [#xmlel{name = <<"identity">>,
            attrs = [{<<"category">>, <<"client">>},
                     {<<"name">>, <<"Psi">>},
                     {<<"type">>, <<"pc">>}]} |
     [feature_elem(F) || F <- features(PEPNodeNS)]].

feature_elem(F) ->
    #xmlel{name = <<"feature">>,
           attrs = [{<<"var">>, F}]}.

caps(PEPNodeNS) ->
    #xmlel{name = <<"c">>,
           attrs = [{<<"xmlns">>, ?NS_CAPS},
                    {<<"hash">>, <<"sha-1">>},
                    {<<"node">>, caps_node_name()},
                    {<<"ver">>, caps_hash(PEPNodeNS)}]}.

features(PEPNodeNS) ->
    [?NS_DISCO_INFO,
     ?NS_DISCO_ITEMS,
     ?NS_GEOLOC,
     ns_notify(?NS_GEOLOC),
     PEPNodeNS,
     ns_notify(PEPNodeNS)].

ns_notify(NS) ->
    <<NS/binary, "+notify">>.

random_node_ns() ->
    base64:encode(crypto:strong_rand_bytes(16)).

caps_hash(PEPNodeNS) ->
    rpc(mim(), mod_caps, make_disco_hash, [feature_elems(PEPNodeNS), sha1]).

caps_node_name() ->
    <<"http://www.chatopus.com">>.

send_presence(From, Type, To) ->
    ToJid = escalus_client:short_jid(To),
    Stanza = escalus_stanza:presence_direct(ToJid, Type),
    escalus_client:send(From, Stanza).

make_friends(Bob, Alice) ->
    % makes uni-directional presence subscriptions while SM is disabled
    % returns stanzas received finally by the inviter
    send_presence(Bob, <<"subscribe">>, Alice),
    escalus:assert(is_iq, escalus_client:wait_for_stanza(Bob)),
    escalus:assert(is_presence, escalus_client:wait_for_stanza(Alice)),
    send_presence(Alice, <<"subscribed">>, Bob),
    escalus:assert(is_iq, escalus_client:wait_for_stanza(Alice)),
    escalus:assert_many([is_message, is_iq]
                        ++ lists:duplicate(2, is_presence),
                        BobStanzas = escalus_client:wait_for_stanzas(Bob, 4)),
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
        attrs = [{<<"xmlns">>, <<"http://www.w3.org/2005/Atom">>}]}.

enable_sm(User) ->
    escalus_client:send(User, escalus_stanza:enable_sm()),
    #xmlel{name = <<"enabled">>} = escalus:wait_for_stanza(User).
