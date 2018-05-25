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
-include_lib("exml/include/exml_stream.hrl").

-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

-export([
         pep_caps_test/1,
         publish_and_notify_test/1,
         send_caps_after_login_test/1,
         h_ok_after_notify_test/1,
         authorize_access_model/1,
         unsubscribe_after_presence_unsubscription/1
        ]).

-export([
         send_initial_presence_with_caps/2
        ]).

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             rpc/4]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, pep_tests}
    ].

groups() ->
    G = [
         {pep_tests, [parallel],
          [
           pep_caps_test,
           publish_and_notify_test,
           send_caps_after_login_test,
           h_ok_after_notify_test,
           authorize_access_model,
           unsubscribe_after_presence_unsubscription
          ]
         }
        ],
    ct_helper:repeat_all_until_all_ok(G).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(dynamic_modules:save_modules(domain(), Config)).

end_per_suite(Config) ->
    dynamic_modules:restore_modules(domain(), Config),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    dynamic_modules:ensure_modules(domain(), required_modules()),
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(TestName, Config) ->
    escalus:init_per_testcase(TestName, Config).

end_per_testcase(TestName, Config) ->
    escalus:end_per_testcase(TestName, Config).

%%--------------------------------------------------------------------
%% Test cases for XEP-0163
%% Comments in test cases refer to sections is the XEP
%%--------------------------------------------------------------------

%% Group: pep_tests (sequence)

pep_caps_test(Config) ->
    escalus:fresh_story(
      Config,
      [{bob, 1}],
      fun(Bob) ->
              NodeNS = random_node_ns(),
              Caps = caps(NodeNS),

              %% Send presence with capabilities (chap. 1 ex. 4)
              %% Server does not know the version string, so it requests feature list
              send_presence_with_caps(Bob, Caps),
              DiscoRequest = escalus:wait_for_stanza(Bob),

              %% Client responds with a list of supported features (chap. 1 ex. 5)
              send_caps_disco_result(Bob, DiscoRequest, NodeNS),

              receive_presence_with_caps(Bob, Bob, Caps)
      end).

publish_and_notify_test(Config) ->
    NodeNS = random_node_ns(),

    escalus:fresh_story(
      [{escalus_overrides,
       [{initial_activity, {?MODULE, send_initial_presence_with_caps, [NodeNS]}}]}
       | Config],
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              escalus_story:make_all_clients_friends([Alice, Bob]),

              pubsub_tools:publish(Alice, <<"item1">>, {pep, NodeNS}, []),
              pubsub_tools:receive_item_notification(
                Bob, <<"item1">>, {escalus_utils:get_short_jid(Alice), NodeNS}, [])
      end).

send_caps_after_login_test(Config) ->
    escalus:fresh_story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              NodeNS = random_node_ns(),
              pubsub_tools:publish(Alice, <<"item2">>, {pep, NodeNS}, []),

              escalus_story:make_all_clients_friends([Alice, Bob]),

              %% Presence subscription triggers PEP last item sending
              %% and sometimes this async process takes place after caps
              %% are updated, leading to duplicated notification
              %% We use timer:sleep here to avoid it for now, because
              %% TODO: mod_pubsub send loop has to be fixed, supervised, refactored etc.
              timer:sleep(1000),

              Caps = caps(NodeNS),
              send_presence_with_caps_and_handle_disco(Bob, Caps, NodeNS),
              receive_presence_with_caps(Bob, Bob, Caps),
              receive_presence_with_caps(Alice, Bob, Caps),

              pubsub_tools:receive_item_notification(
                Bob, <<"item2">>, {escalus_utils:get_short_jid(Alice), NodeNS}, []),

              [] = escalus_client:peek_stanzas(Bob)
      end).

h_ok_after_notify_test(ConfigIn) ->
    Config = escalus_users:update_userspec(ConfigIn, kate,
                                           stream_management, true),
    NodeNS = random_node_ns(),
    escalus:fresh_story(
      [{escalus_overrides,
        [{initial_activity, {?MODULE, send_initial_presence_with_caps, [NodeNS]}}]} | Config ],
      [{alice, 1}, {kate, 1}],
      fun(Alice, Kate) ->
              escalus_story:make_all_clients_friends([Alice, Kate]),

              pubsub_tools:publish(Alice, <<"item2">>, {pep, NodeNS}, []),
              pubsub_tools:receive_item_notification(
                Kate, <<"item2">>, {escalus_utils:get_short_jid(Alice), NodeNS}, []),

              H = escalus_tcp:get_sm_h(Kate#client.rcv_pid),
              escalus:send(Kate, escalus_stanza:sm_ack(H)),

              escalus_connection:send(Kate, escalus_stanza:sm_request()),
              escalus:assert(is_sm_ack,
                             escalus_connection:get_stanza(Kate, stream_mgmt_ack))
      end).

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
              [] = escalus:wait_for_stanzas(Bob, 1),

              pubsub_tools:delete_node(Alice, PepNode, [])
      end).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

required_modules() ->
    [{mod_caps, []},
     {mod_pubsub, [
                   {plugins, [<<"dag">>, <<"pep">>]},
                   {nodetree, <<"dag">>},
                   {pep_mapping, []},
                   {host, "pubsub.@HOST@"}
                  ]}].

send_initial_presence_with_caps(NodeNS, User) ->
    case string:to_lower(binary_to_list(escalus_client:username(User))) of
        "alice" ++ _ -> escalus_story:send_initial_presence(User);
        "bob" ++ _ -> send_presence_with_caps_and_handle_disco(User, caps(NodeNS), NodeNS);
        "kate" ++ _ -> send_presence_with_caps_and_handle_disco(User, caps(NodeNS), NodeNS)
    end.

send_presence_with_caps_and_handle_disco(User, Caps, NodeNS) ->
    send_presence_with_caps(User, Caps),
    DiscoRequest = escalus:wait_for_stanza(User),
    send_caps_disco_result(User, DiscoRequest, NodeNS).

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

domain() ->
    ct:get_config({hosts, mim, domain}).

random_node_ns() ->
    base64:encode(crypto:strong_rand_bytes(16)).

caps_hash(PEPNodeNS) ->
    rpc(mim(), mod_caps, make_disco_hash, [feature_elems(PEPNodeNS), sha1]).

caps_node_name() ->
    <<"http://www.chatopus.com">>.

