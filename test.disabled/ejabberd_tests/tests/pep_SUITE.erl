%%%===================================================================
%%% @copyright (C) 2016, Erlang Solutions Ltd.
%%% @doc Suite for testing Personal Eventing Protocol features
%%%      as described in XEP-0163
%%% @end
%%%===================================================================

-module(pep_SUITE).
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
          {group, pep_tests}
         ].

groups() -> [
             {pep_tests, [sequence], %% TODO: Make these tests independent of each other
              [
               pep_caps_test,
               publish_test,
               notify_test,
               send_caps_after_login_test,
               h_ok_after_notify_test,
               authorize_access_model,
               unsubscribe_after_presence_unsubscription
              ]
             }
            ].

suite() ->
    escalus:suite().

-define(CAPS_HASH, <<"+XtEyyuYqe1dzskS+JMkYABKv2U=">>).
-define(CAPS_NODE_NAME, <<"http://www.chatopus.com">>).
-define(CAPS_NODE, {?CAPS_NODE_NAME, ?CAPS_HASH}).
-define(NS_USER_TUNA, <<"http://jabber.org/protocol/tuna">>).

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
    Users = escalus_users:get_users([alice, bob, kate]),
    escalus:create_users(Config, Users),
    escalus_story:make_everyone_friends(Config, Users).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus_users:get_users([alice, bob, kate])),
    ok.

init_per_testcase(pep_caps_test, Config) ->
    mongoose_helper:clear_caps_cache(?CAPS_NODE),
    escalus:init_per_testcase(pep_caps_test, Config);
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
    escalus:story(
      Config,
      [{bob, 1}],
      fun(Bob) ->
              %% Send presence with capabilities (chap. 1 ex. 4)
              %% Server does not know the version string, so it requests feature list
              DiscoRequest = send_presence_with_caps(Bob, caps(), true),

              %% Client responds with a list of supported features (chap. 1 ex. 5)
              send_caps_disco_result(Bob, DiscoRequest)
      end).

publish_test(Config) ->
    escalus:story(
      Config,
      [{alice, 1}],
      fun(Alice) ->
              %% Account owner publishes item (chap. 3, ex. 6)
              pubsub_tools:publish(Alice, <<"item1">>, {pep, ?NS_USER_TUNE}, [])
      end).

notify_test(Config) ->
    escalus:story(
      [{escalus_overrides,
       [{initial_activity, {?MODULE, send_initial_presence_with_caps}}]}
       | Config],
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              pubsub_tools:receive_item_notification(
                Bob, <<"item1">>, {escalus_utils:get_short_jid(Alice), ?NS_USER_TUNE}, []),

              pubsub_tools:publish(Alice, <<"item2">>, {pep, ?NS_USER_TUNE}, []),
              pubsub_tools:receive_item_notification(
                Bob, <<"item2">>, {escalus_utils:get_short_jid(Alice), ?NS_USER_TUNE}, [])
      end).

send_caps_after_login_test(Config) ->
    escalus:story(
      Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              Caps = caps(),
              send_presence_with_caps(Bob, Caps, false),
              receive_presence_with_caps(Alice, Bob, Caps),

              pubsub_tools:receive_item_notification(
                Bob, <<"item2">>, {escalus_utils:get_short_jid(Alice), ?NS_USER_TUNE}, [])
      end).

h_ok_after_notify_test(ConfigIn) ->
    Config = escalus_users:update_userspec(ConfigIn, kate,
                                           stream_management, true),
    escalus:story(
        [{escalus_overrides, [
            {initial_activity, {?MODULE, send_initial_presence_with_caps}}]} |
             Config
         ],
        [{alice, 1}, {kate, 1}],
        fun(Alice, Kate) ->
            pubsub_tools:receive_item_notification(
                Kate, <<"item2">>,
                {escalus_utils:get_short_jid(Alice), ?NS_USER_TUNE}, []),

            H = escalus_tcp:get_sm_h(Kate),
            escalus:send(Kate, escalus_stanza:sm_ack(H)),

            escalus_connection:send(Kate, escalus_stanza:sm_request()),
            escalus:assert(is_sm_ack,
                escalus_connection:get_stanza(Kate, stream_mgmt_ack))
        end).

authorize_access_model(Config) ->
    escalus:story(Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              {NodeAddr, _} = PepNode = make_pep_node_info(Alice, ?NS_USER_TUNA),
              AccessModel = {<<"pubsub#access_model">>, <<"authorize">>},
              pubsub_tools:create_node(Alice, PepNode, [{config, [AccessModel]}]),

              pubsub_tools:subscribe(Bob, PepNode, [{subscription, <<"pending">>}]),
              BobsRequest = pubsub_tools:receive_subscription_request(Alice, Bob, PepNode, []),

              %% FIXME: Only one item should be here but node_pep is based on node_flat, which
              %% is node_dag's ancestor, so this entry gets duplicated because every plugin
              %% is queried for subscriptions. Nasty fix involves deduplicating entries
              %% in mod_pubsub:get_subscriptions. The proper fix means not hacking node plugins
              %% into serving PEP but it's definitely a major change...
              Subs = [{?NS_USER_TUNA, <<"pending">>}, {?NS_USER_TUNA, <<"pending">>}],
              pubsub_tools:retrieve_user_subscriptions(Bob, NodeAddr, [{expected_result, Subs}]),

              pubsub_tools:submit_subscription_response(Alice, BobsRequest, PepNode, true, []),
              pubsub_tools:receive_subscription_notification(Bob, <<"subscribed">>, PepNode, []),

              pubsub_tools:publish(Alice, <<"fish">>, {pep, ?NS_USER_TUNA}, []),
              pubsub_tools:receive_item_notification(
                Bob, <<"fish">>, {escalus_utils:get_short_jid(Alice), ?NS_USER_TUNA}, []),

              pubsub_tools:delete_node(Alice, PepNode, [])
      end).

unsubscribe_after_presence_unsubscription(Config) ->
    escalus:story(Config,
      [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              PepNode = make_pep_node_info(Alice, ?NS_USER_TUNA),
              pubsub_tools:create_node(Alice, PepNode, []),
              pubsub_tools:subscribe(Bob, PepNode, []),
              pubsub_tools:publish(Alice, <<"fish">>, {pep, ?NS_USER_TUNA}, []),
              pubsub_tools:receive_item_notification(
                Bob, <<"fish">>, {escalus_utils:get_short_jid(Alice), ?NS_USER_TUNA}, []),

              BobJid = escalus_users:get_jid(Config, bob),
              escalus:send(Alice, escalus_stanza:presence_direct(BobJid, <<"unsubscribed">>)),
              %% Bob & Alice get roster update, Bob gets presence unsubscribed & unavailable
              [_, _, _] = escalus:wait_for_stanzas(Bob, 3),
              _ = escalus:wait_for_stanza(Alice),

              %% Unsubscription from PEP nodes is implicit
              pubsub_tools:publish(Alice, <<"salmon">>, {pep, ?NS_USER_TUNA}, []),
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

send_initial_presence_with_caps(User) ->
    case string:to_lower(binary_to_list(escalus_client:username(User))) of
        "alice" -> escalus_story:send_initial_presence(User);
        "bob" -> do_send_presence_with_caps(User, caps());
        "kate" -> do_send_presence_with_caps(User, caps())
    end.

send_presence_with_caps(User, Caps, ExpectDiscoRequest) ->
    do_send_presence_with_caps(User, Caps),
    DiscoRequest = case ExpectDiscoRequest of
                       true -> escalus:wait_for_stanza(User);
                       false -> undefined
                   end,
    receive_presence_with_caps(User, User, Caps),
    DiscoRequest.

do_send_presence_with_caps(User, Caps) ->
    Presence = escalus_stanza:presence(<<"available">>, [Caps]),
    escalus:send(User, Presence).

send_caps_disco_result(User, DiscoRequest) ->
    QueryEl = escalus_stanza:query_el(?NS_DISCO_INFO, feature_elems()),
    DiscoResult = escalus_stanza:iq_result(DiscoRequest, [QueryEl]),
    escalus:send(User, DiscoResult).

subscribe_presence(User1, User2) ->
    ShortJID1 = escalus_utils:get_short_jid(User2),
    SubscribeIq = escalus_stanza:presence_direct(ShortJID1, <<"subscribe">>),
    escalus:send(User1, SubscribeIq),

    _Received = escalus:wait_for_stanza(User2),

    _ReceivedUser1 = escalus:wait_for_stanza(User1),

    ShortJID2 = escalus_utils:get_short_jid(User1),
    SubscribedIq = escalus_stanza:presence_direct(ShortJID2, <<"subscribed">>),
    escalus:send(User2, SubscribedIq),

    _Stanzas = escalus:wait_for_stanzas(User1, 3),

    _SubscribedIq2 = escalus:wait_for_stanza(User2).

request_subscription(User1, User2) ->
    Jid2 = escalus_utils:get_short_jid(User2),
    Subscribe = escalus_stanza:presence_direct(Jid2, <<"subscribe">>),
    escalus:send(User1, Subscribe),

    escalus:assert(is_roster_set, escalus:wait_for_stanza(User1)),

    SubscribeNotification = escalus:wait_for_stanza(User2),
    escalus:assert(is_presence_with_type, [<<"subscribe">>], SubscribeNotification).

accept_subscription(User1, User2) ->
    Jid1 = escalus_utils:get_short_jid(User1),
    Subscribed = escalus_stanza:presence_direct(Jid1, <<"subscribed">>),
    escalus:send(User2, Subscribed),

    escalus:assert(is_roster_set, escalus:wait_for_stanza(User2)),
    escalus:assert(is_roster_set, escalus:wait_for_stanza(User1)),

    SubscribedNotification = escalus:wait_for_stanza(User1),
    escalus:assert(is_presence_with_type, [<<"subscribed">>], SubscribedNotification).

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

feature_elems() ->
    [#xmlel{name = <<"identity">>,
            attrs = [{<<"category">>, <<"client">>},
                     {<<"name">>, <<"Psi">>},
                     {<<"type">>, <<"pc">>}]} |
     [feature_elem(F) || F <- features()]].

feature_elem(F) ->
    #xmlel{name = <<"feature">>,
           attrs = [{<<"var">>, F}]}.

caps() ->
    #xmlel{name = <<"c">>,
           attrs = [{<<"xmlns">>, ?NS_CAPS},
                    {<<"hash">>, <<"sha-1">>},
                    {<<"node">>, ?CAPS_NODE_NAME},
                    {<<"ver">>, ?CAPS_HASH}]}.

features() ->
    [?NS_DISCO_INFO,
     ?NS_DISCO_ITEMS,
     ?NS_GEOLOC,
     ns_notify(?NS_GEOLOC),
     ?NS_USER_TUNE,
     ns_notify(?NS_USER_TUNE)].

ns_notify(NS) ->
    <<NS/binary, "+notify">>.

domain() ->
    ct:get_config({hosts, mim, domain}).
