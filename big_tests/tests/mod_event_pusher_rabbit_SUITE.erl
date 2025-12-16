%%%-------------------------------------------------------------------
%%% @author Kacper Mentel <kacper.mentel@erlang-solutions.com>
%%% @copyright (C) 2018, Kacper Mentel
%%% @doc
%%% Tests for `mod_event_pusher` RabbitMQ backend (`mod_event_pusher_rabbit`).
%%% @end
%%%-------------------------------------------------------------------
-module(mod_event_pusher_rabbit_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-include("assert_received_match.hrl").

-import(distributed_helper, [mim/0, rpc/4]).
-import(domain_helper, [domain/0]).
-import(config_parser_helper, [config/2]).

-compile([export_all, nowarn_export_all]).

-define(QUEUE_NAME, <<"test_queue">>).
-define(DEFAULT_EXCHANGE_TYPE, <<"topic">>).
-define(PRESENCE_EXCHANGE, <<"custom_presence_exchange">>).
-define(CHAT_MSG_EXCHANGE, <<"custom_chat_msg_exchange">>).
-define(GROUP_CHAT_MSG_EXCHANGE, <<"custom_group_chat_msg_exchange">>).
-define(CHAT_MSG_SENT_TOPIC, <<"custom_chat_msg_sent_topic">>).
-define(CHAT_MSG_RECV_TOPIC, <<"custom_chat_msg_recv_topic">>).
-define(GROUP_CHAT_MSG_SENT_TOPIC, <<"custom_group_chat_msg_sent_topic">>).
-define(GROUP_CHAT_MSG_RECV_TOPIC, <<"custom_group_chat_msg_recv_topic">>).
-define(WPOOL_CFG, #{scope => host_type,
                     opts => #{workers => 20, strategy => best_worker, call_timeout => 5000}}).
-define(VHOST, <<"vh1">>).

-define(RABBIT_HTTP_ENDPOINT, "http://127.0.0.1:15672").

-define(UTILS_MODULE, mod_event_pusher_rabbit_utils).

-type rabbit_binding() :: {Queue :: binary(),
                           Exchange :: binary(),
                           RoutingKey :: binary()}.

-type json_object() :: #{binary() => json:decode_value()}.

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, pool_startup},
     {group, module_startup},
     {group, only_presence_module_startup},
     {group, presence_status_publish},
     {group, presence_status_publish_with_confirms},
     {group, only_presence_status_publish},
     {group, chat_message_publish},
     {group, group_chat_message_publish},
     {group, instrumentation},
     {group, filter_and_metadata},
     {group, single_worker}
    ].

groups() ->
    [{pool_startup, [], pool_startup_tests()},
     {module_startup, [], module_startup_tests()},
     {only_presence_module_startup, [], only_presence_module_startup_tests()},
     {presence_status_publish, [], presence_status_publish_tests()},
     {presence_status_publish_with_confirms, [], presence_status_publish_tests()},
     {only_presence_status_publish, [], only_presence_status_publish_tests()},
     {chat_message_publish, [], chat_message_publish_tests()},
     {group_chat_message_publish, [], group_chat_message_publish_tests()},
     {instrumentation, [], instrumentation_tests()},
     {filter_and_metadata, [], filter_and_metadata_tests()},
     {single_worker, [], single_worker_tests()}].

pool_startup_tests() ->
    [rabbit_pool_starts_with_default_config].

module_startup_tests() ->
    [exchanges_are_created_on_module_startup].

only_presence_module_startup_tests() ->
    [only_presence_exchange_is_created_on_module_startup].

presence_status_publish_tests() ->
    [connected_users_push_presence_events_when_change_status,
     presence_messages_are_properly_formatted].

only_presence_status_publish_tests() ->
    [messages_published_events_are_not_executed | presence_status_publish_tests()].

chat_message_publish_tests() ->
    [chat_message_sent_event,
     chat_message_sent_event_properly_formatted,
     chat_message_received_event,
     chat_message_received_event_properly_formatted].

group_chat_message_publish_tests() ->
    [group_chat_message_sent_event,
     group_chat_message_sent_event_properly_formatted,
     group_chat_message_received_event,
     group_chat_message_received_event_properly_formatted,
     group_chat_displayed_marker_is_skipped].

instrumentation_tests() ->
    [connections_events_are_executed,
     connection_failed_events_are_executed,
     messages_published_events_are_executed].

filter_and_metadata_tests() ->
    [messages_published_events_are_not_executed,
     presence_messages_are_properly_formatted_with_metadata].

single_worker_tests() ->
    [connection_is_restarted_on_error,
     connection_is_restarted_with_retries,
     connection_is_restarted_with_retries_and_queue_limit,
     worker_is_restarted_after_failed_retries].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    inets:start(),
    case is_rabbitmq_available() of
        true ->
            instrument_helper:start(
                [{wpool_rabbit_connections, #{host_type => domain(), pool_tag => test_tag}},
                 {wpool_rabbit_connections, #{host_type => domain(), pool_tag => fail_tag}},
                 {wpool_rabbit_messages_published, #{host_type => domain(), pool_tag => event_pusher}}]),
            {ok, _} = application:ensure_all_started(amqp_client),
            muc_helper:load_muc(),
            mongoose_helper:inject_module(mod_event_pusher_filter),
            mongoose_helper:inject_module(?UTILS_MODULE),
            rpc(mim(), ?UTILS_MODULE, start, []),
            escalus:init_per_suite(Config);
        false ->
            {skip, "RabbitMQ server is not available on default port."}
    end.

end_per_suite(Config) ->
    rpc(mim(), ?UTILS_MODULE, stop, []),
    escalus_fresh:clean(),
    muc_helper:unload_muc(),
    escalus:end_per_suite(Config),
    instrument_helper:stop().

init_per_group(GroupName, Config0) ->
    Domain = domain(),
    start_rabbit_tls_wpool(Domain, GroupName),
    Config = dynamic_modules:save_modules(Domain, Config0),
    dynamic_modules:ensure_modules(Domain, required_modules(GroupName)),
    Config.

required_modules(pool_startup) ->
    [{mod_event_pusher, stopped}];
required_modules(filter_and_metadata = GroupName) ->
    [{mod_event_pusher_filter, #{}},
     {mod_event_pusher, #{rabbit => mod_event_pusher_rabbit_opts(GroupName)}}];
required_modules(GroupName) ->
    [{mod_event_pusher, #{rabbit => mod_event_pusher_rabbit_opts(GroupName)}}].

mod_event_pusher_rabbit_opts(GroupName) ->
    ExtraOpts = extra_exchange_opts(GroupName),
    maps:map(fun(Key, Opts) ->
                     config([modules, mod_event_pusher, rabbit, Key], maps:merge(Opts, ExtraOpts))
             end, exchanges(GroupName)).

exchanges(GroupName) when GroupName =:= only_presence_module_startup;
                          GroupName =:= only_presence_status_publish ->
    maps:with([presence_exchange], basic_exchanges());
exchanges(_GroupName) ->
    basic_exchanges().

basic_exchanges() ->
    #{presence_exchange => #{name => ?PRESENCE_EXCHANGE,
                             durable => true},
      chat_msg_exchange => #{name => ?CHAT_MSG_EXCHANGE,
                             sent_topic => ?CHAT_MSG_SENT_TOPIC,
                             recv_topic => ?CHAT_MSG_RECV_TOPIC},
      groupchat_msg_exchange => #{name => ?GROUP_CHAT_MSG_EXCHANGE,
                                  sent_topic => ?GROUP_CHAT_MSG_SENT_TOPIC,
                                  recv_topic => ?GROUP_CHAT_MSG_RECV_TOPIC}
     }.

extra_exchange_opts(module_startup) -> #{type => <<"headers">>};
extra_exchange_opts(_) -> #{}.

end_per_group(_, Config) ->
    delete_exchanges(),
    dynamic_modules:restore_modules(Config),
    stop_rabbit_wpool(domain()).

init_per_testcase(rabbit_pool_starts_with_default_config, Config) ->
    Config;
init_per_testcase(CaseName, Config0) ->
    Config1 = escalus_fresh:create_users(Config0, [{bob, 1}, {alice, 1}]),
    Config2 = maybe_prepare_muc(CaseName, Config1),
    Config = Config2 ++ connect_to_rabbit(),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(rabbit_pool_starts_with_default_config, _Config) ->
    ok;
end_per_testcase(CaseName, Config) ->
    maybe_cleanup_muc(CaseName, Config),
    close_rabbit_connection(Config),
    ensure_no_queues(),
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% GROUP initialization_on_startup
%%--------------------------------------------------------------------

rabbit_pool_starts_with_default_config(_Config) ->
    %% GIVEN
    Domain = domain(),
    Tag = rabbit_event_pusher_default,
    DefaultWpoolConfig = #{type => rabbit, scope => host_type, tag => Tag},
    RabbitWpool = {rabbit, Domain, rabbit_event_pusher_default},
    %% WHEN
    start_rabbit_wpool(Domain, config([modules, mod_event_pusher, rabbit, Tag], DefaultWpoolConfig)),
    %% THEN
    Pools = rpc(mim(), mongoose_wpool, get_pools, []),
    ?assertMatch(RabbitWpool,
                 lists:keyfind(rabbit_event_pusher_default, 3, Pools)),
    %% CLEANUP
    stop_rabbit_wpool(RabbitWpool).

exchanges_are_created_on_module_startup(_Config) ->
    %% GIVEN module is started with custom exchange types
    BaseAttrs = #{<<"type">> => <<"headers">>},
    Expected = [BaseAttrs#{<<"name">> => ?PRESENCE_EXCHANGE, <<"durable">> => true},
                BaseAttrs#{<<"name">> => ?CHAT_MSG_EXCHANGE, <<"durable">> => false},
                BaseAttrs#{<<"name">> => ?GROUP_CHAT_MSG_EXCHANGE, <<"durable">> => false}],
    %% THEN exchanges are created
    ensure_exchanges_present(Expected).

only_presence_exchange_is_created_on_module_startup(_Config) ->
    %% GIVEN module is started with custom exchange types
    Expected = [#{<<"name">> => ?PRESENCE_EXCHANGE, <<"type">> => ?DEFAULT_EXCHANGE_TYPE}],
    ct:sleep(200), % wait for any unwanted exchanges
    %% THEN only the enabled exchanges are created
    {ok, Exchanges} = ensure_exchanges_present(Expected),
    ?assertNot(is_exchange_present(#{<<"name">> => ?CHAT_MSG_EXCHANGE}, Exchanges)),
    ?assertNot(is_exchange_present(#{<<"name">> => ?GROUP_CHAT_MSG_EXCHANGE}, Exchanges)).

%%--------------------------------------------------------------------
%% GROUP (only_)presence_status_publish, filter_and_metadata
%%--------------------------------------------------------------------

connected_users_push_presence_events_when_change_status(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              %% GIVEN
              BobJID = client_lower_short_jid(Bob),
              listen_to_presence_events_from_rabbit([BobJID], Config),
              %% WHEN users generate some traffic.
              send_presence_stanzas([Bob], 1),
              %% THEN  wait for presence events from Rabbit.
              ?assertReceivedMatch({#'basic.deliver'{routing_key = BobJID},
                                    #amqp_msg{}}, timer:seconds(5))
      end).

presence_messages_are_properly_formatted(Config) ->
    escalus:fresh_story_with_config(
      Config, [{bob, 1}], fun presence_messages_are_properly_formatted_story/2).

presence_messages_are_properly_formatted_with_metadata(Config) ->
    escalus:fresh_story_with_config(
      Config, [{bob, 1}],
      fun(_, Bob) ->
              TS = rpc(mim(), erlang, system_time, [microsecond]),
              DecodedMessage = presence_messages_are_properly_formatted_story(Config, Bob),
              #{<<"timestamp">> := T, <<"session_count">> := 0} = DecodedMessage,
              ?assert(is_integer(T) andalso T > TS)
      end).

presence_messages_are_properly_formatted_story(Config, Bob) ->
    %% GIVEN
    BobJID = client_lower_short_jid(Bob),
    BobFullJID = client_lower_full_jid(Bob),
    listen_to_presence_events_from_rabbit([BobJID], Config),
    %% WHEN user logout
    escalus:send(Bob, escalus_stanza:presence(<<"unavailable">>)),
    %% THEN receive message
    DecodedMessage = get_decoded_message_from_rabbit(BobJID),
    ?assertMatch(#{<<"user_id">> := BobFullJID, <<"present">> := false}, DecodedMessage),
    DecodedMessage.

messages_published_events_are_not_executed(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            %% WHEN users chat
            Msg = <<"Hi Alice! There will be no events for this message.">>,
            escalus:send(Bob, escalus_stanza:chat_to(Alice, Msg)),
            %% THEN there is no attempt to publish anything
            ct:sleep(500), % wait for any unexpected events
            instrument_helper:assert_not_emitted(wpool_rabbit_messages_published,
                                                 #{host_type => domain(), pool_tag => event_pusher},
                                                 fun(#{count := 1, payload := P}) ->
                                                         {amqp_msg, _, BinData} = P,
                                                         binary:match(BinData, Msg) /= nomatch
                                                 end)
        end).

%%--------------------------------------------------------------------
%% GROUP chat_message_publish
%%--------------------------------------------------------------------

chat_message_sent_event(Config) ->
    escalus:story(
      Config, [{bob, 1}, {alice, 1}],
      fun(Bob, Alice) ->
              %% GIVEN
              BobJID = client_lower_short_jid(Bob),
              BobChatMsgSentRK = chat_msg_sent_rk(BobJID),
              listen_to_chat_msg_sent_events_from_rabbit([BobJID], Config),
              %% WHEN users chat
              escalus:send(Bob,
                           escalus_stanza:chat_to(Alice, <<"Oh, hi Alice!">>)),
              %% THEN  wait for chat message sent events events from Rabbit.
              ?assertReceivedMatch({#'basic.deliver'{
                                       routing_key = BobChatMsgSentRK},
                                    #amqp_msg{}}, timer:seconds(5))
      end).

chat_message_received_event(Config) ->
    escalus:story(
      Config, [{bob, 1}, {alice, 1}],
      fun(Bob, Alice) ->
              %% GIVEN
              AliceJID = client_lower_short_jid(Alice),
              AliceChatMsgRecvRK = chat_msg_recv_rk(AliceJID),
              listen_to_chat_msg_recv_events_from_rabbit([AliceJID], Config),
              %% WHEN users chat
              escalus:send(Bob,
                           escalus_stanza:chat_to(Alice, <<"Oh, hi Alice!">>)),
              escalus:assert(is_chat_message, [<<"Oh, hi Alice!">>], escalus:wait_for_stanza(Alice)),
              %% THEN  wait for chat message received events events from
              %% Rabbit.
              ?assertReceivedMatch({#'basic.deliver'{
                                       routing_key = AliceChatMsgRecvRK},
                                    #amqp_msg{}}, timer:seconds(5))
      end).

chat_message_sent_event_properly_formatted(Config) ->
    escalus:story(
      Config, [{bob, 1}, {alice, 1}],
      fun(Bob, Alice) ->
              %% GIVEN
              AliceJID = client_lower_short_jid(Alice),
              AliceFullJID = client_lower_full_jid(Alice),
              BobFullJID = client_lower_full_jid(Bob),
              AliceChatMsgSentRK = chat_msg_sent_rk(AliceJID),
              Message = <<"Hi Bob!">>,
              listen_to_chat_msg_sent_events_from_rabbit([AliceJID], Config),
              %% WHEN users chat
              escalus:send(Alice, escalus_stanza:chat_to(Bob, Message)),
              %% THEN
              ?assertMatch(#{<<"from_user_id">> := AliceFullJID,
                             <<"to_user_id">> := BobFullJID,
                             <<"message">> := Message},
                           get_decoded_message_from_rabbit(AliceChatMsgSentRK))
      end).

chat_message_received_event_properly_formatted(Config) ->
    escalus:story(
      Config, [{bob, 1}, {alice, 1}],
      fun(Bob, Alice) ->
              %% GIVEN
              AliceJID = client_lower_short_jid(Alice),
              AliceFullJID = client_lower_full_jid(Alice),
              BobFullJID = client_lower_full_jid(Bob),
              AliceChatMsgRecvRK = chat_msg_recv_rk(AliceJID),
              Message = <<"Hi Alice!">>,
              listen_to_chat_msg_recv_events_from_rabbit([AliceJID], Config),
              %% WHEN users chat
              escalus:send(Bob, escalus_stanza:chat_to(Alice, Message)),
              escalus:assert(is_chat_message, [<<"Hi Alice!">>], escalus:wait_for_stanza(Alice)),
              %% THEN
              ?assertMatch(#{<<"from_user_id">> := BobFullJID,
                             <<"to_user_id">> := AliceFullJID,
                             <<"message">> := Message},
                           get_decoded_message_from_rabbit(AliceChatMsgRecvRK))
      end).

%%--------------------------------------------------------------------
%% GROUP group_message_publish
%%--------------------------------------------------------------------

group_chat_message_sent_event(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              %% GIVEN
              Room = ?config(room, Config),
              RoomAddr = muc_helper:room_address(Room),
              BobJID = client_lower_short_jid(Bob),
              BobGroupChatMsgSentRK = group_chat_msg_sent_rk(BobJID),
              listen_to_group_chat_msg_sent_events_from_rabbit([BobJID], Config),
              %% WHEN users chat
              escalus:send(Bob,
                           muc_helper:stanza_muc_enter_room(Room, nick(Bob))),

              escalus:send(Bob, escalus_stanza:groupchat_to(RoomAddr,
                                                            <<"Hi there!">>)),

              %% THEN  wait for chat message sent events events from Rabbit.
              ?assertReceivedMatch({#'basic.deliver'{
                                       routing_key = BobGroupChatMsgSentRK},
                                    #amqp_msg{}}, timer:seconds(5))
      end).

group_chat_message_received_event(Config) ->
    escalus:story(
      Config, [{bob, 1}, {alice, 1}],
      fun(Bob, Alice) ->
              %% GIVEN
              Room = ?config(room, Config),
              RoomAddr = muc_helper:room_address(Room),
              AliceJID = client_lower_short_jid(Alice),
              AliceGroupChatMsgRecvRK = group_chat_msg_recv_rk(AliceJID),
              listen_to_group_chat_msg_recv_events_from_rabbit([AliceJID],
                                                               Config),
              %% WHEN users chat
              escalus:send(Alice, muc_helper:stanza_muc_enter_room(Room,
                                                                   nick(Alice))),
              escalus:send(Bob, muc_helper:stanza_muc_enter_room(Room,
                                                                 nick(Bob))),

              escalus:send(Bob, escalus_stanza:groupchat_to(RoomAddr,
                                                            <<"Hi there!">>)),

              %% THEN  wait for chat message received events events from Rabbit.
              ?assertReceivedMatch({#'basic.deliver'{
                                       routing_key = AliceGroupChatMsgRecvRK},
                                    #amqp_msg{}}, timer:seconds(5))
      end).

group_chat_message_sent_event_properly_formatted(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              %% GIVEN
              Room = ?config(room, Config),
              RoomAddr = muc_helper:room_address(Room),
              BobJID = client_lower_short_jid(Bob),
              BobFullJID = client_lower_full_jid(Bob),
              BobGroupChatMsgSentRK = group_chat_msg_sent_rk(BobJID),
              Message = <<"Hi there!">>,
              listen_to_group_chat_msg_sent_events_from_rabbit([BobJID], Config),
              %% WHEN a user chat
              escalus:send(Bob, muc_helper:stanza_muc_enter_room(Room,
                                                                 nick(Bob))),

              escalus:send(Bob, escalus_stanza:groupchat_to(RoomAddr, Message)),
              %% THEN
              ?assertMatch(#{<<"from_user_id">> := BobFullJID,
                             <<"to_user_id">> := RoomAddr,
                             <<"message">> := Message},
                           get_decoded_message_from_rabbit(BobGroupChatMsgSentRK))
      end).

group_chat_message_received_event_properly_formatted(Config) ->
    escalus:story(
      Config, [{bob, 1}, {alice, 1}],
      fun(Bob, Alice) ->
              %% GIVEN basic variables
              Room = ?config(room, Config),
              RoomAddr = muc_helper:room_address(Room),
              BobRoomJID = user_room_jid(RoomAddr, Bob),
              AliceJID = client_lower_short_jid(Alice),
              AliceFullJID = client_lower_full_jid(Alice),
              AliceGroupChatMsgRecvRK = group_chat_msg_recv_rk(AliceJID),
              Message = <<"Hi there!">>,
              %% GIVEN users in room
              escalus:send(Alice, muc_helper:stanza_muc_enter_room(Room, nick(Alice))),
              escalus:send(Bob, muc_helper:stanza_muc_enter_room(Room, nick(Bob))),
              % wait for all room stanzas to be processed
              escalus:wait_for_stanzas(Alice, 3),
              escalus:wait_for_stanzas(Bob, 3),
              %% GIVEN Room subscription to Rabbit
              % We subscribe to RMQ now and not earlier to avoid messages other
              % than the one we are testing, `Message` from Bob to Room, like
              % for example affiliations and the like.
              listen_to_group_chat_msg_recv_events_from_rabbit([AliceJID], Config),
              %% WHEN users chat
              escalus:send(Bob, escalus_stanza:groupchat_to(RoomAddr, Message)),
              %% THEN
              ?assertMatch(#{<<"from_user_id">> := BobRoomJID,
                             <<"to_user_id">> := AliceFullJID,
                             <<"message">> := Message},
                           get_decoded_message_from_rabbit(AliceGroupChatMsgRecvRK))
      end).

group_chat_displayed_marker_is_skipped(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Alice) ->
              %% GIVEN basic variables
              Room = ?config(room, Config),
              RoomAddr = muc_helper:room_address(Room),
              AliceJID = client_lower_short_jid(Alice),
              RoutingKeys = [group_chat_msg_sent_rk(AliceJID), group_chat_msg_recv_rk(AliceJID)],
              SentBindings = group_chat_msg_sent_bindings(?QUEUE_NAME, [AliceJID]),
              RecvBindings = group_chat_msg_recv_bindings(?QUEUE_NAME, [AliceJID]),
              %% GIVEN users in room
              escalus:send(Alice, muc_helper:stanza_muc_enter_room(Room, nick(Alice))),
              % wait for all room stanzas to be processed
              escalus:wait_for_stanzas(Alice, 2),
              listen_to_events_from_rabbit(SentBindings ++ RecvBindings, Config),
              %% WHEN Alice sends a displayed marker (which is a groupchat message w/o body)
              Marker = escalus_stanza:chat_marker(RoomAddr, <<"displayed">>, <<"id-123">>),
              escalus:send(Alice, escalus_stanza:setattr(Marker, <<"type">>, <<"groupchat">>)),
              escalus:assert(is_chat_marker, [<<"displayed">>, <<"id-123">>],
                             escalus:wait_for_stanza(Alice)),
              %% THEN there are no sent/recv events in Rabbit
              assert_no_message_from_rabbit(tl(RoutingKeys))
      end).

%%--------------------------------------------------------------------
%% GROUP instrumentation
%%--------------------------------------------------------------------
connections_events_are_executed(_Config) ->
    %% GIVEN
    Domain = domain(),
    Tag = test_tag,
    WpoolConfig = #{type => rabbit, scope => host_type, tag => Tag},
    RabbitWpool = {rabbit, Domain, test_tag},
    %% WHEN
    start_rabbit_wpool(Domain, WpoolConfig),
    assert_connection_event(Tag),
    %% THEN
    Pools = rpc(mim(), mongoose_wpool, get_pools, []),
    ?assertMatch(RabbitWpool,
                 lists:keyfind(test_tag, 3, Pools)),

    %% CLEANUP
    stop_rabbit_wpool(RabbitWpool),
    assert_disconnection_event(Tag).

connection_failed_events_are_executed(_Config) ->
    %% GIVEN incorrect configuration (plain TCP connection to the TLS port)
    Tag = fail_tag,
    WpoolConfig = #{type => rabbit, scope => host_type, tag => Tag,
                    conn_opts => #{port => 5671}},
    Pool = config([outgoing_pools, rabbit, event_pusher], WpoolConfig),

    %% WHEN the pool is started
    Result = rpc(mim(), mongoose_wpool, start_configured_pools, [[Pool], [domain()]]),

    %% THEN connection fails, instrumentation event is emitted, and pool is not started
    ?assertMatch([{error, _}], Result),
    assert_connection_failed_event(Tag),
    Pools = rpc(mim(), mongoose_wpool, get_pools, []),
    ?assertEqual(false, lists:keyfind(Tag, 3, Pools)).

messages_published_events_are_executed(Config) ->
    escalus:story(
        Config, [{bob, 1}, {alice, 1}],
        fun(Bob, Alice) ->
            %% GIVEN
            BobJID = client_lower_short_jid(Bob),
            BobChatMsgSentRK = chat_msg_sent_rk(BobJID),
            SentBindings = chat_msg_sent_bindings(?QUEUE_NAME, [BobJID]),
            AliceJID = client_lower_short_jid(Alice),
            AliceChatMsgRecvRK = chat_msg_recv_rk(AliceJID),
            RecvBindings = chat_msg_recv_bindings(?QUEUE_NAME, [AliceJID]),
            listen_to_events_from_rabbit(SentBindings ++ RecvBindings, Config),
            %% WHEN users chat
            Msg = <<"Oh, hi Alice from the event's test!">>,
            escalus:send(Bob,
                         escalus_stanza:chat_to(Alice, Msg)),
            %% THEN wait for chat message sent events from Rabbit.
            ?assertReceivedMatch({#'basic.deliver'{routing_key = BobChatMsgSentRK},
                                  #amqp_msg{}}, timer:seconds(5)),
            ?assertReceivedMatch({#'basic.deliver'{routing_key = AliceChatMsgRecvRK},
                                  #amqp_msg{}}, timer:seconds(5)),
            instrument_helper:assert(wpool_rabbit_messages_published,
                                     #{host_type => domain(), pool_tag => event_pusher},
                                     fun(#{count := 1, time := T, size := S, payload := P}) ->
                                             {amqp_msg, _, BinData} = P,
                                             T >= 0 andalso S >= 0 andalso
                                                 binary:match(BinData, Msg) /= nomatch
                                     end, #{expected_count => 2}) % for sender and receiver
        end).

connection_is_restarted_on_error(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              %% GIVEN intermittent rabbit connection failure
              BobJID = client_lower_short_jid(Bob),
              listen_to_presence_events_from_rabbit([BobJID], Config),
              {ok, Worker} = get_rabbit_worker(),
              simulate_rabbit_connection_error(),

              %% WHEN user sends presence
              send_presence_stanzas([Bob], 1),

              %% THEN event is delivered because the worker kept its queue
              ?assertReceivedMatch({#'basic.deliver'{routing_key = BobJID},
                                    #amqp_msg{}}, timer:seconds(5)),
              ?assertEqual({ok, Worker}, get_rabbit_worker())
      end).

connection_is_restarted_with_retries(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              %% GIVEN an intermittent rabbit connection failure lasting for 2 connect attempts
              BobJID = client_lower_short_jid(Bob),
              listen_to_presence_events_from_rabbit([BobJID], Config),
              {ok, Worker} = get_rabbit_worker(),
              simulate_rabbit_connection_error(2),

              %% WHEN user sends presence
              send_presence_stanzas([Bob], 1),

              %% THEN event is delivered because the worker kept its queue
              ?assertReceivedMatch({#'basic.deliver'{routing_key = BobJID},
                                    #amqp_msg{}}, timer:seconds(5)),
              ?assertEqual({ok, Worker}, get_rabbit_worker())
      end).

connection_is_restarted_with_retries_and_queue_limit(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              %% GIVEN an intermittent rabbit connection failure lasting for 2 connect attempts
              BobJID = client_lower_short_jid(Bob),
              listen_to_presence_events_from_rabbit([BobJID], Config),
              {ok, Worker} = get_rabbit_worker(),
              simulate_rabbit_connection_error(2),

              %% WHEN user sends 2 presences
              send_presence_stanzas([Bob], 2),

              %% THEN: - first event is delivered because the worker kept its queue
              %%       - second event is dropped because max_worker_queue_len is 1
              DecodedMessage = get_decoded_message_from_rabbit(BobJID),
              ?assertMatch(#{<<"present">> := false}, DecodedMessage),
              assert_no_message_from_rabbit([BobJID]),
              ?assertEqual({ok, Worker}, get_rabbit_worker())
      end).

worker_is_restarted_after_failed_retries(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              %% GIVEN an intermittent rabbit connection failure lasting for 3 connect attempts
              BobJID = client_lower_short_jid(Bob),
              listen_to_presence_events_from_rabbit([BobJID], Config),
              {ok, Worker} = get_rabbit_worker(),
              simulate_rabbit_connection_error(3),

              %% WHEN user sends presence
              send_presence_stanzas([Bob], 1),

              %% THEN event is dropped because worker is restarted (reconnect.attempts was 2)
              assert_no_message_from_rabbit([BobJID]),
              wait_for_new_rabbit_worker(Worker),
              send_presence_stanzas([Bob], 1),
              ?assertReceivedMatch({#'basic.deliver'{routing_key = BobJID}, #amqp_msg{}},
                                   timer:seconds(5))
      end).

%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

-spec connect_to_rabbit() -> proplists:proplist().
connect_to_rabbit() ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{virtual_host = ?VHOST}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    [{rabbit_connection, Connection}, {rabbit_channel, Channel}].

-spec close_rabbit_connection(Config :: proplists:proplist()) -> ok | term().
close_rabbit_connection(Config) ->
    Connection = proplists:get_value(rabbit_connection, Config),
    Channel = proplists:get_value(rabbit_channel, Config),
    amqp_channel:close(Channel),
    amqp_connection:close(Connection).

-spec listen_to_presence_events_from_rabbit(JIDs :: [binary()],
                                            Config :: proplists:proplist()) ->
    ok | term().
listen_to_presence_events_from_rabbit(JIDs, Config) ->
    QueueBindings = presence_bindings(?QUEUE_NAME, JIDs),
    listen_to_events_from_rabbit(QueueBindings, Config).

-spec listen_to_chat_msg_sent_events_from_rabbit(JIDs :: [binary()],
                                                 Config :: proplists:proplist()) ->
    ok | term().
listen_to_chat_msg_sent_events_from_rabbit(JIDs, Config) ->
    QueueBindings = chat_msg_sent_bindings(?QUEUE_NAME, JIDs),
    listen_to_events_from_rabbit(QueueBindings, Config).

-spec listen_to_chat_msg_recv_events_from_rabbit(JIDs :: [binary()],
                                                 Config :: proplists:proplist()) ->
    ok | term().
listen_to_chat_msg_recv_events_from_rabbit(JIDs, Config) ->
    QueueBindings = chat_msg_recv_bindings(?QUEUE_NAME, JIDs),
    listen_to_events_from_rabbit(QueueBindings, Config).

-spec listen_to_group_chat_msg_sent_events_from_rabbit(JIDs :: [binary()],
                                                       Config :: proplists:proplist()) ->
    ok | term().
listen_to_group_chat_msg_sent_events_from_rabbit(JIDs, Config) ->
    QueueBindings = group_chat_msg_sent_bindings(?QUEUE_NAME, JIDs),
    listen_to_events_from_rabbit(QueueBindings, Config).

-spec listen_to_group_chat_msg_recv_events_from_rabbit(JIDs :: [binary()],
                                                       Config :: proplists:proplist()) ->
    ok | term().
listen_to_group_chat_msg_recv_events_from_rabbit(JIDs, Config) ->
    QueueBindings = group_chat_msg_recv_bindings(?QUEUE_NAME, JIDs),
    listen_to_events_from_rabbit(QueueBindings, Config).

-spec listen_to_events_from_rabbit(QueueBindings :: [rabbit_binding()],
                                   Config :: proplists:proplist()) ->
    ok | term().
listen_to_events_from_rabbit(QueueBindings, Config) ->
    Channel = proplists:get_value(rabbit_channel, Config),
    declare_temporary_rabbit_queue(Channel, ?QUEUE_NAME),
    ensure_exchanges_present(get_enabled_exchanges()),
    bind_queues_to_exchanges(Channel, QueueBindings),
    subscribe_to_rabbit_queue(Channel, ?QUEUE_NAME).

get_enabled_exchanges() ->
    Opts = rpc(mim(), gen_mod, get_module_opts, [domain(), mod_event_pusher_rabbit]),
    [#{<<"name">> => Name} || _ := #{name := Name} <- Opts].

-spec declare_temporary_rabbit_queue(Channel :: pid(), Queue :: binary()) -> binary().
declare_temporary_rabbit_queue(Channel, Queue) ->
    #'queue.declare_ok'{} =
        amqp_channel:call(Channel, #'queue.declare'{queue = Queue,
                                                    exclusive = true}).

-spec presence_bindings(Queue :: binary(), JIDs :: [binary()]) ->
    [rabbit_binding()].
presence_bindings(Queue, JIDs) ->
    [{Queue, ?PRESENCE_EXCHANGE, JID} || JID <- JIDs].

-spec chat_msg_sent_bindings(Queue :: binary(), JIDs :: [binary()]) ->
    [rabbit_binding()].
chat_msg_sent_bindings(Queue, JIDs) ->
    [{Queue, ?CHAT_MSG_EXCHANGE, chat_msg_sent_rk(JID)} || JID <- JIDs].

-spec chat_msg_recv_bindings(Queue :: binary(), JIDs :: [binary()]) ->
    [rabbit_binding()].
chat_msg_recv_bindings(Queue, JIDs) ->
    [{Queue, ?CHAT_MSG_EXCHANGE, chat_msg_recv_rk(JID)} || JID <- JIDs].

-spec group_chat_msg_sent_bindings(Queue :: binary(), JIDs :: [binary()]) ->
    [rabbit_binding()].
group_chat_msg_sent_bindings(Queue, JIDs) ->
    [{Queue, ?GROUP_CHAT_MSG_EXCHANGE, group_chat_msg_sent_rk(JID)}
     || JID <- JIDs].

-spec group_chat_msg_recv_bindings(Queue :: binary(), JIDs :: [binary()]) ->
    [rabbit_binding()].
group_chat_msg_recv_bindings(Queue, JIDs) ->
    [{Queue, ?GROUP_CHAT_MSG_EXCHANGE, group_chat_msg_recv_rk(JID)}
     || JID <- JIDs].

-spec bind_queues_to_exchanges(Channel :: pid(),
                               Bindings :: [rabbit_binding()]) ->
    [amqp_client:amqp_method() | ok | blocked | closing].
bind_queues_to_exchanges(Channel, Bindings) ->
    [bind_queue_to_exchange(Channel, Binding) || Binding <- Bindings].

-spec bind_queue_to_exchange(Channel :: pid(), rabbit_binding()) ->
    amqp_client:amqp_method() | ok | blocked | closing.
bind_queue_to_exchange(Channel, {Queue, Exchange, RoutingKey}) ->
    #'queue.bind_ok'{} =
        amqp_channel:call(Channel, #'queue.bind'{exchange = Exchange,
                                                 routing_key = RoutingKey,
                                                 queue = Queue}).

-spec ensure_exchanges_present([json_object()]) -> {ok, [json_object()]}.
ensure_exchanges_present(Expected) ->
    Opts = #{name => ?FUNCTION_NAME,
             validator => fun(Exchanges) -> are_exchanges_present(Expected, Exchanges) end},
    wait_helper:wait_until(fun list_exchanges/0, true, Opts).

-spec ensure_no_queues() -> {ok, []}.
ensure_no_queues() ->
    wait_helper:wait_until(fun list_queues/0, [], #{name => ?FUNCTION_NAME}).

-spec are_exchanges_present([json_object()], [json_object()]) -> boolean().
are_exchanges_present(AttrsList, Exchanges) ->
    lists:all(fun(Attrs) -> is_exchange_present(Attrs, Exchanges) end, AttrsList).

-spec is_exchange_present(json_object(), [json_object()]) -> boolean().
is_exchange_present(Attrs, Exchanges) ->
    Keys = maps:keys(Attrs),
    case lists:filter(fun(Exchange) ->  maps:with(Keys, Exchange) =:= Attrs end, Exchanges) of
        [] -> false;
        [_] -> true
    end.

-spec list_exchanges() -> [json_object()].
list_exchanges() ->
    call_http_api_get("exchanges").

-spec list_queues() -> [json_object()].
list_queues() ->
    call_http_api_get("queues").

-spec ensure_vhost(binary()) -> ok.
ensure_vhost(VHost) ->
    call_http_api_put("vhosts/" ++ binary_to_list(VHost)).

-spec call_http_api_get(string()) -> [json_object()].
call_http_api_get(Path) ->
    get_json_body(call_http_api(get, Path)).

-spec call_http_api_put(string()) -> ok.
call_http_api_put(Path) ->
    {{_, Code, Status}, _Headers, _Body} = call_http_api(put, Path),
    assert_created_or_no_content(Code, Status).

get_json_body({{_, 200, "OK"}, _Headers, Body}) ->
    json:decode(iolist_to_binary(Body)).

call_http_api(Method, Path) ->
    Auth = "Basic " ++ binary_to_list(base64:encode("guest:guest")),
    Headers = [{"Authorization", Auth}],
    URL = ?RABBIT_HTTP_ENDPOINT ++ "/api/" ++ Path,
    {ok, Response} = httpc:request(Method, {URL, Headers}, [], []),
    Response.

assert_created_or_no_content(201, "Created") -> ok;
assert_created_or_no_content(204, "No Content") -> ok.

-spec subscribe_to_rabbit_queue(Channel :: pid(), Queue :: binary()) -> ok.
subscribe_to_rabbit_queue(Channel, Queue) ->
    amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue,
                                                     no_ack = true}, self()),
    receive
        #'basic.consume_ok'{} -> ok
    after
        5000 ->
            throw(io_lib:format("Timeout when subscribing to queue,"
                                "channel=~p, queue=~p", [Channel, Queue]))
    end.

-spec send_presence_stanzas(Users :: [binary()], NumOfMsgs :: non_neg_integer())
                           -> [[ok]] | term().
send_presence_stanzas(Users, NumOfMsgs) ->
    [send_presence_stanza(User, NumOfMsgs) || User <- Users].

-spec send_presence_stanza(Users :: [binary()], NumOfMsgs :: non_neg_integer())
                          -> [ok] | term().
send_presence_stanza(User, NumOfMsgs) ->
    [escalus:send(User, escalus_stanza:presence(make_pres_type(X)))
     || X <- lists:seq(1, NumOfMsgs)].

-spec get_decoded_message_from_rabbit(RoutingKey :: binary()) -> map().
get_decoded_message_from_rabbit(RoutingKey) ->
    receive
        {#'basic.deliver'{routing_key = RoutingKey}, #amqp_msg{payload = Msg}} ->
            ct:log("Decoded rabbit message, rk=~p~nmessage:~ts", [RoutingKey, Msg]),
            jiffy:decode(Msg, [return_maps])
    after
        5000 -> ct:fail("Timeout when decoding message, rk=~p", [RoutingKey])
    end.

-spec assert_no_message_from_rabbit(RoutingKeys :: [binary()]) -> ok.
assert_no_message_from_rabbit(RoutingKeys) ->
    receive
        {#'basic.deliver'{routing_key = RoutingKey}, #amqp_msg{payload = Msg}} ->
            case lists:member(RoutingKey, RoutingKeys) of
                true ->
                    ct:fail("Unexpected rabbit message, rk=~p~nmessage: ~p", [RoutingKey, Msg]);
                false ->
                    ct:log("Skipping rabbit message, rk=~p,~nmessage:~p", [RoutingKey, Msg]),
                    assert_no_message_from_rabbit(RoutingKeys)
            end
    after
        500 -> ok % To save time, this timeout is shorter than in the positive test
    end.

simulate_rabbit_connection_error() ->
    rpc(mim(), ?UTILS_MODULE, ?FUNCTION_NAME, [domain(), 5671, 0]).

simulate_rabbit_connection_error(Count) ->
    rpc(mim(), ?UTILS_MODULE, ?FUNCTION_NAME, [domain(), 5671, Count]).

wait_for_new_rabbit_worker(OldWorker) ->
    {ok, {ok, NewWorker}} =
        wait_helper:wait_until(fun get_rabbit_worker/0, true,
                               #{validator => fun({ok, Worker}) -> Worker =/= OldWorker end}),
    NewWorker.

get_rabbit_worker() ->
     rpc(mim(), mongoose_wpool, get_worker, [rabbit, domain(), event_pusher]).

%%--------------------------------------------------------------------
%% Utils
%%--------------------------------------------------------------------

start_rabbit_wpool(Host) ->
    start_rabbit_wpool(Host, ?WPOOL_CFG).

start_rabbit_tls_wpool(Host, GroupName) ->
    BasicOpts = ?WPOOL_CFG,
    BasicConnOpts = #{tls => tls_config(), port => 5671, virtual_host => ?VHOST},
    ConnOpts = maps:merge(BasicConnOpts, extra_conn_opts(GroupName)),
    ensure_vhost(?VHOST),
    start_rabbit_wpool(Host, maps:merge(BasicOpts#{conn_opts => ConnOpts}, extra_opts(GroupName))).

extra_conn_opts(presence_status_publish_with_confirms) ->
    #{confirms_enabled => true};
extra_conn_opts(single_worker) ->
    #{reconnect => #{attempts => 2, delay => 1000}}; % Note: in case of flaky tests, increase delay
extra_conn_opts(_GroupName) ->
    #{}.

extra_opts(single_worker) ->
    #{opts => #{workers => 1, max_worker_queue_len => 1}};
extra_opts(_) ->
    #{}.

tls_config() ->
    #{certfile => "priv/ssl/fake_cert.pem",
      keyfile => "priv/ssl/fake_key.pem",
      cacertfile => "priv/ssl/cacert.pem"}.

start_rabbit_wpool(Host, WpoolConfig) ->
    rpc(mim(), mongoose_wpool, ensure_started, []),
    Pool = config([outgoing_pools, rabbit, event_pusher], WpoolConfig),
    [{ok, _Pid}] = rpc(mim(), mongoose_wpool, start_configured_pools, [[Pool], [Host]]).

stop_rabbit_wpool({Pool, Host, Tag}) ->
    rpc(mim(), mongoose_wpool, stop, [Pool, Host, Tag]);
stop_rabbit_wpool(Host)->
    stop_rabbit_wpool({rabbit, Host, event_pusher}).

delete_exchanges() ->
    ConnConf = connect_to_rabbit(),
    Channel  = proplists:get_value(rabbit_channel, ConnConf),
    [amqp_channel:call(Channel, #'exchange.delete'{exchange = Ex})
     || Ex <- [?PRESENCE_EXCHANGE, ?CHAT_MSG_EXCHANGE, ?GROUP_CHAT_MSG_EXCHANGE]],
    close_rabbit_connection(ConnConf).

make_pres_type(X) when X rem 2 == 0 ->
    <<"available">>;
make_pres_type(_) ->
    <<"unavailable">>.

chat_msg_sent_rk(JID) -> user_topic_routing_key(JID, ?CHAT_MSG_SENT_TOPIC).

chat_msg_recv_rk(JID) -> user_topic_routing_key(JID, ?CHAT_MSG_RECV_TOPIC).

group_chat_msg_sent_rk(JID) ->
    user_topic_routing_key(JID, ?GROUP_CHAT_MSG_SENT_TOPIC).

group_chat_msg_recv_rk(JID) ->
    user_topic_routing_key(JID, ?GROUP_CHAT_MSG_RECV_TOPIC).

user_topic_routing_key(JID, Topic) -> <<JID/binary, ".", Topic/binary>>.

client_lower_short_jid(Client) ->
    escalus_utils:jid_to_lower(escalus_client:short_jid(Client)).

client_lower_full_jid(Client) ->
    escalus_utils:jid_to_lower(escalus_client:full_jid(Client)).

nick(User) -> escalus_utils:get_username(User).

maybe_prepare_muc(TestCase, Config) ->
    case lists:member(TestCase, group_chat_message_publish_tests()) of
        true -> prepare_muc(Config);
        false -> Config
    end.

maybe_cleanup_muc(TestCase, Config) ->
    case lists:member(TestCase, group_chat_message_publish_tests()) of
        true -> cleanup_muc(Config);
        false -> ok
    end.

prepare_muc(Config) ->
    [User | _] = ?config(escalus_users, Config),
    muc_helper:start_room(Config, User, <<"muc_publish">>, <<"user_nick">>,
                          [{persistent, true},
                           {anonymous, false}]).

cleanup_muc(Config) ->
    muc_helper:destroy_room(Config).

user_room_jid(RoomJID, UserJID) ->
    Nick = nick(UserJID),
    <<RoomJID/binary, "/", Nick/binary>>.

is_rabbitmq_available() ->
    try amqp_connection:start(#amqp_params_network{}) of
        {ok, Conn} ->
            amqp_connection:close(Conn),
            true;
        {error, econnrefused} ->
            false
    catch
        _Err ->
            false
    end.

assert_connection_event(Tag) ->
    instrument_helper:wait_and_assert(wpool_rabbit_connections,
                                      #{host_type => domain(), pool_tag => Tag},
                                      fun(M) -> M =:= #{active => 1, opened => 1} end).

assert_connection_failed_event(Tag) ->
    instrument_helper:wait_and_assert(wpool_rabbit_connections,
                                      #{host_type => domain(), pool_tag => Tag},
                                      fun(M) -> M =:= #{failed => 1} end).

assert_disconnection_event(Tag) ->
    instrument_helper:wait_and_assert(wpool_rabbit_connections,
                                      #{host_type => domain(), pool_tag => Tag},
                                      fun(M) -> M =:= #{active => -1, closed => 1} end).
