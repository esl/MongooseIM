%%%-------------------------------------------------------------------
%%% @author Kacper Mentel <kacper.mentel@erlang-solutions.com>
%%% @copyright (C) 2018, Kacper Mentel
%%% @doc
%%% Tests for `mod_event_pusher` RabbitMQ backend (`mod_event_pusher_rabbit`).
%%% @end
%%%-------------------------------------------------------------------
-module(mod_event_pusher_rabbit_SUITE).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-include("assert_received_match.hrl").

-import(distributed_helper, [mim/0,
                             rpc/4]).

-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

-export([rabbit_pool_starts_with_default_config/1,
         exchanges_are_created_on_module_startup/1]).
-export([connected_users_push_presence_events_when_change_status/1,
         presence_messages_are_properly_formatted/1]).
-export([chat_message_sent_event/1,
         chat_message_sent_event_properly_formatted/1,
         chat_message_received_event/1,
         chat_message_received_event_properly_formatted/1]).
-export([group_chat_message_sent_event/1,
         group_chat_message_sent_event_properly_formatted/1,
         group_chat_message_received_event/1,
         group_chat_message_received_event_properly_formatted/1]).

-define(QUEUE_NAME, <<"test_queue">>).
-define(DEFAULT_EXCHANGE_TYPE, <<"topic">>).
-define(PRESENCE_EXCHANGE, <<"custom_presence_exchange">>).
-define(CHAT_MSG_EXCHANGE, <<"custom_chat_msg_exchange">>).
-define(GROUP_CHAT_MSG_EXCHANGE, <<"custom_group_chat_msg_exchange">>).
-define(CHAT_MSG_SENT_TOPIC, <<"custom_chat_msg_sent_topic">>).
-define(CHAT_MSG_RECV_TOPIC, <<"custom_chat_msg_recv_topic">>).
-define(GROUP_CHAT_MSG_SENT_TOPIC, <<"custom_group_chat_msg_sent_topic">>).
-define(GROUP_CHAT_MSG_RECV_TOPIC, <<"custom_group_chat_msg_recv_topic">>).
-define(MOD_EVENT_PUSHER_RABBIT_CFG,
        [{presence_exchange, [{name, ?PRESENCE_EXCHANGE}]},
         {chat_msg_exchange, [{name, ?CHAT_MSG_EXCHANGE},
                              {sent_topic, ?CHAT_MSG_SENT_TOPIC},
                              {recv_topic, ?CHAT_MSG_RECV_TOPIC}]},
         {groupchat_msg_exchange, [{name, ?GROUP_CHAT_MSG_EXCHANGE},
                                   {sent_topic, ?GROUP_CHAT_MSG_SENT_TOPIC},
                                   {recv_topic, ?GROUP_CHAT_MSG_RECV_TOPIC}]}
        ]).
-define(MOD_EVENT_PUSHER_CFG, [{backends,
                                [{rabbit, ?MOD_EVENT_PUSHER_RABBIT_CFG}]}]).
-define(WPOOL_CFG, {rabbit, host, event_pusher,
                    [{workers, 20}],
                    [%% enables publisher one-to-one confirms
                     %% disabled by default
                     %% {confirms_enabled, true},
                     {amqp_host, "localhost"},
                     {amqp_port, 5672},
                     {amqp_username, "guest"},
                     {amqp_password, "guest"}
                    ]}).
-define(IF_EXCHANGE_EXISTS_RETRIES, 30).
-define(WAIT_FOR_EXCHANGE_INTERVAL, 100). % ms

-type rabbit_binding() :: {Queue :: binary(),
                           Exchange :: binary(),
                           RoutingKey :: binary()}.

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, initialization_on_startup},
     {group, presence_status_publish},
     {group, chat_message_publish},
     {group, group_chat_message_publish}
    ].

groups() ->
    G = [
         {initialization_on_startup, [],
          [
           rabbit_pool_starts_with_default_config,
           exchanges_are_created_on_module_startup
          ]},
         {presence_status_publish, [],
          [
           connected_users_push_presence_events_when_change_status,
           presence_messages_are_properly_formatted
          ]},
         {chat_message_publish, [],
          [
           chat_message_sent_event,
           chat_message_sent_event_properly_formatted,
           chat_message_received_event,
           chat_message_received_event_properly_formatted
          ]},
         {group_chat_message_publish, [],
          [
           group_chat_message_sent_event,
           group_chat_message_sent_event_properly_formatted,
           group_chat_message_received_event,
           group_chat_message_received_event_properly_formatted
          ]}
        ],
    ct_helper:repeat_all_until_all_ok(G).

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Host = ct:get_config({hosts, mim, domain}),
    start_rabbit_wpool(Host),
    {ok, _} = application:ensure_all_started(amqp_client),
    case is_rabbitmq_available() of
        true ->
            muc_helper:load_muc(muc_helper:muc_host()),
            escalus:init_per_suite(Config);
        false ->
            {skip, "RabbitMQ server is not available on default port."}
    end.

end_per_suite(Config) ->
    Host = ct:get_config({hosts, mim, domain}),
    stop_rabbit_wpool(Host),
    escalus_fresh:clean(),
    muc_helper:unload_muc(),
    escalus:end_per_suite(Config).

init_per_group(initialization_on_startup, Config) ->
    Config;
init_per_group(_, Config0) ->
    Host = ct:get_config({hosts, mim, domain}),
    Config = dynamic_modules:save_modules(Host, Config0),
    dynamic_modules:ensure_modules(Host,
                                   [{mod_event_pusher, ?MOD_EVENT_PUSHER_CFG}]),
    Config.

end_per_group(initialization_on_startup, Config) ->
    Config;
end_per_group(_, Config) ->
    delete_exchanges(),
    Host = ct:get_config({hosts, mim, domain}),
    dynamic_modules:stop(Host, mod_event_pusher),
    dynamic_modules:restore_modules(Host, Config),
    escalus:delete_users(Config, escalus:get_users([bob, alice])).

init_per_testcase(rabbit_pool_starts_with_default_config, Config) ->
    Config;
init_per_testcase(CaseName, Config0) ->
    Config1 = escalus_fresh:create_users(Config0, [{bob, 1}, {alice, 1}]),
    Config2 = maybe_prepare_muc(CaseName, Config1),
    Config = Config2 ++ connect_to_rabbit(),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(rabbit_pool_starts_with_default_config, Config) ->
    Config;
end_per_testcase(exchanges_are_created_on_module_startup, Config) ->
    delete_exchanges(),
    stop_mod_event_pusher_rabbit(),
    close_rabbit_connection(Config),
    Config;
end_per_testcase(CaseName, Config) ->
    maybe_cleanup_muc(CaseName, Config),
    close_rabbit_connection(Config),
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% GROUP initialization_on_startup
%%--------------------------------------------------------------------

rabbit_pool_starts_with_default_config(_Config) ->
    %% GIVEN
    Host = ct:get_config({hosts, mim, domain}),
    DefaultWpoolConfig = {rabbit, host, rabbit_event_pusher_default, [], []},
    RabbitWpool = {rabbit, Host, rabbit_event_pusher_default},
    %% WHEN
    start_rabbit_wpool(Host, DefaultWpoolConfig),
    %% THEN
    Pools = rpc(mim(), mongoose_wpool, get_pools, []),
    ?assertMatch(RabbitWpool,
                 lists:keyfind(rabbit_event_pusher_default, 3, Pools)),
    %% CLEANUP
    stop_rabbit_wpool(RabbitWpool).

exchanges_are_created_on_module_startup(Config) ->
    %% GIVEN
    Connection = proplists:get_value(rabbit_connection, Config),
    ExCustomType = <<"headers">>,
    Exchanges = [{?PRESENCE_EXCHANGE, ExCustomType},
                 {?CHAT_MSG_EXCHANGE, ExCustomType},
                 {?GROUP_CHAT_MSG_EXCHANGE, ExCustomType}],
    ConfigWithCustomExchangeType =
        extend_config_with_exchange_type(ExCustomType),
    %% WHEN
    start_mod_event_pusher_rabbit(ConfigWithCustomExchangeType),
    %% THEN exchanges are created
    [?assert(ensure_exchange_present(Connection, Exchange))
     || Exchange <- Exchanges].

%%--------------------------------------------------------------------
%% GROUP presence_status_publish
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
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              %% GIVEN
              BobJID = client_lower_short_jid(Bob),
              BobFullJID = client_lower_full_jid(Bob),
              listen_to_presence_events_from_rabbit([BobJID], Config),
              %% WHEN user logout
              escalus:send(Bob, escalus_stanza:presence(<<"unavailable">>)),
              %% THEN receive message
              ?assertMatch(#{<<"user_id">> := BobFullJID, <<"present">> := false},
                           get_decoded_message_from_rabbit(BobJID))
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
              escalus:wait_for_stanzas(Alice, 2),
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
              escalus:wait_for_stanzas(Alice, 2),
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

%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

-spec connect_to_rabbit() -> proplists:proplist().
connect_to_rabbit() ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{}),
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
    Connection = proplists:get_value(rabbit_connection, Config),
    Channel = proplists:get_value(rabbit_channel, Config),
    declare_temporary_rabbit_queue(Channel, ?QUEUE_NAME),
    wait_for_exchanges_to_be_created(Connection,
                                     [{?PRESENCE_EXCHANGE, ?DEFAULT_EXCHANGE_TYPE},
                                      {?CHAT_MSG_EXCHANGE, ?DEFAULT_EXCHANGE_TYPE},
                                      {?GROUP_CHAT_MSG_EXCHANGE, ?DEFAULT_EXCHANGE_TYPE}]),
    bind_queues_to_exchanges(Channel, QueueBindings),
    subscribe_to_rabbit_queue(Channel, ?QUEUE_NAME).

-spec wait_for_exchanges_to_be_created(Connection :: pid(),
                                       Exchanges :: [binary()]) -> pid().
wait_for_exchanges_to_be_created(Connection, Exchanges) ->
    [ensure_exchange_present(Connection, Exchange) || Exchange <- Exchanges],
    ok.

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

-spec ensure_exchange_present(Connection :: pid(), Exchange :: binary()) ->
                                     {ok, true} | {timeout, any()}.
ensure_exchange_present(Connection, Exchange) ->
    Opts = #{time_left => ?WAIT_FOR_EXCHANGE_INTERVAL * ?IF_EXCHANGE_EXISTS_RETRIES / 1000,
             sleep_time => ?WAIT_FOR_EXCHANGE_INTERVAL},
    case mongoose_helper:wait_until(fun() ->
                                            is_exchange_present(Connection,
                                                                Exchange)
                                    end, true, Opts) of
        {ok, true} -> true;
        {timeout, _} ->
            throw(io_lib:format("Exchange has not been created, exchange=~p",
                                [Exchange]))
    end.

-spec is_exchange_present(Connection :: pid(), Exchange :: binary()) -> boolean().
is_exchange_present(Connection, {ExName, ExType}) ->
    {ok, Channel} = amqp_connection:open_channel(Connection),
    try amqp_channel:call(Channel, #'exchange.declare'{exchange = ExName,
                                                       type = ExType,
                                                       %% this option allows to
                                                       %% check if an exchange exists
                                                       passive = true}) of
        {'exchange.declare_ok'} -> true
    catch
        _Error:_Reason -> false
    end.

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

-spec get_decoded_message_from_rabbit(RoutingKey :: binary()) ->
    map() | no_return().
get_decoded_message_from_rabbit(RoutingKey) ->
    receive
        {#'basic.deliver'{routing_key = RoutingKey}, #amqp_msg{payload = Msg}} ->
            jiffy:decode(Msg, [return_maps])
    after
        5000 -> ct:fail(io_lib:format("Timeout when decoding message, rk=~p",
                                   [RoutingKey]))
    end.

%%--------------------------------------------------------------------
%% Utils
%%--------------------------------------------------------------------

start_mod_event_pusher_rabbit(Config) ->
    Host = ct:get_config({hosts, mim, domain}),
    rpc(mim(), gen_mod, start_module, [Host, mod_event_pusher_rabbit, Config]).

stop_mod_event_pusher_rabbit() ->
    Host = ct:get_config({hosts, mim, domain}),
    rpc(mim(), gen_mod, stop_module, [Host, mod_event_pusher_rabbit]).

start_rabbit_wpool(Host) ->
    start_rabbit_wpool(Host, ?WPOOL_CFG).

start_rabbit_wpool(Host, WpoolConfig) ->
    rpc(mim(), mongoose_wpool, ensure_started, []),
    rpc(mim(), mongoose_wpool, start_configured_pools, [[WpoolConfig], [Host]]).

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

maybe_prepare_muc(TestCase, Config) when
      TestCase == group_chat_message_sent_event orelse
      TestCase == group_chat_message_received_event orelse
      TestCase == group_chat_message_sent_event_properly_formatted orelse
      TestCase == group_chat_message_received_event_properly_formatted ->
    prepare_muc(Config);
maybe_prepare_muc(_, Config) -> Config.

maybe_cleanup_muc(TestCase, Config) when
      TestCase == group_chat_message_sent_event orelse
      TestCase == group_chat_message_received_event orelse
      TestCase == group_chat_message_sent_event_properly_formatted orelse
      TestCase == group_chat_message_received_event_properly_formatted ->
    cleanup_muc(Config);
maybe_cleanup_muc(_, _) -> ok.

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

extend_config_with_exchange_type(ExType) ->
    lists:map(fun({Ex, Opts}) when
                        Ex == presence_exchange orelse
                        Ex == chat_msg_exchange orelse
                        Ex == groupchat_msg_exchange ->
                      {Ex, Opts ++ [{type, ExType}]};
                 (Other) -> Other
              end, ?MOD_EVENT_PUSHER_RABBIT_CFG).

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
