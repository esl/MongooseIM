%%%-------------------------------------------------------------------
%%% @author Kacper Mentel <kacper.mentel@erlang-solutions.com>
%%% @copyright (C) 2018, Kacper Mentel
%%% @doc
%%% Tests for `mod_event_pusher` RabbitMQ backend (`mod_event_pusher_rabbit`).
%%% IMPORTANT: Running this SUITE requires adding `amqp_client` application and
%%% it's dependencies ebin files from Mongoose repo directory to Common Test VM
%%% code paths. One way (recommended) to do so is to use a dedicated mechanism
%%% in the `big_tests/Makefile`: while running make targets export `DEPS`
%%% environment variable providing names of necessary dependencies (include
%%% `amqp_client` as well).
%%% @end
%%%-------------------------------------------------------------------
-module(mod_event_pusher_rabbit_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-include("assert_received_match.hrl").

-import(distributed_helper, [mim/0,
                             rpc/4]).

-define(QUEUE_NAME, <<"test_queue">>).
-define(PRESENCE_EXCHANGE, <<"custom_presence_exchange">>).
-define(MOD_EVENT_PUSHER_RABBIT_CFG, [{presence_exchange, ?PRESENCE_EXCHANGE},
                                      {amqp_host, "localhost"},
                                      {amqp_port, 5672},
                                      {amqp_username, <<"guest">>},
                                      {amqp_password, <<"guest">>}
                                     ]).
-define(MOD_EVENT_PUSHER_CFG, [{backends,
                                [{rabbit, ?MOD_EVENT_PUSHER_RABBIT_CFG}]}]).
-define(IF_EXCHANGE_EXISTS_RETRIES, 30).
-define(WAIT_FOR_EXCHANGE_INTERVAL, 100). % ms

-type rabbit_binding() :: [{binary(), binary(), binary()}].

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, exchange_handling},
     {group, presence_status_publish}
    ].

groups() ->
    [
     {exchange_handling, [],
      [
       exchanges_are_created_on_module_startup,
       exchanges_are_deleted_on_module_stop
      ]},
     {presence_status_publish, [],
      [
       connected_users_push_presence_events_when_change_status,
       presence_messages_are_properly_formatted
      ]}
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(amqp_client),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(exchange_handling, Config) ->
    Config;
init_per_group(_, Config0) ->
    Host = ct:get_config({hosts, mim, domain}),
    Config = dynamic_modules:save_modules(Host, Config0),
    dynamic_modules:ensure_modules(Host,
                                   [{mod_event_pusher, ?MOD_EVENT_PUSHER_CFG}]),
    Config.

end_per_group(exchange_handling, Config) ->
    Config;
end_per_group(_, Config) ->
    Host = ct:get_config({hosts, mim, domain}),
    dynamic_modules:stop(Host, mod_event_pusher),
    dynamic_modules:restore_modules(Host, Config),
    escalus:delete_users(Config, escalus:get_users([bob, alice])).

init_per_testcase(CaseName, Config0) ->
    Config1 = escalus_fresh:create_users(Config0, [{bob, 1}, {alice, 1}]),
    Config = Config1 ++ listen_to_rabbit(),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(exchanges_are_created_on_module_startup, Config) ->
    stop_mod_event_pusher_rabbit(),
    Config;
end_per_testcase(exchanges_are_deleted_on_module_stop, Config) ->
    Config;
end_per_testcase(CaseName, Config) ->
    close_rabbit_connection(Config),
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% GROUP exchange_handling
%%--------------------------------------------------------------------

exchanges_are_created_on_module_startup(Config) ->
    %% GIVEN
    Connection = proplists:get_value(rabbit_connection, Config),
    Channel = proplists:get_value(rabbit_channel, Config),
    Exchanges = [?PRESENCE_EXCHANGE],
    %% WHEN
    start_mod_event_pusher_rabbit(),
    %% THEN exchanges are created
    [?assert(ensure_exchange_present(Connection, Channel, Exchange,
                                 ?IF_EXCHANGE_EXISTS_RETRIES))
     || Exchange <- Exchanges].

exchanges_are_deleted_on_module_stop(Config) ->
    %% GIVEN
    Connection = proplists:get_value(rabbit_connection, Config),
    Channel = proplists:get_value(rabbit_channel, Config),
    Exchanges = [?PRESENCE_EXCHANGE],
    %% WHEN
    start_mod_event_pusher_rabbit(),
    stop_mod_event_pusher_rabbit(),
    %% THEN
    wait_for_exchanges_to_be_deleted(Connection, Exchanges),
    [?assert(ensure_exchange_absent(Connection, Channel, Exchange, 1))
     || Exchange <- Exchanges].

%%--------------------------------------------------------------------
%% GROUP presence_status_publish
%%--------------------------------------------------------------------

connected_users_push_presence_events_when_change_status(Config) ->
    escalus:story(
      Config, [{bob, 1}, {alice, 1}],
      fun(Bob, Alice) ->
              %% GIVEN
              escalus:wait_for_stanzas(Bob, 1),
              escalus:wait_for_stanzas(Alice, 1),
              BobJID = nick_to_jid(bob, Config),
              AliceJID = nick_to_jid(alice, Config),
              listen_to_presence_events_from_rabbit([BobJID, AliceJID], Config),
              %% WHEN users generate some traffic.
              send_presence_stanzas([Bob, Alice], 1),
              %% THEN  wait for presence events from Rabbit.
              ?assertReceivedMatch({#'basic.deliver'{
                                       routing_key = BobJID},
                                    #amqp_msg{}}, timer:seconds(5)),
              ?assertReceivedMatch({#'basic.deliver'{
                                       routing_key = AliceJID},
                                    #amqp_msg{}}, timer:seconds(5))
      end).

presence_messages_are_properly_formatted(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              %% GIVEN
              escalus:wait_for_stanzas(Bob, 1),
              BobJID = nick_to_jid(bob, Config),
              listen_to_presence_events_from_rabbit([BobJID], Config),
              %% WHEN user logout
              escalus:send(Bob, escalus_stanza:presence(<<"unavailable">>)),
              %% THEN receive message
              ?assertMatch(#{<<"user_id">> := BobJID, <<"present">> := false},
                           get_decoded_message_from_rabbit(BobJID))
      end).
%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

-spec listen_to_rabbit() -> proplists:proplist().
listen_to_rabbit() ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "localhost"}),
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

-spec listen_to_events_from_rabbit(QueueBindings :: [rabbit_binding()],
                                   Config :: proplists:proplist()) ->
    ok | term().
listen_to_events_from_rabbit(QueueBindings, Config) ->
    Connection = proplists:get_value(rabbit_connection, Config),
    Channel = proplists:get_value(rabbit_channel, Config),
    declare_rabbit_queue(Channel, ?QUEUE_NAME),
    wait_for_exchanges_to_be_created(Connection, [?PRESENCE_EXCHANGE]),
    bind_queues_to_exchanges(Channel, QueueBindings),
    subscribe_to_rabbit_queue(Channel, ?QUEUE_NAME).

-spec wait_for_exchanges_to_be_created(Connection :: pid(),
                                       Exchanges :: [binary()]) -> pid().
wait_for_exchanges_to_be_created(Connection, Exchanges) ->
    {ok, TestChannel} = amqp_connection:open_channel(Connection),
    [ensure_exchange_present(Connection, TestChannel, Exchange,
                         ?IF_EXCHANGE_EXISTS_RETRIES)
     || Exchange <- Exchanges],
    ok.

wait_for_exchanges_to_be_deleted(Connection, Exchanges) ->
    {ok, TestChannel} = amqp_connection:open_channel(Connection),
    [ensure_exchange_absent(Connection, TestChannel, Exchange,
                        ?IF_EXCHANGE_EXISTS_RETRIES)
     || Exchange <- Exchanges],
    ok.

-spec declare_rabbit_queue(Channel :: pid(), Queue :: binary()) -> binary().
declare_rabbit_queue(Channel, Queue) ->
    #'queue.declare_ok'{} =
        amqp_channel:call(Channel, #'queue.declare'{queue = Queue,
                                                    exclusive = true}).

-spec presence_bindings(Queue :: binary(), JIDs :: [binary()]) ->
    [rabbit_binding()].
presence_bindings(Queue, JIDs) ->
    [{Queue, ?PRESENCE_EXCHANGE, JID} || JID <- JIDs].

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

-spec ensure_exchange_present(Connection :: pid(), Channel :: pid(),
                          Exchange :: binary(),
                          Retries :: non_neg_integer()) ->
    true | no_return().
ensure_exchange_present(_Connection, Channel, Exchange, 1) ->
    amqp_channel:call(Channel, #'exchange.declare'{exchange = Exchange,
                                                   type = <<"topic">>,
                                                   passive = true}),
    true;
ensure_exchange_present(Connection, Channel, Exchange, Retries) ->
    try amqp_channel:call(Channel, #'exchange.declare'{exchange = Exchange,
                                                       type = <<"topic">>,
                                                       passive = true}) of
        {'exchange.declare_ok'} -> true
    catch
        _Error:_Reason ->
            timer:sleep(?WAIT_FOR_EXCHANGE_INTERVAL),
            %% The old channel `Channel` is closed.
            {ok, NewChannel} = amqp_connection:open_channel(Connection),
            ensure_exchange_present(Connection, NewChannel, Exchange, Retries - 1)
    end.

-spec ensure_exchange_absent(Connection :: pid(), Channel :: pid(),
                         Exchange :: binary(),
                         Retries :: non_neg_integer()) ->
    true | no_return().
ensure_exchange_absent(_Connection, Channel, Exchange, 1) ->
    try amqp_channel:call(Channel, #'exchange.declare'{exchange = Exchange,
                                                       type = <<"topic">>,
                                                       passive = true}) of
        {'exchange.declare_ok'} -> throw("Some exchange is present.")
    catch
        _Error:_Reason -> true
    end;
ensure_exchange_absent(Connection, Channel, Exchange, Retries) ->
    try amqp_channel:call(Channel, #'exchange.declare'{exchange = Exchange,
                                                       type = <<"topic">>,
                                                       passive = true}) of
        {'exchange.declare_ok'} ->
            timer:sleep(?WAIT_FOR_EXCHANGE_INTERVAL),
            ensure_exchange_absent(Connection, Channel, Exchange, Retries - 1)
    catch
        _Error:_Reason -> true
    end.

-spec subscribe_to_rabbit_queue(Channel :: pid(), Queue :: binary()) -> ok.
subscribe_to_rabbit_queue(Channel, Queue) ->
    amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue,
                                                     no_ack = true}, self()),
    receive
        #'basic.consume_ok'{} -> ok
    after
        5000 -> throw("Subscribe channel timeout.")
    end.

-spec send_presence_stanzas(Users :: [binary()], NumOfMsgs :: non_neg_integer())
                           -> [[ok]] | term().
send_presence_stanzas(Users, NumOfMsgs) ->
    [send_presence_stanza(User, NumOfMsgs) || User <- Users].

-spec send_presence_stanza(Users :: [binary()], NumOfMsgs :: non_neg_integer())
                          -> [ok] | term().
send_presence_stanza(User, NumOfMsgs) ->
    [escalus:send(User, escalus_stanza:presence(make_pres_stanza(X)))
     || X <- lists:seq(1, NumOfMsgs)].

-spec get_decoded_message_from_rabbit(RoutingKey :: binary()) ->
    map() | no_return().
get_decoded_message_from_rabbit(RoutingKey) ->
    receive
        {#'basic.deliver'{routing_key = RoutingKey}, #amqp_msg{payload = Msg}} ->
            jiffy:decode(Msg, [return_maps])
    after
        5000 -> ct:fail(timeout)
    end.

%%--------------------------------------------------------------------
%% Utils
%%--------------------------------------------------------------------

start_mod_event_pusher_rabbit() ->
    Host = ct:get_config({hosts, mim, domain}),
    rpc(mim(), gen_mod, start_module, [Host, mod_event_pusher_rabbit,
                                       ?MOD_EVENT_PUSHER_RABBIT_CFG]).

stop_mod_event_pusher_rabbit() ->
    Host = ct:get_config({hosts, mim, domain}),
    rpc(mim(), gen_mod, stop_module, [Host, mod_event_pusher_rabbit]).

make_pres_stanza(X) when X rem 2 == 0 ->
    <<"available">>;
make_pres_stanza(_) ->
    <<"unavailable">>.

%% @doc Get a binary jid of the user, that tagged with `UserName' in the config.
nick_to_jid(UserName, Config) when is_atom(UserName) ->
    UserSpec = escalus_users:get_userspec(Config, UserName),
    escalus_utils:jid_to_lower(escalus_users:get_jid(Config, UserSpec)).

nick(User) -> escalus_utils:get_username(User).
