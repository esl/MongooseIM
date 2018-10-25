%%%-------------------------------------------------------------------
%%% @author Kacper Mentel <kacper.mentel@erlang-solutions.com>
%%% @copyright (C) 2018, Kacper Mentel
%%% @doc
%%% Generic RabbitMQ worker. The module is responsible for formatting and
%%% sending events to AMQP client. Worker holds it's own AMQP connection and
%%% channel in it's state.
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_rabbit_worker).

-include_lib("mongooseim/include/mongoose.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-behaviour(gen_server).

%%%===================================================================
%%% Metrics
%%%===================================================================

-define(BACKEND, mod_event_pusher_rabbit).
-define(CONNECTIONS_ACTIVE_METRIC, [backends, ?BACKEND, connections_active]).
-define(CONNECTIONS_OPENED_METRIC, [backends, ?BACKEND, connections_opened]).
-define(CONNECTIONS_CLOSED_METRIC, [backends, ?BACKEND, connections_closed]).
-define(CONNECTIONS_FAILED_METRIC, [backends, ?BACKEND, connections_failed]).
-define(MESSAGES_PUBLISHED_METRIC, [backends, ?BACKEND, messages_published]).
-define(MESSAGES_FAILED_METRIC, [backends, ?BACKEND, messages_failed]).
-define(MESSAGES_TIMEOUT_METRIC, [backends, ?BACKEND, messages_timeout]).
-define(MESSAGE_PUBLISH_TIME_METRIC,
        [backends, ?BACKEND, message_publish_time]).
-define(MESSAGE_PAYLOAD_SIZE_METRIC,
        [backends, ?BACKEND, message_payload_size]).

%% API
-export([start_link/0, list_metrics/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-record(state, {connection :: pid(),
                channel :: pid(),
                host :: binary(),
                confirms_enabled = false :: boolean()}).

-type publish_opts() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the RabbitMQ worker.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

list_metrics() ->
    [{?CONNECTIONS_ACTIVE_METRIC, spiral},
     {?CONNECTIONS_OPENED_METRIC, spiral},
     {?CONNECTIONS_CLOSED_METRIC, spiral},
     {?CONNECTIONS_FAILED_METRIC, spiral},
     {?MESSAGES_PUBLISHED_METRIC, spiral},
     {?MESSAGES_FAILED_METRIC, spiral},
     {?MESSAGES_TIMEOUT_METRIC, spiral},
     {?MESSAGE_PUBLISH_TIME_METRIC, histogram},
     {?MESSAGE_PAYLOAD_SIZE_METRIC, histogram}].

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    process_flag(trap_exit, true),
    self() ! {init, Opts},
    {ok, #state{host = proplists:get_value(host, Opts)}}.

handle_call({create_exchanges, Exchanges}, _From,
            #state{channel = Channel} = State) ->
    [declare_exchange(Channel, ExName, ExType) || {ExName, ExType} <- Exchanges],
    {reply, ok, State};
handle_call({delete_exchanges, Exchanges}, _From,
            #state{channel = Channel} = State) ->
    [delete_exchange(Channel, ExName) || {ExName, _} <- Exchanges],
    {reply, ok, State}.

handle_cast({user_presence_changed, EventData}, State) ->
    publish_status(EventData, State),
    {noreply, State};
handle_cast({user_chat_msg_sent, EventData}, State) ->
    publish_chat_msg_sent(EventData, State),
    {noreply, State};
handle_cast({user_chat_msg_recv, EventData}, State) ->
    publish_chat_msg_received(EventData, State),
    {noreply, State};
handle_cast({user_groupchat_msg_sent, EventData}, State) ->
    publish_groupchat_msg_sent(EventData, State),
    {noreply, State};
handle_cast({user_groupchat_msg_recv, EventData}, State) ->
    publish_groupchat_msg_recv(EventData, State),
    {noreply, State}.

handle_info({init, Opts}, State = #state{host = Host}) ->
    {Connection, Channel} = establish_rabbit_connection(Opts, Host),
    IsConfirmOn = maybe_enable_confirms(Channel, Opts),
    {noreply, State#state{connection = Connection, channel = Channel,
                          confirms_enabled = IsConfirmOn}}.

terminate(_Reason, #state{connection = Connection, channel = Channel,
                          host = Host}) ->
    close_rabbit_connection(Connection, Channel, Host),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec declare_exchange(Channel :: pid(), Name :: binary(), Type :: binary()) ->
  term().
declare_exchange(Channel, Name, Type) ->
    amqp_channel:call(Channel, #'exchange.declare'{exchange = Name,
                                                   type = Type}).

-spec delete_exchange(Channel :: pid(), Exchange :: binary()) -> term().
delete_exchange(Channel, Exchange) ->
    amqp_channel:call(Channel, #'exchange.delete'{exchange = Exchange}).

-spec publish_status(map(), Opts :: publish_opts()) -> term().
publish_status(#{user_jid := {User, Host, _} = JID, exchange := Exchange,
                 status := Status}, Opts) ->
    RoutingKey = jid:to_binary({User, Host}),
    Message = make_presence_msg(JID, Status),
    publish(Exchange, RoutingKey, Message, Opts#state{host = Host}).

-spec publish_chat_msg_sent(EventData :: map(), Opts :: publish_opts()) -> ok.
publish_chat_msg_sent(EventData = #{from_jid := From, topic := Topic}, Opts) ->
    RoutingKey = user_topic_routing_key(From, Topic),
    publish_chat_msg_event(RoutingKey, EventData, Opts).

-spec publish_chat_msg_received(EventData :: map(), Opts :: publish_opts()) -> ok.
publish_chat_msg_received(EventData = #{to_jid := To, topic := Topic}, Opts) ->
    RoutingKey = user_topic_routing_key(To, Topic),
    publish_chat_msg_event(RoutingKey, EventData, Opts).

-spec publish_groupchat_msg_sent(EventData :: map(), Opts :: publish_opts()) -> ok.
publish_groupchat_msg_sent(EventData = #{from_jid := From, topic := Topic},
                           Opts) ->
    RoutingKey = user_topic_routing_key(From, Topic),
    publish_chat_msg_event(RoutingKey, EventData, Opts).

-spec publish_groupchat_msg_recv(EventData :: map(), Opts :: publish_opts()) -> ok.
publish_groupchat_msg_recv(EventData = #{to_jid := To, topic := Topic}, Opts) ->
    RoutingKey = user_topic_routing_key(To, Topic),
    publish_chat_msg_event(RoutingKey, EventData, Opts).

-spec publish_chat_msg_event(RoutingKey :: binary(), map(), Opts :: publish_opts()) ->
    term().
publish_chat_msg_event(RoutingKey, #{from_jid := From,
                                     to_jid := To,
                                     msg := UserMsg,
                                     exchange := Exchange,
                                     host := Host}, Opts) ->
    Message = make_chat_msg(From, To, UserMsg),
    publish(Exchange, RoutingKey, Message, Opts#state{host = Host}).

-spec publish(Exchange :: binary(), RoutingKey :: binary(), Message :: binary(),
              Opts :: publish_opts()) -> term().
publish(Exchange, RoutingKey, Message, Opts = #state{host = Host}) ->
    {PublishTime, Result} =
        timer:tc(fun publish_message_and_wait_for_confirm/4,
                 [Exchange, RoutingKey, Message, Opts]),
    case Result of
        true ->
            update_messages_published_metrics(Host, PublishTime, Message),
            ?DEBUG("event=rabbit_message_sent message=~1000p", [Message]);
        false ->
            update_messages_failed_metrics(Host),
            ?WARNING_MSG("event=rabbit_message_sent_failed reason=negative_ack",
                         []);
        timeout ->
            update_messages_timeout_metrics(Host),
            ?WARNING_MSG("event=rabbit_message_sent_failed reason=timeout",
                         [])
    end.

-spec user_topic_routing_key(JID :: {binary(), binary(), binary()},
                             Topic :: binary()) -> binary().
user_topic_routing_key({User, Host, _Res}, Topic) ->
    JID = jid:to_binary({User, Host}),
    <<JID/binary, ".", Topic/binary>>.

-spec make_presence_msg(JID :: binary(), Status :: atom()) -> binary().
make_presence_msg(JID, Status) ->
    Msg = #{user_id => jid:to_binary(JID), present => is_user_online(Status)},
    jiffy:encode(Msg).

-spec make_chat_msg(From :: binary(), To :: atom(), UserMsg :: binary()) ->
    binary().
make_chat_msg(From, To, UserMsg) ->
    Msg = #{to_user_id => jid:to_binary(To),
            message => UserMsg,
            from_user_id => jid:to_binary(From)},
    jiffy:encode(Msg).

-spec is_user_online(online | offline) -> boolean().
is_user_online(online) -> true;
is_user_online(offline) -> false.

-spec publish_message_and_wait_for_confirm(Exchange :: binary(),
                                           RoutingKey :: binary(),
                                           Message :: binary(),
                                           Opts :: publish_opts()) -> term().
publish_message_and_wait_for_confirm(Exchange, RoutingKey, Message,
                                     #state{channel = Channel,
                                            confirms_enabled = IsConfirmOn}) ->
    amqp_channel:call(Channel, #'basic.publish'{exchange = Exchange,
                                                routing_key = RoutingKey},
                      #amqp_msg{payload = Message}),
    case IsConfirmOn of
        true ->
            amqp_channel:wait_for_confirms(Channel);
        false ->
            ok
    end.

-spec establish_rabbit_connection(Opts :: proplists:proplist(),
                                  Host :: jid:server()) -> ok.
establish_rabbit_connection(Opts, Host) ->
    AMQPOpts = proplists:get_value(amqp_client_opts, Opts),
    case amqp_connection:start(AMQPOpts) of
        {ok, Connection} ->
            update_success_connections_metrics(Host),
            {ok, Channel} = amqp_connection:open_channel(Connection),
            ?DEBUG("event=rabbit_connection_established", []),
            {Connection, Channel};
        {error, Error} ->
            update_failed_connections_metrics(Host),
            ?ERROR_MSG("event=rabbit_connection_failed reason=~1000p", [Error]),
            exit("connection to a Rabbit server failed")
    end.

-spec maybe_enable_confirms(Channel :: pid(), Opts :: proplists:proplist()) -> ok.
maybe_enable_confirms(Channel, Opts) ->
    case proplists:get_value(confirms_enabled, Opts) of
        true ->
            #'confirm.select_ok'{} =
                amqp_channel:call(Channel, #'confirm.select'{}),
            true;
        false ->
            false
    end.

-spec close_rabbit_connection(Connection :: pid(), Channel :: pid(),
                              Host :: jid:server()) ->
    ok | term().
close_rabbit_connection(Connection, Channel, Host) ->
    update_closed_connections_metrics(Host),
    try amqp_channel:close(Channel)
    catch
        _Error:_Reason -> already_closed
    end,
    amqp_connection:close(Connection).

-spec update_messages_published_metrics(Host :: jid:server(),
                                        PublishTime :: non_neg_integer(),
                                        Message :: binary()) -> any().
update_messages_published_metrics(Host, PublishTime, Message) ->
    mongoose_metrics:update(Host, ?MESSAGES_PUBLISHED_METRIC, 1),
    mongoose_metrics:update(Host, ?MESSAGE_PUBLISH_TIME_METRIC, PublishTime),
    mongoose_metrics:update(Host, ?MESSAGE_PAYLOAD_SIZE_METRIC,
                            byte_size(Message)).

-spec update_messages_failed_metrics(Host :: jid:server()) -> any().
update_messages_failed_metrics(Host) ->
    mongoose_metrics:update(Host, ?MESSAGES_FAILED_METRIC, 1).

-spec update_messages_timeout_metrics(Host :: jid:server()) -> any().
update_messages_timeout_metrics(Host) ->
    mongoose_metrics:update(Host, ?MESSAGES_TIMEOUT_METRIC, 1).

-spec update_success_connections_metrics(Host :: jid:server()) -> any().
update_success_connections_metrics(Host) ->
    mongoose_metrics:update(Host, ?CONNECTIONS_ACTIVE_METRIC, 1),
    mongoose_metrics:update(Host, ?CONNECTIONS_OPENED_METRIC, 1).

-spec update_failed_connections_metrics(Host :: jid:server()) -> any().
update_failed_connections_metrics(Host) ->
    mongoose_metrics:update(Host, ?CONNECTIONS_FAILED_METRIC, 1).

-spec update_closed_connections_metrics(Host :: jid:server()) -> any().
update_closed_connections_metrics(Host) ->
    mongoose_metrics:update(Host, ?CONNECTIONS_ACTIVE_METRIC, -1),
    mongoose_metrics:update(Host, ?CONNECTIONS_CLOSED_METRIC, 1).
