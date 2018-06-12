%%%-------------------------------------------------------------------
%%% @author Kacper Mentel <kacper.mentel@erlang-solutions.com>
%%% @copyright (C) 2018, Kacper Mentel
%%% @doc
%%% Backend for `mod_event_pusher` that supports RabbitMQ integeration.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_event_pusher_rabbit).

-behaviour(gen_mod).
-behaviour(mod_event_pusher).

-include_lib("mongooseim/include/mod_event_pusher_events.hrl").
-include_lib("mongooseim/include/mongoose.hrl").
-include_lib("mongooseim/include/jlib.hrl").
-include_lib("mongooseim/include/mod_event_pusher_rabbit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%%%===================================================================
%%% Types and definitions
%%%===================================================================

-define(DEFAULT_PRESENCE_EXCHANGE, <<"presence">>).
-define(DEFAULT_CHAT_MSG_EXCHANGE, <<"chat_msg">>).
-define(DEFAULT_CHAT_MSG_SENT_TOPIC, <<"chat_msg_sent">>).
-define(DEFAULT_CHAT_MSG_RECV_TOPIC, <<"chat_msg_recv">>).
-define(DEFAULT_GROUP_CHAT_MSG_EXCHANGE, <<"groupchat_msg">>).
-define(DEFAULT_GROUP_CHAT_MSG_SENT_TOPIC, <<"groupchat_msg_sent">>).
-define(DEFAULT_GROUP_CHAT_MSG_RECV_TOPIC, <<"groupchat_msg_recv">>).

-type chat_event() :: user_chat_msg_sent
                    | user_chat_msg_recv
                    | user_groupchat_msg_sent
                    | user_groupchat_msg_recv.

%%%===================================================================
%%% Exports
%%%===================================================================

%% MIM module callbacks
-export([start/2, stop/1]).

%% API
-export([push_event/3]).

%% Types
-export_type([chat_event/0]).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-spec start(Host :: jid:server(), Opts :: proplists:proplist()) -> ok.
start(Host, _Opts) ->
    application:ensure_all_started(amqp_client),
    application:ensure_all_started(worker_pool),
    initialize_metrics(Host),
    mongoose_metrics:init_subscriptions(),
    WorkerNum = opt(Host, pool_size, 100),
    wpool:start_sup_pool(pool_name(Host),
                         [{worker, {mongoose_rabbit_worker,
                                    [{amqp_client_opts, amqp_client_opts(Host)},
                                     {host, Host}]
                                   }},
                          {workers, WorkerNum}]),
    create_exchanges(Host),
    ok.

-spec stop(Host :: jid:server()) -> ok.
stop(Host) ->
    delete_exchanges(Host),
    wpool:stop_sup_pool(pool_name(Host)),
    ok.

push_event(Acc, _, #user_status_event{jid = UserJID, status = Status}) ->
    publish_user_presence_change(UserJID, Status),
    Acc;
push_event(Acc, _, ChatEvent = #chat_event{}) ->
    publish_user_chat_event(ChatEvent),
    Acc;
push_event(Acc, _, _) ->
    Acc.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec create_exchanges(Host :: jid:server()) -> ok.
create_exchanges(Host) ->
    wpool:call(pool_name(Host), {create_exchanges, exchanges(Host)},
               available_worker).

-spec delete_exchanges(Host :: jid:server()) -> ok.
delete_exchanges(Host) ->
    wpool:call(pool_name(Host), {delete_exchanges, exchanges(Host)},
               available_worker).

-spec publish_user_presence_change(JID :: jid:jid(), Status :: atom()) -> ok.
publish_user_presence_change(JID = #jid{lserver = Host}, Status) ->
    UserJID = jid:to_lower(JID),
    PresenceExchange = opt(Host, presence_exchange, ?DEFAULT_PRESENCE_EXCHANGE),
    wpool:cast(pool_name(Host), {user_presence_changed,
                                 #{user_jid => UserJID,
                                   status => Status,
                                   exchange => PresenceExchange}},
               available_worker).

-spec publish_user_chat_event(#chat_event{}) -> ok.
publish_user_chat_event(#chat_event{from = From,
                                    to = To,
                                    packet = Packet,
                                    type = Type,
                                    direction = Direction}) ->
    Host = get_host(From, To, Direction),
    FromJID = jid:to_lower(From),
    ToJID = jid:to_lower(To),
    Message = extract_message(Packet),
    Exchange = get_chat_exchange(Type, Host),
    Event = get_chat_event(Type, Direction),
    Topic = get_chat_topic(Event, Host),
    wpool:cast(pool_name(Host), {Event, #{from_jid => FromJID,
                                          to_jid => ToJID,
                                          msg => Message,
                                          exchange => Exchange,
                                          topic => Topic,
                                          host => Host}},
               available_worker).

-spec get_host(jid:jid(), jid:jid(), in | out) -> binary().
get_host(#jid{lserver = Host}, _To, in) -> Host;
get_host(_From, #jid{lserver = Host}, out) -> Host.

-spec extract_message(Packet :: exml:element()) -> binary().
extract_message(Packet) ->
    Body = exml_query:subelement(Packet, <<"body">>),
    exml_query:cdata(Body).

-spec get_chat_exchange(chat | groupchat, Host :: binary()) -> binary().
get_chat_exchange(chat, Host) ->
    opt(Host, chat_msg_exchange, ?DEFAULT_CHAT_MSG_EXCHANGE);
get_chat_exchange(groupchat, Host) ->
    opt(Host, groupchat_msg_exchange, ?DEFAULT_GROUP_CHAT_MSG_EXCHANGE).

-spec get_chat_event(chat | groupchat, in | out) -> chat_event().
get_chat_event(chat, in) -> user_chat_msg_sent;
get_chat_event(chat, out) -> user_chat_msg_recv;
get_chat_event(groupchat, in) -> user_groupchat_msg_sent;
get_chat_event(groupchat, out) -> user_groupchat_msg_recv.

-spec get_chat_topic(event(), Host :: binary()) -> binary().
get_chat_topic(user_chat_msg_sent, Host) ->
    opt(Host, chat_msg_sent_topic, ?DEFAULT_CHAT_MSG_SENT_TOPIC);
get_chat_topic(user_chat_msg_recv, Host) ->
    opt(Host, chat_msg_recv_topic, ?DEFAULT_CHAT_MSG_RECV_TOPIC);
get_chat_topic(user_groupchat_msg_sent, Host) ->
    opt(Host, groupchat_msg_sent_topic, ?DEFAULT_GROUP_CHAT_MSG_SENT_TOPIC);
get_chat_topic(user_groupchat_msg_recv, Host) ->
    opt(Host, groupchat_msg_recv_topic, ?DEFAULT_GROUP_CHAT_MSG_RECV_TOPIC).


-spec pool_name(Host :: jid:lserver()) -> atom().
pool_name(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).

-spec amqp_client_opts(Host :: jid:lserver()) ->
    amqp_client:amqp_params_network().
amqp_client_opts(Host) ->
    DefaultParams = #amqp_params_network{},
    #amqp_params_network{
       host = opt(Host, amqp_host, DefaultParams#amqp_params_network.host),
       port = opt(Host, amqp_port, DefaultParams#amqp_params_network.port),
       username = opt(Host, amqp_username,
                      DefaultParams#amqp_params_network.username),
       password = opt(Host, amqp_password,
                      DefaultParams#amqp_params_network.password)}.

-spec exchanges(Host :: jid:server()) -> [binary()].
exchanges(Host) ->
    [
     opt(Host, presence_exchange, ?DEFAULT_PRESENCE_EXCHANGE),
     opt(Host, chat_msg_exchange, ?DEFAULT_CHAT_MSG_EXCHANGE),
     opt(Host, groupchat_msg_exchange, ?DEFAULT_GROUP_CHAT_MSG_EXCHANGE)
    ].

-spec initialize_metrics(Host :: jid:server()) -> ok.
initialize_metrics(Host) ->
    [mongoose_metrics:ensure_metric(Host, Name, Type)
     || {Name, Type} <- [{?RABBIT_CONNECTIONS_METRIC, spiral},
                         {?MESSAGES_PUBLISHED_METRIC, spiral},
                         {?MESSAGES_FAILED_METRIC, spiral},
                         {?MESSAGES_TIMEOUT_METRIC, spiral},
                         {?MESSAGE_PUBLISH_TIME_METRIC, histogram},
                         {?MESSAGE_PAYLOAD_SIZE_METRIC, histogram}
                        ]].

%% Getter for module options
-spec opt(Host :: jid:lserver(), Option :: atom()) -> Value :: term().
opt(Host, Option) ->
    opt(Host, Option, undefined).

%% Getter for module options with default value
-spec opt(Host :: jid:lserver(), Option :: atom(), Default :: term()) ->
     Value :: term().
opt(Host, Option, Default) ->
    gen_mod:get_module_opt(Host, ?MODULE, Option, Default).
