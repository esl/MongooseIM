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
-include_lib("amqp_client/include/amqp_client.hrl").

%%%===================================================================
%%% Types and definitions
%%%===================================================================

-define(DEFAULT_PRESENCE_EXCHANGE, <<"presence">>).
-define(DEFAULT_CHAT_MSG_EXCHANGE, <<"chat_msg">>).
-define(DEFAULT_CHAT_MSG_SENT_TOPIC, <<"chat_msg_sent">>).
-define(DEFAULT_CHAT_MSG_RECV_TOPIC, <<"chat_msg_recv">>).

-record(chat_event_data, {from :: jid:jid(),
                          to :: jid:jid(),
                          packet :: exml:element(),
                          topic :: binary(),
                          event :: chat_msg_event()}).

-type chat_msg_event() :: user_chat_msg_sent
                        | user_chat_msg_recv.
-type chat_event_data() :: #chat_event_data{}.

%%%===================================================================
%%% Exports
%%%===================================================================

%% MIM module callbacks
-export([start/2, stop/1]).

%% API
-export([push_event/3]).

%% Types
-export_type([chat_msg_event/0]).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-spec start(Host :: jid:server(), Opts :: proplists:proplist()) -> ok.
start(Host, _Opts) ->
    application:ensure_all_started(amqp_client),
    application:ensure_all_started(worker_pool),
    WorkerNum = opt(Host, pool_size, 100),
    wpool:start_sup_pool(pool_name(Host),
                         [{worker, {mongoose_rabbit_worker,
                                    [{amqp_client_opts, amqp_client_opts(Host)}]
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
push_event(Acc, _, #chat_event{type = chat, direction = in, from = From,
                               to = To, packet = Packet}) ->
    EventData = #chat_event_data{from = From, to = To, packet = Packet,
                                 event = user_chat_msg_sent},
    publish_user_chat_message_sent_event(EventData),
    Acc;
push_event(Acc, _, #chat_event{type = chat, direction = out, from = From,
                               to = To, packet = Packet}) ->
    EventData = #chat_event_data{from = From, to = To, packet = Packet,
                                 event = user_chat_recv_sent},
    publish_user_chat_message_received_event(EventData),
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
    UserJID = jid_to_lower_binary(JID),
    PresenceExchange = opt(Host, presence_exchange, ?DEFAULT_PRESENCE_EXCHANGE),
    wpool:cast(pool_name(Host), {user_presence_changed,
                                 #{user_jid => UserJID,
                                   status => Status,
                                   exchange => PresenceExchange}},
               available_worker).

-spec publish_user_chat_message_sent_event(EventData :: chat_event_data()) -> ok.
publish_user_chat_message_sent_event(EventData =
                                         #chat_event_data{
                                            from = #jid{lserver = Host}}) ->
    Topic = opt(Host, chat_msg_sent_topic, ?DEFAULT_CHAT_MSG_SENT_TOPIC),
    publish_user_chat_message_event(EventData#chat_event_data{topic = Topic}).

-spec publish_user_chat_message_received_event(EventData :: chat_event_data()) ->
    ok.
publish_user_chat_message_received_event(EventData =
                                             #chat_event_data{
                                                from = #jid{lserver = Host}}) ->
    Topic = opt(Host, chat_msg_recv_topic, ?DEFAULT_CHAT_MSG_RECV_TOPIC),
    publish_user_chat_message_event(EventData#chat_event_data{topic = Topic}).

-spec publish_user_chat_message_event(EventData :: chat_event_data()) -> ok.
publish_user_chat_message_event(#chat_event_data{from = From =
                                                     #jid{lserver = Host},
                                                 to = To,
                                                 packet = Packet,
                                                 topic = Topic,
                                                 event = Event}) ->
    FromJID = jid_to_lower_binary(From),
    ToJID = jid_to_lower_binary(To),
    Message = extract_message(Packet),
    ChatMsgExchange = opt(Host, chat_msg_exchange, ?DEFAULT_CHAT_MSG_EXCHANGE),
    wpool:cast(pool_name(Host), {Event, #{from_jid => FromJID,
                                          to_jid => ToJID,
                                          msg => Message,
                                          exchange => ChatMsgExchange,
                                          topic => Topic}},
               available_worker).

-spec jid_to_lower_binary(JID :: jid:jid()) -> binary().
jid_to_lower_binary(JID) ->
    {LowerUser, LowerHost, _} = jid:to_lower(JID),
    jid:to_binary({LowerUser, LowerHost}).

-spec extract_message(Packet :: exml:element()) -> binary().
extract_message(Packet) ->
    Body = exml_query:subelement(Packet, <<"body">>),
    exml_query:cdata(Body).

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
     opt(Host, chat_msg_exchange, ?DEFAULT_CHAT_MSG_EXCHANGE)
    ].

%% Getter for module options
-spec opt(Host :: jid:lserver(), Option :: atom()) -> Value :: term().
opt(Host, Option) ->
    opt(Host, Option, undefined).

%% Getter for module options with default value
-spec opt(Host :: jid:lserver(), Option :: atom(), Default :: term()) ->
     Value :: term().
opt(Host, Option, Default) ->
    gen_mod:get_module_opt(Host, ?MODULE, Option, Default).
