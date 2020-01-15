%%==============================================================================
%% Copyright 2018 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc
%% Backend for `mod_event_pusher' that supports RabbitMQ integration.
%% @end
%%==============================================================================

-module(mod_event_pusher_rabbit).
-author('kacper.mentel@erlang-solutions.com').

-include_lib("mongooseim/include/mongoose.hrl").
-include_lib("mongooseim/include/mod_event_pusher_events.hrl").

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).
-behaviour(mod_event_pusher).

%%%===================================================================
%%% Definitions
%%%===================================================================

-define(POOL_TAG, event_pusher).

-define(DEFAULT_PRESENCE_EXCHANGE, <<"presence">>).
-define(DEFAULT_PRESENCE_EXCHANGE_TYPE, <<"topic">>).
-define(DEFAULT_CHAT_MSG_EXCHANGE, <<"chat_msg">>).
-define(DEFAULT_CHAT_MSG_EXCHANGE_TYPE, <<"topic">>).
-define(DEFAULT_CHAT_MSG_SENT_TOPIC, <<"chat_msg_sent">>).
-define(DEFAULT_CHAT_MSG_RECV_TOPIC, <<"chat_msg_recv">>).
-define(DEFAULT_GROUP_CHAT_MSG_EXCHANGE, <<"groupchat_msg">>).
-define(DEFAULT_GROUP_CHAT_MSG_EXCHANGE_TYPE, <<"topic">>).
-define(DEFAULT_GROUP_CHAT_MSG_SENT_TOPIC, <<"groupchat_msg_sent">>).
-define(DEFAULT_GROUP_CHAT_MSG_RECV_TOPIC, <<"groupchat_msg_recv">>).

%%%===================================================================
%%% Exports
%%%===================================================================

%% MIM module callbacks
-export([start/2, stop/1]).

%% API
-export([push_event/3]).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-spec start(Host :: jid:server(), Opts :: proplists:proplist()) -> ok.
start(Host, _Opts) ->
    initialize_metrics(Host),
    create_exchanges(Host),
    ok.

-spec stop(Host :: jid:server()) -> ok.
stop(_Host) ->
    ok.

push_event(Acc, _, #user_status_event{jid = UserJID, status = Status}) ->
    handle_user_presence_change(UserJID, Status),
    Acc;
push_event(Acc, _, ChatEvent = #chat_event{}) ->
    handle_user_chat_event(ChatEvent),
    Acc;
push_event(Acc, _, _) ->
    Acc.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec initialize_metrics(Host :: jid:server()) -> [ok | {ok | error, term()}].
initialize_metrics(Host) ->
    [mongoose_metrics:ensure_metric(Host, Name, Type)
     || {Name, Type} <- mongoose_rabbit_worker:list_metrics(?POOL_TAG)].

-spec create_exchanges(Host :: jid:server()) -> ok.
create_exchanges(Host) ->
    Exchanges = exchanges(Host),
    Res =
        [call_rabbit_worker(Host, {amqp_call,
                                   mongoose_amqp:exchange_declare(ExName, Type)})
         || {ExName, Type} <- Exchanges],
    verify_exchanges_were_created_or_crash(Res, Exchanges).

-spec handle_user_presence_change(JID :: jid:jid(), Status :: atom()) -> ok.
handle_user_presence_change(JID = #jid{lserver = Host}, Status) ->
    Exchange = exchange_opt(Host, presence_exchange, name,
                            ?DEFAULT_PRESENCE_EXCHANGE),
    RoutingKey = presence_routing_key(JID),
    Message = presence_msg(JID, Status),
    PublishMethod = mongoose_amqp:basic_publish(Exchange, RoutingKey),
    AMQPMessage = mongoose_amqp:message(Message),
    cast_rabbit_worker(Host, {amqp_publish, PublishMethod, AMQPMessage}).

-spec handle_user_chat_event(#chat_event{}) -> ok.
handle_user_chat_event(#chat_event{from = From, to = To, packet = Packet,
                                   type = Type, direction = Direction}) when
      Type == chat orelse Type == groupchat ->
    Host = get_host(From, To, Direction),
    UserMessage = extract_message(Packet),
    Message = chat_msg(From, To, UserMessage),
    Exchange = get_chat_exchange(Type, Host),
    RoutingKey = chat_event_routing_key(Type, Direction, From, To, Host),
    PublishMethod = mongoose_amqp:basic_publish(Exchange, RoutingKey),
    AMQPMessage = mongoose_amqp:message(Message),
    cast_rabbit_worker(Host, {amqp_publish, PublishMethod, AMQPMessage});
handle_user_chat_event(_) -> ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

-spec call_rabbit_worker(Host :: binary(), Msg :: term()) -> term().
call_rabbit_worker(Host, Msg) ->
    mongoose_wpool:call(rabbit, Host, ?POOL_TAG, Msg).

-spec cast_rabbit_worker(Host :: binary(), Msg :: term()) -> ok.
cast_rabbit_worker(Host, Msg) ->
    mongoose_wpool:cast(rabbit, Host, ?POOL_TAG, Msg).

-spec exchanges(Host :: jid:server()) -> [{binary(), binary()}].
exchanges(Host) ->
    [{
      exchange_opt(Host, ExKey, name, DefName),
      exchange_opt(Host, ExKey, type, DefType)
     } || {ExKey, DefName, DefType} <-
              [
               {presence_exchange, ?DEFAULT_PRESENCE_EXCHANGE,
                ?DEFAULT_PRESENCE_EXCHANGE_TYPE},
               {chat_msg_exchange, ?DEFAULT_CHAT_MSG_EXCHANGE,
                ?DEFAULT_CHAT_MSG_EXCHANGE_TYPE},
               {groupchat_msg_exchange, ?DEFAULT_GROUP_CHAT_MSG_EXCHANGE,
                ?DEFAULT_GROUP_CHAT_MSG_EXCHANGE_TYPE}
              ]].

-spec get_chat_exchange(chat | groupchat, Host :: binary()) -> binary().
get_chat_exchange(chat, Host) ->
    exchange_opt(Host, chat_msg_exchange, name, ?DEFAULT_CHAT_MSG_EXCHANGE);
get_chat_exchange(groupchat, Host) ->
    exchange_opt(Host, groupchat_msg_exchange, name,
                 ?DEFAULT_GROUP_CHAT_MSG_EXCHANGE).

-spec get_host(jid:jid(), jid:jid(), in | out) -> binary().
get_host(#jid{lserver = Host}, _To, in) -> Host;
get_host(_From, #jid{lserver = Host}, out) -> Host.

-spec extract_message(Packet :: exml:element()) -> binary().
extract_message(Packet) ->
    Body = exml_query:subelement(Packet, <<"body">>),
    exml_query:cdata(Body).

-spec presence_routing_key(JID :: jid:jid()) -> binary().
presence_routing_key(JID) ->
    {User, Host, _} = jid:to_lower(JID),
    jid:to_binary({User, Host}).

-spec chat_event_routing_key(chat | groupchat, in | out, From :: jid:jid(),
                             To :: jid:jid(), Host :: binary()) -> binary().
chat_event_routing_key(chat, in, From, _To, Host) ->
    Topic = exchange_opt(Host, chat_msg_exchange, sent_topic,
                         ?DEFAULT_CHAT_MSG_SENT_TOPIC),
    user_topic_routing_key(From, Topic);
chat_event_routing_key(chat, out, _From, To, Host) ->
    Topic = exchange_opt(Host, chat_msg_exchange, recv_topic,
                         ?DEFAULT_CHAT_MSG_RECV_TOPIC),
    user_topic_routing_key(To, Topic);
chat_event_routing_key(groupchat, in, From, _To, Host) ->
    Topic = exchange_opt(Host, groupchat_msg_exchange, sent_topic,
                         ?DEFAULT_GROUP_CHAT_MSG_SENT_TOPIC),
    user_topic_routing_key(From, Topic);
chat_event_routing_key(groupchat, out, _From, To, Host) ->
    Topic = exchange_opt(Host, groupchat_msg_exchange, recv_topic,
                         ?DEFAULT_GROUP_CHAT_MSG_RECV_TOPIC),
    user_topic_routing_key(To, Topic).

-spec user_topic_routing_key(JID :: jid:jid(), Topic :: binary()) -> binary().
user_topic_routing_key(JID, Topic) ->
    {User, Host, _Res} = jid:to_lower(JID),
    BinJID = jid:to_binary({User, Host}),
    <<BinJID/binary, ".", Topic/binary>>.

-spec presence_msg(JID :: jid:jid(), Status :: atom()) -> binary().
presence_msg(JID, Status) ->
    Msg = #{user_id => jid:to_binary(jid:to_lower(JID)), present => is_user_online(Status)},
    jiffy:encode(Msg).

-spec chat_msg(From :: jid:jid(), To :: jid:jid(), UserMsg :: binary()) ->
    binary().
chat_msg(From, To, UserMsg) ->
    Msg = #{to_user_id => jid:to_binary(jid:to_lower(To)),
            message => UserMsg,
            from_user_id => jid:to_binary(jid:to_lower(From))},
    jiffy:encode(Msg).

-spec is_user_online(online | offline) -> boolean().
is_user_online(online) -> true;
is_user_online(offline) -> false.

-spec exchange_opt(Host :: jid:lserver(), ExchangeKey :: atom(),
                   Option :: atom(), Default :: term()) -> term().
exchange_opt(Host, ExchangeKey, Option, Default) ->
    ExchangeOptions = opt(Host, ExchangeKey, []),
    proplists:get_value(Option, ExchangeOptions, Default).

-spec opt(Host :: jid:lserver(), Option :: atom(), Default :: term()) ->
     Value :: term().
opt(Host, Option, Default) ->
    gen_mod:get_module_opt(Host, ?MODULE, Option, Default).

-spec verify_exchanges_were_created_or_crash(Res :: list(),
                                             Exchanges :: [{binary(), binary()}])
    -> ok | no_return().
verify_exchanges_were_created_or_crash(Res, Exchanges) ->
    case lists:all(fun(E) ->
                           element(2, E) == mongoose_amqp:exchange_declare_ok()
                   end, Res) of
        true ->
            ok;
        false ->
            erlang:error(io_lib:format("Creating exchanges failed, exchanges=~p",
                                       [Exchanges]))
    end.
