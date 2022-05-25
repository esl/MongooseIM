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
-include("mongoose_config_spec.hrl").

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).
-behaviour(mod_event_pusher).

%%%===================================================================
%%% Definitions
%%%===================================================================

-define(POOL_TAG, event_pusher).

-type exchange_opts() :: #{name := binary(), type := binary(),
                           sent_topic => binary(), recv_topic => binary()}.

%%%===================================================================
%%% Exports
%%%===================================================================

%% MIM module callbacks
-export([start/2, stop/1, config_spec/0]).

%% API
-export([push_event/2]).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, _Opts) ->
    initialize_metrics(HostType),
    create_exchanges(HostType),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{<<"presence_exchange">> => exchange_spec(<<"presence">>),
                       <<"chat_msg_exchange">> => msg_exchange_spec(<<"chat_msg">>),
                       <<"groupchat_msg_exchange">> => msg_exchange_spec(<<"groupchat_msg">>)}}.

msg_exchange_spec(Name) ->
    Spec = #section{items = Items, defaults = Defaults} = exchange_spec(Name),
    Spec#section{items = Items#{<<"sent_topic">> => #option{type = binary,
                                                            validate = non_empty},
                                <<"recv_topic">> => #option{type = binary,
                                                            validate = non_empty}},
                 defaults = Defaults#{<<"sent_topic">> => <<Name/binary, "_sent">>,
                                      <<"recv_topic">> => <<Name/binary, "_recv">>},
                 include = always
                }.

exchange_spec(Name) ->
    #section{items = #{<<"name">> => #option{type = binary,
                                             validate = non_empty},
                       <<"type">> => #option{type = binary,
                                             validate = non_empty}},
             defaults = #{<<"name">> => Name,
                          <<"type">> => <<"topic">>},
             include = always}.

push_event(Acc, #user_status_event{jid = UserJID, status = Status}) ->
    handle_user_presence_change(mongoose_acc:host_type(Acc), UserJID, Status),
    Acc;
push_event(Acc, ChatEvent = #chat_event{}) ->
    handle_user_chat_event(mongoose_acc:host_type(Acc), ChatEvent),
    Acc;
push_event(Acc, _) ->
    Acc.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec initialize_metrics(mongooseim:host_type()) -> [ok | {ok | error, term()}].
initialize_metrics(HostType) ->
    [mongoose_metrics:ensure_metric(HostType, Name, Type)
     || {Name, Type} <- mongoose_rabbit_worker:list_metrics(?POOL_TAG)].

-spec create_exchanges(mongooseim:host_type()) -> ok.
create_exchanges(HostType) ->
    Exchanges = exchanges(HostType),
    Res = [call_rabbit_worker(HostType, {amqp_call,
                                         mongoose_amqp:exchange_declare(ExName, Type)})
           || #{name := ExName, type := Type} <- Exchanges],
    verify_exchanges_were_created_or_crash(Res, Exchanges).

-spec handle_user_presence_change(mongooseim:host_type(), JID :: jid:jid(), Status :: atom()) -> ok.
handle_user_presence_change(HostType, JID, Status) ->
    #{name := ExchangeName} = exchange_opts(HostType, presence_exchange),
    RoutingKey = presence_routing_key(JID),
    Message = presence_msg(JID, Status),
    PublishMethod = mongoose_amqp:basic_publish(ExchangeName, RoutingKey),
    AMQPMessage = mongoose_amqp:message(Message),
    cast_rabbit_worker(HostType, {amqp_publish, PublishMethod, AMQPMessage}).

-spec handle_user_chat_event(mongooseim:host_type(), #chat_event{}) -> ok.
handle_user_chat_event(HostType, #chat_event{from = From, to = To, packet = Packet,
                                             type = Type, direction = Direction}) when
      Type == chat orelse Type == groupchat ->
    UserMessage = extract_message(Packet),
    Message = chat_msg(From, To, UserMessage),
    #{name := ExchangeName} = ExchangeOpts = exchange_opts(HostType, msg_type_to_key(Type)),
    RoutingKey = chat_event_routing_key(ExchangeOpts, Direction, From, To),
    PublishMethod = mongoose_amqp:basic_publish(ExchangeName, RoutingKey),
    AMQPMessage = mongoose_amqp:message(Message),
    cast_rabbit_worker(HostType, {amqp_publish, PublishMethod, AMQPMessage});
handle_user_chat_event(_HostType, _) -> ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

-spec call_rabbit_worker(mongooseim:host_type(), Msg :: term()) -> term().
call_rabbit_worker(HostType, Msg) ->
    mongoose_wpool:call(rabbit, HostType, ?POOL_TAG, Msg).

-spec cast_rabbit_worker(mongooseim:host_type(), Msg :: term()) -> ok.
cast_rabbit_worker(HostType, Msg) ->
    mongoose_wpool:cast(rabbit, HostType, ?POOL_TAG, Msg).

-spec exchanges(mongooseim:host_type()) -> [exchange_opts()].
exchanges(HostType) ->
    [exchange_opts(HostType, ExKey) || ExKey <- exchange_keys()].

exchange_keys() ->
    [presence_exchange, chat_msg_exchange, groupchat_msg_exchange].

-spec extract_message(Packet :: exml:element()) -> binary().
extract_message(Packet) ->
    Body = exml_query:subelement(Packet, <<"body">>),
    exml_query:cdata(Body).

-spec presence_routing_key(JID :: jid:jid()) -> jid:literal_jid().
presence_routing_key(JID) ->
    {LUser, LServer, _} = jid:to_lower(JID),
    jid:to_binary({LUser, LServer}).

-spec chat_event_routing_key(exchange_opts(), in | out, From :: jid:jid(),
                             To :: jid:jid()) -> binary().
chat_event_routing_key(#{sent_topic := Topic}, in, From, _To) ->
    user_topic_routing_key(From, Topic);
chat_event_routing_key(#{recv_topic := Topic}, out, _From, To) ->
    user_topic_routing_key(To, Topic).

msg_type_to_key(chat) -> chat_msg_exchange;
msg_type_to_key(groupchat) -> groupchat_msg_exchange.

-spec user_topic_routing_key(JID :: jid:jid(), Topic :: binary()) -> binary().
user_topic_routing_key(JID, Topic) ->
    {LUser, LServer, _Res} = jid:to_lower(JID),
    BinJID = jid:to_binary({LUser, LServer}),
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

-spec exchange_opts(mongooseim:host_type(), gen_mod:opt_key()) -> exchange_opts().
exchange_opts(HostType, ExchangeKey) ->
    gen_mod:get_module_opt(HostType, ?MODULE, ExchangeKey).

-spec verify_exchanges_were_created_or_crash(Res :: list(), [exchange_opts()])
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
