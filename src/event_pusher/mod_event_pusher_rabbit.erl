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

-include_lib("mongooseim/include/mod_event_pusher_events.hrl").
-include_lib("mongooseim/include/mongoose_config_spec.hrl").
-include_lib("mongooseim/include/mongoose.hrl").

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

%%%===================================================================
%%% Definitions
%%%===================================================================

-define(POOL_TAG, event_pusher).

-type exchange_key() :: presence_exchange | chat_msg_exchange | groupchat_msg_exchange.
-type exchange_opts() :: #{name := binary(), type := binary(),
                           sent_topic => binary(), recv_topic => binary(),
                           durable := boolean()}.
-type worker_call_result() :: {ok | error | exit | throw, term()}.

%%%===================================================================
%%% Exports
%%%===================================================================

%% MIM module callbacks
-export([start/2, stop/1, hooks/1, config_spec/0]).

%% hook handlers
-export([push_event/3]).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    create_exchanges(HostType, Opts),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{push_event, HostType, fun ?MODULE:push_event/3, #{}, 50}].

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
                                      <<"recv_topic">> => <<Name/binary, "_recv">>}}.

exchange_spec(Name) ->
    #section{items = #{<<"name">> => #option{type = binary,
                                             validate = non_empty},
                       <<"type">> => #option{type = binary,
                                             validate = non_empty},
                       <<"durable">> => #option{type = boolean}},
             defaults = #{<<"name">> => Name,
                          <<"type">> => <<"topic">>,
                          <<"durable">> => false}}.

-spec push_event(mod_event_pusher:push_event_acc(), mod_event_pusher:push_event_params(),
                 gen_hook:extra()) -> {ok, mod_event_pusher:push_event_acc()}.
push_event(HookAcc, #{event := Event}, #{host_type := HostType}) ->
    maybe
        {ok, ExchangeKey} ?= event_to_key(Event),
        {ok, ExchangeOpts} ?= exchange_opts(HostType, ExchangeKey),
        #{metadata := Metadata} = HookAcc,
        handle_event(HostType, Event, Metadata, ExchangeOpts)
    end,
    {ok, HookAcc}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec create_exchanges(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
create_exchanges(HostType, Opts) ->
    Exchanges = maps:with(exchange_keys(), Opts),
    Results = maps:map(fun(_Key, Value) -> create_exchange(HostType, Value) end, Exchanges),
    verify_exchanges_were_created_or_crash(Results).

-spec create_exchange(mongooseim:host_type(), exchange_opts()) -> worker_call_result().
create_exchange(HostType, #{name := ExName, type := Type, durable := Durable}) ->
    call_rabbit_worker(HostType, {amqp_call, mongoose_amqp:exchange_declare(ExName, Type, Durable)}).

-spec handle_event(mongooseim:host_type(), #user_status_event{} | #chat_event{},
                   mod_event_pusher:metadata(), exchange_opts()) -> ok.
handle_event(HostType, Event, Metadata, ExchangeOpts = #{name := ExchangeName}) ->
    case message(Event) of
        Message = #{} ->
            MessageJSON = iolist_to_binary(jiffy:encode(maps:merge(Message, Metadata))),
            RoutingKey = routing_key(Event, ExchangeOpts),
            PublishMethod = mongoose_amqp:basic_publish(ExchangeName, RoutingKey),
            AMQPMessage = mongoose_amqp:message(MessageJSON),
            cast_rabbit_worker(HostType, {amqp_publish, PublishMethod, AMQPMessage});
        skip ->
            ok
    end.

%%%===================================================================
%%% Helpers
%%%===================================================================

-spec call_rabbit_worker(mongooseim:host_type(), Msg :: term()) -> worker_call_result().
call_rabbit_worker(HostType, Msg) ->
    mongoose_wpool:call(rabbit, HostType, ?POOL_TAG, Msg).

-spec cast_rabbit_worker(mongooseim:host_type(), Msg :: term()) -> ok.
cast_rabbit_worker(HostType, Msg) ->
    try
        mongoose_wpool:cast(rabbit, HostType, ?POOL_TAG, Msg)
    catch
        exit:no_workers ->
            ?LOG_NOTICE(#{what => no_event_pusher_rabbit_worker_available,
                          text => <<"Dropping request because no rabbit worker is available">>,
                          host_type => HostType, dropped_request => Msg})
    end.

-spec exchange_keys() -> [exchange_key()].
exchange_keys() ->
    [presence_exchange, chat_msg_exchange, groupchat_msg_exchange].

-spec routing_key(#user_status_event{} | #chat_event{}, exchange_opts()) -> binary().
routing_key(#user_status_event{jid = JID}, _) ->
    jid:to_binary(jid:to_lus(JID));
routing_key(#chat_event{direction = in, from = From}, #{sent_topic := Topic}) ->
    user_topic_routing_key(From, Topic);
routing_key(#chat_event{direction = out, to = To}, #{recv_topic := Topic}) ->
    user_topic_routing_key(To, Topic).

-spec message(#user_status_event{} | #chat_event{}) -> skip | #{atom() => binary()}.
message(#user_status_event{jid = JID, status = Status}) ->
    #{user_id => jid:to_binary(jid:to_lower(JID)),
      present => is_user_online(Status)};
message(#chat_event{packet = Packet, from = From, to = To}) ->
    case exml_query:path(Packet, [{element, <<"body">>}, cdata]) of
        undefined ->
            skip; % skip (group)chat messages w/o body, e.g. displayed markers
        MsgBody ->
            #{from_user_id => jid:to_binary(jid:to_lower(From)),
              to_user_id => jid:to_binary(jid:to_lower(To)),
              message => MsgBody}
    end.

-spec event_to_key(mod_event_pusher:event()) -> {ok, exchange_key()} | skip.
event_to_key(#user_status_event{}) -> {ok, presence_exchange};
event_to_key(#chat_event{type = chat}) -> {ok, chat_msg_exchange};
event_to_key(#chat_event{type = groupchat}) -> {ok, groupchat_msg_exchange};
event_to_key(_) -> skip.

-spec user_topic_routing_key(JID :: jid:jid(), Topic :: binary()) -> binary().
user_topic_routing_key(JID, Topic) ->
    BinJID = jid:to_binary(jid:to_lus(JID)),
    <<BinJID/binary, ".", Topic/binary>>.

-spec is_user_online(online | offline) -> boolean().
is_user_online(online) -> true;
is_user_online(offline) -> false.

-spec exchange_opts(mongooseim:host_type(), gen_mod:opt_key()) ->
          {ok, exchange_opts()} | {error, not_found}.
exchange_opts(HostType, ExchangeKey) ->
    gen_mod:lookup_module_opt(HostType, ?MODULE, ExchangeKey).

-spec verify_exchanges_were_created_or_crash(#{exchange_key() => worker_call_result()}) -> ok.
verify_exchanges_were_created_or_crash(Res) ->
    OkResult = {ok, mongoose_amqp:exchange_declare_ok()},
    Failures = maps:filter(fun(_Key, Value) -> Value =/= OkResult end, Res),
    case map_size(Failures) of
        0 ->
            ok;
        _ ->
            error(#{what => creating_exchanges_failed, failures => Failures})
    end.
