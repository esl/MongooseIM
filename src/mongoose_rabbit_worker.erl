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
%% Generic RabbitMQ worker. The module is responsible for interacting with
%% RabbitMQ server. Worker holds it's own AMQP connection and channel in it's
%% state.
%% @end
%%==============================================================================

-module(mongoose_rabbit_worker).
-author('kacper.mentel@erlang-solutions.com').

-include_lib("mongooseim/include/mongoose.hrl").

-behaviour(gen_server).

-export([start_link/0, list_metrics/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-record(state, {connection :: pid(),
                channel :: pid(),
                host :: binary(),
                confirms :: boolean()}).

-type worker_opts() :: #state{}.

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
    {ok, #state{}}.

handle_call({amqp_call, Method}, _From, State) ->
    Res = handle_amqp_call(Method, State),
    {reply, Res, State}.

handle_cast({amqp_publish, Method, Payload, Opts}, State) ->
    NewState = State#state{host = proplists:get_value(host, Opts)},
    handle_amqp_publish(Method, Payload, NewState),
    {noreply, NewState}.

handle_info({init, Opts}, State) ->
    Host = proplists:get_value(host, Opts),
    {Connection, Channel} = establish_rabbit_connection(Opts, Host),
    IsConfirmEnabled = maybe_enable_confirms(Channel, Opts),
    {noreply, State#state{host = Host, connection = Connection,
                          channel = Channel, confirms = IsConfirmEnabled}}.

terminate(_Reason, #state{connection = Connection, channel = Channel,
                          host = Host}) ->
    close_rabbit_connection(Connection, Channel, Host),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec handle_amqp_call(Method :: mongoose_amqp:method(), worker_opts()) ->
    {ok, term()} | {error, term(), term()}.
handle_amqp_call(Method, #state{channel = Channel}) ->
    try amqp_channel:call(Channel, Method) of
        Result ->
            {ok, Result}
    catch
        Error:Reason ->
            {error, Error, Reason}
    end.

-spec handle_amqp_publish(Method :: mongoose_amqp:method(),
                          Payload :: mongoose_amqp:message(),
                          Opts :: worker_opts()) -> ok | no_return().
handle_amqp_publish(Method, Payload, Opts = #state{host = Host}) ->
    {PublishTime, Result} =
        timer:tc(fun publish_message_and_wait_for_confirm/3,
                 [Method, Payload, Opts]),
    case Result of
        true ->
            update_messages_published_metrics(Host, PublishTime, Payload),
            ?DEBUG("event=rabbit_message_sent message=~1000p", [Payload]);
        false ->
            update_messages_failed_metrics(Host),
            ?WARNING_MSG("event=rabbit_message_sent_failed reason=negative_ack",
                         []);
        timeout ->
            update_messages_timeout_metrics(Host),
            ?WARNING_MSG("event=rabbit_message_sent_failed reason=timeout",
                         [])
    end.

-spec publish_message_and_wait_for_confirm(Method :: mongoose_amqp:method(),
                                           Payload :: mongoose_amqp:message(),
                                           worker_opts()) ->
    true | no_return().
publish_message_and_wait_for_confirm(Method, Payload,
                                     #state{channel = Channel,
                                            confirms = IsConfirmEnabled}) ->
    amqp_channel:call(Channel, Method, Payload),
    case IsConfirmEnabled of
        true ->
            amqp_channel:wait_for_confirms(Channel);
        false ->
            true
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

-spec close_rabbit_connection(Connection :: pid(), Channel :: pid(),
                              Host :: jid:server()) ->
    ok | no_return().
close_rabbit_connection(Connection, Channel, Host) ->
    update_closed_connections_metrics(Host),
    try amqp_channel:close(Channel)
    catch
        _Error:_Reason -> already_closed
    end,
    amqp_connection:close(Connection).

-spec maybe_enable_confirms(Channel :: pid(), worker_opts()) ->
    boolean() | no_return().
maybe_enable_confirms(Channel, Opts) ->
    case proplists:get_value(confirms, Opts) of
        true ->
            ConfirmCallRes = mongoose_amqp:confirm_select_ok(),
            ConfirmCallRes =
                amqp_channel:call(Channel, mongoose_amqp:confirm_select()),
            true;
        false ->
            false
    end.

-spec update_messages_published_metrics(Host :: jid:server(),
                                        PublishTime :: non_neg_integer(),
                                        Message :: mongoose_amqp:message()) ->
    any().
update_messages_published_metrics(Host, PublishTime, Payload) ->
    mongoose_metrics:update(Host, ?MESSAGES_PUBLISHED_METRIC, 1),
    mongoose_metrics:update(Host, ?MESSAGE_PUBLISH_TIME_METRIC, PublishTime),
    mongoose_metrics:update(Host, ?MESSAGE_PAYLOAD_SIZE_METRIC,
                            byte_size(term_to_binary(Payload))).

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
