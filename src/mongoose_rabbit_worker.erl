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
%% RabbitMQ server. Worker holds its own AMQP connection and channel in its
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

-record(state, {amqp_client_opts :: mongoose_amqp:network_params() | undefined,
                connection :: pid() | undefined,
                channel :: pid() | undefined,
                host :: binary() | undefined,
                confirms :: boolean() | undefined,
                max_queue_len :: non_neg_integer() | infinity | undefined}).

-type worker_opts() :: #state{}.

-define(REPLY_REQ_DROPPED(State), {reply, request_dropped, State}).
-define(NOREPLY_REQ_DROPPED(State), {noreply, State}).

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

handle_call(Req, From, State) ->
    maybe_handle_request(fun do_handle_call/3, [Req, From, State],
                         ?REPLY_REQ_DROPPED(State)).

handle_cast(Req, State) ->
   maybe_handle_request(fun do_handle_cast/2, [Req, State],
                        ?NOREPLY_REQ_DROPPED(State)).

handle_info(Req = {init, _}, State) ->
    do_handle_info(Req, State);
handle_info(Req, State) ->
    maybe_handle_request(fun do_handle_info/2, [Req, State],
                         ?NOREPLY_REQ_DROPPED(State)).

terminate(_Reason, #state{connection = Connection, channel = Channel,
                          host = Host}) ->
    close_rabbit_connection(Connection, Channel, Host),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_handle_call({amqp_call, Method}, _From, State = #state{channel = Channel}) ->
    try amqp_channel:call(Channel, Method) of
        Res ->
            {reply, {ok, Res}, State}
    catch
        Error:Reason ->
            {FreshConn, FreshChann} = maybe_restart_rabbit_connection(State),
            {reply, {Error, Reason}, State#state{connection = FreshConn,
                                                 channel = FreshChann}}
    end.

do_handle_cast({amqp_publish, Method, Payload}, State) ->
    handle_amqp_publish(Method, Payload, State).

do_handle_info({init, Opts}, State) ->
    Host = proplists:get_value(host, Opts),
    AMQPClientOpts = proplists:get_value(amqp_client_opts, Opts),
    {Connection, Channel} = establish_rabbit_connection(AMQPClientOpts, Host),
    IsConfirmEnabled = maybe_enable_confirms(Channel, Opts),
    MaxMsgQueueLen = proplists:get_value(max_queue_len, Opts),
    {noreply, State#state{host = Host, amqp_client_opts = AMQPClientOpts,
                          connection = Connection, channel = Channel,
                          confirms = IsConfirmEnabled,
                          max_queue_len = MaxMsgQueueLen}}.

-spec handle_amqp_publish(Method :: mongoose_amqp:method(),
                          Payload :: mongoose_amqp:message(),
                          Opts :: worker_opts()) -> {noreply, worker_opts()}.
handle_amqp_publish(Method, Payload, Opts = #state{host = Host}) ->
    {PublishTime, Result} =
        timer:tc(fun publish_message_and_wait_for_confirm/3,
                 [Method, Payload, Opts]),
    case Result of
        true ->
            update_messages_published_metrics(Host, PublishTime, Payload),
            ?DEBUG("event=rabbit_message_sent message=~1000p", [Payload]),
            {noreply, Opts};
        false ->
            update_messages_failed_metrics(Host),
            ?WARNING_MSG("event=rabbit_message_sent_failed reason=negative_ack",
                         []),
            {noreply, Opts};
        {channel_exception, Error, Reason} ->
            update_messages_failed_metrics(Host),
            ?WARNING_MSG("event=rabbit_message_sent_failed reason=~1000p:~1000p",
                         [Error, Reason]),
            {FreshConn, FreshChann} = maybe_restart_rabbit_connection(Opts),
            {noreply, Opts#state{connection = FreshConn, channel = FreshChann}};
        timeout ->
            update_messages_timeout_metrics(Host),
            ?WARNING_MSG("event=rabbit_message_sent_failed reason=timeout", []),
            {noreply, Opts}
    end.

-spec publish_message_and_wait_for_confirm(Method :: mongoose_amqp:method(),
                                           Payload :: mongoose_amqp:message(),
                                           worker_opts()) ->
    boolean() | timeout | channel_exception.
publish_message_and_wait_for_confirm(Method, Payload,
                                     #state{channel = Channel,
                                            confirms = IsConfirmEnabled}) ->
    try amqp_channel:call(Channel, Method, Payload) of
        _Res ->
            maybe_wait_for_confirms(Channel, IsConfirmEnabled)
    catch
        Error:Reason -> {channel_exception, Error, Reason}
    end.

-spec maybe_wait_for_confirms(Channel :: pid(), boolean()) ->
    boolean() | timeout.
maybe_wait_for_confirms(Channel, true) ->
    amqp_channel:wait_for_confirms(Channel);
maybe_wait_for_confirms(_, _) -> true.

-spec maybe_restart_rabbit_connection(worker_opts()) -> {pid(), pid()}.
maybe_restart_rabbit_connection(#state{connection = Conn, channel = Chann,
                                       host = Host,
                                       amqp_client_opts = AMQPOpts}) ->
    case is_process_alive(Conn) of
        true ->
            {Conn, amqp_connection:open_channel(Conn)};
        false ->
            establish_rabbit_connection(AMQPOpts, Host)
    end.

-spec establish_rabbit_connection(Opts :: mongoose_amqp:network_params(),
                                  Host :: jid:server()) -> {pid(), pid()}.
establish_rabbit_connection(AMQPOpts, Host) ->
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

-spec maybe_enable_confirms(Channel :: pid(), proplists:proplist()) ->
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

-spec maybe_handle_request(Callback :: function(), Args :: [term()],
                           Reply :: term()) -> term().
maybe_handle_request(Callback, Args, Reply) ->
    State = lists:last(Args),
    case is_msq_queue_max_limit_reached(State#state.max_queue_len) of
        false ->
            apply(Callback, Args);
        true ->
            ?WARNING_MSG("event=rabbit_worker_request_dropped "
                         ++ "reason=queue_message_length_limit_reached "
                         ++ "limit=~p", [State#state.max_queue_len]),
            Reply
    end.

-spec is_msq_queue_max_limit_reached(Limit :: infinity | non_neg_integer()) ->
    boolean().
is_msq_queue_max_limit_reached(infinity) -> false;
is_msq_queue_max_limit_reached(Limit) ->
    case process_info(self(), message_queue_len) of
        {_, QueueLen} when QueueLen > Limit ->
            true;
        _Else ->
            false
    end.
