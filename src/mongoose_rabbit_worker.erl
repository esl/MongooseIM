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

-include("mongoose.hrl").

-behaviour(gen_server).

-export([start_link/0, instrumentation/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-ignore_xref([start_link/0]).

-type state() :: #{amqp_client_opts := mongoose_amqp:network_params(),
                   connection := pid(),
                   channel := pid(),
                   host_type := mongooseim:host_type_or_global(),
                   pool_tag := atom(),
                   confirms := boolean(),
                   max_queue_len := non_neg_integer() | infinity}.

-type worker_opts() :: state().
-type publish_result() :: boolean() | timeout | {channel_exception, any(), any()}.

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

-spec instrumentation(mongooseim:host_type_or_global(), mongoose_wpool:tag()) -> [mongoose_instrument:spec()].
instrumentation(HostType, Tag) ->
    [{wpool_rabbit_connections, #{pool_tag => Tag, host_type => HostType},
      #{metrics => #{active => counter, opened => spiral, closed => spiral, failed => spiral}}},
     {wpool_rabbit_messages_published, #{pool_tag => Tag, host_type => HostType},
      #{metrics => #{count => spiral, failed => spiral, timeout => spiral, time => histogram,
                     size => histogram}}}].

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    process_flag(trap_exit, true),
    %TODO: Refactor with handle_continue when OTP 21 is minimal supported version
    do_init(Opts).

handle_call(Req, From, State) ->
    maybe_handle_request(fun do_handle_call/3, [Req, From, State],
                         {reply, request_dropped, State}).

handle_cast(Req, State) ->
   maybe_handle_request(fun do_handle_cast/2, [Req, State], {noreply, State}).

handle_info(Req, State) ->
    maybe_handle_request(fun do_handle_info/2, [Req, State], {noreply, State}).

terminate(_Reason, #{connection := Connection, channel := Channel,
                     host_type := HostType, pool_tag := PoolTag}) ->
    close_rabbit_connection(Connection, Channel, HostType, PoolTag),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_init(Opts) ->
    HostType = proplists:get_value(host_type, Opts),
    PoolTag = proplists:get_value(pool_tag, Opts),
    AMQPClientOpts = proplists:get_value(amqp_client_opts, Opts),
    {Connection, Channel} =
        establish_rabbit_connection(AMQPClientOpts, HostType, PoolTag),
    IsConfirmEnabled = maybe_enable_confirms(Channel, Opts),
    MaxMsgQueueLen = proplists:get_value(max_queue_len, Opts),
    {ok, #{host_type => HostType, amqp_client_opts => AMQPClientOpts,
           connection => Connection, channel => Channel,
           confirms => IsConfirmEnabled, max_queue_len => MaxMsgQueueLen,
           pool_tag => PoolTag}}.

do_handle_call({amqp_call, Method}, _From, State = #{channel := Channel}) ->
    try amqp_channel:call(Channel, Method) of
        Res ->
            {reply, {ok, Res}, State}
    catch
        Error:Reason ->
            {FreshConn, FreshChann} = maybe_restart_rabbit_connection(State),
            {reply, {Error, Reason}, State#{connection := FreshConn,
                                            channel := FreshChann}}
    end.

do_handle_cast({amqp_publish, Method, Payload}, State) ->
    handle_amqp_publish(Method, Payload, State).

do_handle_info(Req, State) ->
    ?UNEXPECTED_INFO(Req),
    {noreply, State}.

-spec handle_amqp_publish(Method :: mongoose_amqp:method(),
                          Payload :: mongoose_amqp:message(),
                          Opts :: worker_opts()) -> {noreply, worker_opts()}.
handle_amqp_publish(Method, Payload, Opts = #{host_type := HostType,
                                              pool_tag := PoolTag}) ->
    PublishArgs = [Method, Payload, Opts],
    Res = mongoose_instrument:span(wpool_rabbit_messages_published, #{host_type => HostType, pool_tag => PoolTag},
                                   fun publish_message_and_wait_for_confirm/3,
                                   PublishArgs,
                                   fun(PublishTime, Result) ->
                                       handle_publish_result(PublishTime, Result, HostType, PoolTag, PublishArgs)
                                   end),
    case Res of
        {channel_exception, _, _} ->
            {FreshConn, FreshChann} = maybe_restart_rabbit_connection(Opts),
            {noreply, Opts#{connection := FreshConn, channel := FreshChann}};
        _ ->
            {noreply, Opts}
    end.

-spec handle_publish_result(integer(), publish_result(), mongooseim:host_type_or_global(),
                            mongoose_wpool:tag(), [_]) ->
    mongoose_instrument:measurements().
handle_publish_result(PublishTime, true, HostType, PoolTag, [Method, Payload, Opts]) ->
    ?LOG_DEBUG(#{what => rabbit_message_sent, host_type => HostType, tag => PoolTag,
                 method => Method, payload => Payload, opts => Opts}),
    #{count => 1, time => PublishTime, size => byte_size(term_to_binary(Payload))};
handle_publish_result(_PublishTime, false, HostType, PoolTag, [Method, Payload, Opts]) ->
    ?LOG_WARNING(#{what => rabbit_message_sent_failed, reason => negative_ack, host_type => HostType, tag => PoolTag,
                   method => Method, payload => Payload, opts => Opts}),
    #{failed => 1};
handle_publish_result(_PublishTime, {channel_exception, Error, Reason}, HostType, PoolTag, [Method, Payload, Opts]) ->
    ?LOG_ERROR(#{what => rabbit_message_sent_failed,
                 class => Error, reason => Reason, host_type => HostType, tag => PoolTag,
                 method => Method, payload => Payload, opts => Opts}),
    #{failed => 1};
handle_publish_result(_PublishTime, timeout, HostType, PoolTag, [Method, Payload, Opts]) ->
    ?LOG_ERROR(#{what => rabbit_message_sent_failed, reason => timeout, host_type => HostType, tag => PoolTag,
                 method => Method, payload => Payload, opts => Opts}),
    #{timeout => 1}.

-spec publish_message_and_wait_for_confirm(Method :: mongoose_amqp:method(),
                                           Payload :: mongoose_amqp:message(),
                                           worker_opts()) ->
    publish_result().
publish_message_and_wait_for_confirm(Method, Payload,
                                     #{channel := Channel,
                                       confirms := IsConfirmEnabled}) ->
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
maybe_restart_rabbit_connection(#{connection := Conn, host_type := HostType,
                                  pool_tag := PoolTag,
                                  amqp_client_opts := AMQPOpts}) ->
    case is_process_alive(Conn) of
        true ->
            {ok, Channel} = amqp_connection:open_channel(Conn),
            {Conn, Channel};
        false ->
            establish_rabbit_connection(AMQPOpts, HostType, PoolTag)
    end.

-spec establish_rabbit_connection(Opts :: mongoose_amqp:network_params(),
                                  HostType :: mongooseim:host_type_or_global(), PoolTag :: atom())
    -> {pid(), pid()}.
establish_rabbit_connection(AMQPOpts, HostType, PoolTag) ->
    case amqp_connection:start(AMQPOpts) of
        {ok, Connection} ->
            mongoose_instrument:execute(wpool_rabbit_connections, #{host_type => HostType, pool_tag => PoolTag},
                                        #{active => 1, opened => 1}),
            {ok, Channel} = amqp_connection:open_channel(Connection),
            ?LOG_DEBUG(#{what => rabbit_connection_established,
                         host_type => HostType, pool_tag => PoolTag, opts => AMQPOpts}),
            {Connection, Channel};
        {error, Error} ->
            mongoose_instrument:execute(wpool_rabbit_connections, #{host_type => HostType, pool_tag => PoolTag},
                                        #{failed => 1}),
            ?LOG_ERROR(#{what => rabbit_connection_failed, reason => Error,
                         host_type => HostType, pool_tag => PoolTag, opts => AMQPOpts}),
            exit("connection to a Rabbit server failed")
    end.

-spec close_rabbit_connection(Connection :: pid(), Channel :: pid(),
                              HostType :: mongooseim:host_type_or_global(), PoolTag :: atom()) ->
    ok | no_return().
close_rabbit_connection(Connection, Channel, HostType, PoolTag) ->
    mongoose_instrument:execute(wpool_rabbit_connections, #{host_type => HostType, pool_tag => PoolTag},
                                #{active => -1, closed => 1}),
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

-spec maybe_handle_request(Callback :: function(), Args :: [term()],
                           Reply :: term()) -> term().
maybe_handle_request(Callback, Args, Reply) ->
    State = lists:last(Args),
    Limit = maps:get(max_queue_len, State),
    case is_msq_queue_max_limit_reached(Limit) of
        false ->
            apply(Callback, Args);
        true ->
            ?LOG_WARNING(#{what => rabbit_worker_request_dropped,
                           reason => queue_message_length_limit_reached,
                           limit => Limit}),
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
