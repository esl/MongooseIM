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

%% API
-export([instrumentation/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-type state() :: #{host_type := mongooseim:host_type_or_global(),
                   pool_tag := atom(),
                   opts := opts(),
                   connection => pid(),
                   channel => pid()}.

-type opts() :: #{host := string(),
                  port := inet:port_number(),
                  username := binary(),
                  password := binary(),
                  virtual_host := binary(),
                  confirms_enabled := boolean(),
                  reconnect := reconnect()}.

-type reconnect() :: #{attempts := non_neg_integer(),
                       delay := non_neg_integer() % milliseconds
                      }.

-type publish_result() :: boolean() | timeout | {channel_exception, any(), any()}.

%%%===================================================================
%%% API
%%%===================================================================

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

-spec init(state()) -> {ok, state()}.
init(State) ->
    process_flag(trap_exit, true),
    {ok, establish_rabbit_connection(State)}.

-spec handle_call({amqp_call, mongoose_amqp:method()}, gen_server:from(), state()) ->
          {reply, {ok | error | exit | throw, mongoose_amqp:method() | atom()}, state()}.
handle_call({amqp_call, Method}, _From, State = #{channel := Channel}) ->
    try amqp_channel:call(Channel, Method) of
        Res ->
            {reply, {ok, Res}, State}
    catch
        Error:Reason ->
            {reply, {Error, Reason}, maybe_restart_rabbit_connection(State)}
    end.

-spec handle_cast({amqp_publish, mongoose_amqp:method(), mongoose_amqp:message()}, state()) ->
          {noreply, state()}.
handle_cast({amqp_publish, Method, Payload}, State) ->
    handle_amqp_publish(Method, Payload, State).

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', _Ref, process, Connection, _}, State) ->
    {noreply, case State of
                  #{connection := Connection} -> establish_rabbit_connection(State);
                  #{} -> State % probably already reconnected
              end};
handle_info(Req, State) ->
    ?UNEXPECTED_INFO(Req),
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #{connection := Connection, channel := Channel,
                     host_type := HostType, pool_tag := PoolTag}) ->
    close_rabbit_connection(Connection, Channel, HostType, PoolTag),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec handle_amqp_publish(mongoose_amqp:method(), mongoose_amqp:message(), state()) ->
          {noreply, state()}.
handle_amqp_publish(Method, Payload, State = #{host_type := HostType, pool_tag := PoolTag}) ->
    PublishArgs = [Method, Payload, State],
    Res = mongoose_instrument:span(wpool_rabbit_messages_published,
                                   #{host_type => HostType, pool_tag => PoolTag},
                                   fun publish_message_and_wait_for_confirm/3,
                                   PublishArgs,
                                   fun(PublishTime, Result) ->
                                       handle_publish_result(PublishTime, Result, HostType,
                                                             PoolTag, PublishArgs)
                                   end),
    case Res of
        {channel_exception, _, _} ->
            {noreply, maybe_restart_rabbit_connection(State)};
        _ ->
            {noreply, State}
    end.

-spec handle_publish_result(integer(), publish_result(), mongooseim:host_type_or_global(),
                            mongoose_wpool:tag(), [_]) ->
    mongoose_instrument:measurements().
handle_publish_result(PublishTime, true, HostType, PoolTag, [Method, Payload, Opts]) ->
    ?LOG_DEBUG(#{what => rabbit_message_sent, host_type => HostType, tag => PoolTag,
                 method => Method, payload => Payload, opts => Opts}),
    #{count => 1, time => PublishTime, size => byte_size(term_to_binary(Payload)),
      payload => Payload};
handle_publish_result(_PublishTime, false, HostType, PoolTag, [Method, Payload, Opts]) ->
    ?LOG_WARNING(#{what => rabbit_message_sent_failed, reason => negative_ack,
                   host_type => HostType, tag => PoolTag, method => Method, payload => Payload,
                   opts => Opts}),
    #{failed => 1, payload => Payload};
handle_publish_result(_PublishTime, {channel_exception, Error, Reason}, HostType, PoolTag,
                      [Method, Payload, Opts]) ->
    ?LOG_ERROR(#{what => rabbit_message_sent_failed,
                 class => Error, reason => Reason, host_type => HostType, tag => PoolTag,
                 method => Method, payload => Payload, opts => Opts}),
    #{failed => 1, payload => Payload};
handle_publish_result(_PublishTime, timeout, HostType, PoolTag, [Method, Payload, Opts]) ->
    ?LOG_ERROR(#{what => rabbit_message_sent_failed, reason => timeout, host_type => HostType,
                 tag => PoolTag, method => Method, payload => Payload, opts => Opts}),
    #{timeout => 1, payload => Payload}.

-spec publish_message_and_wait_for_confirm(Method :: mongoose_amqp:method(),
                                           Payload :: mongoose_amqp:message(), state()) ->
    publish_result().
publish_message_and_wait_for_confirm(Method, Payload, #{channel := Channel, opts := Opts}) ->
    try amqp_channel:call(Channel, Method, Payload) of
        _Res ->
            maybe_wait_for_confirms(Channel, Opts)
    catch
        Error:Reason -> {channel_exception, Error, Reason}
    end.

-spec maybe_wait_for_confirms(Channel :: pid(), opts()) -> boolean() | timeout.
maybe_wait_for_confirms(Channel, #{confirms_enabled := true}) ->
    amqp_channel:wait_for_confirms(Channel);
maybe_wait_for_confirms(_, _) ->
    true.

-spec maybe_restart_rabbit_connection(state()) -> state().
maybe_restart_rabbit_connection(#{connection := Connection, opts := Opts} = State) ->
    case is_process_alive(Connection) of
        true ->
            State#{channel := open_amqp_channel(Connection, Opts)};
        false ->
            establish_rabbit_connection(State)
    end.

-spec establish_rabbit_connection(state()) -> state().
establish_rabbit_connection(State = #{opts := #{reconnect := #{attempts := Attempts}}}) ->
    establish_rabbit_connection(State, Attempts).

-spec establish_rabbit_connection(state(), non_neg_integer()) -> state().
establish_rabbit_connection(State, RemainingAttempts) ->
    case start_amqp_connection(State) of
        {ok, NewState} ->
            NewState;
        {error, Error} when RemainingAttempts > 0 ->
            ?LOG_WARNING(#{what => rabbit_connection_failed, reason => Error, worker_state => State,
                           remaining_attempts => RemainingAttempts}),
            #{opts := #{reconnect := #{delay := Delay}}} = State,
            timer:sleep(Delay),
            establish_rabbit_connection(State, RemainingAttempts - 1);
        {error, Error} when RemainingAttempts =:= 0 ->
            ErrorInfo = #{what => rabbit_connection_failed, reason => Error, worker_state => State},
            ?LOG_ERROR(ErrorInfo),
            exit(ErrorInfo)
    end.

-spec start_amqp_connection(state()) -> {ok, state()} | {error, term()}.
start_amqp_connection(State) ->
    #{opts := Opts, host_type := HostType, pool_tag := PoolTag} = State,
    case amqp_connection:start(mongoose_amqp:network_params(Opts)) of
        {ok, Connection} ->
            monitor(process, Connection), % resulting ref is ignored as there is only one monitor
            mongoose_instrument:execute(wpool_rabbit_connections,
                                        #{host_type => HostType, pool_tag => PoolTag},
                                        #{active => 1, opened => 1}),
            Channel = open_amqp_channel(Connection, Opts),
            ?LOG_DEBUG(#{what => rabbit_connection_established,
                         host_type => HostType, pool_tag => PoolTag, opts => Opts}),
            {ok, State#{connection => Connection, channel => Channel}};
        {error, Error} ->
            mongoose_instrument:execute(wpool_rabbit_connections,
                                        #{host_type => HostType, pool_tag => PoolTag},
                                        #{failed => 1}),
            {error, Error}
    end.

-spec open_amqp_channel(pid(), opts()) -> pid().
open_amqp_channel(Connection, Opts) ->
    {ok, Channel} = amqp_connection:open_channel(Connection),
    maybe_enable_confirms(Channel, Opts),
    Channel.

-spec close_rabbit_connection(Connection :: pid(), Channel :: pid(),
                              HostType :: mongooseim:host_type_or_global(), PoolTag :: atom()) ->
    ok | no_return().
close_rabbit_connection(Connection, Channel, HostType, PoolTag) ->
    mongoose_instrument:execute(wpool_rabbit_connections,
                                #{host_type => HostType, pool_tag => PoolTag},
                                #{active => -1, closed => 1}),
    try amqp_channel:close(Channel)
    catch
        _Error:_Reason -> already_closed
    end,
    amqp_connection:close(Connection).

-spec maybe_enable_confirms(Channel :: pid(), opts()) -> ok.
maybe_enable_confirms(Channel, #{confirms_enabled := true}) ->
    ConfirmCallRes = mongoose_amqp:confirm_select_ok(),
    ConfirmCallRes = amqp_channel:call(Channel, mongoose_amqp:confirm_select()),
    ok;
maybe_enable_confirms(_Channel, #{}) ->
    ok.
