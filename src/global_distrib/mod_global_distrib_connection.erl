%%==============================================================================
%% Copyright 2017 Erlang Solutions Ltd.
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
%%==============================================================================

-module(mod_global_distrib_connection).
-author('konrad.zemek@erlang-solutions.com').

-behaviour(gen_server).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("global_distrib_metrics.hrl").

-record(state, {
          socket :: mod_global_distrib_transport:t(),
          host :: atom(),
          peer :: tuple() | unknown,
          conn_id :: binary()
         }).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-ignore_xref([start_link/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link(Endpoint :: mod_global_distrib_utils:endpoint(),
                 Server :: jid:lserver()) -> {ok, pid()} | {error, any()}.
start_link(Endpoint, Server) ->
    gen_server:start_link(?MODULE, [Endpoint, Server], []).

init([{Addr, Port}, Server]) ->
    ConnID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    ?LOG_DEBUG(#{what => gd_new_outgoing_connection,
                 server => Server, address => Addr, port => Port,
                 pid => self(), conn_id => ConnID}),
    process_flag(trap_exit, true),
    MetricServer = mod_global_distrib_utils:binary_to_metric_atom(Server),
    mod_global_distrib_utils:ensure_metric(?GLOBAL_DISTRIB_MESSAGES_SENT(MetricServer), spiral),
    mod_global_distrib_utils:ensure_metric(
      ?GLOBAL_DISTRIB_SEND_QUEUE_TIME(MetricServer), histogram),
    mod_global_distrib_utils:ensure_metric(
      ?GLOBAL_DISTRIB_OUTGOING_ESTABLISHED(MetricServer), spiral),
    mod_global_distrib_utils:ensure_metric(
      ?GLOBAL_DISTRIB_OUTGOING_ERRORED(MetricServer), spiral),
    mod_global_distrib_utils:ensure_metric(
      ?GLOBAL_DISTRIB_OUTGOING_CLOSED(MetricServer), spiral),
    try
        {ok, RawSocket} = gen_tcp:connect(Addr, Port, [binary, {active, false}]),
        {ok, Socket} = mod_global_distrib_transport:wrap(RawSocket, opt(connections),
                                                         #{connect => true}),
        GdStart = gd_start(Server, ConnID),
        ok = mod_global_distrib_transport:send(Socket, <<(byte_size(GdStart)):32, GdStart/binary>>),
        mod_global_distrib_transport:setopts(Socket, [{active, once}]),
        mongoose_metrics:update(global, ?GLOBAL_DISTRIB_OUTGOING_ESTABLISHED(MetricServer), 1),
        {ok, #state{socket = Socket, host = MetricServer, conn_id = ConnID,
                    peer = mod_global_distrib_transport:peername(Socket)}}
    catch
        error:{badmatch, Reason}:StackTrace ->
            ?LOG_ERROR(#{what => gd_connection_failed,
                         server => Server, address => Addr, port => Port,
                         reason => Reason, conn_id => ConnID, stacktrace => StackTrace}),
            {stop, normal}
    end.

handle_call(Msg, From, State) ->
    gen_server:reply(From, ok),
    handle_cast(Msg, State).

handle_cast({data, Stamp, Data}, #state{socket = Socket, host = ToHost} = State) ->
    QueueTimeNative = erlang:monotonic_time() - Stamp,
    QueueTimeUS = erlang:convert_time_unit(QueueTimeNative, native, microsecond),
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_SEND_QUEUE_TIME(ToHost), QueueTimeUS),
    ClockTime = erlang:system_time(microsecond),
    Annotated = <<(byte_size(Data) + 8):32, ClockTime:64, Data/binary>>,
    case mod_global_distrib_transport:send(Socket, Annotated) of
        ok ->
            mongoose_metrics:update(global, ?GLOBAL_DISTRIB_MESSAGES_SENT(ToHost), 1);
        Error ->
            ?LOG_ERROR(#{what => gd_cant_send_packet,
                         reason => Error, packet => Data, conn_id => State#state.conn_id}),
            error(Error)
    end,
    {noreply, State}.

handle_info({tcp, _Socket, RawData}, #state{socket = Socket} = State) ->
    ok = mod_global_distrib_transport:setopts(Socket, [{active, once}]),
    %% Feeding data to drive the TLS state machine (in case of TLS connection)
    {ok, _} = mod_global_distrib_transport:recv_data(Socket, RawData),
    {noreply, State};
handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
    ?LOG_ERROR(#{what => gd_outgoing_socket_error,
                 reason => Reason, peer => State#state.peer, conn_id => State#state.conn_id}),
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_OUTGOING_ERRORED(State#state.host), 1),
    {stop, {error, Reason}, State};
handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, State) ->
    ?LOG_ERROR(#{what => gd_outgoing_socket_error,
                 reason => Reason, peer => State#state.peer, conn_id => State#state.conn_id}),
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_OUTGOING_CLOSED(State#state.host), 1),
    catch mod_global_distrib_transport:close(State#state.socket),
    ignore.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec opt(Key :: atom()) -> term().
opt(Key) ->
    mod_global_distrib_utils:opt(mod_global_distrib, Key).

gd_start(Server, ConnID) ->
    Attrs = [{<<"server">>, Server}, {<<"conn_id">>, ConnID}],
    exml:to_binary(#xmlel{name = <<"gd_start">>, attrs = Attrs}).
