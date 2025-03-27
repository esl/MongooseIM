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
          host :: binary(),
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
    try
        {ok, RawSocket} = gen_tcp:connect(Addr, Port, [binary, {active, false}]),
        {ok, Socket} = mod_global_distrib_transport:wrap(RawSocket, opt(connections), client),
        GdStart = gd_start(Server, ConnID),
        ok = mod_global_distrib_transport:send(Socket, <<(byte_size(GdStart)):32, GdStart/binary>>),
        mod_global_distrib_transport:setopts(Socket, [{active, once}]),
        mongoose_instrument:execute(?GLOBAL_DISTRIB_OUTGOING_ESTABLISHED, #{},
                                    #{count => 1, host => Server}),
        {ok, #state{socket = Socket, host = Server, conn_id = ConnID,
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
    mongoose_instrument:execute(?GLOBAL_DISTRIB_SEND_QUEUE, #{}, #{time => QueueTimeUS, host => ToHost}),
    ClockTime = erlang:system_time(microsecond),
    Annotated = <<(byte_size(Data) + 8):32, ClockTime:64, Data/binary>>,
    case mod_global_distrib_transport:send(Socket, Annotated) of
        ok ->
            mongoose_instrument:execute(?GLOBAL_DISTRIB_MESSAGES_SENT, #{}, #{count => 1, host => ToHost});
        Error ->
            ?LOG_ERROR(#{what => gd_cant_send_packet,
                         reason => Error, packet => Data, conn_id => State#state.conn_id}),
            error(Error)
    end,
    {noreply, State}.

handle_info({Tag, _Socket, _RawData}, #state{socket = Socket} = State)
  when Tag == tcp; Tag == ssl ->
    ok = mod_global_distrib_transport:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({Tag, _Socket}, State) when Tag == tcp_closed; Tag == ssl_closed ->
    {stop, normal, State};
handle_info({Tag, _Socket, Reason}, State) when Tag == tcp_error; Tag == ssl_error ->
    ?LOG_ERROR(#{what => gd_outgoing_socket_error,
                 reason => Reason, peer => State#state.peer, conn_id => State#state.conn_id}),
    mongoose_instrument:execute(?GLOBAL_DISTRIB_OUTGOING_ERRORED, #{}, #{count => 1, host => State#state.host}),
    {stop, {error, Reason}, State};
handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, State) ->
    case Reason of
        shutdown ->
            ?LOG_INFO(#{what => gd_outgoing_socket_error,
                        reason => Reason, peer => State#state.peer, conn_id => State#state.conn_id});
        _ ->
            ?LOG_ERROR(#{what => gd_outgoing_socket_error,
                         reason => Reason, peer => State#state.peer, conn_id => State#state.conn_id})
    end,
    mongoose_instrument:execute(?GLOBAL_DISTRIB_OUTGOING_CLOSED, #{}, #{count => 1, host => State#state.host}),
    catch mod_global_distrib_transport:close(State#state.socket),
    ignore.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec opt(Key :: atom()) -> term().
opt(Key) ->
    mod_global_distrib_utils:opt(mod_global_distrib, Key).

gd_start(Server, ConnID) ->
    Attrs = #{<<"server">> => Server, <<"conn_id">> => ConnID},
    exml:to_binary(#xmlel{name = <<"gd_start">>, attrs = Attrs}).
