%%%----------------------------------------------------------------------
%%% File    : ejabberd_socket.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Socket with zlib and TLS support library
%%% Created : 23 Aug 2006 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_socket).
-author('alexey@process-one.net').

-behaviour(mongoose_transport).

%% API
-export([start/4,
         connect/3,
         connect/4,
         starttls/2,
         starttls/3,
         send/2,
         change_shaper/2,
         monitor/1,
         get_sockmod/1,
         get_peer_certificate/1,
         close/1,
         sockname/1,
         peername/1]).

-ignore_xref([change_shaper/2, compress/3, connect/3, get_peer_certificate/1,
              get_sockmod/1, sockname/1]).

-include("mongoose.hrl").

-record(socket_state, {sockmod    :: gen_tcp | mongoose_tls,
                       socket     :: term(),
                       receiver   :: pid() | atom() | tuple(),
                       connection_details :: mongoose_tcp_listener:connection_details()
                      }).
-type socket_state() :: #socket_state{}.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function:
%% Description:
%%--------------------------------------------------------------------
-spec start(module(), gen_tcp:socket(),
            mongoose_tcp_listener:options(),
            mongoose_tcp_listener:connection_details()) -> ok.
start(Module, Socket, Opts, ConnectionDetails) ->
    RecPid = ejabberd_receiver:start(Socket, none, Opts),
    SocketData = #socket_state{sockmod = gen_tcp,
                               socket = Socket,
                               receiver = RecPid,
                               connection_details = ConnectionDetails},
    %% set receiver as socket's controlling process before
    %% the M:start/2 call, that is required for c2s legacy
    %% TLS connection support.
    case gen_tcp:controlling_process(Socket, RecPid) of
        ok ->
            case Module:start(SocketData, Opts) of
                {ok, Pid} ->
                    ejabberd_receiver:become_controller(RecPid, Pid);
                {error, _Reason} ->
                    gen_tcp:close(Socket),
                    ejabberd_receiver:close(RecPid)
            end;
        {error, _Reason} ->
            gen_tcp:close(Socket)
    end.

-type option_value() :: 'asn1' | 'cdr' | 'false' | 'fcgi' | 'http' | 'http_bin'
                      | 'line' | 'once' | 'raw' | 'sunrm' | 'tpkt' | 'true'
                      | integer() | inet:ip_address().
-type option() :: 'binary' | 'inet' | 'inet6' | 'list'
                | {atom(), option_value()}
                | {'raw', non_neg_integer(), non_neg_integer(), binary()}.
-spec connect(Addr :: atom() | string() | inet:ip_address(),
              Port :: inet:port_number(),
              Opts :: [option()]) -> {'error', atom()} | {'ok', socket_state()}.
connect(Addr, Port, Opts) ->
    connect(Addr, Port, Opts, infinity).


-spec connect(Addr :: atom() | string() | inet:ip_address(),
              Port :: inet:port_number(),
              Opts :: [option()],
              Timeout :: non_neg_integer() | infinity
              ) -> {'error', atom()} | {'ok', socket_state()}.
connect(Addr, Port, Opts, Timeout) ->
    case gen_tcp:connect(Addr, Port, Opts, Timeout) of
        {ok, Socket} ->
            %% Receiver options are configurable only for listeners
            %% It might make sense to make them configurable for outgoing s2s connections as well
            ReceiverOpts = #{max_stanza_size => infinity, hibernate_after => 0},
            Receiver = ejabberd_receiver:start(Socket, none, ReceiverOpts),
            {SrcAddr, SrcPort} = case inet:sockname(Socket) of
                                     {ok, {A, P}} ->  {A, P};
                                     {error, _} -> {unknown, unknown}
                                 end,
            SocketData = #socket_state{sockmod = gen_tcp,
                                       socket = Socket,
                                       receiver = Receiver,
                                       connection_details = #{proxy => false,
                                                              src_address => SrcAddr,
                                                              src_port => SrcPort,
                                                              dest_address => Addr,
                                                              dest_port => Port}},
            Pid = self(),
            case gen_tcp:controlling_process(Socket, Receiver) of
                ok ->
                    ejabberd_receiver:become_controller(Receiver, Pid),
                    {ok, SocketData};
                {error, _Reason} = Error ->
                    gen_tcp:close(Socket),
                    Error
            end;
        {error, _Reason} = Error ->
            Error
    end.


-spec tcp_to_tls(socket_state(), mongoose_tls:options()) -> mongoose_tls:tls_socket().
tcp_to_tls(#socket_state{receiver = Receiver}, TLSOpts) ->
    ejabberd_receiver:starttls(Receiver, TLSOpts).

get_tls_socket(#socket_state{receiver = Receiver}) ->
    case ejabberd_receiver:get_socket(Receiver) of
        {ok, TLSSocket} -> TLSSocket;
        _ -> invalid_socket
    end.


-spec starttls(socket_state(), mongoose_tls:options()) -> socket_state().
starttls(SocketData, TLSOpts) ->
    tcp_to_tls(SocketData, TLSOpts),
    case get_tls_socket(SocketData) of
        invalid_socket ->
            exit(invalid_socket_after_upgrade_to_tls);
        NewSocket ->
            SocketData#socket_state{socket = NewSocket, sockmod = mongoose_tls}
    end.


-spec starttls(socket_state(), mongoose_tls:options(), _) -> socket_state().
starttls(SocketData, TLSOpts, Data) ->
    tcp_to_tls(SocketData, TLSOpts),
    send(SocketData, Data), %% send last negotiation chunk via tcp
    NewSocket = get_tls_socket(SocketData),
    SocketData#socket_state{socket = NewSocket, sockmod = mongoose_tls}.

%% @doc sockmod=gen_tcp|mongoose_tls
send(SocketData, Data) ->
    case catch (SocketData#socket_state.sockmod):send(
             SocketData#socket_state.socket, Data) of
        ok -> ok;
        {error, timeout} ->
            ?LOG_INFO(#{what => socket_error, reason => timeout,
                        socket => SocketData#socket_state.sockmod}),
            exit(normal);
        Error ->
            ?LOG_INFO(#{what => socket_error, reason => Error,
                        socket => SocketData#socket_state.sockmod}),
            exit(normal)
    end.

-spec change_shaper(#socket_state{receiver::atom() | pid() | tuple()}, _) -> any().
change_shaper(SocketData, Shaper)
  when is_pid(SocketData#socket_state.receiver) ->
    ejabberd_receiver:change_shaper(SocketData#socket_state.receiver, Shaper);
change_shaper(SocketData, Shaper)
  when is_atom(SocketData#socket_state.receiver) ->
    (SocketData#socket_state.receiver):change_shaper(
      SocketData#socket_state.socket, Shaper).


-spec monitor(socket_state()) -> reference().
monitor(SocketData) when is_pid(SocketData#socket_state.receiver) ->
    erlang:monitor(process, SocketData#socket_state.receiver);
monitor(SocketData) when is_atom(SocketData#socket_state.receiver) ->
    (SocketData#socket_state.receiver):monitor(
      SocketData#socket_state.socket).


-spec get_sockmod(socket_state()) -> gen_tcp | mongoose_tls.
get_sockmod(SocketData) ->
    SocketData#socket_state.sockmod.


-spec get_peer_certificate(socket_state()) -> mongoose_tls:cert().
get_peer_certificate(#socket_state{sockmod = mongoose_tls, socket = Socket}) ->
    mongoose_tls:get_peer_certificate(Socket);
get_peer_certificate(_SocketData) ->
    no_peer_cert.


-spec close(socket_state()) -> ok.
close(SocketData) ->
    ejabberd_receiver:close(SocketData#socket_state.receiver).


-spec sockname(socket_state()) -> mongoose_transport:peername_return().
sockname(#socket_state{connection_details = #{dest_address := DestAddr,
                                              dest_port := DestPort}}) ->
    {ok, {DestAddr, DestPort}}.


-spec peername(socket_state()) -> mongoose_transport:peername_return().
peername(#socket_state{connection_details = #{src_address := SrcAddr,
                                              src_port := SrcPort}}) ->
    {ok, {SrcAddr, SrcPort}}.
