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
         compress/3,
         send/2,
         send_xml/2,
         change_shaper/2,
         monitor/1,
         get_sockmod/1,
         get_peer_certificate/1,
         close/1,
         sockname/1,
         peername/1]).

-include("mongoose.hrl").

-record(socket_state, {sockmod    :: ejabberd:sockmod(),
                       socket     :: term(),
                       receiver   :: pid() | atom() | tuple()
                      }).
-type socket_state() :: #socket_state{}.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function:
%% Description:
%%--------------------------------------------------------------------
-spec start(atom() | tuple(), ejabberd:sockmod(),
    Socket :: port(), Opts :: [{atom(), _}]) -> ok.
start(Module, SockMod, Socket, Opts) ->
    case Module:socket_type() of
        xml_stream ->
            {ReceiverMod, Receiver, RecRef} =
                case catch SockMod:custom_receiver(Socket) of
                    {receiver, RecMod, RecPid} ->
                        {RecMod, RecPid, RecMod};
                    _ ->
                        RecPid = ejabberd_receiver:start(Socket, SockMod, none, Opts),
                        {ejabberd_receiver, RecPid, RecPid}
                end,
            SocketData = #socket_state{sockmod = SockMod,
                                       socket = Socket,
                                       receiver = RecRef},
            %% set receiver as socket's controlling process before
            %% the M:start/2 call, that is required for c2s legacy
            %% TLS connection support.
            case SockMod:controlling_process(Socket, Receiver) of
                ok ->
                    case Module:start({?MODULE, SocketData}, Opts) of
                        {ok, Pid} ->
                            ReceiverMod:become_controller(Receiver, Pid);
                        {error, _Reason} ->
                            SockMod:close(Socket),
                            case ReceiverMod of
                                ejabberd_receiver ->
                                    ReceiverMod:close(Receiver);
                                _ ->
                                    ok
                            end
                    end;
                {error, _Reason} ->
                    SockMod:close(Socket)
            end;

        independent ->
            ok;

        raw ->
            case Module:start({SockMod, Socket}, Opts) of
                {ok, Pid} ->
                    case SockMod:controlling_process(Socket, Pid) of
                        ok ->
                            ok;
                        {error, _Reason} ->
                            SockMod:close(Socket)
                    end;
                {error, _Reason} ->
                    SockMod:close(Socket)
            end
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
            Receiver = ejabberd_receiver:start(Socket, gen_tcp, none, Opts),
            SocketData = #socket_state{sockmod = gen_tcp,
                                       socket = Socket,
                                       receiver = Receiver},
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


-spec tcp_to_tls(socket_state(), list()) -> ejabberd_tls:tls_socket().
tcp_to_tls(#socket_state{receiver = Receiver}, TLSOpts) ->
    SanitizedTLSOpts = case lists:keyfind(protocol_options, 1, TLSOpts) of
        false -> [{protocol_options, "no_sslv2|no_sslv3|no_tlsv1|no_tlsv1_1"} | TLSOpts];
        {_, ProtoOpts} ->
            NewProtoOpts = {protocol_options, string:join(ProtoOpts, "|")},
            lists:keyreplace(protocol_options, 1, TLSOpts, NewProtoOpts)
    end,
    ejabberd_receiver:starttls(Receiver, SanitizedTLSOpts).

get_tls_socket(#socket_state{receiver = Receiver}) ->
    case ejabberd_receiver:get_socket(Receiver) of
        {ok, TLSSocket} -> TLSSocket;
        _ -> invalid_socket
    end.


-spec starttls(socket_state(), list()) -> socket_state().
starttls(SocketData, TLSOpts) ->
    tcp_to_tls(SocketData, TLSOpts),
    case get_tls_socket(SocketData) of
        invalid_socket ->
            exit(invalid_socket_after_upgrade_to_tls);
        NewSocket ->
            SocketData#socket_state{socket = NewSocket, sockmod = ejabberd_tls}
    end.


-spec starttls(socket_state(), _, _) -> socket_state().
starttls(SocketData, TLSOpts, Data) ->
    tcp_to_tls(SocketData, TLSOpts),
    send(SocketData, Data), %% send last negotiation chunk via tcp
    NewSocket = get_tls_socket(SocketData),
    SocketData#socket_state{socket = NewSocket, sockmod = ejabberd_tls}.

-spec compress(socket_state(), integer(), _) -> socket_state().
compress(SocketData, InflateSizeLimit, Data) ->
    {ok, ZlibSocket} = ejabberd_zlib:enable_zlib(
                         SocketData#socket_state.sockmod,
                         SocketData#socket_state.socket,
                         InflateSizeLimit),
    ejabberd_receiver:compress(SocketData#socket_state.receiver, ZlibSocket),
    send(SocketData, Data),
    SocketData#socket_state{socket = ZlibSocket, sockmod = ejabberd_zlib}.

%% @doc sockmod=gen_tcp|fast_tls|ejabberd_zlib (ejabberd:sockmod())
send(SocketData, Data) ->
    case catch (SocketData#socket_state.sockmod):send(
             SocketData#socket_state.socket, Data) of
        ok -> ok;
        {error, timeout} ->
            ?INFO_MSG("Timeout on ~p:send", [SocketData#socket_state.sockmod]),
            exit(normal);
        Error ->
            ?DEBUG("Error in ~p:send: ~p", [SocketData#socket_state.sockmod, Error]),
            exit(normal)
    end.


%% @doc Can only be called when in c2s StateData#state.xml_socket is true
%% This function is used for HTTP bind
%% sockmod=mod_bosh_socket|mod_websockets or any custom module
-spec send_xml(socket_state(), mongoose_transport:send_xml_input()) -> ok.
send_xml(SocketData, Data) ->
    catch (SocketData#socket_state.sockmod):send_xml(
            SocketData#socket_state.socket, Data).


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


-spec get_sockmod(socket_state()) -> ejabberd:sockmod().
get_sockmod(SocketData) ->
    SocketData#socket_state.sockmod.


-spec get_peer_certificate(socket_state()) -> mongoose_transport:peercert_return().
get_peer_certificate(#socket_state{sockmod = ejabberd_tls, socket = Socket}) ->
    ejabberd_tls:get_peer_certificate(Socket);
get_peer_certificate(_SocketData) ->
    no_peer_cert.


-spec close(socket_state()) -> ok.
close(SocketData) ->
    ejabberd_receiver:close(SocketData#socket_state.receiver).


-spec sockname(socket_state()) -> mongoose_transport:peername_return().
sockname(#socket_state{sockmod = SockMod, socket = Socket}) ->
    case SockMod of
        gen_tcp ->
            inet:sockname(Socket);
        _ ->
            SockMod:sockname(Socket)
    end.


-spec peername(socket_state()) -> mongoose_transport:peername_return().
peername(#socket_state{sockmod = SockMod, socket = Socket}) ->
    case SockMod of
        gen_tcp ->
            inet:peername(Socket);
        _ ->
            SockMod:peername(Socket)
    end.
