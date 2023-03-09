%%%----------------------------------------------------------------------
%%% File    : mongoose_transport.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : tansport module for s2s and components connection
%%% Created : 18 Jan 2017
%%%----------------------------------------------------------------------

-module(mongoose_transport).
-author('piotr.nosek@erlang-solutions.com').

-include("mongoose.hrl").

-include_lib("public_key/include/public_key.hrl").
%%----------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------

-type t() :: any().
-type send_xml_input() :: {xmlstreamelement, exml:element()}
                        | jlib:xmlstreamstart()
                        | jlib:xmlstreamend().
-type peer() :: {inet:ip_address(), inet:port_number()}.
-type peername_return() :: {ok, peer()} | {error, inet:posix()}.
-type peercert_return() :: no_peer_cert | {ok, #'Certificate'{}}.

-export_type([t/0, send_xml_input/0, peer/0, peername_return/0, peercert_return/0]).

-record(socket_data, {sockmod    :: gen_tcp | mongoose_tls,
                      socket     :: term(),
                      receiver   :: pid() | atom() | tuple(),
                      connection_details :: mongoose_tcp_listener:connection_details()
                     }).

-type socket_data() :: #socket_data{}.


%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------

-export([accept/4, connect/4, close/1, send/2]).
-export([wait_for_tls_handshake/2, wait_for_tls_handshake/3,
         connect_tls/2, get_peer_certificate/1]).
-export([monitor/1, peername/1, change_shaper/2]).

-spec accept(module(), gen_tcp:socket(),
            mongoose_tcp_listener:options(),
            mongoose_tcp_listener:connection_details()) -> ok.
accept(Module, Socket, Opts, ConnectionDetails) ->
    RecPid = ejabberd_receiver:start(Socket, none, Opts),
    SocketData = #socket_data{sockmod = gen_tcp,
                              socket = Socket,
                              receiver = RecPid,
                              connection_details = ConnectionDetails},
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

-spec connect(Addr :: atom() | string() | inet:ip_address(),
              Port :: inet:port_number(),
              Opts :: [gen_tcp:connect_option()],
              Timeout :: non_neg_integer() | infinity
              ) -> {'error', atom()} | {'ok', socket_data()}.
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
            SocketData = #socket_data{sockmod = gen_tcp,
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

-spec close(socket_data()) -> ok.
close(SocketData) ->
    ejabberd_receiver:close(SocketData#socket_data.receiver).

-spec wait_for_tls_handshake(socket_data(), mongoose_tls:options()) -> socket_data().
wait_for_tls_handshake(#socket_data{receiver = Receiver} = SocketData, TLSOpts) ->
    ejabberd_receiver:starttls(Receiver, TLSOpts#{connect => false}),
    case ejabberd_receiver:get_socket(Receiver) of
        {ok, TLSSocket} ->
            SocketData#socket_data{socket = TLSSocket, sockmod = mongoose_tls};
        invalid_socket ->
            exit(invalid_socket_after_upgrade_to_tls)
    end.

-spec wait_for_tls_handshake(socket_data(), mongoose_tls:options(), binary()) -> socket_data().
wait_for_tls_handshake(#socket_data{receiver = Receiver} = SocketData, TLSOpts, Data) ->
    ejabberd_receiver:starttls(Receiver, TLSOpts#{connect => false}),
    send(SocketData, Data), %% send last negotiation chunk via tcp
    case ejabberd_receiver:get_socket(Receiver) of
        {ok, TLSSocket} ->
            SocketData#socket_data{socket = TLSSocket, sockmod = mongoose_tls};
        invalid_socket ->
            exit(invalid_socket_after_upgrade_to_tls)
    end.

-spec connect_tls(socket_data(), mongoose_tls:options()) -> socket_data().
connect_tls(#socket_data{receiver = Receiver} = SocketData, TLSOpts) ->
    ejabberd_receiver:starttls(Receiver, TLSOpts#{connect => true}),
    case ejabberd_receiver:get_socket(Receiver) of
        {ok, TLSSocket} ->
            SocketData#socket_data{socket = TLSSocket, sockmod = mongoose_tls};
        invalid_socket ->
            exit(invalid_socket_after_upgrade_to_tls)
    end.

send(#socket_data{sockmod = SockMod, socket = Socket}, Data) ->
    case catch SockMod:send(Socket, Data) of
        ok -> ok;
        {error, timeout} ->
            ?LOG_INFO(#{what => socket_error, reason => timeout,
                        socket => SockMod}),
            exit(normal);
        Error ->
            ?LOG_INFO(#{what => socket_error, reason => Error,
                        socket => SockMod}),
            exit(normal)
    end.

-spec get_peer_certificate(socket_data()) -> mongoose_tls:cert().
get_peer_certificate(#socket_data{sockmod = mongoose_tls, socket = Socket}) ->
    mongoose_tls:get_peer_certificate(Socket);
get_peer_certificate(_SocketData) ->
    no_peer_cert.

-spec peername(socket_data()) -> mongoose_transport:peername_return().
peername(#socket_data{connection_details = #{src_address := SrcAddr,
                                             src_port := SrcPort}}) ->
    {ok, {SrcAddr, SrcPort}}.

-spec monitor(socket_data()) -> reference().
monitor(#socket_data{receiver = Receiver}) ->
    erlang:monitor(process, Receiver).

-spec change_shaper(socket_data(), _) -> any().
change_shaper(#socket_data{receiver = Receiver}, Shaper)  ->
    ejabberd_receiver:change_shaper(Receiver, Shaper).

