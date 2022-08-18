-module(mongoose_c2s_socket).

-include("mongoose_logger.hrl").

-export([new_socket/2,
         tcp_to_tls/2,
         handle_socket_data/2,
         activate_socket/1,
         close/1,
         send_text/2,
         has_peer_cert/2,
         is_channel_binding_supported/1,
         is_ssl/1,
         get_ip/1
        ]).

-record(c2s_socket, {
          transport :: transport(),
          socket :: ranch_transport:socket(),
          ip :: undefined | {inet:ip_address(), inet:port_number()},
          ranch_ref :: ranch:ref()
         }).
-type socket() :: #c2s_socket{}.
-type transport() :: ranch_tcp | ranch_ssl | fast_tls.
-export_type([transport/0, socket/0]).

%%%----------------------------------------------------------------------
%%% socket helpers
%%%----------------------------------------------------------------------

-spec new_socket(ranch:ref(), mongoose_listener:options()) -> socket().
new_socket(RanchRef, Opts = #{proxy_protocol := true}) ->
    {ok, #{src_address := PeerIp, src_port := PeerPort}} = ranch:recv_proxy_header(RanchRef, 1000),
    verify_ip_is_not_blacklisted(PeerIp),
    {ok, TcpSocket} = ranch:handshake(RanchRef),
    Ip = {PeerIp, PeerPort},
    handle_socket_and_ssl_config(RanchRef, Opts, TcpSocket, Ip);
new_socket(RanchRef, Opts = #{proxy_protocol := false}) ->
    {ok, TcpSocket} = ranch:handshake(RanchRef),
    {ok, {PeerIp, _PeerPort} = Ip} = ranch_tcp:peername(TcpSocket),
    verify_ip_is_not_blacklisted(PeerIp),
    handle_socket_and_ssl_config(RanchRef, Opts, TcpSocket, Ip).

handle_socket_and_ssl_config(
  RanchRef, #{tls := #{mode := tls, module := TlsMod, opts := TlsOpts}}, TcpSocket, Ip) ->
    case tcp_to_tls(TlsMod, TcpSocket, TlsOpts) of
        {ok, TlsSocket} ->
            C2SSocket = #c2s_socket{transport = TlsMod, socket = TlsSocket,
                                    ip = Ip, ranch_ref = RanchRef},
            activate_socket(C2SSocket),
            C2SSocket;
        {error, closed} ->
            throw({stop, {shutdown, tls_closed}});
        {error, timeout} ->
            throw({stop, {shutdown, tls_timeout}});
        {error, {tls_alert, TlsAlert}} ->
            throw({stop, TlsAlert})
    end;
handle_socket_and_ssl_config(RanchRef, _Opts, TcpSocket, Ip) ->
    C2SSocket = #c2s_socket{transport = ranch_tcp, socket = TcpSocket,
                            ip = Ip, ranch_ref = RanchRef},
    activate_socket(C2SSocket),
    C2SSocket.

-spec tcp_to_tls(socket(), mongoose_listener:options()) -> {ok, socket()} | {error, term()}.
tcp_to_tls(#c2s_socket{transport = ranch_tcp, socket = TcpSocket} = C2SSocket,
           #{tls := #{module := TlsMod, opts := TlsOpts}}) ->
    ranch_tcp:setopts(TcpSocket, [{active, false}]),
    case tcp_to_tls(TlsMod, TcpSocket, TlsOpts) of
        {ok, TlsSocket} ->
            C2SSocket1 = C2SSocket#c2s_socket{transport = TlsMod, socket = TlsSocket},
            activate_socket(C2SSocket1),
            {ok, C2SSocket1};
        {error, Reason} ->
            {error, Reason}
    end;
tcp_to_tls(_, _) ->
    {error, already_tls_connection}.

tcp_to_tls(fast_tls, TcpSocket, TlsOpts) ->
    case fast_tls:tcp_to_tls(TcpSocket, TlsOpts) of
        {ok, TlsSocket} ->
            fast_tls:recv_data(TlsSocket, <<>>),
            {ok, TlsSocket};
        Other -> Other
    end;
tcp_to_tls(just_tls, TcpSocket, TlsOpts) ->
    case ranch_ssl:handshake(TcpSocket, TlsOpts, 1000) of
        {ok, TlsSocket, _} -> {ok, TlsSocket};
        Other -> Other
    end.

verify_ip_is_not_blacklisted(PeerIp) ->
    case mongoose_hooks:check_bl_c2s(PeerIp) of
        true ->
            ?LOG_INFO(#{what => c2s_blacklisted_ip, ip => PeerIp,
                        text => <<"Connection attempt from blacklisted IP">>}),
            throw({stop, {shutdown, ip_blacklisted}});
        false ->
            ok
    end.

-spec handle_socket_data(socket(), {tcp | ssl, term(), iodata()}) ->
    iodata() | {error, term()}.
handle_socket_data(#c2s_socket{transport = fast_tls, socket = TlsSocket}, {tcp, _Socket, Data}) ->
    mongoose_metrics:update(global, [data, xmpp, received, encrypted_size], iolist_size(Data)),
    case fast_tls:recv_data(TlsSocket, Data) of
        {ok, DecryptedData} ->
            DecryptedData;
        {error, Reason} ->
            {error, Reason}
    end;
handle_socket_data(#c2s_socket{transport = ranch_ssl, socket = Socket}, {ssl, Socket, Data}) ->
    mongoose_metrics:update(global, [data, xmpp, received, encrypted_size], iolist_size(Data)),
    Data;
handle_socket_data(#c2s_socket{transport = ranch_tcp, socket = Socket}, {tcp, Socket, Data}) ->
    Data;
handle_socket_data(_, _) ->
    {error, bad_packet}.

-spec activate_socket(socket()) -> ok | {error, term()}.
activate_socket(#c2s_socket{transport = fast_tls, socket = Socket}) ->
    fast_tls:setopts(Socket, [{active, once}]);
activate_socket(#c2s_socket{transport = ranch_ssl, socket = Socket}) ->
    ranch_ssl:setopts(Socket, [{active, once}]);
activate_socket(#c2s_socket{transport = ranch_tcp, socket = Socket}) ->
    ranch_tcp:setopts(Socket, [{active, once}]).

-spec close(socket()) -> ok.
close(#c2s_socket{transport = fast_tls, socket = Socket}) when Socket =/= undefined ->
    fast_tls:close(Socket);
close(#c2s_socket{transport = ranch_ssl, socket = Socket}) when Socket =/= undefined ->
    ranch_ssl:close(Socket);
close(#c2s_socket{transport = ranch_tcp, socket = Socket}) when Socket =/= undefined ->
    ranch_tcp:close(Socket);
close(_) ->
    ok.

-spec send_text(socket(), iodata()) -> ok | {error, term()}.
send_text(#c2s_socket{transport = fast_tls, socket = Socket}, Text) ->
    fast_tls:send(Socket, Text);
send_text(#c2s_socket{transport = ranch_ssl, socket = Socket}, Text) ->
    ranch_ssl:send(Socket, Text);
send_text(#c2s_socket{transport = ranch_tcp, socket = Socket}, Text) ->
    ranch_tcp:send(Socket, Text).

-spec has_peer_cert(socket(), mongoose_listener:options()) -> boolean().
has_peer_cert(#c2s_socket{transport = fast_tls, socket = Socket}, #{tls := TlsOpts}) ->
    case {fast_tls:get_verify_result(Socket), fast_tls:get_peer_certificate(Socket), TlsOpts} of
        {0, {ok, _}, _} -> true;
        {18, {ok, _}, #{verify_mode := selfsigned_peer}} -> true;
        {_, {ok, _}, _} -> false;
        {_, error, _} -> false
    end;
has_peer_cert(#c2s_socket{transport = ranch_ssl, socket = Socket}, _) ->
    case ssl:peercert(Socket) of
        {ok, _PeerCert} -> true;
        _ -> false
    end;
has_peer_cert(#c2s_socket{transport = ranch_tcp}, _) ->
    false.

-spec is_channel_binding_supported(socket()) -> boolean().
is_channel_binding_supported(#c2s_socket{transport = Transport}) ->
    fast_tls =:= Transport.

-spec is_ssl(socket()) -> term().
is_ssl(#c2s_socket{transport = Transport}) ->
    ranch_tcp =/= Transport.

-spec get_ip(socket()) -> term().
get_ip(#c2s_socket{ip = Ip}) ->
    Ip.
