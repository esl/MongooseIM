-module(mongoose_c2s_ranch).
-behaviour(mongoose_c2s_socket).

-export([socket_new/2,
         socket_peername/1,
         tcp_to_tls/2,
         socket_handle_data/2,
         socket_activate/1,
         socket_close/1,
         socket_send_xml/2,
         has_peer_cert/2,
         is_channel_binding_supported/1,
         is_ssl/1]).

-record(state, {
          transport :: transport(),
          ranch_ref :: ranch:ref(),
          socket :: ranch_transport:socket(),
          ip :: {inet:ip_address(), inet:port_number()}
         }).

-type state() :: #state{}.
-type transport() :: ranch_tcp | just_tls | fast_tls. % just_tls = ranch_ssl

-spec socket_new(term(), mongoose_listener:options()) -> state().
socket_new({ranch_tcp, RanchRef}, #{proxy_protocol := true}) ->
    {ok, #{src_address := PeerIp, src_port := PeerPort}} = ranch:recv_proxy_header(RanchRef, 1000),
    {ok, TcpSocket} = ranch:handshake(RanchRef),
    #state{
        transport = ranch_tcp,
        ranch_ref = RanchRef,
        socket = TcpSocket,
        ip = {PeerIp, PeerPort}};
socket_new({ranch_tcp, RanchRef}, #{proxy_protocol := false}) ->
    {ok, TcpSocket} = ranch:handshake(RanchRef),
    {ok, Ip} = ranch_tcp:peername(TcpSocket),
    #state{
        transport = ranch_tcp,
        ranch_ref = RanchRef,
        socket = TcpSocket,
        ip = Ip}.

-spec socket_peername(state()) -> {inet:ip_address(), inet:port_number()}.
socket_peername(#state{ip = Ip}) ->
    Ip.

-spec tcp_to_tls(state(), mongoose_listener:options()) ->
  {ok, state()} | {error, term()}.
tcp_to_tls(#state{socket = TcpSocket} = State, #{tls := #{module := TlsMod, opts := TlsOpts}}) ->
    ranch_tcp:setopts(TcpSocket, [{active, false}]),
    case tcp_to_tls(TlsMod, TcpSocket, TlsOpts) of
        {ok, TlsSocket} ->
            {ok, State#state{transport = TlsMod, socket = TlsSocket}};
        {error, Reason} ->
            {error, Reason}
    end.

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

-spec socket_handle_data(state(), {tcp | ssl, term(), iodata()}) ->
  iodata() | {raw, [exml:element()]} | {error, term()}.
socket_handle_data(#state{transport = fast_tls, socket = TlsSocket}, {tcp, _Socket, Data}) ->
    mongoose_metrics:update(global, [data, xmpp, received, encrypted_size], iolist_size(Data)),
    case fast_tls:recv_data(TlsSocket, Data) of
        {ok, DecryptedData} ->
            DecryptedData;
        {error, Reason} ->
            {error, Reason}
    end;
socket_handle_data(#state{transport = just_tls, socket = Socket}, {ssl, Socket, Data}) ->
    mongoose_metrics:update(global, [data, xmpp, received, encrypted_size], iolist_size(Data)),
    Data;
socket_handle_data(#state{transport = ranch_tcp, socket = Socket}, {tcp, Socket, Data}) ->
    Data.

-spec socket_activate(state()) -> ok.
socket_activate(#state{transport = fast_tls, socket = Socket}) ->
    fast_tls:setopts(Socket, [{active, once}]);
socket_activate(#state{transport = just_tls, socket = Socket}) ->
    ranch_ssl:setopts(Socket, [{active, once}]);
socket_activate(#state{transport = ranch_tcp, socket = Socket}) ->
    ranch_tcp:setopts(Socket, [{active, once}]).

-spec socket_close(state()) -> ok.
socket_close(#state{transport = fast_tls, socket = Socket}) ->
    fast_tls:close(Socket);
socket_close(#state{transport = just_tls, socket = Socket}) ->
    ranch_ssl:close(Socket);
socket_close(#state{transport = ranch_tcp, socket = Socket}) ->
    ranch_tcp:close(Socket).

-spec socket_send_xml(state(), iodata() | exml_stream:element() | [exml_stream:element()]) ->
    ok | {error, term()}.
socket_send_xml(#state{transport = Transport, socket = Socket}, XML) ->
    Text = exml:to_iolist(XML),
    case send(Transport, Socket, Text) of
        ok ->
            mongoose_metrics:update(global, [data, xmpp, sent, xml_stanza_size], iolist_size(Text)),
            ok;
        Error ->
            Error
        end.

send(fast_tls, Socket, Data) ->
    fast_tls:send(Socket, Data);
send(just_tls, Socket, Data) ->
    ranch_ssl:send(Socket, Data);
send(ranch_tcp, Socket, Data) ->
    ranch_tcp:send(Socket, Data).

-spec has_peer_cert(mongoose_c2s_socket:state(), mongoose_listener:options()) -> boolean().
has_peer_cert(#state{transport = fast_tls, socket = Socket}, #{tls := TlsOpts}) ->
    case {fast_tls:get_verify_result(Socket), fast_tls:get_peer_certificate(Socket), TlsOpts} of
        {0, {ok, _}, _} -> true;
        %% 18 is OpenSSL's and fast_tls's error code for self-signed certs
        {18, {ok, _}, #{verify_mode := selfsigned_peer}} -> true;
        {_, {ok, _}, _} -> false;
        {_, error, _} -> false
    end;
has_peer_cert(#state{transport = just_tls, socket = Socket}, _) ->
    case ssl:peercert(Socket) of
        {ok, _PeerCert} -> true;
        _ -> false
    end;
has_peer_cert(#state{transport = ranch_tcp}, _) ->
    false.

-spec is_channel_binding_supported(mongoose_c2s_socket:state()) -> boolean().
is_channel_binding_supported(#state{transport = Transport}) ->
    fast_tls == Transport.

-spec is_ssl(mongoose_c2s_socket:state()) -> boolean().
is_ssl(#state{transport = Transport}) ->
    ranch_tcp /= Transport.
