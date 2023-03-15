-module(mongoose_c2s_ranch).
-behaviour(mongoose_c2s_socket).

-export([socket_new/2,
         socket_peername/1,
         tcp_to_tls/2,
         socket_handle_data/2,
         socket_activate/1,
         socket_close/1,
         socket_send_xml/2,
         get_peer_certificate/2,
         has_peer_cert/2,
         is_channel_binding_supported/1,
         get_tls_last_message/1,
         is_ssl/1]).

-record(state, {
          transport :: transport(),
          ranch_ref :: ranch:ref(),
          socket :: ranch_transport:socket(),
          ip :: {inet:ip_address(), inet:port_number()}
         }).

-type state() :: #state{}.
-type transport() :: ranch_tcp | just_tls | fast_tls.

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
tcp_to_tls(#state{socket = TcpSocket} = State, #{tls := #{module := TlsMod} = TlsConfig}) ->
    case tcp_to_tls(TlsMod, TcpSocket, TlsConfig) of
        {ok, TlsSocket} ->
            {ok, State#state{transport = TlsMod, socket = TlsSocket}};
        {error, Reason} ->
            {error, Reason}
    end.

tcp_to_tls(fast_tls, TcpSocket, TlsConfig) ->
    PreparedOpts = mongoose_tls:prepare_options(fast_tls, maps:remove(module, TlsConfig)),
    ranch_tcp:setopts(TcpSocket, [{active, false}]),
    case fast_tls:tcp_to_tls(TcpSocket, PreparedOpts) of
        {ok, TlsSocket} ->
            fast_tls:recv_data(TlsSocket, <<>>),
            {ok, TlsSocket};
        Other -> Other
    end;
tcp_to_tls(just_tls, TcpSocket, TlsConfig) ->
    case just_tls:tcp_to_tls(TcpSocket, TlsConfig) of
        {ok, TlsSocket} -> {ok, TlsSocket};
        Other -> Other
    end.

-spec socket_handle_data(state(), {tcp | ssl, term(), iodata()}) ->
  iodata() | {raw, [exml:element()]} | {error, term()}.
socket_handle_data(#state{transport = fast_tls, socket = TlsSocket}, {tcp, _, Data}) ->
    case fast_tls:recv_data(TlsSocket, Data) of
        {ok, DecryptedData} ->
            DataSize = byte_size(DecryptedData),
            mongoose_metrics:update(global, [data, xmpp, received, c2s, tls], DataSize),
            DecryptedData;
        {error, Reason} ->
            {error, Reason}
    end;
socket_handle_data(#state{transport = just_tls}, {ssl, _, Data}) ->
    mongoose_metrics:update(global, [data, xmpp, received, c2s, tls], byte_size(Data)),
    Data;
socket_handle_data(#state{transport = ranch_tcp, socket = Socket}, {tcp, Socket, Data}) ->
    mongoose_metrics:update(global, [data, xmpp, received, c2s, tcp], byte_size(Data)),
    Data.

-spec socket_activate(state()) -> ok.
socket_activate(#state{transport = fast_tls, socket = Socket}) ->
    fast_tls:setopts(Socket, [{active, once}]);
socket_activate(#state{transport = just_tls, socket = Socket}) ->
    just_tls:setopts(Socket, [{active, once}]);
socket_activate(#state{transport = ranch_tcp, socket = Socket}) ->
    ranch_tcp:setopts(Socket, [{active, once}]).

-spec socket_close(state()) -> ok.
socket_close(#state{transport = fast_tls, socket = Socket}) ->
    fast_tls:close(Socket);
socket_close(#state{transport = just_tls, socket = Socket}) ->
    just_tls:close(Socket);
socket_close(#state{transport = ranch_tcp, socket = Socket}) ->
    ranch_tcp:close(Socket).

-spec socket_send_xml(state(), iodata() | exml_stream:element() | [exml_stream:element()]) ->
    ok | {error, term()}.
socket_send_xml(#state{transport = Transport, socket = Socket}, XML) ->
    Text = exml:to_iolist(XML),
    case send(Transport, Socket, Text) of
        ok ->
            ok;
        Error ->
            Error
        end.

-spec send(transport(), ranch_transport:socket(), iodata()) -> ok | {error, term()}.
send(fast_tls, Socket, Data) ->
    mongoose_metrics:update(global, [data, xmpp, sent, c2s, tls], iolist_size(Data)),
    fast_tls:send(Socket, Data);
send(just_tls, Socket, Data) ->
    mongoose_metrics:update(global, [data, xmpp, sent, c2s, tls], iolist_size(Data)),
    just_tls:send(Socket, Data);
send(ranch_tcp, Socket, Data) ->
    mongoose_metrics:update(global, [data, xmpp, sent, c2s, tcp], iolist_size(Data)),
    ranch_tcp:send(Socket, Data).

-spec get_peer_certificate(state(), mongoose_listener:options()) ->
    mongoose_c2s_socket:peercert_return().
get_peer_certificate(#state{transport = fast_tls, socket = Socket}, #{tls := TlsOpts}) ->
    case {fast_tls:get_verify_result(Socket), fast_tls:get_peer_certificate(Socket), TlsOpts} of
        {0, {ok, Cert}, _} -> {ok, Cert};
        %% 18 is OpenSSL's and fast_tls's error code for self-signed certs
        {18, {ok, Cert}, #{verify_mode := selfsigned_peer}} -> {ok, Cert};
        {Error, {ok, Cert}, _} -> {bad_cert, fast_tls:get_cert_verify_string(Error, Cert)};
        {_, error, _} -> no_peer_cert
    end;
get_peer_certificate(#state{transport = just_tls, socket = Socket}, _) ->
    just_tls:get_peer_certificate(Socket);
get_peer_certificate(#state{transport = ranch_tcp}, _) ->
    no_peer_cert.

-spec has_peer_cert(state(), mongoose_listener:options()) -> boolean().
has_peer_cert(State, LOpts) ->
    case get_peer_certificate(State, LOpts) of
        {ok, _} -> true;
        _ -> false
    end.

-spec is_channel_binding_supported(state()) -> boolean().
is_channel_binding_supported(#state{transport = Transport}) ->
    fast_tls == Transport.

-spec get_tls_last_message(state()) -> {ok, binary()} | {error, term()}.
get_tls_last_message(#state{transport = fast_tls, socket = Socket}) ->
    fast_tls:get_tls_last_message(peer, Socket);
get_tls_last_message(_) ->
    {error, undefined}.

-spec is_ssl(state()) -> boolean().
is_ssl(#state{transport = Transport}) ->
    ranch_tcp /= Transport.
