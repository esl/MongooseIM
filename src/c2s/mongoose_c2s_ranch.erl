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
         export_key_materials/5,
         is_ssl/1]).

-record(state, {
          transport :: transport(),
          ranch_ref :: ranch:ref(),
          socket :: ranch_transport:socket(),
          ip :: {inet:ip_address(), inet:port_number()}
         }).

-type state() :: #state{}.
-type transport() :: ranch_tcp | just_tls.

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
tcp_to_tls(#state{socket = TcpSocket} = State, #{tls := TlsConfig}) ->
    case do_tcp_to_tls(TcpSocket, TlsConfig) of
        {ok, TlsSocket} ->
            {ok, State#state{transport = just_tls, socket = TlsSocket}};
        {error, Reason} ->
            {error, Reason}
    end.

do_tcp_to_tls(TcpSocket, TlsConfig) ->
    case just_tls:tcp_to_tls(TcpSocket, TlsConfig) of
        {ok, TlsSocket} -> {ok, TlsSocket};
        Other -> Other
    end.

-spec socket_handle_data(state(), {tcp | ssl, term(), iodata()}) ->
  iodata() | {raw, [exml:element()]} | {error, term()}.
socket_handle_data(#state{transport = just_tls}, {ssl, _, Data}) ->
    mongoose_instrument:execute(c2s_tls_data_in, #{}, #{byte_size => iolist_size(Data)}),
    Data;
socket_handle_data(#state{transport = ranch_tcp, socket = Socket}, {tcp, Socket, Data}) ->
    mongoose_instrument:execute(c2s_tcp_data_in, #{}, #{byte_size => iolist_size(Data)}),
    Data.

-spec socket_activate(state()) -> ok.
socket_activate(#state{transport = just_tls, socket = Socket}) ->
    just_tls:setopts(Socket, [{active, once}]);
socket_activate(#state{transport = ranch_tcp, socket = Socket}) ->
    ranch_tcp:setopts(Socket, [{active, once}]).

-spec socket_close(state()) -> ok.
socket_close(#state{transport = just_tls, socket = Socket}) ->
    just_tls:close(Socket);
socket_close(#state{transport = ranch_tcp, socket = Socket}) ->
    ranch_tcp:close(Socket).

-spec socket_send_xml(state(), exml_stream:element() | [exml_stream:element()]) ->
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
send(just_tls, Socket, Data) ->
    mongoose_instrument:execute(c2s_tls_data_out, #{}, #{byte_size => iolist_size(Data)}),
    just_tls:send(Socket, Data);
send(ranch_tcp, Socket, Data) ->
    mongoose_instrument:execute(c2s_tcp_data_out, #{}, #{byte_size => iolist_size(Data)}),
    ranch_tcp:send(Socket, Data).

-spec get_peer_certificate(state(), mongoose_listener:options()) ->
    mongoose_c2s_socket:peercert_return().
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
-spec export_key_materials(state(), Labels, Contexts, WantedLengths, ConsumeSecret) ->
    {ok, ExportKeyMaterials} |
    {error, undefined_tls_material | exporter_master_secret_already_consumed | bad_input}
      when
      Labels :: [binary()],
      Contexts :: [binary() | no_context],
      WantedLengths :: [non_neg_integer()],
      ConsumeSecret :: boolean(),
      ExportKeyMaterials :: binary() | [binary()].

-if(?OTP_RELEASE >= 27).
is_channel_binding_supported(#state{transport = Transport}) ->
    Transport =:= just_tls.
export_key_materials(#state{transport = just_tls, socket = SslSocket}, Labels, Contexts, WantedLengths, ConsumeSecret) ->
    just_tls:export_key_materials(SslSocket, Labels, Contexts, WantedLengths, ConsumeSecret);
export_key_materials(#state{}, _, _, _, _) ->
    {error, undefined_tls_material}.
-else.
is_channel_binding_supported(#state{transport = _}) ->
    false.
export_key_materials(_SslSocket, _, _, _, _) ->
    {error, undefined_tls_material}.
-endif.

-spec is_ssl(state()) -> boolean().
is_ssl(#state{transport = Transport}) ->
    ranch_tcp /= Transport.
