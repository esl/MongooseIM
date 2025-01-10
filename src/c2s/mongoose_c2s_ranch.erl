-module(mongoose_c2s_ranch).
-behaviour(mongoose_c2s_socket).

-export([new/3,
         socket_peername/1,
         tcp_to_tls/2,
         socket_handle_data/2,
         socket_activate/1,
         socket_close/1,
         socket_send_xml/2,
         get_peer_certificate/1,
         has_peer_cert/2,
         is_channel_binding_supported/1,
         export_key_materials/5,
         is_ssl/1]).

-record(ranch_tcp, {
          socket :: inet:socket(),
          ranch_ref :: ranch:ref(),
          ip :: {inet:ip_address(), inet:port_number()}
         }).

-record(ranch_ssl, {
          socket :: ssl:sslsocket(),
          ranch_ref :: ranch:ref(),
          ip :: {inet:ip_address(), inet:port_number()},
          verify_results = [] :: list()
         }).

-type socket() :: #ranch_tcp{} | #ranch_ssl{}.

-spec new(mongoose_listener:transport_module(), ranch:ref(), mongoose_listener:options()) -> socket().
new(ranch_tcp, Ref, Opts) ->
    {ok, Socket, ConnectionDetails} = mongoose_listener:read_connection_details(Ref, ranch_tcp, Opts),
    #{src_address := PeerIp, src_port := PeerPort} = ConnectionDetails,
    #ranch_tcp{ranch_ref = Ref, socket = Socket, ip = {PeerIp, PeerPort}};
new(ranch_ssl, Ref, Opts) ->
    {ok, Socket, ConnectionDetails} = mongoose_listener:read_connection_details(Ref, ranch_ssl, Opts),
    #{src_address := PeerIp, src_port := PeerPort} = ConnectionDetails,
    #ranch_ssl{ranch_ref = Ref, socket = Socket, ip = {PeerIp, PeerPort}}.

-spec socket_peername(socket()) -> {inet:ip_address(), inet:port_number()}.
socket_peername(#ranch_tcp{ip = Ip}) ->
    Ip;
socket_peername(#ranch_ssl{ip = Ip}) ->
    Ip.

-spec tcp_to_tls(socket(), mongoose_listener:options()) ->
  {ok, socket()} | {error, term()}.
tcp_to_tls(#ranch_tcp{socket = TcpSocket, ranch_ref = Ref, ip = Ip}, #{tls := TlsConfig}) ->
    case do_tcp_to_tls(TcpSocket, TlsConfig) of
        {ok, TlsSocket, VerifyResults} ->
            {ok, #ranch_ssl{socket = TlsSocket, ranch_ref = Ref, ip = Ip,
                            verify_results = VerifyResults}};
        {error, Reason} ->
            {error, Reason}
    end;
tcp_to_tls(#ranch_ssl{}, _) ->
    {error, already_tls_connection}.

do_tcp_to_tls(TCPSocket, Options) ->
    inet:setopts(TCPSocket, [{active, false}]),
    {Ref, SSLOpts} = just_tls:prepare_connection(Options),
    Ret = ssl:handshake(TCPSocket, SSLOpts, 5000),
    VerifyResults = just_tls:receive_verify_results(Ref),
    case Ret of
        {ok, SSLSocket} ->
            {ok, SSLSocket, VerifyResults};
        _ -> Ret
    end.

-spec socket_handle_data(socket(), {tcp | ssl, term(), iodata()}) ->
  iodata() | {raw, [exml:element()]} | {error, term()}.
socket_handle_data(#ranch_ssl{}, {ssl, _, Data}) ->
    mongoose_instrument:execute(c2s_tls_data_in, #{}, #{byte_size => iolist_size(Data)}),
    Data;
socket_handle_data(#ranch_tcp{socket = Socket}, {tcp, Socket, Data}) ->
    mongoose_instrument:execute(c2s_tcp_data_in, #{}, #{byte_size => iolist_size(Data)}),
    Data.

-spec socket_activate(socket()) -> ok.
socket_activate(#ranch_ssl{socket = Socket}) ->
    ranch_ssl:setopts(Socket, [{active, once}]);
socket_activate(#ranch_tcp{socket = Socket}) ->
    ranch_tcp:setopts(Socket, [{active, once}]).

-spec socket_close(socket()) -> ok.
socket_close(#ranch_ssl{socket = Socket}) ->
    ranch_ssl:close(Socket);
socket_close(#ranch_tcp{socket = Socket}) ->
    ranch_tcp:close(Socket).

-spec socket_send_xml(socket(), exml_stream:element() | [exml_stream:element()]) ->
    ok | {error, term()}.
socket_send_xml(#ranch_ssl{socket = Socket}, XML) ->
    Data = exml:to_iolist(XML),
    mongoose_instrument:execute(c2s_tls_data_out, #{}, #{byte_size => iolist_size(Data)}),
    ranch_ssl:send(Socket, Data);
socket_send_xml(#ranch_tcp{socket = Socket}, XML) ->
    Data = exml:to_iolist(XML),
    mongoose_instrument:execute(c2s_tcp_data_out, #{}, #{byte_size => iolist_size(Data)}),
    ranch_tcp:send(Socket, Data).

-spec get_peer_certificate(socket()) -> mongoose_c2s_socket:peercert_return().
get_peer_certificate(#ranch_ssl{socket = Socket, verify_results = []}) ->
    case ssl:peercert(Socket) of
        {ok, PeerCert} ->
            {ok, public_key:pkix_decode_cert(PeerCert, plain)};
        _ -> no_peer_cert
    end;
get_peer_certificate(#ranch_ssl{verify_results = [Err | _]}) ->
    {bad_cert, just_tls:error_to_list(Err)};
get_peer_certificate(#ranch_tcp{}) ->
    no_peer_cert.

-spec has_peer_cert(socket(), mongoose_listener:options()) -> boolean().
has_peer_cert(State, LOpts) ->
    case get_peer_certificate(State) of
        {ok, _} -> true;
        _ -> false
    end.

-spec is_channel_binding_supported(socket()) -> boolean().
-spec export_key_materials(socket(), Labels, Contexts, WantedLengths, ConsumeSecret) ->
    {ok, ExportKeyMaterials} |
    {error, undefined_tls_material | exporter_master_secret_already_consumed | bad_input}
      when
      Labels :: [binary()],
      Contexts :: [binary() | no_context],
      WantedLengths :: [non_neg_integer()],
      ConsumeSecret :: boolean(),
      ExportKeyMaterials :: binary() | [binary()].

-if(?OTP_RELEASE >= 27).
is_channel_binding_supported(#ranch_ssl{}) ->
    true;
is_channel_binding_supported(#ranch_tcp{}) ->
    false.
export_key_materials(#ranch_ssl{socket = Socket}, Labels, Contexts, WantedLengths, ConsumeSecret) ->
    ssl:export_key_materials(Socket, Labels, Contexts, WantedLengths, ConsumeSecret);
export_key_materials(#ranch_tcp{}, _, _, _, _) ->
    {error, undefined_tls_material}.
-else.
is_channel_binding_supported(_) ->
    false.
export_key_materials(_, _, _, _, _) ->
    {error, undefined_tls_material}.
-endif.

-spec is_ssl(socket()) -> boolean().
is_ssl(#ranch_ssl{}) ->
    true;
is_ssl(#ranch_tcp{}) ->
    false.
