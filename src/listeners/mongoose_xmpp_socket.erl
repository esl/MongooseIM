-module(mongoose_xmpp_socket).

-include_lib("public_key/include/public_key.hrl").

-define(DEF_SOCKET_OPTS,
        binary, {active, false}, {packet, raw},
        {send_timeout, 15000}, {send_timeout_close, true}).

-export([accept/4,
         connect/4,
         handle_data/2,
         activate/1,
         close/1,
         is_channel_binding_supported/1,
         export_key_materials/5,
         get_peer_certificate/1,
         has_peer_cert/2,
         tcp_to_tls/3,
         is_ssl/1,
         send_xml/2]).

-export([get_ip/1,
         get_transport/1,
         get_conn_type/1]).

-callback peername(state()) -> mongoose_transport:peer().
-callback tcp_to_tls(state(), mongoose_listener:options(), side()) ->
    {ok, state()} | {error, term()}.
-callback handle_data(state(), {tcp | ssl, term(), binary()}) ->
    binary() | {raw, [exml:element()]} | {error, term()}.
-callback activate(state()) -> ok.
-callback close(state()) -> ok.
-callback send_xml(state(), iodata() | exml_stream:element() | [exml_stream:element()]) ->
    ok | {error, atom()}.
-callback get_peer_certificate(state()) -> peercert_return().
-callback has_peer_cert(state(), mongoose_listener:options()) -> boolean().
-callback is_channel_binding_supported(state()) -> boolean().
-callback export_key_materials(state(), Labels, Contexts, WantedLengths, ConsumeSecret) ->
    {ok, ExportKeyMaterials} |
    {error, atom() | exporter_master_secret_already_consumed | bad_input}
      when
      Labels :: [binary()],
      Contexts :: [binary() | no_context],
      WantedLengths :: [non_neg_integer()],
      ConsumeSecret :: boolean(),
      ExportKeyMaterials :: binary() | [binary()].
-callback is_ssl(state()) -> boolean().

-record(ranch_tcp, {
          socket :: inet:socket(),
          connection_type = mongoose_listener:connection_type(),
          ranch_ref :: ranch:ref(),
          ip :: mongoose_transport:peer()
         }).

-record(ranch_ssl, {
          socket :: ssl:sslsocket(),
          connection_type :: mongoose_listener:connection_type(),
          ranch_ref :: ranch:ref(),
          ip :: mongoose_transport:peer(),
          verify_results = [] :: list()
         }).

-record(xmpp_socket, {
          module :: module(),
          state :: state(),
          connection_type = mongoose_listener:connection_type(),
          ip :: mongoose_transport:peer()
         }).

-type socket() :: #ranch_tcp{} | #ranch_ssl{} | #xmpp_socket{}.

-type state() :: term().
-type side() :: client | server.
-type conn_type() :: tcp | tls.
-type peercert_return() :: no_peer_cert | {bad_cert, term()} | {ok, #'Certificate'{}}.
-type with_tls_opts() :: #{tls := just_tls:options(), _ => _}.
-export_type([socket/0, state/0, side/0, conn_type/0, peercert_return/0]).

-spec accept(mongoose_listener:transport_module(),
             mongoose_listener:connection_type(),
             ranch:ref(),
             mongoose_listener:options()) -> socket().
accept(ranch_tcp, Type, Ref, LOpts) ->
    {ok, Socket, ConnDetails} = mongoose_listener:read_connection_details(Ref, ranch_tcp, LOpts),
    #{src_address := PeerIp, src_port := PeerPort} = ConnDetails,
    SocketState = #ranch_tcp{socket = Socket, connection_type = Type,
                             ranch_ref = Ref, ip = {PeerIp, PeerPort}},
    activate(SocketState),
    SocketState;
accept(ranch_ssl, Type, Ref, LOpts) ->
    {ok, Socket, ConnDetails} = mongoose_listener:read_connection_details(Ref, ranch_ssl, LOpts),
    #{src_address := PeerIp, src_port := PeerPort} = ConnDetails,
    SocketState = #ranch_ssl{socket = Socket, connection_type = Type,
                             ranch_ref = Ref, ip = {PeerIp, PeerPort}},
    activate(SocketState),
    SocketState;
accept(Module, Type, State, _LOpts) ->
    PeerIp = Module:peername(State),
    SocketState = #xmpp_socket{module = Module, state = State,
                               connection_type = Type, ip = PeerIp},
    activate(SocketState),
    SocketState.

-spec connect(mongoose_addr_list:addr(),
              with_tls_opts(),
              mongoose_listener:connection_type(),
              timeout()) -> socket() | {error, timeout | inet:posix() | any()}.
connect(#{ip_tuple := Addr, ip_version := Inet, port := Port, tls := false}, Opts, Type, Timeout) ->
    SockOpts = socket_options(false, Inet, Opts),
    case gen_tcp:connect(Addr, Port, SockOpts, Timeout) of
        {ok, Socket} ->
            SocketState = #ranch_tcp{socket = Socket, connection_type = Type,
                                     ranch_ref = {self(), Type}, ip = {Addr, Port}},
            activate(SocketState),
            SocketState;
        {error, Reason} ->
            {error, Reason}
    end;
connect(#{ip_tuple := Addr, ip_version := Inet, port := Port, tls := true}, Opts, Type, Timeout) ->
    SockOpts = socket_options(true, Inet, Opts),
    case ssl:connect(Addr, Port, SockOpts, Timeout) of
        {ok, Socket} ->
            SocketState = #ranch_ssl{socket = Socket, connection_type = Type,
                                     ranch_ref = {self(), Type}, ip = {Addr, Port}},
            activate(SocketState),
            SocketState;
        {error, Reason} ->
            {error, Reason}
    end.

-spec socket_options(true, inet | inet6, with_tls_opts()) -> [ssl:tls_client_option()];
                    (false, inet | inet6, with_tls_opts()) -> [gen_tcp:connect_option()].
socket_options(true, Inet, #{tls := TlsOpts}) ->
    [Inet, ?DEF_SOCKET_OPTS | just_tls:make_client_opts(TlsOpts)];
socket_options(false, Inet, _) ->
    [Inet, ?DEF_SOCKET_OPTS].

-spec activate(socket()) -> ok | {error, term()}.
activate(#ranch_tcp{socket = Socket}) ->
    ranch_tcp:setopts(Socket, [{active, once}]);
activate(#ranch_ssl{socket = Socket}) ->
    ranch_ssl:setopts(Socket, [{active, once}]);
activate(#xmpp_socket{module = Module, state = State}) ->
    Module:activate(State).

-spec tcp_to_tls(socket(), with_tls_opts(), side()) -> {ok, socket()} | {error, term()}.
tcp_to_tls(#ranch_tcp{socket = TcpSocket, connection_type = Type, ranch_ref = Ref, ip = Ip},
           #{tls := TlsConfig}, Side) ->
    inet:setopts(TcpSocket, [{active, false}]),
    Ret = case Side of
        server ->
            SslOpts = just_tls:make_server_opts(TlsConfig),
            ssl:handshake(TcpSocket, SslOpts, 5000);
        client ->
            SslOpts = just_tls:make_client_opts(TlsConfig),
            ssl:connect(TcpSocket, SslOpts, 5000)
    end,
    VerifyResults = just_tls:receive_verify_results(),
    case Ret of
        {ok, SslSocket} ->
            ssl:setopts(SslSocket, [{active, once}]),
            {ok, #ranch_ssl{socket = SslSocket, connection_type = Type,
                            ranch_ref = Ref, ip = Ip, verify_results = VerifyResults}};
        {error, Reason} ->
            {error, Reason}
    end;
tcp_to_tls(#ranch_ssl{}, _, _) ->
    {error, already_tls_connection};
tcp_to_tls(#xmpp_socket{module = Module, state = State} = C2SSocket, LOpts, Mode) ->
    case Module:tcp_to_tls(State, LOpts, Mode) of
        {ok, NewState} ->
            {ok, C2SSocket#xmpp_socket{state = NewState}};
        Error ->
            Error
    end.

-spec handle_data(socket(), {tcp | ssl, term(), binary()}) ->
    binary() | {raw, [term()]} | {error, term()}.
handle_data(#ranch_tcp{connection_type = Type}, {tcp, _, Data}) ->
    mongoose_instrument:execute(tcp_data_in, #{connection_type => Type}, #{byte_size => iolist_size(Data)}),
    Data;
handle_data(#ranch_ssl{connection_type = Type}, {ssl, _, Data}) ->
    mongoose_instrument:execute(tls_data_in, #{connection_type => Type}, #{byte_size => iolist_size(Data)}),
    Data;
handle_data(#xmpp_socket{module = Module, state = State, connection_type = Type}, Payload) ->
    mongoose_instrument:execute(tcp_data_in, #{connection_type => Type}, #{byte_size => 0}),
    Module:handle_data(State, Payload).

-spec close(socket()) -> ok.
close(#ranch_tcp{socket = Socket}) ->
    ranch_tcp:close(Socket);
close(#ranch_ssl{socket = Socket}) ->
    ranch_ssl:close(Socket);
close(#xmpp_socket{module = Module, state = State}) ->
    Module:close(State).

-spec send_xml(socket(), exml_stream:element() | [exml_stream:element()]) -> ok | {error, atom()}.
send_xml(#ranch_tcp{socket = Socket, connection_type = Type}, XML) ->
    Data = exml:to_iolist(XML),
    mongoose_instrument:execute(tcp_data_out, #{connection_type => Type}, #{byte_size => iolist_size(Data)}),
    ranch_tcp:send(Socket, Data);
send_xml(#ranch_ssl{socket = Socket, connection_type = Type}, XML) ->
    Data = exml:to_iolist(XML),
    mongoose_instrument:execute(tls_data_out, #{connection_type => Type}, #{byte_size => iolist_size(Data)}),
    ranch_ssl:send(Socket, Data);
send_xml(#xmpp_socket{module = Module, state = State, connection_type = Type}, XML) ->
    mongoose_instrument:execute(tcp_data_out, #{connection_type => Type}, #{byte_size => exml:xml_size(XML)}),
    Module:send_xml(State, XML).

-spec get_peer_certificate(socket()) -> peercert_return().
get_peer_certificate(#ranch_tcp{}) ->
    no_peer_cert;
get_peer_certificate(#ranch_ssl{socket = Socket, verify_results = []}) ->
    case ssl:peercert(Socket) of
        {ok, PeerCert} ->
            {ok, public_key:pkix_decode_cert(PeerCert, plain)};
        _ -> no_peer_cert
    end;
get_peer_certificate(#ranch_ssl{verify_results = [Err | _]}) ->
    {bad_cert, just_tls:error_to_list(Err)};
get_peer_certificate(#xmpp_socket{module = Module, state = State}) ->
    Module:get_peer_certificate(State).

-spec has_peer_cert(socket(), mongoose_listener:options()) -> boolean().
has_peer_cert(Socket, _LOpts) ->
    case get_peer_certificate(Socket) of
        {ok, _} -> true;
        _ -> false
    end.

-spec is_ssl(socket()) -> boolean().
is_ssl(#ranch_tcp{}) ->
    false;
is_ssl(#ranch_ssl{}) ->
    true;
is_ssl(#xmpp_socket{module = Module, state = State}) ->
    Module:is_ssl(State).

-spec get_transport(socket()) -> module().
get_transport(#ranch_tcp{}) ->
    ranch_tcp;
get_transport(#ranch_ssl{}) ->
    ranch_ssl;
get_transport(#xmpp_socket{module = Module}) ->
    Module.

-spec get_conn_type(socket()) -> conn_type().
get_conn_type(Socket) ->
    case is_ssl(Socket) of
        true -> tls;
        false -> tcp
    end.

-spec get_ip(socket()) -> mongoose_transport:peer().
get_ip(#ranch_tcp{ip = Ip}) ->
    Ip;
get_ip(#ranch_ssl{ip = Ip}) ->
    Ip;
get_ip(#xmpp_socket{module = Module, state = State}) ->
    Module:peername(State).

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
    false;
is_channel_binding_supported(#xmpp_socket{module = Module, state = State}) ->
    Module:is_channel_binding_supported(State).

export_key_materials(#ranch_ssl{socket = Socket}, Labels, Contexts, WantedLengths, ConsumeSecret) ->
    ssl:export_key_materials(Socket, Labels, Contexts, WantedLengths, ConsumeSecret);
export_key_materials(#ranch_tcp{}, _, _, _, _) ->
    {error, undefined_tls_material};
export_key_materials(#xmpp_socket{module = Module, state = State},
                     Labels, Contexts, WantedLengths, ConsumeSecret) ->
    Module:export_key_materials(State, Labels, Contexts, WantedLengths, ConsumeSecret).
-else.
is_channel_binding_supported(#ranch_ssl{}) ->
    false;
is_channel_binding_supported(#ranch_tcp{}) ->
    false;
is_channel_binding_supported(#xmpp_socket{module = Module, state = State}) ->
    Module:is_channel_binding_supported(State).

export_key_materials(#ranch_tcp{}, _, _, _, _) ->
    {error, undefined_tls_material};
export_key_materials(#ranch_ssl{}, _, _, _, _) ->
    {error, undefined_tls_material};
export_key_materials(#xmpp_socket{module = Module, state = State},
                     Labels, Contexts, WantedLengths, ConsumeSecret) ->
    Module:export_key_materials(State, Labels, Contexts, WantedLengths, ConsumeSecret).
-endif.
