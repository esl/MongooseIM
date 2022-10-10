-module(mongoose_c2s_socket).

-include("mongoose_logger.hrl").

-export([new/3,
         handle_data/2,
         activate/1,
         close/1,
         is_channel_binding_supported/1,
         has_peer_cert/2,
         tcp_to_tls/2,
         is_ssl/1,
         send_xml/2]).

-export([get_ip/1,
         get_transport/1,
         get_conn_type/1]).

-callback socket_new(term(), mongoose_c2s:listener_opts()) -> state().
-callback socket_peername(state()) -> {inet:ip_address(), inet:port_number()}.
-callback tcp_to_tls(state(), mongoose_c2s:listener_opts()) ->
    {ok, state()} | {error, term()}.
-callback socket_handle_data(state(), {tcp | ssl, term(), iodata()}) ->
    iodata() | {raw, [exml:element()]} | {error, term()}.
-callback socket_activate(state()) -> ok.
-callback socket_close(state()) -> ok.
-callback socket_send_xml(state(), iodata() | exml_stream:element() | [exml_stream:element()]) ->
    ok | {error, term()}.
-callback has_peer_cert(mongoose_c2s_socket:state(), mongoose_c2s:listener_opts()) -> boolean().
-callback is_channel_binding_supported(mongoose_c2s_socket:state()) -> boolean().
-callback is_ssl(mongoose_c2s_socket:state()) -> boolean().

-record(c2s_socket, {module :: module(),
                     state :: state()}).
-type socket() :: #c2s_socket{}.
-type state() :: term().
-type conn_type() :: c2s | c2s_tls.
-export_type([socket/0, state/0, conn_type/0]).

-spec new(module(), term(), mongoose_listener:options()) -> socket().
new(Module, SocketOpts, LOpts) ->
    State = Module:socket_new(SocketOpts, LOpts),
    PeerIp = Module:socket_peername(State),
    verify_ip_is_not_blacklisted(PeerIp),
    C2SSocket = #c2s_socket{
        module = Module,
        state = State},
    handle_socket_and_ssl_config(C2SSocket, LOpts).

verify_ip_is_not_blacklisted(PeerIp) ->
    case mongoose_hooks:check_bl_c2s(PeerIp) of
        true ->
            ?LOG_INFO(#{what => c2s_blacklisted_ip, ip => PeerIp,
                        text => <<"Connection attempt from blacklisted IP">>}),
            throw({stop, {shutdown, ip_blacklisted}});
        false ->
            ok
    end.

handle_socket_and_ssl_config(C2SSocket, #{tls := #{mode := tls}} = LOpts) ->
    case tcp_to_tls(C2SSocket, LOpts) of
        {ok, TlsC2SSocket} ->
            activate(TlsC2SSocket),
            TlsC2SSocket;
        {error, closed} ->
            throw({stop, {shutdown, tls_closed}});
        {error, timeout} ->
            throw({stop, {shutdown, tls_timeout}});
        {error, {tls_alert, TlsAlert}} ->
            throw({stop, TlsAlert})
    end;
handle_socket_and_ssl_config(C2SSocket, _Opts) ->
    activate(C2SSocket),
    C2SSocket.

-spec tcp_to_tls(socket(), mongoose_listener:options()) -> {ok, socket()} | {error, term()}.
tcp_to_tls(#c2s_socket{module = Module, state = State} = C2SSocket, LOpts) ->
    case Module:tcp_to_tls(State, LOpts) of
        {ok, NewState} ->
            {ok, C2SSocket#c2s_socket{state = NewState}};
        Error ->
            Error
        end.

-spec handle_data(socket(), {tcp | ssl, term(), iodata()}) ->
    iodata() | {raw, [term()]} | {error, term()}.
handle_data(#c2s_socket{module = Module, state = State},
            {Transport, _Socket, _Data} = Payload) when Transport == tcp; Transport == ssl ->
    Module:socket_handle_data(State, Payload);
handle_data(_, _) ->
    {error, bad_packet}.

-spec activate(socket()) -> ok | {error, term()}.
activate(#c2s_socket{module = Module, state = State}) ->
    Module:socket_activate(State).

-spec close(socket()) -> ok.
close(#c2s_socket{module = Module, state = State}) ->
    Module:socket_close(State).

% TODO: Check if XML can be an array
-spec send_xml(socket(), exml_stream:element() | [exml_stream:element()]) -> ok | {error, term()}.
send_xml(#c2s_socket{module = Module, state = State}, XML) ->
    Module:socket_send_xml(State, XML).

%% 18 is OpenSSL's and fast_tls's error code for self-signed certs
-spec has_peer_cert(socket(), mongoose_listener:options()) -> boolean().
has_peer_cert(#c2s_socket{module = Module, state = State}, LOpts) ->
    Module:has_pert_cert(State, LOpts).

-spec is_channel_binding_supported(socket()) -> boolean().
is_channel_binding_supported(#c2s_socket{module = Module, state = State}) ->
    Module:is_channel_binding_supported(State).

-spec is_ssl(socket()) -> boolean().
is_ssl(#c2s_socket{module = Module, state = State}) ->
    Module:is_ssl(State).

-spec get_transport(socket()) -> module().
get_transport(#c2s_socket{module = Module}) ->
    Module.

-spec get_conn_type(socket()) -> conn_type().
get_conn_type(Socket) ->
    case is_ssl(Socket) of
        true -> c2s_tls;
        false -> c2s
    end.

-spec get_ip(socket()) -> term().
get_ip(#c2s_socket{module = Module, state = State}) ->
    Module:socket_peername(State).
