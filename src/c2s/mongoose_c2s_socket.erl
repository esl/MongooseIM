-module(mongoose_c2s_socket).

-include_lib("public_key/include/public_key.hrl").
-include("mongoose_logger.hrl").

-export([new/3,
         handle_data/2,
         activate/1,
         close/1,
         is_channel_binding_supported/1,
         get_tls_last_message/1,
         get_peer_certificate/2,
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
-callback get_peer_certificate(state(), mongoose_c2s:listener_opts()) -> peercert_return().
-callback has_peer_cert(state(), mongoose_c2s:listener_opts()) -> boolean().
-callback is_channel_binding_supported(state()) -> boolean().
-callback get_tls_last_message(state()) -> {ok, binary()} | {error, term()}.
-callback is_ssl(state()) -> boolean().

-record(c2s_socket, {module :: module(),
                     state :: state()}).
-type socket() :: #c2s_socket{}.
-type state() :: term().
-type conn_type() :: c2s | c2s_tls.
-type peercert_return() :: no_peer_cert | {bad_cert, term()} | {ok, #'Certificate'{}}.
-export_type([socket/0, state/0, conn_type/0, peercert_return/0]).

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
handle_data(#c2s_socket{module = Module, state = State}, Payload) ->
    Module:socket_handle_data(State, Payload);
handle_data(_, _) ->
    {error, bad_packet}.

-spec activate(socket()) -> ok | {error, term()}.
activate(#c2s_socket{module = Module, state = State}) ->
    Module:socket_activate(State).

-spec close(socket()) -> ok.
close(#c2s_socket{module = Module, state = State}) ->
    Module:socket_close(State).

-spec send_xml(socket(), exml_stream:element() | [exml_stream:element()]) -> ok | {error, term()}.
send_xml(#c2s_socket{module = Module, state = State}, XML) ->
    Module:socket_send_xml(State, XML).

-spec get_peer_certificate(socket(), mongoose_c2s:listener_opts()) -> peercert_return().
get_peer_certificate(#c2s_socket{module = Module, state = State}, LOpts) ->
    Module:get_peer_certificate(State, LOpts).

-spec has_peer_cert(socket(), mongoose_listener:options()) -> boolean().
has_peer_cert(#c2s_socket{module = Module, state = State}, LOpts) ->
    Module:has_peer_cert(State, LOpts).

-spec is_channel_binding_supported(socket()) -> boolean().
is_channel_binding_supported(#c2s_socket{module = Module, state = State}) ->
    Module:is_channel_binding_supported(State).

-spec is_ssl(socket()) -> boolean().
is_ssl(#c2s_socket{module = Module, state = State}) ->
    Module:is_ssl(State).

-spec get_transport(socket()) -> module().
get_transport(#c2s_socket{module = Module}) ->
    Module.

-spec get_tls_last_message(socket()) -> {ok, binary()} | {error, term()}.
get_tls_last_message(#c2s_socket{module = Module, state = State}) ->
    Module:get_tls_last_message(State).

-spec get_conn_type(socket()) -> conn_type().
get_conn_type(Socket) ->
    case is_ssl(Socket) of
        true -> c2s_tls;
        false -> c2s
    end.

-spec get_ip(socket()) -> term().
get_ip(#c2s_socket{module = Module, state = State}) ->
    Module:socket_peername(State).
