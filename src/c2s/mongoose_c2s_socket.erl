-module(mongoose_c2s_socket).

-include_lib("public_key/include/public_key.hrl").
-include("mongoose_logger.hrl").

-export([new/3,
         receive_data/2,
         send_data/2,
         activate/1,
         close/1,
         is_channel_binding_supported/1,
         get_tls_last_message/1,
         get_peer_certificate/2,
         has_peer_cert/2,
         tcp_to_tls/2,
         is_ssl/1]).

-export([get_ip/1,
         get_transport/1,
         get_conn_type/1]).

-callback socket_new(term(), mongoose_c2s:listener_opts()) -> state().
-callback socket_peername(state()) -> {inet:ip_address(), inet:port_number()}.
-callback tcp_to_tls(state(), mongoose_c2s:listener_opts()) ->
    {ok, state()} | {error, term()}.
-callback receive_data(state(), input()) -> iodata() | {error, term()}.
-callback send_data(state(), iodata()) -> ok | {error, term()}.
-callback socket_activate(state()) -> ok.
-callback socket_close(state()) -> ok.
-callback get_peer_certificate(mongoose_c2s_socket:state(), mongoose_c2s:listener_opts()) -> peercert_return().
-callback has_peer_cert(mongoose_c2s_socket:state(), mongoose_c2s:listener_opts()) -> boolean().
-callback is_channel_binding_supported(mongoose_c2s_socket:state()) -> boolean().
-callback get_tls_last_message(mongoose_c2s_socket:state()) -> {ok, binary()} | {error, term()}.
-callback is_ssl(mongoose_c2s_socket:state()) -> boolean().

-record(c2s_socket, {module :: module(),
                     parser :: exml_stream:parser(),
                     shaper :: shaper:shaper(),
                     state :: state()}).
-type socket() :: #c2s_socket{}.
-type state() :: term().
-type conn_type() :: c2s | c2s_tls.
-type peercert_return() :: no_peer_cert | {bad_cert, term()} | {ok, #'Certificate'{}}.
-type input_tag() :: tcp | ssl.
-type input() :: {input_tag(), term(), iodata()}.
-type output() :: exml_stream:element() | [exml_stream:element()].
-type received_data() :: {ok, socket(), [exml:element()], integer()}
                       | {socket_error, socket(), term()}
                       | {parser_error, socket(), term()}.
-export_type([socket/0, input/0, output/0, received_data/0, state/0, conn_type/0, peercert_return/0]).

-spec new(module(), term(), mongoose_listener:options()) -> socket().
new(Module, SocketOpts, #{shaper := ShaperName, max_stanza_size := MaxStanzaSize} = LOpts) ->
    ParserOpts = [{max_child_size, MaxStanzaSize}, {infinite_stream, maps:get(infinite_stream, LOpts, false)}],
    State = Module:socket_new(SocketOpts, LOpts),
    PeerIp = Module:socket_peername(State),
    verify_ip_is_not_blacklisted(PeerIp),
    {ok, Parser} = exml_stream:new_parser(ParserOpts),
    Shaper = shaper:new(ShaperName),
    C2SSocket = #c2s_socket{
        module = Module,
        parser = Parser,
        shaper = Shaper,
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
tcp_to_tls(#c2s_socket{module = Module, parser = Parser, state = State} = C2SSocket, LOpts) ->
    case Module:tcp_to_tls(State, LOpts) of
        {ok, NewState} ->
            {ok, NewParser} = exml_stream:reset_parser(Parser),
            {ok, C2SSocket#c2s_socket{state = NewState, parser = NewParser}};
        Error ->
            Error
    end.

-spec receive_data(socket(), input()) -> received_data().
receive_data(Socket = #c2s_socket{module = Module, state = State}, Payload) ->
    case Module:receive_data(State, Payload) of
        {error, Reason} ->
            {socket_error, Socket, Reason};
        Data ->
            handle_socket_packet(Socket, Data)
    end;
receive_data(Socket, _) ->
    {socket_error, Socket, bad_packet}.

-spec handle_socket_packet(socket(), iodata()) -> received_data().
handle_socket_packet(Socket = #c2s_socket{parser = Parser, shaper = Shaper}, Packet) ->
    ?LOG_DEBUG(#{what => received_xml_on_stream, packet => Packet, c2s_pid => self()}),
    case exml_stream:parse(Parser, Packet) of
        {error, Reason} ->
            {parser_error, Socket, Reason};
        {ok, NewParser, XmlElements} ->
            Size = iolist_size(Packet),
            NewSocket = Socket#c2s_socket{parser = NewParser},
            {NewShaper, Pause} = shaper:update(Shaper, Size),
            mongoose_metrics:update(global, [data, xmpp, received, xml_stanza_size], Size),
            NewSocket = Socket#c2s_socket{shaper = NewShaper},
            {ok, NewSocket, XmlElements, Pause}
    end.

-spec send_data(socket(), output()) -> ok | {error, term()}.
send_data(#c2s_socket{module = Module, state = State}, Xml) ->
    Data = exml:to_iolist(Xml),
    Module:send_data(State, Data).

-spec activate(socket()) -> ok | {error, term()}.
activate(#c2s_socket{module = Module, state = State}) ->
    Module:socket_activate(State).

-spec close(socket()) -> ok.
close(#c2s_socket{module = Module, parser = Parser, state = State}) ->
    close_parser(Parser),
    Module:socket_close(State).

-spec close_parser(undefined | exml_stream:parser()) -> ok.
close_parser(undefined) -> ok;
close_parser(Parser) -> exml_stream:free_parser(Parser).

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
