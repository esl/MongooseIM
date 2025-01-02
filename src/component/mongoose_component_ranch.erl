-module(mongoose_component_ranch).

-behaviour(mongoose_component_socket).

-export([socket_new/2,
         socket_peername/1,
         socket_handle_data/2,
         socket_activate/1,
         socket_close/1,
         socket_send_xml/2
        ]).

-record(state, {
          ranch_ref :: ranch:ref(),
          socket :: ranch_transport:socket(),
          ip :: {inet:ip_address(), inet:port_number()}
         }).

-type state() :: #state{}.

-spec socket_new(term(), mongoose_listener:options()) -> state().
socket_new({ranch_tcp, RanchRef}, _Opts) ->
    {ok, TcpSocket} = ranch:handshake(RanchRef),
    {ok, Ip} = ranch_tcp:peername(TcpSocket),
    #state{
        ranch_ref = RanchRef,
        socket = TcpSocket,
        ip = Ip}.

-spec socket_peername(state()) -> {inet:ip_address(), inet:port_number()}.
socket_peername(#state{ip = Ip}) ->
    Ip.

-spec socket_handle_data(state(), {tcp, term(), iodata()}) ->
  iodata() | {raw, [exml:element()]} | {error, term()}.
socket_handle_data(#state{socket = Socket}, {tcp, Socket, Data}) ->
    mongoose_instrument:execute(component_tcp_data_in, #{}, #{byte_size => byte_size(Data)}),
    Data.

-spec socket_activate(state()) -> ok.
socket_activate(#state{socket = Socket}) ->
    ranch_tcp:setopts(Socket, [{active, once}]).

-spec socket_close(state()) -> ok.
socket_close(#state{socket = Socket}) ->
    ranch_tcp:close(Socket).

-spec socket_send_xml(state(), iodata() | exml_stream:element() | [exml_stream:element()]) ->
    ok | {error, term()}.
socket_send_xml(#state{socket = Socket}, XML) ->
    Text = exml:to_iolist(XML),
    case send(Socket, Text) of
        ok ->
            ok;
        Error ->
            Error
        end.

-spec send(ranch_transport:socket(), iodata()) -> ok | {error, term()}.
send(Socket, Data) ->
    mongoose_instrument:execute(component_tcp_data_out, #{}, #{byte_size => iolist_size(Data)}),
    ranch_tcp:send(Socket, Data).
