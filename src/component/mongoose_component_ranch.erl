-module(mongoose_component_ranch).

-behaviour(mongoose_component_socket).

-export([new/3,
         peername/1,
         handle_data/2,
         activate/1,
         close/1,
         send_xml/2
        ]).

-record(ranch_tcp, {
          socket :: inet:socket(),
          ranch_ref :: ranch:ref(),
          ip :: {inet:ip_address(), inet:port_number()}
         }).

-record(ranch_ssl, {
          socket :: ssl:sslsocket(),
          ranch_ref :: ranch:ref(),
          ip :: {inet:ip_address(), inet:port_number()}
         }).

-type socket() :: #ranch_tcp{} | #ranch_ssl{}.

-spec new(mongoose_listener:transport_module(), ranch:ref(), mongoose_listener:options()) -> socket().
new(ranch_tcp, Ref, Opts) ->
    {ok, Socket, ConnectionDetails} = mongoose_listener:read_connection_details(Ref, ranch_tcp, Opts),
    #{src_address := PeerIp, src_port := PeerPort} = ConnectionDetails,
    #ranch_tcp{socket = Socket, ranch_ref = Ref, ip = {PeerIp, PeerPort}};
new(ranch_ssl, Ref, Opts) ->
    {ok, Socket, ConnectionDetails} = mongoose_listener:read_connection_details(Ref, ranch_ssl, Opts),
    #{src_address := PeerIp, src_port := PeerPort} = ConnectionDetails,
    #ranch_ssl{socket = Socket, ranch_ref = Ref, ip = {PeerIp, PeerPort}}.

-spec peername(socket()) -> {inet:ip_address(), inet:port_number()}.
peername(#ranch_tcp{ip = Ip}) -> Ip;
peername(#ranch_ssl{ip = Ip}) -> Ip.

-spec handle_data(socket(), {tcp, term(), iodata()}) ->
  iodata() | {raw, [exml:element()]} | {error, term()}.
handle_data(#ranch_tcp{socket = Socket}, {tcp, Socket, Data}) ->
    mongoose_instrument:execute(component_tcp_data_in, #{}, #{byte_size => iolist_size(Data)}),
    Data;
handle_data(#ranch_ssl{socket = Socket}, {ssl, Socket, Data}) ->
    mongoose_instrument:execute(component_tls_data_in, #{}, #{byte_size => iolist_size(Data)}),
    Data.

-spec activate(socket()) -> ok.
activate(#ranch_tcp{socket = Socket}) ->
    ranch_tcp:setopts(Socket, [{active, once}]);
activate(#ranch_ssl{socket = Socket}) ->
    ranch_ssl:setopts(Socket, [{active, once}]).

-spec close(socket()) -> ok.
close(#ranch_tcp{socket = Socket}) ->
    ranch_tcp:close(Socket);
close(#ranch_ssl{socket = Socket}) ->
    ranch_ssl:close(Socket).

-spec send_xml(socket(), exml_stream:element() | [exml_stream:element()]) ->
    ok | {error, term()}.
send_xml(#ranch_tcp{socket = Socket}, XML) ->
    Data = exml:to_iolist(XML),
    mongoose_instrument:execute(component_tcp_data_out, #{}, #{byte_size => iolist_size(Data)}),
    ranch_tcp:send(Socket, Data);
send_xml(#ranch_ssl{socket = Socket}, XML) ->
    Data = exml:to_iolist(XML),
    mongoose_instrument:execute(component_tls_data_out, #{}, #{byte_size => iolist_size(Data)}),
    ranch_ssl:send(Socket, Data).
