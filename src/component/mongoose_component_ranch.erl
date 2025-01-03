-module(mongoose_component_ranch).

-behaviour(mongoose_component_socket).

-export([new/2,
         peername/1,
         handle_data/2,
         activate/1,
         close/1,
         send_xml/2
        ]).

-record(ranch_tcp, {
          ranch_ref :: ranch:ref(),
          socket :: inet:socket(),
          ip :: {inet:ip_address(), inet:port_number()}
         }).

-record(ranch_ssl, {
          ranch_ref :: ranch:ref(),
          socket :: ssl:sslsocket(),
          ip :: {inet:ip_address(), inet:port_number()}
         }).

-type socket() :: #ranch_tcp{} | #ranch_ssl{}.
-type transport() :: ranch_tcp | ranch_ssl.

-spec new({transport(), ranch:ref()}, mongoose_listener:options()) -> socket().
new({ranch_tcp, RanchRef}, _Opts) ->
    {ok, Socket} = ranch:handshake(RanchRef),
    {ok, Ip} = ranch_tcp:peername(Socket),
    #ranch_tcp{
       ranch_ref = RanchRef,
       socket = Socket,
       ip = Ip};
new({ranch_ssl, RanchRef}, _Opts) ->
    {ok, Socket} = ranch:handshake(RanchRef),
    {ok, Ip} = ranch_ssl:peername(Socket),
    #ranch_ssl{
       ranch_ref = RanchRef,
       socket = Socket,
       ip = Ip}.

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
