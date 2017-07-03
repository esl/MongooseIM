-module(mod_global_distrib_transport).

-record(?MODULE, {
           transport :: fast_tls | gen_tcp,
           socket :: fast_tls:tls_socket() | gen_tcp:socket()
          }).

-type t() :: #?MODULE{}.

-export([wrap/2, setopts/2, recv_data/2, close/1, send/2]).

wrap(Socket, [connect | false]) ->
    wrap(Socket, false);
wrap(Socket, false) ->
    {ok, #?MODULE{transport = gen_tcp, socket = Socket}};
wrap(Socket, Opts) ->
    case fast_tls:tcp_to_tls(Socket, Opts) of
        {ok, TLSSocket} -> {ok, #?MODULE{transport = fast_tls, socket = TLSSocket}};
        Error -> Error
    end.

setopts(#?MODULE{transport = gen_tcp, socket = Socket}, Opts) ->
    inet:setopts(Socket, Opts);
setopts(#?MODULE{transport = fast_tls, socket = Socket}, Opts) ->
    fast_tls:setopts(Socket, Opts).

recv_data(#?MODULE{transport = gen_tcp}, Data) ->
    {ok, Data};
recv_data(#?MODULE{transport = fast_tls, socket = Socket}, Data) ->
    fast_tls:recv_data(Socket, Data).

close(#?MODULE{transport = gen_tcp, socket = Socket}) ->
    gen_tcp:close(Socket);
close(#?MODULE{transport = fast_tls, socket = Socket}) ->
    fast_tls:close(Socket).

send(#?MODULE{transport = gen_tcp, socket = Socket}, Data) ->
    gen_tcp:send(Socket, Data);
send(#?MODULE{transport = fast_tls, socket = Socket}, Data) ->
    fast_tls:send(Socket, Data).

