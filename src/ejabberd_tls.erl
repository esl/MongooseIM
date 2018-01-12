-module(ejabberd_tls).
-author('denys.gonchar@erlang-solutions.com').

%% tls interfaces required by ejabberd_socket & ejabberd_receiver modules.
-export([ tcp_to_tls/2,
          send/2,
          recv_data/2,
          controlling_process/2,
          sockname/1,
          peername/1,
          setopts/2,
          get_peer_certificate/1,
          close/1 ]).

-export([get_sockmod/1]).

-record(ejabberd_tls_socket, { tls_module,
                               tcp_socket,
                               tls_socket
                             }).

-define(FN_WRP1(FN),FN(#ejabberd_tls_socket{tls_module = M, tls_socket = S}) -> M:FN(S)).
-define(FN_WRP2(FN),FN(#ejabberd_tls_socket{tls_module = M, tls_socket = S},P1) -> M:FN(S,P1)).

tcp_to_tls(TCPSocket,Opts) ->
  Module = proplists:get_value(tls_module, Opts, fast_tls),
  NewOpts = proplists:delete(tls_module,Opts),
  case Module:tcp_to_tls(TCPSocket,NewOpts) of
    {ok,TLSSocket} ->
      {ok, #ejabberd_tls_socket{ tls_module = Module,
                                 tcp_socket = TCPSocket,
                                 tls_socket = TLSSocket }};
    Error -> Error
  end.

?FN_WRP2(send).

?FN_WRP2(recv_data).

?FN_WRP2(controlling_process).

?FN_WRP1(sockname).

?FN_WRP1(peername).

?FN_WRP2(setopts).

?FN_WRP1(close).

get_peer_certificate(#ejabberd_tls_socket{tls_module = fast_tls, tls_socket = Socket}) ->
  case {fast_tls:get_verify_result(Socket), fast_tls:get_peer_certificate(Socket)} of
    {0, {ok, Cert}} -> {ok, Cert};
    {Error, {ok, Cert}} -> {bad_cert, fast_tls:get_cert_verify_string(Error, Cert)};
    {_, error} -> no_peer_cert
  end;
?FN_WRP1(get_peer_certificate).

get_sockmod(#ejabberd_tls_socket{tls_module = Module}) -> Module.
