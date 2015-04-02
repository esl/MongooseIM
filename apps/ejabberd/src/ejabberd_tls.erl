%%%----------------------------------------------------------------------
%%% File    : ejabberd_tls.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Interface to openssl
%%% Created : 24 Jul 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_tls).
-author('alexey@process-one.net').

-export([tcp_to_tls/2, tls_to_tcp/1,
         send/2,
         recv_data/2,
         setopts/2,
         sockname/1, peername/1,
         controlling_process/2,
         close/1,
         get_peer_certificate/1,
         get_verify_result/1,
         get_cert_verify_string/2
         ]).

-include("ejabberd.hrl").

-define(SET_CERTIFICATE_FILE_ACCEPT, 1).
-define(SET_CERTIFICATE_FILE_CONNECT, 2).
-define(SET_ENCRYPTED_INPUT,  3).
-define(SET_DECRYPTED_OUTPUT, 4).
-define(GET_ENCRYPTED_OUTPUT, 5).
-define(GET_DECRYPTED_INPUT,  6).
-define(GET_PEER_CERTIFICATE, 7).
-define(GET_VERIFY_RESULT,    8).
-define(VERIFY_NONE, 16#10000).

-ifdef(SSL40).
-define(CERT_DECODE, {public_key, pkix_decode_cert, plain}).
-define(CERT_SELFSIGNED, {public_key, pkix_is_self_signed}).
-else.
-define(CERT_DECODE, {ssl_pkix, decode_cert, [pkix]}).
-define(CERT_SELFSIGNED, {erlang, is_atom}). %% Dummy function for old OTPs
-endif.


-record(tlssock, {tcpsock  :: gen_tcp:socket(),
                  tlsport  :: port()
                 }).
-type tlssock() :: #tlssock{}.

-spec tcp_to_tls(gen_tcp:socket(), [any()]
                ) -> {ok, tlssock()} | {'error','no_certfile' | string()}.
tcp_to_tls(TCPSocket, Options) ->
    case lists:keysearch(certfile, 1, Options) of
	{value, {certfile, CertFile}} ->
	    Port = open_port({spawn, tls_drv}, [binary]),
	    Flags =
		case lists:member(verify_none, Options) of
		    true ->
			?VERIFY_NONE;
		    false ->
			0
		end,
	    Command = case lists:member(connect, Options) of
			  true ->
			      ?SET_CERTIFICATE_FILE_CONNECT;
			  false ->
			      ?SET_CERTIFICATE_FILE_ACCEPT
		      end,
	    Ciphers = case lists:keysearch(ciphers, 1, Options) of
			      {value, {ciphers, C}} ->
				      C++[0];
			      false ->
				      []
		      end,
	    case port_control(Port, Command bor Flags, CertFile ++ [0] ++ Ciphers) of
		<<0>> ->
		    {ok, #tlssock{tcpsock = TCPSocket, tlsport = Port}};
		<<1, Error/binary>> ->
		    {error, binary_to_list(Error)}
	    end;
	false ->
	    {error, no_certfile}
    end.


-spec tls_to_tcp(tlssock()) -> gen_tcp:socket().
tls_to_tcp(#tlssock{tcpsock = TCPSocket, tlsport = Port}) ->
    port_close(Port),
    TCPSocket.

recv_data(TLSSock, Packet) ->
    case catch recv_data1(TLSSock, Packet) of
        {'EXIT', Reason} ->
            {error, Reason};
        Res ->
            Res
    end.


-spec recv_data1(TLSSock :: tlssock(), Packet :: iolist()
                ) -> {'error', atom() | [byte()]} | {'ok',binary()}.
recv_data1(#tlssock{tcpsock = TCPSocket, tlsport = Port}, Packet) ->
    case port_control(Port, ?SET_ENCRYPTED_INPUT, Packet) of
        <<0>> ->
            case port_control(Port, ?GET_DECRYPTED_INPUT, []) of
                <<0, In/binary>> ->
                    case port_control(Port, ?GET_ENCRYPTED_OUTPUT, []) of
                        <<0, Out/binary>> ->
                            case gen_tcp:send(TCPSocket, Out) of
                                ok ->
                                    %?PRINT("IN: ~p~n", [{TCPSocket, binary_to_list(In)}]),
                                    {ok, In};
                                Error ->
                                    Error
                            end;
                        <<1, Error/binary>> ->
                            {error, binary_to_list(Error)}
                    end;
                <<1, Error/binary>> ->
                    {error, binary_to_list(Error)}
            end;
        <<1, Error/binary>> ->
            {error, binary_to_list(Error)}
    end.


-spec send(tlssock(), iolist()) -> 'ok' | {'error',atom() | [byte()]}.
send(#tlssock{tcpsock = TCPSocket, tlsport = Port} = TLSSock, Packet) ->
    case port_control(Port, ?SET_DECRYPTED_OUTPUT, Packet) of
        <<0>> ->
            %?PRINT("OUT: ~p~n", [{TCPSocket, lists:flatten(Packet)}]),
            case port_control(Port, ?GET_ENCRYPTED_OUTPUT, []) of
                <<0, Out/binary>> ->
                    gen_tcp:send(TCPSocket, Out);
                <<1, Error/binary>> ->
                    {error, binary_to_list(Error)}
            end;
        <<1, Error/binary>> ->
            {error, binary_to_list(Error)};
        <<2>> -> % Dirty hack
            receive
                {timeout, _Timer, _} ->
                    {error, timeout}
            after 100 ->
                    send(TLSSock, Packet)
            end
    end.


-type option() :: gen_sctp:option() | gen_tcp:option() | gen_udp:option().
-spec setopts(tlssock(), [option()]) -> 'ok' | {'error',atom()}.
setopts(#tlssock{tcpsock = TCPSocket}, Opts) ->
    inet:setopts(TCPSocket, Opts).


-spec sockname(tlssock()) ->
  {'error',atom()} | {'ok',{inet:ip_address(), inet:port_number()}}.
sockname(#tlssock{tcpsock = TCPSocket}) ->
    inet:sockname(TCPSocket).


-spec peername(tlssock()) ->
  {'error',atom()} | {'ok',{inet:ip_address(), inet:port_number()}}.
peername(#tlssock{tcpsock = TCPSocket}) ->
    inet:peername(TCPSocket).


-spec controlling_process(tlssock(), pid()) -> 'ok' | {'error',atom()}.
controlling_process(#tlssock{tcpsock = TCPSocket}, Pid) ->
    gen_tcp:controlling_process(TCPSocket, Pid).


-spec close(tlssock()) -> 'true'.
close(#tlssock{tcpsock = TCPSocket, tlsport = Port}) ->
    gen_tcp:close(TCPSocket),
    port_close(Port).


-spec get_peer_certificate(tlssock()) -> 'error' | {'ok',_}.
get_peer_certificate(#tlssock{tlsport = Port}) ->
    case port_control(Port, ?GET_PEER_CERTIFICATE, []) of
        <<0, BCert/binary>> ->
            {CertMod, CertFun, CertSecondArg} = ?CERT_DECODE,
            case catch apply(CertMod, CertFun, [BCert, CertSecondArg]) of
                {ok, Cert} -> %% returned by R13 and older
                    {ok, Cert};
                {'Certificate', _, _, _} = Cert ->
                    {ok, Cert};
                _ ->
                    error
            end;
        <<1>> ->
            error
    end.

-spec get_verify_result(tlssock()) -> byte().
get_verify_result(#tlssock{tlsport = Port}) ->
    <<Res>> = port_control(Port, ?GET_VERIFY_RESULT, []),
    Res.


-spec get_cert_verify_string(_,_) -> binary().
get_cert_verify_string(CertVerifyRes, Cert) ->
    BCert = public_key:pkix_encode('Certificate', Cert, plain),
    {CertMod, CertFun} = ?CERT_SELFSIGNED,
    IsSelfsigned = apply(CertMod, CertFun, [BCert]),
    case {CertVerifyRes, IsSelfsigned} of
        {21, true} -> <<"self-signed certificate">>;
        _ -> cert_verify_code(CertVerifyRes)
    end.

%% http://www.openssl.org/docs/apps/verify.html
cert_verify_code(0) -> <<"ok">>;
cert_verify_code(2) -> <<"unable to get issuer certificate">>;
cert_verify_code(3) -> <<"unable to get certificate CRL">>;
cert_verify_code(4) -> <<"unable to decrypt certificate's signature">>;
cert_verify_code(5) -> <<"unable to decrypt CRL's signature">>;
cert_verify_code(6) -> <<"unable to decode issuer public key">>;
cert_verify_code(7) -> <<"certificate signature failure">>;
cert_verify_code(8) -> <<"CRL signature failure">>;
cert_verify_code(9) -> <<"certificate is not yet valid">>;
cert_verify_code(10) -> <<"certificate has expired">>;
cert_verify_code(11) -> <<"CRL is not yet valid">>;
cert_verify_code(12) -> <<"CRL has expired">>;
cert_verify_code(13) -> <<"format error in certificate's notBefore field">>;
cert_verify_code(14) -> <<"format error in certificate's notAfter field">>;
cert_verify_code(15) -> <<"format error in CRL's lastUpdate field">>;
cert_verify_code(16) -> <<"format error in CRL's nextUpdate field">>;
cert_verify_code(17) -> <<"out of memory">>;
cert_verify_code(18) -> <<"self signed certificate">>;
cert_verify_code(19) -> <<"self signed certificate in certificate chain">>;
cert_verify_code(20) -> <<"unable to get local issuer certificate">>;
cert_verify_code(21) -> <<"unable to verify the first certificate">>;
cert_verify_code(22) -> <<"certificate chain too long">>;
cert_verify_code(23) -> <<"certificate revoked">>;
cert_verify_code(24) -> <<"invalid CA certificate">>;
cert_verify_code(25) -> <<"path length constraint exceeded">>;
cert_verify_code(26) -> <<"unsupported certificate purpose">>;
cert_verify_code(27) -> <<"certificate not trusted">>;
cert_verify_code(28) -> <<"certificate rejected">>;
cert_verify_code(29) -> <<"subject issuer mismatch">>;
cert_verify_code(30) -> <<"authority and subject key identifier mismatch">>;
cert_verify_code(31) -> <<"authority and issuer serial number mismatch">>;
cert_verify_code(32) -> <<"key usage does not include certificate signing">>;
cert_verify_code(50) -> <<"application verification failure">>;
cert_verify_code(X) -> <<"Unknown OpenSSL error code: ", (list_to_binary(integer_to_list(X)))/binary>>.
