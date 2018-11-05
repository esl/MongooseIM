%%%=============================================================================
%%% @copyright (C) 1999-2018, Erlang Solutions Ltd
%%% @author Denys Gonchar <denys.gonchar@erlang-solutions.com>
%%% @doc TLS backend based on standard Erlang's SSL application
%%% @end
%%%=============================================================================
-module(just_tls).
-copyright("2018, Erlang Solutions Ltd.").
-author('denys.gonchar@erlang-solutions.com').

-behavior(ejabberd_tls).

-include_lib("public_key/include/public_key.hrl").

-record(tls_socket, {verify_results = [],
                     ssl_socket
}).

%ejabberd_tls behavior
-export([tcp_to_tls/2,
         send/2,
         recv_data/2,
         controlling_process/2,
         sockname/1,
         peername/1,
         setopts/2,
         get_peer_certificate/1,
         close/1]).

-export([verify_fun/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% APIs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -callback tcp_to_tls(inet:socket(), Opts :: list()) -> {ok, tls_socket()} | {error, any()}.
tcp_to_tls(TCPSocket, Options) ->
    inet:setopts(TCPSocket, [{active, false}]),
    Opts = format_opts(Options),
    {Ref, NewOpts} = set_verify_fun(Opts),
    Ret = case lists:member(connect, Opts) of
              false -> ssl:ssl_accept(TCPSocket, NewOpts);
              true -> ssl:connect(TCPSocket, NewOpts)
          end,
    VerifyResults = receive_verify_results(Ref),
    case Ret of
        {ok, SSLSocket} ->
            {ok, #tls_socket{ssl_socket = SSLSocket, verify_results = VerifyResults}};
        _ -> Ret
    end.


%% -callback send(tls_socket(), binary()) -> ok | {error, any()}.
send(#tls_socket{ssl_socket = SSLSocket}, Packet) -> ssl:send(SSLSocket, Packet).


%% -callback recv_data(tls_socket(), binary()) -> {ok, binary()} | {error, any()}.
recv_data(_, <<"">>) ->
    %% such call is required for fast_tls to accomplish
    %% tls handshake, for just_tls we can ignore it
    {ok, <<"">>};
recv_data(#tls_socket{ssl_socket = SSLSocket}, Data1) ->
    case ssl:recv(SSLSocket, 0, 0) of
        {ok, Data2} -> {ok, <<Data1/binary, Data2/binary>>};
        _ -> {ok, Data1}
    end.


%% -callback controlling_process(tls_socket(), pid()) -> ok | {error, any()}.
controlling_process(#tls_socket{ssl_socket = SSLSocket}, Pid) ->
    ssl:controlling_process(SSLSocket, Pid).


%% -callback sockname(tls_socket()) -> {ok, {inet:ip_address(), inet:port_number()}} |
%%                                     {error, any()}.
sockname(#tls_socket{ssl_socket = SSLSocket}) -> ssl:sockname(SSLSocket).


%% -callback peername(tls_socket()) -> {ok, {inet:ip_address(), inet:port_number()}} |
%%                                     {error, any()}.
peername(#tls_socket{ssl_socket = SSLSocket}) -> ssl:peername(SSLSocket).


%% -callback setopts(tls_socket(), Opts::list()) -> ok | {error, any()}.
setopts(#tls_socket{ssl_socket = SSLSocket}, Opts) -> ssl:setopts(SSLSocket, Opts).


%% -callback get_peer_certificate(tls_socket()) -> {ok, Cert::any()}       |
%%                                                 {bad_cert, bitstring()} |
%%                                                 no_peer_cert.
get_peer_certificate(#tls_socket{verify_results = [], ssl_socket = SSLSocket}) ->
    case ssl:peercert(SSLSocket) of
        {ok, PeerCert} ->
            Cert = public_key:pkix_decode_cert(PeerCert, plain),
            {ok, Cert};
        _ -> no_peer_cert
    end;
get_peer_certificate(#tls_socket{verify_results = [Err | _]}) ->
    {bad_cert, error_to_list(Err)}.


%% -callback close(tls_socket()) -> ok.
close(#tls_socket{ssl_socket = SSLSocket}) -> ssl:close(SSLSocket).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_opts(Opts) ->
    NewOpts = remove_duplicates(format_opts([], Opts)),
    case proplists:is_defined(verify, NewOpts) of
        true -> NewOpts;
        false -> [{verify, verify_peer} | NewOpts]
    end.

format_opts(NewOpts, [])      -> NewOpts;
format_opts(NewOpts, [verify_none | T]) ->
    format_opts([{verify, verify_none} | NewOpts], T);
format_opts(NewOpts, [{certfile, File} | T]) ->
    format_opts([{certfile, File} | NewOpts], T);
format_opts(NewOpts, [{cafile, File} | T]) ->
    format_opts([{cacertfile, File} | NewOpts], T);
format_opts(NewOpts, [{dhfile, File} | T]) ->
    format_opts([{dhfile, File} | NewOpts], T);
format_opts(NewOpts, [{ssl_options, SSLOpts} | T]) ->
    format_opts(NewOpts ++ SSLOpts, T);
format_opts(NewOpts, [_ | T]) -> format_opts(NewOpts, T). %ignore unknown options

remove_duplicates(PropList) ->
    F = fun
            (T, NewPropList) when is_tuple(T) ->
                lists:keystore(element(1, T), 1, NewPropList, T);
            (_, NewPropList) -> NewPropList
        end,
    lists:foldl(F, [], PropList).

error_to_list(_Error) ->
    %TODO: implement later if needed
    "verify_fun failed".
%% doc
%% this function translates the `verify_fun` tuple from `ssl_options` to the real fun
%% which will later be used when TCP socket is upgraded to TLS
%% accepted format is:
%% {verify_fun, {PredefinedValidationFun,DisconnectOnFailure}
%%  PredefinedValidationFun is one of the following:
%%     none - no validation of the clients certificate - any cert is accepted.
%%     peer - standard verification of the certificate.
%%     selfsigned_peer -  the same as peer but also accepts self-signed certificates
%%  DisconnectOnFailure is boolean parameter:
%%     true - drop connection if certificate verification failed
%%     false - connect anyway, but later return {bad_cert,Error}
%%             on certificate verification (the same as fast_tls do).
set_verify_fun(Opts) ->
    case proplists:get_value(verify_fun, Opts) of
        undefined -> {dummy_ref, Opts};
        {VerifyFun, true} ->
            {dummy_ref, lists:keyreplace(verify_fun, 1, Opts, {verify_fun, verify_fun(VerifyFun)})};
        {VerifyFun, false} ->
            Ref = erlang:make_ref(),
            {Ref, lists:keyreplace(verify_fun, 1, Opts, {verify_fun, verify_fun(Ref, VerifyFun)})}
    end.

verify_fun(Ref, VerifyFun) when is_reference(Ref) ->
    {Fun, State} = verify_fun(VerifyFun),
    {verify_fun_wrapper(Ref, Fun), State}.

verify_fun_wrapper(Ref, Fun) when is_reference(Ref), is_function(Fun, 3) ->
    Pid = self(),
    fun(Cert, Event, UserState) ->
        Ret = Fun(Cert, Event, UserState),
        case {Ret, Event} of
            {{valid, _}, _} -> Ret;
            {{unknown, NewState}, {extension, #'Extension'{critical = true}}} ->
                send_verification_failure(Pid, Ref, unknown_critical_extension),
                {valid, NewState};
            {{unknown, _}, {extension, _}} -> Ret;
            {_, _} -> %% {fail,Reason} = Ret
                send_verification_failure(Pid, Ref, Ret),
                {valid, UserState} %return the last valid user state
        end
    end.

verify_fun(peer) ->
    {fun
         (_, {bad_cert, _} = R, _) -> {fail, R};
         (_, {extension, _}, S) -> {unknown, S};
         (_, valid, S) -> {valid, S};
         (_, valid_peer, S) -> {valid, S}
     end, []};
verify_fun(selfsigned_peer) ->
    {fun
         (_, {bad_cert, selfsigned_peer}, S) -> {valid, S};
         (_, {bad_cert, _} = R, _) -> {fail, R};
         (_, {extension, _}, S) -> {unknown, S};
         (_, valid, S) -> {valid, S};
         (_, valid_peer, S) -> {valid, S}
     end, []};
verify_fun(none) ->
    {fun(_, _, S) -> {valid, S} end, []}.


send_verification_failure(Pid, Ref, Reason) ->
    Pid ! {cert_verification_failure, Ref, Reason}.

receive_verify_results(dummy_ref) -> [];
receive_verify_results(Ref)       -> receive_verify_results(Ref, []).

receive_verify_results(Ref, Acc) ->
    receive
        {cert_verification_failure, Ref, Reason} ->
            receive_verify_results(Ref, [Reason | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.
