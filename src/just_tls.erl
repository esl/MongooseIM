%%%=============================================================================
%%% @copyright (C) 1999-2018, Erlang Solutions Ltd
%%% @author Denys Gonchar <denys.gonchar@erlang-solutions.com>
%%% @doc TLS backend based on standard Erlang's SSL application
%%% @end
%%%=============================================================================
-module(just_tls).
-copyright("2018, Erlang Solutions Ltd.").
-author('denys.gonchar@erlang-solutions.com').

-behaviour(mongoose_tls).

-include_lib("public_key/include/public_key.hrl").

-record(tls_socket, {verify_results = [],
                     ssl_socket
}).

-type tls_socket() :: #tls_socket{}.
-export_type([tls_socket/0]).

% mongoose_tls behaviour
-export([tcp_to_tls/2,
         send/2,
         recv_data/2,
         controlling_process/2,
         sockname/1,
         peername/1,
         setopts/2,
         get_peer_certificate/1,
         close/1]).

% API
-export([make_ssl_opts/1, make_cowboy_ssl_opts/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% APIs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec tcp_to_tls(inet:socket(), mongoose_tls:options()) ->
          {ok, mongoose_tls:tls_socket()} | {error, any()}.
tcp_to_tls(TCPSocket, Options) ->
    inet:setopts(TCPSocket, [{active, false}]),
    {Ref, SSLOpts} = format_opts_with_ref(Options, false),
    Ret = case Options of
              #{connect := true} ->
                  % Currently unused as ejabberd_s2s_out uses fast_tls,
                  % and outgoing pools use Erlang SSL directly
                  ssl:connect(TCPSocket, SSLOpts);
              #{} ->
                  ssl:handshake(TCPSocket, SSLOpts, 5000)
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

%% @doc Prepare SSL options for direct use of ssl:connect/2 or ssl:handshake/2
%% The `disconnect_on_failure' option is not supported
-spec make_ssl_opts(mongoose_tls:options()) -> [ssl:tls_option()].
make_ssl_opts(Opts) ->
    {dummy_ref, SSLOpts} = format_opts_with_ref(Opts, false),
    SSLOpts.

-spec make_cowboy_ssl_opts(mongoose_tls:options()) -> [ssl:tls_option()].
make_cowboy_ssl_opts(Opts) ->
    FailIfNoPeerCert = fail_if_no_peer_cert_opt(Opts),
    {dummy_ref, SSLOpts} = format_opts_with_ref(Opts, FailIfNoPeerCert),
    SSLOpts.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_opts_with_ref(Opts, FailIfNoPeerCert) ->
    Verify = verify_opt(Opts),
    {Ref, VerifyFun} = verify_fun_opt(Opts),
    SNIOpts = sni_opts(Opts),
    SSLOpts = maps:to_list(maps:with(ssl_option_keys(), Opts)),
    {Ref, [{fail_if_no_peer_cert, FailIfNoPeerCert}, {verify, Verify}, {verify_fun, VerifyFun}] ++
     SNIOpts ++ SSLOpts}.

ssl_option_keys() ->
    [certfile, cacertfile, ciphers, keyfile, password, versions, dhfile].

sni_opts(#{server_name_indication := SNIOpts}) ->
    process_sni_opts(SNIOpts);
sni_opts(#{}) ->
    [].

process_sni_opts(#{enabled := false}) ->
    [{server_name_indication, disable}];
process_sni_opts(#{enabled := true, host := SNIHost, protocol := https}) ->
    [{server_name_indication, SNIHost},
     {customize_hostname_check, [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}];
process_sni_opts(#{enabled := true, host := SNIHost, protocol := default}) ->
    [{server_name_indication, SNIHost}];
process_sni_opts(#{enabled := true}) ->
    [].

error_to_list(_Error) ->
    %TODO: implement later if needed
    "verify_fun failed".

verify_opt(#{verify_mode := none}) -> verify_none;
verify_opt(#{}) -> verify_peer.

fail_if_no_peer_cert_opt(#{verify_mode := peer}) -> true;
fail_if_no_peer_cert_opt(#{verify_mode := selfsigned_peer}) -> true;
fail_if_no_peer_cert_opt(#{}) -> false.

%% This function translates TLS options to the function
%% which will later be used when TCP socket is upgraded to TLS
%%  `verify_mode` is one of the following:
%%     none - no validation of the clients certificate - any cert is accepted.
%%     peer - standard verification of the certificate.
%%     selfsigned_peer - the same as peer but also accepts self-signed certificates
%%  `disconnect_on_failure` is a boolean parameter:
%%     true - drop connection if certificate verification failed
%%     false - connect anyway, but later return {bad_cert,Error}
%%             on certificate verification (the same as fast_tls do).
verify_fun_opt(#{verify_mode := Mode, disconnect_on_failure := false}) ->
    Ref = erlang:make_ref(),
    {Ref, verify_fun(Ref, Mode)};
verify_fun_opt(#{verify_mode := Mode}) ->
    {dummy_ref, verify_fun(Mode)}.

verify_fun(Ref, Mode) when is_reference(Ref) ->
    {Fun, State} = verify_fun(Mode),
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
