%%%=============================================================================
%%% @copyright (C) 1999-2018, Erlang Solutions Ltd
%%% @author Denys Gonchar <denys.gonchar@erlang-solutions.com>
%%% @doc TLS backend based on standard Erlang's SSL application
%%% @end
%%%=============================================================================
-module(just_tls).
-copyright("2018, Erlang Solutions Ltd.").
-author('denys.gonchar@erlang-solutions.com').

-include_lib("public_key/include/public_key.hrl").

-type cert() :: {ok, Cert::any()} | {bad_cert, bitstring()} | no_peer_cert.

%% Options used for client-side and server-side TLS connections.
%% All modules implementing this behaviour have to support the mandatory 'verify_mode' option.
%% Other options should be supported if the implementing module supports it.
-type options() :: #{module => module(),
                     connect => boolean(), % set to 'true' for a client-side call to tcp_to_tls/2
                     verify_mode := peer | selfsigned_peer | none,
                     mode => tls | starttls | starttls_required, % only ejabberd_s2s_out doesn't use it (yet)
                     certfile => string(),
                     cacertfile => string(),
                     ciphers => string(),
                     dhfile => string(), % server-only

                     %% only for just_tls
                     disconnect_on_failure => boolean(),
                     keyfile => string(),
                     password => string(),
                     versions => [atom()],
                     server_name_indication => sni_options() % client-only
                    }.

-type sni_options() :: #{enabled := boolean,
                         protocol := default | https,
                         host => string()}.

-record(tls_socket, {verify_results = [],
                     ssl_socket
}).

-type tls_socket() :: #tls_socket{}.
-export_type([options/0, tls_socket/0, cert/0]).

-export([tcp_to_tls/2,
         send/2,
         peername/1,
         setopts/2,
         get_peer_certificate/1,
         close/1]).

% API
-export([prepare_connection/1,
         receive_verify_results/1, error_to_list/1,
         make_ssl_opts/1, make_cowboy_ssl_opts/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% APIs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec tcp_to_tls(inet:socket(), options()) ->
          {ok, tls_socket()} | {error, any()}.
tcp_to_tls(TCPSocket, Options) ->
    inet:setopts(TCPSocket, [{active, false}]),
    {Ref1, Ret} = case Options of
                    #{connect := true} ->
                        % Currently unused as ejabberd_s2s_out uses fast_tls,
                        % and outgoing pools use Erlang SSL directly
                        % Do not set `fail_if_no_peer_cert_opt` for SSL client
                        % as it is a server only option.
                        {Ref, SSLOpts} = format_opts_with_ref(Options, false),
                        {Ref, ssl:connect(TCPSocket, SSLOpts)};
                    #{} ->
                        FailIfNoPeerCert = fail_if_no_peer_cert_opt(Options),
                        {Ref, SSLOpts} = format_opts_with_ref(Options, FailIfNoPeerCert),
                        {Ref, ssl:handshake(TCPSocket, SSLOpts, 5000)}
                 end,
    VerifyResults = receive_verify_results(Ref1),
    case Ret of
        {ok, SSLSocket} ->
            {ok, #tls_socket{ssl_socket = SSLSocket, verify_results = VerifyResults}};
        _ -> Ret
    end.

-spec send(tls_socket(), binary()) -> ok | {error, any()}.
send(#tls_socket{ssl_socket = SSLSocket}, Packet) -> ssl:send(SSLSocket, Packet).

-spec peername(tls_socket()) -> {ok, mongoose_transport:peer()} |
                                     {error, any()}.
peername(#tls_socket{ssl_socket = SSLSocket}) -> ssl:peername(SSLSocket).


-spec setopts(tls_socket(), Opts::list()) -> ok | {error, any()}.
setopts(#tls_socket{ssl_socket = SSLSocket}, Opts) -> ssl:setopts(SSLSocket, Opts).

get_peer_certificate(#tls_socket{verify_results = [], ssl_socket = SSLSocket}) ->
    case ssl:peercert(SSLSocket) of
        {ok, PeerCert} ->
            Cert = public_key:pkix_decode_cert(PeerCert, plain),
            {ok, Cert};
        _ -> no_peer_cert
    end;
get_peer_certificate(#tls_socket{verify_results = [Err | _]}) ->
    {bad_cert, error_to_list(Err)}.

-spec close(tls_socket()) -> ok.
close(#tls_socket{ssl_socket = SSLSocket}) -> ssl:close(SSLSocket).

%% @doc Prepare SSL options for direct use of ssl:handshake/2 (server side)
-spec prepare_connection(options()) -> {dummy_ref | reference(), [ssl:tls_server_option()]}.
prepare_connection(Options) ->
    FailIfNoPeerCert = fail_if_no_peer_cert_opt(Options),
    format_opts_with_ref(Options, FailIfNoPeerCert).

%% @doc Prepare SSL options for direct use of ssl:connect/2 (client side)
%% The `disconnect_on_failure' option is expected to be unset or true
-spec make_ssl_opts(options()) -> [ssl:tls_option()].
make_ssl_opts(Opts) ->
    {dummy_ref, SSLOpts} = format_opts_with_ref(Opts, false),
    SSLOpts.

%% @doc Prepare SSL options for direct use of ssl:handshake/2 (server side)
%% The `disconnect_on_failure' option is expected to be unset or true
-spec make_cowboy_ssl_opts(options()) -> [ssl:tls_option()].
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

%% accept empty peer certificate if explicitly requested not to fail
fail_if_no_peer_cert_opt(#{disconnect_on_failure := false}) -> false;
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
%%             on certificate verification.
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
