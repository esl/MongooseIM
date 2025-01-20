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
-export([receive_verify_results/0, error_to_list/1,
         make_client_opts/1, make_server_opts/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% APIs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec tcp_to_tls(inet:socket(), options()) ->
          {ok, tls_socket()} | {error, any()}.
tcp_to_tls(TCPSocket, Options) ->
    inet:setopts(TCPSocket, [{active, false}]),
    Ret = case Options of
              #{connect := true} ->
                  % Currently unused as ejabberd_s2s_out uses fast_tls,
                  % and outgoing pools use Erlang SSL directly
                  SSLOpts = format_opts(Options, client),
                  ssl:connect(TCPSocket, SSLOpts);
              #{} ->
                  SSLOpts = format_opts(Options, server),
                  ssl:handshake(TCPSocket, SSLOpts, 5000)
          end,
    VerifyResults = receive_verify_results(),
    case Ret of
        {ok, SSLSocket} ->
            {ok, #tls_socket{ssl_socket = SSLSocket, verify_results = VerifyResults}};
        _ -> Ret
    end.

-spec send(tls_socket(), binary()) -> ok | {error, any()}.
send(#tls_socket{ssl_socket = SSLSocket}, Packet) ->
    ssl:send(SSLSocket, Packet).

-spec peername(tls_socket()) -> {ok, mongoose_transport:peer()} | {error, any()}.
peername(#tls_socket{ssl_socket = SSLSocket}) ->
    ssl:peername(SSLSocket).

-spec setopts(tls_socket(), Opts :: list()) -> ok | {error, any()}.
setopts(#tls_socket{ssl_socket = SSLSocket}, Opts) ->
    ssl:setopts(SSLSocket, Opts).

-spec get_peer_certificate(tls_socket()) ->
    {ok, Cert::any()} | {bad_cert, bitstring()} | no_peer_cert.
get_peer_certificate(#tls_socket{verify_results = [], ssl_socket = SSLSocket}) ->
    case ssl:peercert(SSLSocket) of
        {ok, PeerCert} ->
            Cert = public_key:pkix_decode_cert(PeerCert, plain),
            {ok, Cert};
        _ -> no_peer_cert
    end;
get_peer_certificate(#tls_socket{verify_results = [Err | _]}) ->
    {bad_cert, error_to_list(Err)}.

-spec close(tls_socket()) -> ok | {error, _}.
close(#tls_socket{ssl_socket = SSLSocket}) ->
    ssl:close(SSLSocket).

%% @doc Prepare SSL options for direct use of ssl:connect/2 (client side)
-spec make_client_opts(options()) -> [ssl:tls_option()].
make_client_opts(Opts) ->
    format_opts(Opts, client).

%% @doc Prepare SSL options for direct use of ssl:handshake/2 (server side)
-spec make_server_opts(options()) -> [ssl:tls_option()].
make_server_opts(Opts) ->
    format_opts(Opts, server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_opts(Opts, ClientOrServer) ->
    SslOpts0 = maps:to_list(maps:with(ssl_option_keys(), Opts)),
    SslOpts1 = verify_mode_opt(SslOpts0, Opts),
    SslOpts2 = verify_fun_opt(SslOpts1, Opts),
    SslOpts3 = hibernate_opt(SslOpts2, Opts),
    case ClientOrServer of
        client -> sni_opts(SslOpts3, Opts);
        server -> fail_if_no_peer_cert_opt(SslOpts3, Opts)
    end.

ssl_option_keys() ->
    [certfile, cacertfile, ciphers, keyfile, password, versions, dhfile].

%% accept empty peer certificate if explicitly requested not to fail
fail_if_no_peer_cert_opt(Opts, #{disconnect_on_failure := false}) ->
    [{fail_if_no_peer_cert, false} | Opts];
fail_if_no_peer_cert_opt(Opts, #{verify_mode := Mode})
  when Mode =:= peer; Mode =:= selfsigned_peer ->
    [{fail_if_no_peer_cert, true} | Opts];
fail_if_no_peer_cert_opt(Opts, #{}) ->
    [{fail_if_no_peer_cert, false} | Opts].

sni_opts(Opts, #{server_name_indication := #{enabled := false}}) ->
    [{server_name_indication, disable} | Opts];
sni_opts(Opts, #{server_name_indication := #{enabled := true, host := SNIHost, protocol := default}}) ->
    [{server_name_indication, SNIHost} | Opts];
sni_opts(Opts, #{server_name_indication := #{enabled := true, host := SNIHost, protocol := https}}) ->
    [{server_name_indication, SNIHost},
     {customize_hostname_check, [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]} | Opts];
sni_opts(Opts, #{}) ->
    Opts.

hibernate_opt(Opts, #{hibernate_after := Timeout}) ->
    [{hibernate_after, Timeout} | Opts];
hibernate_opt(Opts, #{}) ->
    Opts.

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
verify_fun_opt(Opts, #{verify_mode := Mode, disconnect_on_failure := false}) ->
    Alias = erlang:alias([reply]),
    [{verify_fun, verify_fun_accept_fun(Alias, Mode)} | Opts];
verify_fun_opt(Opts, #{verify_mode := selfsigned_peer}) ->
    [{verify_fun, verify_self_signed_peer_fun()} | Opts];
verify_fun_opt(Opts, #{verify_mode := _}) ->
    Opts.

verify_mode_opt(Opts, #{verify_mode := none}) ->
    [{verify, verify_none} | Opts];
verify_mode_opt(Opts, #{}) ->
    [{verify, verify_peer} | Opts].

verify_fun_accept_fun(Alias, peer) ->
    {fun(_, {bad_cert, _} = R, S) ->
             send_verification_failure(Alias, R),
             {valid, [R | S]};
        (_, {extension, #'Extension'{critical = true}}, S) ->
             send_verification_failure(Alias, unknown_critical_extension),
             {valid, [unknown_critical_extension | S]};
        (_, _, S) -> {valid, S}
     end, []};
verify_fun_accept_fun(Alias, selfsigned_peer) ->
    {fun(_, {bad_cert, B} = R, S) when B =/= selfsigned_peer ->
             send_verification_failure(Alias, R),
             {valid, [R | S]};
        (_, {extension, #'Extension'{critical = true}}, S) ->
             send_verification_failure(Alias, unknown_critical_extension),
             {valid, [unknown_critical_extension | S]};
        (_, _, S) -> {valid, S}
     end, []};
verify_fun_accept_fun(Alias, none) ->
    {fun(_, {extension, #'Extension'{critical = true}}, S) ->
             send_verification_failure(Alias, unknown_critical_extension),
             {valid, S};
        (_, _, S) -> {valid, S}
     end, []}.

verify_self_signed_peer_fun() ->
    {fun(_, {bad_cert, selfsigned_peer}, S) -> {valid, S};
        (_, {bad_cert, _} = R, _) -> {fail, R};
        (_, {extension, _}, S) -> {unknown, S};
        (_, valid, S) -> {valid, S};
        (_, valid_peer, S) -> {valid, S}
     end, []}.

send_verification_failure(Alias, Reason) ->
    Alias ! {?MODULE, cert_verification_failure, Reason}.

receive_verify_results() ->
    receive_verify_results([]).

receive_verify_results(Acc) ->
    receive
        {?MODULE, cert_verification_failure, Reason} ->
            receive_verify_results([Reason | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

error_to_list(_Error) ->
    %TODO: implement later if needed
    "verify_fun_callback failed".
