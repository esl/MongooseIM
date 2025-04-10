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
                     verify_mode := peer | selfsigned_peer | none,
                     mode => tls | starttls | starttls_required, % only mongoose_s2s_out doesn't use it (yet)
                     certfile => string(),
                     cacertfile => string(),
                     ciphers => string(),
                     dhfile => string(), % server-only
                     disconnect_on_failure => boolean(),
                     keyfile => string(),
                     password => string(),
                     versions => [atom()],
                     server_name_indication => sni_options(), % client-only
                     early_data => boolean(),
                     session_tickets => stateless
                    }.

-type sni_options() :: #{enabled := boolean,
                         protocol := default | https,
                         host => string()}.

-export_type([options/0, cert/0]).

-export([tcp_to_tls/3]).

% API
-export([receive_verify_results/0, error_to_list/1,
         make_client_opts/1, make_server_opts/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% APIs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec tcp_to_tls(inet:socket(), options(), mongoose_xmpp_socket:side()) ->
    {ok, ssl:sslsocket()} | {error, any()}.
tcp_to_tls(Socket, Opts, client) ->
    TlsOpts = format_opts(Opts, client),
    inet:setopts(Socket, [{active, false}]),
    ssl:connect(Socket, TlsOpts, 5000);
tcp_to_tls(Socket, Opts, server) ->
    TlsOpts = format_opts(Opts, server),
    inet:setopts(Socket, [{active, false}]),
    ssl:handshake(Socket, TlsOpts, 5000).

%% @doc Prepare SSL options for direct use of ssl:connect/2 (client side)
-spec make_client_opts(options()) -> [ssl:tls_client_option()].
make_client_opts(Opts) ->
    format_opts(Opts, client).

%% @doc Prepare SSL options for direct use of ssl:handshake/2 (server side)
-spec make_server_opts(options()) -> [ssl:tls_server_option()].
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
    SslOpts4 = session_tickets_opt(ClientOrServer, SslOpts3, Opts),
    SslOpts5 = early_data_opt(ClientOrServer, SslOpts4, Opts),
    SslOpts6 = maybe_use_system_certificates(SslOpts5, Opts),
    case ClientOrServer of
        client -> sni_opts(SslOpts6, Opts);
        server -> fail_if_no_peer_cert_opt(SslOpts6, Opts)
    end.

ssl_option_keys() ->
    [certfile, cacertfile, ciphers, keyfile, password, versions, dhfile].

%% Use CA certificates provided by the OS when `verify_mode` is peer
%% or selfsigned_peer and `cacertfile` was not set
maybe_use_system_certificates(Opts, #{cacertfile := _}) ->
    Opts;
maybe_use_system_certificates(Opts, #{verify_mode := Mode})
  when Mode =:= peer; Mode =:= selfsigned_peer ->
    [{cacerts, public_key:cacerts_get()} | Opts];
maybe_use_system_certificates(Opts, #{}) ->
    Opts.

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

session_tickets_opt(server, SslOpts, _Opts = #{session_tickets := stateless}) ->
    [{session_tickets, stateless} | SslOpts];
session_tickets_opt(_ClientOrServer, SslOpts, _Opts) ->
    SslOpts.

early_data_opt(server, SslOpts, #{early_data := true}) ->
    [{early_data, enabled} | SslOpts];
early_data_opt(_ClientOrServer, SslOpts, _Opts) ->
    SslOpts.

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
