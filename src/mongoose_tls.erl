%%%=============================================================================
%%% @copyright (C) 1999-2018, Erlang Solutions Ltd
%%% @author Denys Gonchar <denys.gonchar@erlang-solutions.com>
%%% @doc this module provides general TLS interface for MongooseIM.
%%%
%%% by default tls_module is set to fast_tls, alternatively it can be any
%%% module that implements mongoose_tls behaviour
%%% @end
%%%=============================================================================
-module(mongoose_tls).
-copyright("2018, Erlang Solutions Ltd.").
-author('denys.gonchar@erlang-solutions.com').

%% tls interfaces required by mongoose_transport module.
-export([prepare_options/2,
         tcp_to_tls/2,
         default_ciphers/0,
         send/2,
         recv_data/2,
         controlling_process/2,
         sockname/1,
         peername/1,
         setopts/2,
         get_peer_certificate/1,
         close/1]).

-export([get_sockmod/1]).

-ignore_xref([behaviour_info/1, close/1, controlling_process/2, peername/1,
              send/2, setopts/2, sockname/1]).

-ignore_xref([get_sockmod/1]).

-type tls_socket() :: fast_tls:tls_socket() | just_tls:tls_socket().
-type cert() :: {ok, Cert::any()} | {bad_cert, bitstring()} | no_peer_cert.

%% Options used for client-side and server-side TLS connections.
%% All modules implementing this behaviour have to support the mandatory 'verify_mode' option.
%% Other options should be supported if the implementing module supports it.
-type options() :: #{module => module(), % fast_tls by default
                     connect => boolean(), % set to 'true' for a client-side call to tcp_to_tls/2
                     verify_mode := peer | selfsigned_peer | none,
                     certfile => string(),
                     cacertfile => string(),
                     ciphers => string(),
                     dhfile => string(), % server-only

                     %% only for just_tls
                     disconnect_on_failure => boolean(),
                     keyfile => string(),
                     password => string(),
                     versions => [atom()],
                     server_name_indication => sni_options(), % client-only

                     % only for fast_tls
                     protocol_options => [string()]}.

-type sni_options() :: #{enabled := boolean,
                         protocol := default | https,
                         host => string()}.

-export_type([options/0,
              tls_socket/0,
              cert/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% behaviour definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-callback tcp_to_tls(inet:socket(), options()) -> {ok, tls_socket()} | {error, any()}.

-callback send(tls_socket(), binary()) -> ok | {error , any()}.

-callback recv_data(tls_socket(), binary()) -> {ok, binary()} | {error, any()}.

-callback controlling_process(tls_socket(), pid()) -> ok | {error, any()}.

-callback sockname(tls_socket()) -> {ok, mongoose_transport:peer()} |
                                    {error, any()}.

-callback peername(tls_socket()) -> {ok, mongoose_transport:peer()} |
                                    {error, any()}.

-callback setopts(tls_socket(), Opts::list()) -> ok | {error, any()}.

-callback get_peer_certificate(tls_socket()) -> cert().

-callback close(tls_socket()) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% socket type definition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(mongoose_tls_socket, {tls_module :: module(),
                              tcp_socket :: inet:socket(),
                              tls_socket :: tls_socket(),
                              tls_opts :: options(),
                              has_cert :: boolean()
}).

-type socket() :: #mongoose_tls_socket{}.

-export_type([socket/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% APIs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec tcp_to_tls(inet:socket(), options()) -> {ok, socket()} | {error, any()}.
tcp_to_tls(TCPSocket, Opts) ->
    Module = maps:get(module, Opts, fast_tls),
    PreparedOpts = prepare_options(Module, maps:remove(module, Opts)),
    case Module:tcp_to_tls(TCPSocket, PreparedOpts) of
        {ok, TLSSocket} ->
            HasCert = has_peer_cert(Opts),
            {ok, #mongoose_tls_socket{tls_module = Module,
                                      tcp_socket = TCPSocket,
                                      tls_socket = TLSSocket,
                                      tls_opts   = Opts,
                                      has_cert   = HasCert}};
        Error -> Error
    end.

-spec prepare_options(module(), options()) -> any().
prepare_options(fast_tls, Opts) ->
    %% fast_tls is an external library and its API cannot use Opts directly
    lists:flatmap(fun({K, V}) -> fast_tls_opt(K, V) end, maps:to_list(Opts));
prepare_options(_Module, Opts) ->
    Opts.

fast_tls_opt(connect, true) -> [connect];
fast_tls_opt(connect, false) -> [];
fast_tls_opt(mode, _) -> [];
fast_tls_opt(verify_mode, peer) -> [];
fast_tls_opt(verify_mode, none) -> [verify_none];
fast_tls_opt(cacertfile, File) -> [{cafile, File}];
fast_tls_opt(dhfile, File) -> [{dhfile, File}];
fast_tls_opt(certfile, File) -> [{certfile, File}];
fast_tls_opt(ciphers, Ciphers) -> [{ciphers, Ciphers}];
fast_tls_opt(protocol_options, ProtoOpts) -> [{protocol_options, string:join(ProtoOpts, "|")}].

default_ciphers() ->
    "TLSv1.2:TLSv1.3".

-spec send(socket(), binary()) -> ok | {error, any()}.
send(#mongoose_tls_socket{tls_module = M, tls_socket = S}, B) -> M:send(S, B).

-spec recv_data(socket(), binary()) -> {ok, binary()} | {error, any()}.
recv_data(#mongoose_tls_socket{tls_module = M, tls_socket = S}, B) -> M:recv_data(S, B).

-spec controlling_process(socket(), pid()) -> ok | {error, any()}.
controlling_process(#mongoose_tls_socket{tls_module = M, tls_socket = S}, Pid) ->
    M:controlling_process(S, Pid).

-spec sockname(socket()) -> {ok, mongoose_transport:peer()} | {error, any()}.
sockname(#mongoose_tls_socket{tls_module = M, tls_socket = S}) -> M:sockname(S).

-spec peername(socket()) -> {ok, mongoose_transport:peer()} | {error, any()}.
peername(#mongoose_tls_socket{tls_module = M, tls_socket = S}) -> M:peername(S).

-spec setopts(socket(), Opts::list()) -> ok | {error, any()}.
setopts(#mongoose_tls_socket{tls_module = M, tls_socket = S}, Opts) -> M:setopts(S, Opts).

-spec get_peer_certificate(socket()) -> cert().
get_peer_certificate(#mongoose_tls_socket{has_cert = false}) ->
    no_peer_cert;
get_peer_certificate(#mongoose_tls_socket{tls_module = just_tls, tls_socket = S}) ->
    just_tls:get_peer_certificate(S);
get_peer_certificate(#mongoose_tls_socket{tls_module = fast_tls, tls_socket = S,
                                          tls_opts = TLSOpts}) ->
    case {fast_tls:get_verify_result(S), fast_tls:get_peer_certificate(S)} of
        {0, {ok, Cert}} -> {ok, Cert};
        {Error, {ok, Cert}} ->
            maybe_allow_selfsigned(Error, Cert, TLSOpts);
        {_, error} -> no_peer_cert
    end.

-spec close(socket()) -> ok.
close(#mongoose_tls_socket{tls_module = M, tls_socket = S}) -> M:close(S).

-spec get_sockmod(socket()) -> module().
get_sockmod(#mongoose_tls_socket{tls_module = Module}) -> Module.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec has_peer_cert(options()) -> boolean().
has_peer_cert(#{connect := true}) ->
    true; % server always provides cert
has_peer_cert(#{verify_mode := VerifyMode}) ->
    VerifyMode =/= none. % client provides cert only when requested

%% 18 is OpenSSL's and fast_tls's error code for self-signed certs
maybe_allow_selfsigned(18, Cert, #{verify_mode := selfsigned_peer}) ->
    {ok, Cert};
maybe_allow_selfsigned(Error, Cert, _SSLOpts) ->
    cert_verification_error(Error, Cert).

cert_verification_error(Error, Cert) ->
    {bad_cert, fast_tls:get_cert_verify_string(Error, Cert)}.
