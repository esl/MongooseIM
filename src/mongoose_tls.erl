%%%=============================================================================
%%% @copyright (C) 1999-2018, Erlang Solutions Ltd
%%% @author Denys Gonchar <denys.gonchar@erlang-solutions.com>
%%% @doc this module provides general TLS interface for MongooseIM.
%%%=============================================================================
-module(mongoose_tls).
-copyright("2018, Erlang Solutions Ltd.").
-author('denys.gonchar@erlang-solutions.com').

%% tls interfaces required by mongoose_transport module.
-export([tcp_to_tls/2,
         default_ciphers/0,
         send/2,
         recv_data/2,
         controlling_process/2,
         sockname/1,
         peername/1,
         setopts/2,
         close/1]).

-export([get_sockmod/1]).

-ignore_xref([behaviour_info/1, close/1, controlling_process/2, peername/1,
              send/2, setopts/2, sockname/1]).

-ignore_xref([get_sockmod/1]).

-type tls_socket() :: just_tls:tls_socket().
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
    PreparedOpts = maps:remove(module, Opts),
    case just_tls:tcp_to_tls(TCPSocket, PreparedOpts) of
        {ok, TLSSocket} ->
            HasCert = has_peer_cert(Opts),
            {ok, #mongoose_tls_socket{tls_module = just_tls,
                                      tcp_socket = TCPSocket,
                                      tls_socket = TLSSocket,
                                      tls_opts   = Opts,
                                      has_cert   = HasCert}};
        Error -> Error
    end.

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
