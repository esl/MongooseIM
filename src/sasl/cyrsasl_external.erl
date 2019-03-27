%%%=============================================================================
%%% @copyright (C) 1999-2018, Erlang Solutions Ltd
%%% @author Denys Gonchar <denys.gonchar@erlang-solutions.com>
%%% @doc SASL EXTERNAL implementation (XEP178)
%%%
%%% SASL EXTERNAL mechanism requires client's SSL certificate. the purpose of
%%% this module is to parse the certificate & get authorization identity (if
%%% any provided by the client). this module doesn't make authorization, it
%%% only prepares all the data and provides it to auth. backend.
%%%
%%% the next extra fields are added to mongoose_credentials record:
%%%   * pem_cert        - certificate in PEM format
%%%   * der_cert        - certificate in DER format
%%%   * common_name     - CN field (bitstring) of client's cert (if available)
%%%   * xmpp_addresses  - list of "id-on-xmppAddr" fields (bitstrings) of
%%%                       client's certificate (if available)
%%%   * requested_name  - authorization identity (if requested by the client)
%%%
%%% note that it's auth. backend responsibility to add clients "username" field
%%% to mongoose_credentials record (this one is mandatory for auth.).
%%% @end
%%%=============================================================================
-module(cyrsasl_external).
-copyright("2018, Erlang Solutions Ltd.").
-author('denys.gonchar@erlang-solutions.com').

-xep([{xep, 178}, {version, "1.1"}, {comment, "partially implemented."}]).

-include("mongoose.hrl").
-include("jlib.hrl").

-export([start/1, stop/0, mech_new/2, mech_step/2]).

-behaviour(cyrsasl).

-record(state, {creds :: mongoose_credentials:t()}).
-type sasl_external_state() :: #state{}.

start(_Opts) ->
    cyrsasl:register_mechanism(<<"EXTERNAL">>, ?MODULE, cert),
    ok.

stop() ->
    ok.

-spec mech_new(Host :: ejabberd:server(),
               Creds :: mongoose_credentials:t()) -> {ok, sasl_external_state()}.
mech_new(_Host, Creds) ->
    Cert = mongoose_credentials:get(Creds, client_cert, no_cert),
    maybe_extract_certs(Cert, Creds).

maybe_extract_certs(no_cert, Creds) ->
    {ok, #state{creds = mongoose_credentials:extend(Creds, [{cert_file, false}])}};
maybe_extract_certs(Cert, Creds) ->
    DerCert = public_key:pkix_encode('Certificate', Cert, plain),
    PemCert = public_key:pem_encode([{'Certificate', DerCert, not_encrypted}]),
    CertFields = get_common_name(Cert) ++ get_xmpp_addresses(Cert),
    SaslExternalCredentials = [{cert_file, true}, {pem_cert, PemCert}, {der_cert, DerCert} | CertFields],
    {ok, #state{creds = mongoose_credentials:extend(Creds, SaslExternalCredentials)}}.


-spec mech_step(State :: sasl_external_state(),
                ClientIn :: binary()) -> {ok, mongoose_credentials:t()} | {error, binary()}.
mech_step(#state{creds = Creds}, User) ->
    case mongoose_credentials:get(Creds, cert_file) of
        false ->
            {error, <<"not-authorized">>};
        true ->
            do_mech_step(Creds, User)
    end.
do_mech_step(Creds, User) ->
    XmppAddrs = get_credentials(Creds, xmpp_addresses),
    CommonName = get_credentials(Creds, common_name),
    Server = mongoose_credentials:lserver(Creds),
    case check_auth_req(XmppAddrs, CommonName, User, Server) of
        {error, Error} ->
            {error, Error};
        {ok, Name} ->
            NewCreds = mongoose_credentials:extend(Creds, [{username, Name}]),
            authorize(NewCreds)
    end.

authorize(Creds) ->
    case ejabberd_auth:authorize(Creds) of
        {ok, NewCreds} -> {ok, NewCreds};
        {error, not_authorized} -> {error, <<"not-authorized">>}
    end.

check_auth_req([], CommonName, <<"">>, Server) ->
    case ejabberd_auth:get_opt(Server, cyrsasl_external, standard) of
        use_common_name when is_binary(CommonName) ->
            {ok, CommonName};
        _ ->
            {error, <<"not-authorized">>}
    end;
check_auth_req([OneXmppAddr], _, <<"">>, Server) ->
    verify_server(OneXmppAddr, Server);
check_auth_req(_, _,  <<"">>, _) ->
    {error, <<"not-authorized">>};
check_auth_req([], undefined,  User, Server) ->
    verify_server(User, Server);
check_auth_req([], CommonName,  User, Server) ->
    CNOption = ejabberd_auth:get_opt(Server, cyrsasl_external, standard),
    maybe_use_common_name(CommonName, User, Server, CNOption);
check_auth_req([_], _,  _, _) ->
    {error, <<"invalid-authzid">>};
check_auth_req(XmppAddrs, _,  User, Server) ->
    case lists:member(User, XmppAddrs) of
        true ->
            verify_server(User, Server);
        _ ->
            {error, <<"not-authorized">>}
    end.

maybe_use_common_name(_, User, Server, allow_just_user_identity) ->
    verify_server(User, Server);
maybe_use_common_name(CommonName, User, Server, use_common_name) ->
    case verify_server(User, Server) of
        {ok, CommonName} ->
            {ok, CommonName};
        _ ->
            {error, <<"not-authorized">>}
    end;
maybe_use_common_name(_, _, _, standard) ->
    {error, <<"not-authorized">>}.

get_common_name(Cert) ->
    case cert_utils:get_common_name(Cert) of
        error -> [];
        CN -> [{common_name, CN}]
    end.

get_xmpp_addresses(Cert) ->
    case cert_utils:get_xmpp_addresses(Cert) of
        [] -> [{xmpp_addresses, []}];
        XmmpAddresses -> [{xmpp_addresses, XmmpAddresses}]
    end.

get_credentials(Cred, Key) ->
    mongoose_credentials:get(Cred, Key, undefined).

verify_server(Jid, Server) ->
    JidRecord = jid:binary_to_bare(Jid),
    case JidRecord#jid.lserver of
        Server ->
            {ok, JidRecord#jid.user};
        _ ->
            {error, <<"not-authorized">>}
    end.
