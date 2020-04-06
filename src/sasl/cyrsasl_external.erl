%%%=============================================================================
%%% @copyright (C) 1999-2020, Erlang Solutions Ltd
%%% @doc SASL EXTERNAL implementation (XEP178)
%%%
%%% SASL EXTERNAL mechanism requires client's SSL certificate. the purpose of
%%% this module is to parse the certificate and get authorization identity (if
%%% any provided by the client). this module doesn't make authorization, it
%%% only prepares all the data and provides it to auth. backend.
%%%
%%% the next extra fields are added to the mongoose_credentials record by this
%%% module:
%%%   * pem_cert        - certificate in PEM format
%%%   * der_cert        - certificate in DER format
%%%   * common_name     - CN field (bitstring) of the client's cert (if available)
%%%   * xmpp_addresses  - list of provided "xmppAddr" fields (bare jids) provided in
%%%                       the client's certificate (empty list if not available)
%%%   * auth_id         - authorization identity (bare jid, if provided by the client)
%%%
%%% @end
%%%=============================================================================
-module(cyrsasl_external).

-xep([{xep, 178}, {version, "1.1"}, {comment, "partially implemented."}]).

-include("mongoose.hrl").
-include("jlib.hrl").

-export([mechanism/0, mech_new/2, mech_step/2]).

-behaviour(cyrsasl).

-callback verify_creds(Creds :: mongoose_credentials:t()) ->
    {ok, Username :: binary()} | {error, Error :: binary()}.

-record(state, {creds :: mongoose_credentials:t()}).
-type sasl_external_state() :: #state{}.

-spec mechanism() -> cyrsasl:mechanism().
mechanism() ->
    <<"EXTERNAL">>.

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
            NewCreds = maybe_add_auth_id(Creds, User),
            do_mech_step(NewCreds)
    end.

%%%=============================================================================
%%%  local functions
%%%=============================================================================
get_common_name(Cert) ->
    case cert_utils:get_common_name(Cert) of
        error -> [];
        CN -> [{common_name, CN}]
    end.

get_xmpp_addresses(Cert) ->
    XmmpAddresses = cert_utils:get_xmpp_addresses(Cert),
    [{xmpp_addresses, XmmpAddresses}].

maybe_add_auth_id(Creds, <<"">>) ->
    Creds;
maybe_add_auth_id(Creds, User) ->
    mongoose_credentials:set(Creds, auth_id, User).

do_mech_step(Creds) ->
    Server = mongoose_credentials:lserver(Creds),
    VerificationList = get_verification_list(Server),
    case verification_loop(VerificationList, Creds) of
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

get_verification_list(Server) ->
    case ejabberd_auth:get_opt(Server, cyrsasl_external, [standard]) of
        [] -> [standard];
        List when is_list(List) -> List;
        standard -> [standard];
        use_common_name -> [standard, common_name];
        allow_just_user_identity -> [standard, auth_id]
    end.

verification_loop([VerificationFn | T], Creds) ->
    case verify_creds(VerificationFn, Creds) of
        {error, Error} when T =:= [] ->
            {error, Error};
        {error, _} ->
            verification_loop(T, Creds);
        {ok, Name} ->
            {ok, Name}
    end.

verify_creds(standard, Creds) ->
    standard_verification(Creds);
verify_creds(common_name, Creds) ->
    common_name_verification(Creds);
verify_creds(auth_id, Creds) ->
    auth_id_verification(Creds);
verify_creds({mod, Mod}, Creds) ->
    custom_verification(Mod, Creds).


standard_verification(Creds) ->
    Server = mongoose_credentials:lserver(Creds),
    XmppAddrs = get_credentials(Creds, xmpp_addresses),
    AuthId = get_credentials(Creds, auth_id),
    XmppAddr = case {XmppAddrs, AuthId} of
                   {[OneXmppAddr], undefined} ->
                       OneXmppAddr;
                   {[_], _} ->
                       {error, <<"invalid-authzid">>};
                   {[_, _ | _], undefined} ->
                       {error, <<"invalid-authzid">>};
                   _ ->
                       case lists:member(AuthId, XmppAddrs) of
                           true ->
                               AuthId;
                           _ ->
                               {error, <<"not-authorized">>}
                       end
               end,
    verify_server(XmppAddr, Server).

common_name_verification(Creds) ->
    Server = mongoose_credentials:lserver(Creds),
    AuthId = get_credentials(Creds, auth_id),
    CommonName = get_credentials(Creds, common_name),
    case AuthId of
        undefined when is_binary(CommonName) ->
            {ok, CommonName};
        _ ->
            case verify_server(AuthId, Server) of
                {ok, CommonName} -> {ok, CommonName};
                _ -> {error, <<"invalid-authzid">>}
            end
    end.

auth_id_verification(Creds) ->
    Server = mongoose_credentials:lserver(Creds),
    AuthId = get_credentials(Creds, auth_id),
    verify_server(AuthId, Server).

custom_verification(Module, Creds) ->
    try
        case apply(Module, verify_creds, [Creds]) of
            {ok, Username} when is_binary(Username) ->
                {ok, Username};
            {error, Error} when is_binary(Error) ->
                {error, Error};
            InvalidReturnValue ->
                ?ERROR_MSG("~p:verify_cert/1 invalid return value: ~p", [Module, InvalidReturnValue]),
                {error, <<"not-authorized">>}
        end
    catch
        Class:Exception ->
            ?ERROR_MSG("~p:verify_cert/1 crashed: ~p", [Module, {Class, Exception}]),
            {error, <<"not-authorized">>}
    end.

get_credentials(Cred, Key) ->
    mongoose_credentials:get(Cred, Key, undefined).

verify_server(undefined, _Server) ->
    {error, <<"not-authorized">>};
verify_server({error, Error}, _Server) ->
    {error, Error};
verify_server(Jid, Server) ->
    JidRecord = jid:binary_to_bare(Jid),
    case JidRecord#jid.lserver of
        Server ->
            {ok, JidRecord#jid.user};
        _ ->
            {error, <<"not-authorized">>}
    end.

