-module(cyrsasl_external).
-author('denys.gonchar@erlang-solutions.com').

-xep([{xep, 178}, {version, "1.1"}, {comment, "partially implemented."}]).

-export([start/1, stop/0, mech_new/2, mech_step/2]).

-behaviour(cyrsasl).

-record(state, {creds}).

start(_Opts) ->
    cyrsasl:register_mechanism(<<"EXTERNAL">>, ?MODULE, cert),
    ok.

stop() ->
    ok.

-spec mech_new(Host :: ejabberd:server(),
               Creds :: mongoose_credentials:t()) -> {ok, tuple()}.
mech_new(_Host, Creds) ->
  case mongoose_credentials:get(Creds,client_cert,undefined) of
    undefined -> {ok, #state{creds = invalid_cert}};
    Cert ->
      DerCert = public_key:pkix_encode('Certificate', Cert, plain),
      PemCert = public_key:pem_encode([{'Certificate', DerCert, not_encrypted}]),
      ExtraFields = get_common_name(Cert) ++ get_xmpp_addresses(Cert),
      {ok, #state{creds = mongoose_credentials:extend(Creds, [ {pem_cert, PemCert},
                                                               {der_cert, DerCert}
                                                               | ExtraFields
                                                             ])}}
  end.

-spec mech_step(State :: tuple(), ClientIn :: binary()) -> R when
      R :: {ok, mongoose_credentials:t()} | {error, binary()}.
mech_step(#state{creds = invalid_cert}, _) ->
  {error, <<"not-authorized">>};
mech_step(#state{creds = Creds}, <<"">>) ->
  authorize(Creds);
mech_step(#state{creds = Creds}, User) ->
  authorize(mongoose_credentials:set(Creds, requested_name, User)).

authorize(Creds) ->
  %% auth backend is responsible to add username to Creds.
  case ejabberd_auth:authorize(Creds) of
    {ok, NewCreds} -> {ok, NewCreds};
    {error, invalid_authid} -> {error, <<"invalid-authzid">>};
    _ -> {error, <<"not-authorized">>}
  end.

get_common_name(Cert) ->
  case cert_utils:get_common_name(Cert) of
    error -> [];
    CN -> [{common_name, CN}]
  end.

get_xmpp_addresses(Cert) ->
  case cert_utils:get_xmpp_addresses(Cert) of
    [] -> [];
    XmmpAddresses -> [{xmpp_addressses, XmmpAddresses}]
  end.