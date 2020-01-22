%%%=============================================================================
%%% @copyright (C) 1999-2019, Erlang Solutions Ltd
%%% @author Denys Gonchar <denys.gonchar@erlang-solutions.com>
%%% @doc cyrsasl external verification backend
%%%
%%% this module is added for demo & testing purposes only.
%%%
%%% the next extra fields are added to mongoose_credentials record by cyrsasl_external:
%%%   * pem_cert        - certificate in PEM format
%%%   * der_cert        - certificate in DER format
%%%   * common_name     - CN field (bitstring) of the client's cert (if available)
%%%   * xmpp_addresses  - list of provided "xmppAddr" fields (bare jids) provided in
%%%                       the client's certificate (empty list if not available)
%%%   * auth_id         - authorization identity (bare jid, if provided by the client)
%%%
%%% @end
%%% Created : 17. Apr 2019 11:49
%%%-------------------------------------------------------------------
-module(cyrsasl_external_verification).
-copyright("2018, Erlang Solutions Ltd.").
-author('denys.gonchar@erlang-solutions.com').

-behaviour(cyrsasl_external).

%% API
-export([verify_creds/1]).

-include("jid.hrl").


-spec verify_creds(Creds :: mongoose_credentials:t()) ->
    {ok, Username :: binary()} | {error, Error :: binary()}.

verify_creds(Creds) ->
    AuthId = mongoose_credentials:get(Creds, auth_id, undefined),
    XmppAddr = case mongoose_credentials:get(Creds, xmpp_addresses) of
                   [Addr] -> Addr;
                   _ -> undefined
               end,
    CN = mongoose_credentials:get(Creds, common_name, undefined),
    [JID | _] = [Name || Name <- [AuthId, XmppAddr, CN, <<"">>], Name =/= undefined],
    Server = mongoose_credentials:lserver(Creds),
    case jid:from_binary(JID) of
        #jid{luser = User, lserver = Server, lresource = <<"">>} when User =/= <<"">> ->
            {ok, User};
        _ -> {error, <<"not-authorized">>}
    end.
