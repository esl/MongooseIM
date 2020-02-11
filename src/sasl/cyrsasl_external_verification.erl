%%%=============================================================================
%%% @copyright (C) 1999-2020, Erlang Solutions Ltd
%%% @doc cyrsasl external verification backend
%%%
%%% this module is added only for the demo and testing purposes.
%%%
%%% if your custom verification backend requires any initialisation/termination
%%% logic, it can be added by implementation of 'gen_mod' or 'mongoose_service'
%%% behaviour.
%%%
%%% the next extra fields are added to the mongoose_credentials record by
%%% the cyrsasl_external module:
%%%   * pem_cert        - certificate in PEM format
%%%   * der_cert        - certificate in DER format
%%%   * common_name     - CN field (bitstring) of the client's cert (if available)
%%%   * xmpp_addresses  - list of provided "xmppAddr" fields (bare jids) provided in
%%%                       the client's certificate (empty list if not available)
%%%   * auth_id         - authorization identity (bare jid, if provided by the client)
%%%
%%% this verification module picks user name of a JID provided in one of the following
%%% sources:
%%%   * auth_id (if provided by the client)
%%%   * xmpp_addresses (ignored if list is empty or contains more than one JID)
%%%   * common_name
%%% sources are checked in the same order as mentioned in the list above, the first
%%% successful source is selected. the server part of the JID is verified and it must
%%% correspond to the host were user is trying to connect.
%%% @end
%%%=============================================================================
-module(cyrsasl_external_verification).
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
