%%%=============================================================================
%%% @copyright (C) 1999-2018, Erlang Solutions Ltd
%%% @author Denys Gonchar <denys.gonchar@erlang-solutions.com>
%%% @doc a dedicated module to provide SSL certificate parsing functionality.
%%% @end
%%%=============================================================================
-module(cert_utils).
-copyright("2018, Erlang Solutions Ltd.").
-author('denys.gonchar@erlang-solutions.com').

-include("mongoose_logger.hrl").

-export([get_cert_domains/1,
         get_common_name/1,
         get_xmpp_addresses/1,
         get_dns_addresses/1
]).

-include_lib("public_key/include/public_key.hrl").
-include("XmppAddr.hrl").
-include("jlib.hrl").

-type certificate() :: #'Certificate'{}.

-spec get_common_name(certificate()) -> bitstring() | error.
get_common_name(Cert) ->
    try
        {rdnSequence, RDNSequence} = Cert#'Certificate'.tbsCertificate#'TBSCertificate'.subject,
        [{ok, {_, CN}}] = ['OTP-PUB-KEY':decode('X520CommonName', V) ||
            AtributesList <- RDNSequence,
            #'AttributeTypeAndValue'{type = ?'id-at-commonName', value = V} <- AtributesList],
        CN
    catch
        Class:Exception:StackTrace ->
            log_exception(Class,Exception,StackTrace),
            error
    end.


-spec get_xmpp_addresses(certificate()) -> [bitstring()].
get_xmpp_addresses(Cert) ->
    try
        Extensions = Cert#'Certificate'.tbsCertificate#'TBSCertificate'.extensions,
        [BinVal] = [convert_to_bin(V) ||
            #'Extension'{extnID = ?'id-ce-subjectAltName', extnValue = V} <- Extensions],
        {ok, SANs} = 'OTP-PUB-KEY':decode('SubjectAltName', BinVal),
        XmppAddresses =
            [begin
                 case 'XmppAddr':decode('XmppAddr', V) of
                     {ok, XmppAddr} -> XmppAddr;
                     Error ->
                         ?DEBUG("'XmppAddr':decode/2 failed with ~p",[Error]),
                         ok
                 end
             end || {otherName, #'AnotherName'{'type-id' = ?'id-on-xmppAddr', value = V}} <- SANs],
        [Addr || Addr <- XmppAddresses, is_binary(Addr)]
    catch
        Class:Exception:StackTrace ->
            log_exception(Class, Exception,StackTrace),
            []
    end.


-spec get_dns_addresses(certificate()) -> [string()].
get_dns_addresses(Cert) ->
    try
        Extensions = Cert#'Certificate'.tbsCertificate#'TBSCertificate'.extensions,
        [BinVal] = [convert_to_bin(V) ||
            #'Extension'{extnID = ?'id-ce-subjectAltName', extnValue = V} <- Extensions],
        {ok, SANs} = 'OTP-PUB-KEY':decode('SubjectAltName', BinVal),
        [DNS || {dNSName, DNS} <- SANs]
    catch
        Class:Exception:StackTrace ->
            log_exception(Class,Exception,StackTrace),
            []
    end.


-spec get_cert_domains(certificate()) -> [bitstring()].
get_cert_domains(Cert) ->
    CN = get_common_name(Cert),
    Addresses = get_xmpp_addresses(Cert),
    Domains = get_dns_addresses(Cert),
    lists:append([get_lserver_from_addr(CN, false) |
                  [get_lserver_from_addr(Addr, true) || Addr <- Addresses, is_binary(Addr)] ++
                  [get_lserver_from_addr(DNS, false) || DNS <- Domains, is_list(DNS)]]).


convert_to_bin(Val) when is_list(Val) ->
    list_to_binary(Val);
convert_to_bin(Val) ->
    Val.

-spec get_lserver_from_addr(bitstring() | string(), boolean()) -> [bitstring()].
get_lserver_from_addr(V, UTF8) when is_binary(V); is_list(V) ->
    Val = convert_to_bin(V),
    case {jid:from_binary(Val), UTF8} of
        {#jid{luser = <<"">>, lserver = LD, lresource = <<"">>}, true} ->
            case ejabberd_s2s:domain_utf8_to_ascii(LD) of
                false -> [];
                PCLD -> [PCLD]
            end;
        {#jid{luser = <<"">>, lserver = LD, lresource = <<"">>}, _} -> [LD];
        _ -> []
    end;
get_lserver_from_addr(_, _) -> [].


log_exception(Class,Exception,StackTrace) ->
    ?DEBUG("failed to parse certificate with ~p:~p~n\t~p~n",
           [Class,Exception,StackTrace]).
