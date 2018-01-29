-module(cert_utils).
-author('denys.gonchar@erlang-solutions.com').

-export([ get_cert_domains/1,
          get_common_name/1,
          get_xmpp_addresses/1,
          get_dns_addresses/1
        ]).

-include_lib("public_key/include/public_key.hrl").
-include("XmppAddr.hrl").
-include("jlib.hrl").

-type certificate() :: #'Certificate'{}.

-spec get_common_name(certificate()) -> string() | error.
get_common_name(Cert) ->
  try
    {rdnSequence, RDNSequence} = Cert#'Certificate'.tbsCertificate#'TBSCertificate'.subject,
    [{ok, {_, CN}}] = ['OTP-PUB-KEY':decode('X520CommonName', V) ||
      AtributesList <- RDNSequence,
      #'AttributeTypeAndValue'{type = ?'id-at-commonName', value = V} <- AtributesList],
    CN
  catch
    _:_ -> error
  end.


-spec get_xmpp_addresses(certificate()) -> [any()].
get_xmpp_addresses(Cert) ->
  try
    Extensions = Cert#'Certificate'.tbsCertificate#'TBSCertificate'.extensions,
    [BinVal] = [convert_to_bin(V) ||
      #'Extension'{extnID = ?'id-ce-subjectAltName', extnValue = V} <- Extensions],
    {ok, SANs} = 'OTP-PUB-KEY':decode('SubjectAltName', BinVal),
    [begin
       {ok, XmppAddr} = 'XmppAddr':decode('XmppAddr', V),
       XmppAddr
     end || {otherName, #'AnotherName'{'type-id' = ?'id-on-xmppAddr', value = V}} <- SANs]
  catch
    _:_ -> []
  end.


-spec get_dns_addresses(certificate()) -> [any()].
get_dns_addresses(Cert) ->
  try
    Extensions = Cert#'Certificate'.tbsCertificate#'TBSCertificate'.extensions,
    [BinVal] = [convert_to_bin(V) ||
      #'Extension'{extnID = ?'id-ce-subjectAltName', extnValue = V} <- Extensions],
    {ok, SANs} = 'OTP-PUB-KEY':decode('SubjectAltName', BinVal),
    [DNS || {dNSName, DNS} <- SANs]
  catch
    _:_ -> []
  end.


-spec get_cert_domains(certificate()) -> [any()].
get_cert_domains(Cert) ->
  CN = get_common_name(Cert),
  Addresses = get_xmpp_addresses(Cert),
  Domains = get_dns_addresses(Cert),
  lists:flatten(get_lserver_from_addr(CN,false) ++
    [get_lserver_from_addr(Addr,true) || Addr <- Addresses, is_binary(Addr)] ++
    [get_lserver_from_addr(DNS,false) || DNS <- Domains, is_list(DNS)]).


convert_to_bin(Val) when is_list(Val) ->
  list_to_binary(Val);
convert_to_bin(Val) ->
  Val.


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


