-module(cert_utils).
-author('denys.gonchar@erlang-solutions.com').

-export([ get_cert_domains/1 ]).
        
-include_lib("public_key/include/public_key.hrl").
-include("XmppAddr.hrl").
-include("jlib.hrl").

-type certificate() :: #'Certificate'{}.

-spec get_cert_domains(certificate()) ->  [any()].
get_cert_domains(Cert) ->
    {rdnSequence, Subject} =
        (Cert#'Certificate'.tbsCertificate)#'TBSCertificate'.subject,
    Extensions =
        (Cert#'Certificate'.tbsCertificate)#'TBSCertificate'.extensions,
    lists:flatmap(
      fun(#'AttributeTypeAndValue'{type = ?'id-at-commonName',
                                   value = Val}) ->
              case 'OTP-PUB-KEY':decode('X520CommonName', Val) of
                  {ok, {_, D1}} ->
                      D = convert_decoded_cn(D1),
                      get_lserver_from_decoded_cn(D);
                  _ ->
                      []
              end;
         (_) ->
              []
      end, lists:flatten(Subject)) ++
        lists:flatmap(
          fun(#'Extension'{extnID = ?'id-ce-subjectAltName',
                           extnValue = Val}) ->
                  BVal = convert_extnval_to_bin(Val),
                  DSAN = 'OTP-PUB-KEY':decode('SubjectAltName', BVal),
                  get_lserver_from_decoded_extnval(DSAN);
             (_) ->
                  []
          end, Extensions).


convert_decoded_cn(Val) when is_list(Val) ->
    list_to_binary(Val);
convert_decoded_cn(Val) when is_binary(Val) ->
    Val;
convert_decoded_cn(_) ->
    error.

get_lserver_from_decoded_cn(error) ->
    [];
get_lserver_from_decoded_cn(D) ->
    case jid:from_binary(D) of
        #jid{luser = <<"">>,
             lserver = LD,
             lresource = <<"">>} ->
            [LD];
        _ ->
            []
    end.

get_lserver_from_decoded_extnval({ok, SANs}) ->
    lists:flatmap(
      fun({otherName,
           #'AnotherName'{'type-id' = ?'id-on-xmppAddr',
                          value = XmppAddr
                         }}) ->
              D = 'XmppAddr':decode('XmppAddr', XmppAddr),
              get_idna_lserver_from_decoded_xmpp_addr(D);
         ({dNSName, D}) when is_list(D) ->
              JID = jid:from_binary(list_to_binary(D)),
              get_lserver_from_jid(JID);
         (_) ->
              []
      end, SANs);
get_lserver_from_decoded_extnval(_) ->
    [].

get_idna_lserver_from_decoded_xmpp_addr({ok, D}) when is_binary(D) ->
    case jid:from_binary(D) of
        #jid{luser = <<"">>,
             lserver = LD,
             lresource = <<"">>} ->
            case ejabberd_s2s:domain_utf8_to_ascii(LD) of
                false ->
                    [];
                PCLD ->
                    [PCLD]
            end;
        _ ->
            []
    end;
get_idna_lserver_from_decoded_xmpp_addr(_) ->
    [].

get_lserver_from_jid(#jid{luser = <<"">>,
                     lserver = LD,
                     lresource = <<"">>}) ->
    [LD];
get_lserver_from_jid(_) ->
    [].

convert_extnval_to_bin(Val) when is_list(Val) ->
    list_to_binary(Val);
convert_extnval_to_bin(Val) ->
    Val.
    
