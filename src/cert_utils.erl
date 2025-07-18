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
         get_xmpp_addresses/1
]).

-include_lib("public_key/include/public_key.hrl").
-include("XmppAddr.hrl").
-include("jlib.hrl").

-type certificate() :: binary() | #'Certificate'{} | #'OTPCertificate'{}.

-spec get_common_name(certificate()) -> bitstring() | error.
get_common_name(Cert0) ->
    try
        Cert = ensure_cert(Cert0),
        #'OTPCertificate'{tbsCertificate = TBSCert} = Cert,
        #'OTPTBSCertificate'{subject = {rdnSequence, RDNs}} = TBSCert,
        CNs = [V || RDN <- RDNs,
                     #'AttributeTypeAndValue'{type = ?'id-at-commonName', value = V} <- RDN],
        case CNs of
            [CN0 | _] ->
                normalize_dirstring(CN0);
            [] ->
                error
         end
    catch
        Class:Exception:StackTrace ->
            log_exception(Cert0, Class, Exception, StackTrace),
            error
    end.


-spec get_xmpp_addresses(certificate()) -> [bitstring()].
get_xmpp_addresses(Cert0) ->
    try
        Cert = ensure_cert(Cert0),
        SANs = subject_alt_names(Cert),
        XmppAddrs =
            [maybe_decode_xmpp(unwrap_asn1(Val))
             || {otherName, ON} <- SANs,
                {Oid, Val} <- [unwrap_othername(ON)],
                Oid =:= ?'id-on-xmppAddr'],
        [Addr || Addr <- XmppAddrs, is_binary(Addr)]
    catch
        Class:Exception:StackTrace ->
            log_exception(Cert0, Class, Exception,StackTrace),
            []
    end.


-spec get_dns_addresses(certificate()) -> [string()].
get_dns_addresses(Cert0) ->
    try
        Cert = ensure_cert(Cert0),
        SANs = subject_alt_names(Cert),
        [DNS || {dNSName, DNS} <- SANs]
    catch
        Class:Exception:StackTrace ->
            log_exception(Cert0, Class, Exception, StackTrace),
            []
    end.


-spec get_cert_domains(certificate()) -> [bitstring()].
get_cert_domains(Cert0) ->
    Cert = ensure_cert(Cert0),
    CN = get_common_name(Cert),
    Addresses = get_xmpp_addresses(Cert),
    Domains = get_dns_addresses(Cert),
    CNs = case CN of error -> []; _ -> get_lserver_from_addr(CN, false) end,
    lists:append(
        [CNs |
         [get_lserver_from_addr(Addr, true) || Addr <- Addresses, is_binary(Addr)] ++
         [get_lserver_from_addr(DNS, false) || DNS <- Domains, is_list(DNS)]]).


convert_to_bin(Val) when is_list(Val) ->
    list_to_binary(Val);
convert_to_bin(Val) ->
    Val.

-spec get_lserver_from_addr(bitstring() | string(), boolean()) -> [binary()].
get_lserver_from_addr(V, UTF8) when is_binary(V); is_list(V) ->
    Val = convert_to_bin(V),
    case jid:from_binary(Val) of
        #jid{lserver = LD, lresource = <<"">>} ->
            case UTF8 of
                true ->
                    case mongoose_s2s_lib:domain_utf8_to_ascii(LD, binary) of
                        false -> [];
                        PCLD -> [PCLD]
                    end;
                false ->
                    [LD]
            end;
        _ ->
            []
    end;
get_lserver_from_addr(_, _) -> [].


log_exception(_Cert, Class, Exception, StackTrace) ->
    ?LOG_ERROR(#{what => <<"cert_parsing_failed">>,
                 text => <<"failed to parse certificate">>,
                 class => Class, reason => Exception, stacktrace => StackTrace}).

-spec ensure_cert(certificate()) -> #'OTPCertificate'{}.
ensure_cert(#'OTPCertificate'{} = Cert) ->
    Cert;
ensure_cert(#'Certificate'{} = PlainCert) ->
    Der = public_key:pkix_encode('Certificate', PlainCert, plain),
    public_key:pkix_decode_cert(Der, otp);
ensure_cert(Bin) when is_binary(Bin) ->
    public_key:pkix_decode_cert(Bin, otp).

-spec subject_alt_names(#'OTPCertificate'{}) -> [public_key:general_name()].
subject_alt_names(#'OTPCertificate'{tbsCertificate = TBSCert}) ->
    #'OTPTBSCertificate'{extensions = Exts} = TBSCert,
    case lists:keyfind(?'id-ce-subjectAltName', #'Extension'.extnID, Exts) of
        #'Extension'{extnValue = SANs} -> SANs;
        false -> []
    end.

maybe_decode_xmpp(Bin) when is_binary(Bin) ->
    case 'XmppAddr':decode('XmppAddr', Bin) of
        {ok, XmppAddr} -> XmppAddr;
        Error ->
            ?LOG_DEBUG(#{what => get_xmpp_addresses_failed,
                         text => <<"'XmppAddr':decode/2 failed">>,
                         reason => Error}),
            ok
    end;
maybe_decode_xmpp(_) ->
    ok.

normalize_dirstring({utf8String, Bin}) when is_binary(Bin) -> Bin;
normalize_dirstring({printableString, Str}) when is_list(Str) -> list_to_binary(Str);
normalize_dirstring({ia5String, Str}) when is_list(Str) -> list_to_binary(Str);
normalize_dirstring(Bin) when is_binary(Bin) -> Bin;
normalize_dirstring(Str) when is_list(Str) -> list_to_binary(Str);
normalize_dirstring(_) -> <<>>.

unwrap_asn1({'INSTANCE OF', _Oid, Bin}) when is_binary(Bin) -> Bin;
unwrap_asn1({asn1_OPENTYPE, Bin}) -> Bin;
unwrap_asn1(Bin) when is_binary(Bin) -> Bin;
unwrap_asn1(Val) -> Val.

unwrap_othername({'INSTANCE OF', Oid, Val}) -> {Oid, Val};
%% OTP 27 (plain decode) represents OtherName as a 3‑tuple
unwrap_othername(Another)
    when is_tuple(Another),
         tuple_size(Another) =:= 3,
         element(1, Another) =:= 'AnotherName' ->
    {element(2, Another), element(3, Another)};
unwrap_othername(Other) ->
    Other.
