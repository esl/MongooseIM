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
-type general_name() :: term().

-spec get_common_name(certificate()) -> bitstring() | error.
get_common_name(Cert) ->
    try
        Cert1 = ensure_cert(Cert),
        #'OTPCertificate'{tbsCertificate = TBSCert} = Cert1,
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
            log_exception(Cert, Class, Exception, StackTrace),
            error
    end.


-spec get_xmpp_addresses(certificate()) -> [bitstring()].
get_xmpp_addresses(Cert) ->
    try
        Cert1 = ensure_cert(Cert),
        SANs = subject_alt_names(Cert1),
        XmppAddrs =
            [maybe_decode_xmpp(unwrap_asn1(Val))
             || {otherName, ON} <- SANs,
                {Oid, Val} <- [unwrap_othername(ON)],
                Oid =:= ?'id-on-xmppAddr'],
        [Addr || Addr <- XmppAddrs, is_binary(Addr)]
    catch
        Class:Exception:StackTrace ->
            log_exception(Cert, Class, Exception,StackTrace),
            []
    end.


-spec get_dns_addresses(certificate()) -> [string()].
get_dns_addresses(Cert) ->
    try
        Cert1 = ensure_cert(Cert),
        SANs = subject_alt_names(Cert1),
        [DNS || {dNSName, DNS} <- SANs]
    catch
        Class:Exception:StackTrace ->
            log_exception(Cert, Class, Exception, StackTrace),
            []
    end.


-spec get_cert_domains(certificate()) -> [bitstring()].
get_cert_domains(Cert) ->
    Cert1 = ensure_cert(Cert),
    CN = get_common_name(Cert1),
    Addresses = get_xmpp_addresses(Cert1),
    Domains = get_dns_addresses(Cert1),
    lists:append([get_lserver_from_addr(CN, false) |
                  [get_lserver_from_addr(Addr, true) || Addr <- Addresses, is_binary(Addr)] ++
                  [get_lserver_from_addr(DNS, false) || DNS <- Domains, is_list(DNS)]]).


convert_to_bin(Val) when is_list(Val) ->
    list_to_binary(Val);
convert_to_bin(Val) ->
    Val.

-spec get_lserver_from_addr(bitstring() | string(), boolean()) -> [binary()].
get_lserver_from_addr(V, UTF8) when is_binary(V); is_list(V) ->
    Val = convert_to_bin(V),
    case {jid:from_binary(Val), UTF8} of
        {#jid{luser = <<"">>, lserver = LD, lresource = <<"">>}, true} ->
            case mongoose_s2s_lib:domain_utf8_to_ascii(LD, binary) of
                false -> [];
                PCLD -> [PCLD]
            end;
        {#jid{luser = <<"">>, lserver = LD, lresource = <<"">>}, _} -> [LD];
        _ -> []
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

-spec subject_alt_names(#'OTPCertificate'{}) -> [general_name()].
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
            ?LOG_INFO(#{what => get_xmpp_addresses_failed, reason => Error}),
            ok
    end;
maybe_decode_xmpp(_) ->
    ok.

normalize_dirstring({utf8String, Bin}) when is_binary(Bin) -> Bin;
normalize_dirstring({printableString, Str}) when is_list(Str) -> list_to_binary(Str);
normalize_dirstring({ia5String, Str}) when is_list(Str) -> list_to_binary(Str);
normalize_dirstring(Bin) when is_binary(Bin) -> Bin;
normalize_dirstring(Str) when is_list(Str) -> list_to_binary(Str).

unwrap_asn1({'INSTANCE OF', _Oid, Bin}) when is_binary(Bin) -> Bin;
unwrap_asn1({asn1_OPENTYPE, Bin}) -> Bin;
unwrap_asn1(Bin) when is_binary(Bin) -> Bin.

unwrap_othername({'INSTANCE OF', Oid, Val}) -> {Oid, Val};
%% TODO: Remove this clause once we drop support for OTP 27,
%% which uses a 3-tuple {AnotherName, Oid, Val} format for OtherName.
unwrap_othername({'AnotherName', Oid, Val}) -> {Oid, Val}.
