-module(mongoose_c2s_stanzas).
-compile([export_all, nowarn_export_all]).

-include("jlib.hrl").
-include("mongoose_ns.hrl").
-include_lib("exml/include/exml.hrl").

stream_header(Server, Version, Lang, StreamId) ->
    VersionStr = case Version of
                    <<>> -> <<>>;
                     _ -> <<" version='", (Version)/binary, "'">>
                 end,
    LangStr = case Lang of
                  <<>> -> <<>>;
                  _ when is_binary(Lang) -> <<" xml:lang='", (Lang)/binary, "'">>
              end,
    <<"<?xml version='1.0'?>",
      "<stream:stream xmlns='jabber:client' ",
      "xmlns:stream='http://etherx.jabber.org/streams' ",
      "id='", (StreamId)/binary, "' ",
      "from='", (Server)/binary, "'",
      (VersionStr)/binary,
      (LangStr)/binary, ">">>.

-spec stream_features([exml:element() | exml:cdata()]) -> exml:element().
stream_features(Features) ->
    #xmlel{name = <<"stream:features">>, children = Features}.

-spec stream_features_before_auth(mongooseim:host_type(), jid:lserver()) -> exml:element().
stream_features_before_auth(HostType, LServer) ->
    Features = determine_features(HostType, LServer),
    stream_features(Features).

%% From RFC 6120, section 5.3.1:
%%
%% If TLS is mandatory-to-negotiate, the receiving entity SHOULD NOT
%% advertise support for any stream feature except STARTTLS during the
%% initial stage of the stream negotiation process, because further stream
%% features might depend on prior negotiation of TLS given the order of
%% layers in XMPP (e.g., the particular SASL mechanisms offered by the
%% receiving entity will likely depend on whether TLS has been negotiated).
%%
%% http://xmpp.org/rfcs/rfc6120.html#tls-rules-mtn
determine_features(HostType, LServer) ->
    [starttls_stanza(optional)
     | mongoose_hooks:c2s_stream_features(HostType, LServer) ++ maybe_sasl_mechanisms(HostType)].

maybe_sasl_mechanisms(HostType) ->
    case cyrsasl:listmech(HostType) of
        [] -> [];
        Mechanisms ->
            [#xmlel{name = <<"mechanisms">>,
                    attrs = [{<<"xmlns">>, ?NS_SASL}],
                    children = [ mechanism(M)
                                 || M <- Mechanisms, mongoose_c2s:filter_mechanism(M) ]}]
    end.

-spec mechanism(binary()) -> exml:element().
mechanism(M) ->
    #xmlel{name = <<"mechanism">>, children = [#xmlcdata{content = M}]}.

-spec starttls_stanza(required | optional) -> exml:element().
starttls_stanza(TLSRequired) when TLSRequired =:= required; TLSRequired =:= optional ->
    #xmlel{name = <<"starttls">>,
           attrs = [{<<"xmlns">>, ?NS_TLS}],
           children = [ #xmlel{name = <<"required">>} || TLSRequired =:= required ]}.

-spec tls_proceed() -> exml:element().
tls_proceed() ->
    #xmlel{name = <<"proceed">>,
           attrs = [{<<"xmlns">>, ?NS_TLS}]}.

-spec stream_features_after_auth(mongooseim:host_type(), jid:lserver()) -> exml:element().
stream_features_after_auth(HostType, LServer) ->
    Features = [#xmlel{name = <<"bind">>,
                       attrs = [{<<"xmlns">>, ?NS_BIND}]}
                | hook_enabled_features(HostType, LServer)],
    stream_features(Features).

hook_enabled_features(HostType, LServer) ->
    mongoose_hooks:c2s_stream_features(HostType, LServer).

-spec sasl_success_stanza(any()) -> exml:element().
sasl_success_stanza(ServerOut) ->
    C = case ServerOut of
            undefined -> [];
            _ -> [#xmlcdata{content = jlib:encode_base64(ServerOut)}]
        end,
    #xmlel{name = <<"success">>,
           attrs = [{<<"xmlns">>, ?NS_SASL}],
           children = C}.

-spec sasl_failure_stanza(binary() | {binary(), iodata() | undefined}) -> exml:element().
sasl_failure_stanza(Error) when is_binary(Error) ->
    sasl_failure_stanza({Error, undefined});
sasl_failure_stanza({Error, Text}) ->
    #xmlel{name = <<"failure">>,
           attrs = [{<<"xmlns">>, ?NS_SASL}],
           children = [#xmlel{name = Error} | maybe_text_tag(Text)]}.

maybe_text_tag(undefined) -> [];
maybe_text_tag(Text) ->
    [#xmlel{name = <<"text">>,
            children = [#xmlcdata{content = Text}]}].

-spec sasl_challenge_stanza([exml:element() | exml:cdata()]) -> exml:element().
sasl_challenge_stanza(Challenge) ->
    #xmlel{name = <<"challenge">>,
           attrs = [{<<"xmlns">>, ?NS_SASL}],
           children = Challenge}.

-spec successful_resource_binding(jlib:iq(), jid:jid()) -> exml:element().
successful_resource_binding(IQ, Jid) ->
    JIDEl = #xmlel{name = <<"jid">>,
                   children = [#xmlcdata{content = jid:to_binary(Jid)}]},
    Res = IQ#iq{type = result,
                sub_el = [#xmlel{name = <<"bind">>,
                                 attrs = [{<<"xmlns">>, ?NS_BIND}],
                                 children = [JIDEl]}]},
    jlib:iq_to_xml(Res).
