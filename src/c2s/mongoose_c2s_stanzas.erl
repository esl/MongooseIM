-module(mongoose_c2s_stanzas).

-include_lib("exml/include/exml_stream.hrl").
-include("jlib.hrl").

-export([
         stream_header/4,
         stream_features_before_auth/4,
         tls_proceed/0,
         stream_features_after_auth/2,
         sasl_success_stanza/1,
         sasl_failure_stanza/1,
         sasl_challenge_stanza/1,
         successful_resource_binding/2,
         successful_session_establishment/1
        ]).

-spec stream_header(binary(), binary(), binary(), binary()) -> exml_stream:start().
stream_header(Server, Version, Lang, StreamId) ->
    Attrs = [{<<"xmlns">>, ?NS_CLIENT},
             {<<"xmlns:stream">>, <<"http://etherx.jabber.org/streams">>},
             {<<"id">>, StreamId},
             {<<"from">>, Server}],
    Attrs1 = case Version of
                 <<>> -> Attrs;
                 _ -> [{<<"version">>, Version} | Attrs]
             end,
    Attrs2 = case Lang of
                 <<>> -> Attrs1;
                 _ -> [{<<"xml:lang">>, Lang} | Attrs1]
             end,
    #xmlstreamstart{name = <<"stream:stream">>,
                    attrs = Attrs2}.

-spec stream_features([exml:element() | exml:cdata()]) -> exml:element().
stream_features(Features) ->
    #xmlel{name = <<"stream:features">>, children = Features}.

-spec stream_features_before_auth(
        mongooseim:host_type(), jid:lserver(), mongoose_listener:options(), mongoose_c2s:c2s_data()) ->
    exml:element().
stream_features_before_auth(HostType, LServer, LOpts, StateData) ->
    IsSSL = mongoose_c2s_socket:is_ssl(mongoose_c2s:get_socket(StateData)),
    Features = determine_features(HostType, LServer, LOpts, IsSSL, StateData),
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
determine_features(_, _, #{tls := #{mode := starttls_required}}, false, _StateData) ->
    [starttls_stanza(required)];
determine_features(HostType, LServer, #{tls := #{mode := tls}}, _, StateData) ->
    mongoose_hooks:c2s_stream_features(HostType, LServer) ++ maybe_sasl_mechanisms(HostType, StateData);
determine_features(HostType, LServer, _, _, StateData) ->
    [starttls_stanza(optional)
     | mongoose_hooks:c2s_stream_features(HostType, LServer) ++ maybe_sasl_mechanisms(HostType, StateData)].

maybe_sasl_mechanisms(HostType, StateData) ->
    case cyrsasl:listmech(HostType) of
        [] -> [];
        Mechanisms ->
            [#xmlel{name = <<"mechanisms">>,
                    attrs = [{<<"xmlns">>, ?NS_SASL}],
                    children = [ mechanism(M)
                                 || M <- Mechanisms, mongoose_c2s:filter_mechanism(StateData, M) ]}]
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
    mongoose_hooks:roster_get_versioning_feature(HostType)
    ++ mongoose_hooks:c2s_stream_features(HostType, LServer).

-spec sasl_success_stanza(binary()) -> exml:element().
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

-spec successful_session_establishment(jlib:iq()) -> exml:element().
successful_session_establishment(IQ) ->
    Res = IQ#iq{type = result,
                sub_el = [#xmlel{name = <<"session">>,
                                 attrs = [{<<"xmlns">>, ?NS_SESSION}]}]},
    jlib:iq_to_xml(Res).
