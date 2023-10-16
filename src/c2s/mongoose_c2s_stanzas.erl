-module(mongoose_c2s_stanzas).

-include("jlib.hrl").
-include("mongoose.hrl").
-include_lib("exml/include/exml_stream.hrl").

-export([
         stream_header/1,
         stream_features_before_auth/1,
         tls_proceed/0,
         tls_failure/0,
         stream_features_after_auth/1,
         sasl_success_stanza/1,
         sasl_failure_stanza/1,
         sasl_challenge_stanza/1,
         successful_resource_binding/2,
         successful_session_establishment/1
        ]).

-spec stream_header(mongoose_c2s:data()) -> exml_stream:start().
stream_header(StateData) ->
    Lang = mongoose_c2s:get_lang(StateData),
    LServer = mongoose_c2s:get_lserver(StateData),
    StreamId = mongoose_c2s:get_stream_id(StateData),
    MaybeFrom = [ {<<"to">>, jid:to_binary(Jid)}
                  || Jid <- [mongoose_c2s:get_jid(StateData)], Jid =/= undefined],
    Attrs = [{<<"xmlns">>, ?NS_CLIENT},
             {<<"xmlns:stream">>, <<"http://etherx.jabber.org/streams">>},
             {<<"id">>, StreamId},
             {<<"from">>, LServer},
             {<<"version">>, ?XMPP_VERSION},
             {<<"xml:lang">>, Lang} | MaybeFrom ],
    #xmlstreamstart{name = <<"stream:stream">>, attrs = Attrs}.

-spec stream_features([exml:element() | exml:cdata()]) -> exml:element().
stream_features(Features) ->
    #xmlel{name = <<"stream:features">>, children = Features}.

-spec stream_features_before_auth(mongoose_c2s:data()) -> exml:element().
stream_features_before_auth(StateData) ->
    HostType = mongoose_c2s:get_host_type(StateData),
    LServer = mongoose_c2s:get_lserver(StateData),
    Socket = mongoose_c2s:get_socket(StateData),
    LOpts = mongoose_c2s:get_listener_opts(StateData),
    IsSSL = mongoose_c2s_socket:is_ssl(Socket),
    Features = determine_features(StateData, HostType, LServer, LOpts, IsSSL),
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
determine_features(_, _, _, #{tls := #{mode := starttls_required}}, false) ->
    [starttls_stanza(required)];
determine_features(StateData, HostType, LServer, #{tls := #{mode := starttls}}, false) ->
    InitialFeatures = [starttls_stanza(optional) | maybe_sasl_mechanisms(StateData)],
    StreamFeaturesParams = #{c2s_data => StateData, lserver => LServer},
    mongoose_hooks:c2s_stream_features(HostType, StreamFeaturesParams, InitialFeatures);
determine_features(StateData, HostType, LServer, _, _) ->
    InitialFeatures = maybe_sasl_mechanisms(StateData),
    StreamFeaturesParams = #{c2s_data => StateData, lserver => LServer},
    mongoose_hooks:c2s_stream_features(HostType, StreamFeaturesParams, InitialFeatures).

-spec maybe_sasl_mechanisms(mongoose_c2s:data()) -> [exml:element()].
maybe_sasl_mechanisms(StateData) ->
    case mongoose_c2s:get_auth_mechs(StateData) of
        [] -> [];
        Mechanisms ->
            [#xmlel{name = <<"mechanisms">>,
                    attrs = [{<<"xmlns">>, ?NS_SASL}],
                    children = [ mechanism(M) || M <- Mechanisms ]}]
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

-spec tls_failure() -> exml:element().
tls_failure() ->
    #xmlel{name = <<"failure">>,
           attrs = [{<<"xmlns">>, ?NS_TLS}]}.

-spec stream_features_after_auth(mongoose_c2s:data()) -> exml:element().
stream_features_after_auth(StateData) ->
    case mongoose_c2s:get_listener_opts(StateData) of
        #{backwards_compatible_session := false} ->
            Features = [#xmlel{name = <<"bind">>,
                               attrs = [{<<"xmlns">>, ?NS_BIND}]}
                        | hook_enabled_features(StateData)],
            stream_features(Features);
        #{backwards_compatible_session := true} ->
            Features = [#xmlel{name = <<"session">>,
                               attrs = [{<<"xmlns">>, ?NS_SESSION}]},
                        #xmlel{name = <<"bind">>,
                               attrs = [{<<"xmlns">>, ?NS_BIND}]}
                        | hook_enabled_features(StateData)],
            stream_features(Features)
    end.

hook_enabled_features(StateData) ->
    HostType = mongoose_c2s:get_host_type(StateData),
    LServer = mongoose_c2s:get_lserver(StateData),
    InitialFeatures = mongoose_hooks:roster_get_versioning_feature(HostType),
    StreamFeaturesParams = #{c2s_data => StateData, lserver => LServer},
    mongoose_hooks:c2s_stream_features(HostType, StreamFeaturesParams, InitialFeatures).

-spec sasl_success_stanza(undefined | binary()) -> exml:element().
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

-spec sasl_challenge_stanza(binary()) -> exml:element().
sasl_challenge_stanza(ServerOut) ->
    #xmlel{name = <<"challenge">>,
           attrs = [{<<"xmlns">>, ?NS_SASL}],
           children = [#xmlcdata{content = jlib:encode_base64(ServerOut)}]}.

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
