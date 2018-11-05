%%==============================================================================
%% Copyright 2018 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
%%
%% @author Michal Piotrowski <michal.piotrowski@erlang-solutions.com>
-module(sip_to_jingle).


-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("nksip/include/nksip.hrl").

-export([parse_sdp_attributes/1]).
-export([sdp_media_to_content_el/2]).

parse_sdp_attributes(Attrs) ->
    parse_sdp_attributes(Attrs, []).

parse_sdp_attributes([], Acc) ->
    Acc;
parse_sdp_attributes([Attr | Rest], Acc) ->
    NewAcc = parse_sdp_attribute(Attr, Acc),
    parse_sdp_attributes(Rest, NewAcc).

parse_sdp_attribute({<<"group">>, [Method | Contents]}, Acc) ->
    ContentEls = [sdp_group_content_to_el(Content) || Content <- Contents],
    El = #xmlel{name = <<"group">>,
                attrs = [{<<"xmlns">>, <<"urn:xmpp:jingle:apps:grouping:0">>},
                         {<<"semantics">>, Method}],
                children = ContentEls},
    [El | Acc];
parse_sdp_attribute(_, Acc) ->
    Acc.

sdp_group_content_to_el(Content) ->
    #xmlel{name = <<"content">>,
           attrs = [{<<"name">>, Content}]}.


%% This function assumes that all codecs and their attributes were
%% extracted from the attributes list.
%% There are still attributes in the list describing other content parameters
sdp_media_to_content_el(#sdp_m{media = Media, attributes = Attrs}, CodecMap) ->
    Codecs = maps:get(Media, CodecMap),
    PayloadEls = [codec_to_payload(Codec) || Codec <- Codecs],
    Content  = parse_nksip_media_attrs(Attrs),
    AdditionalDescriptionEls = description_elements_from_content(Content),

    DescriptionEl = #xmlel{name = <<"description">>,
                           attrs = [{<<"xmlns">>, <<"urn:xmpp:jingle:apps:rtp:1">>},
                                    {<<"media">>, Media}],
                           children = PayloadEls ++ AdditionalDescriptionEls},
    TransportEl = make_transport_el(maps:get(transport, Content)),

    #xmlel{name = <<"content">>,
           attrs = [{<<"creator">>, <<"initiator">>},
                    {<<"name">>, maps:get(name, Content, Media)},
                    {<<"senders">>, maps:get(sender, Content)}],
           children = [DescriptionEl, TransportEl]}.

codec_to_payload({ID, RTPMap, Attrs}) ->
    Parameters = lists:flatmap(fun codec_attr_to_params/1, Attrs),
    {Name, ClockRate, Channels} = parse_codec_rtpmap(RTPMap),
    #xmlel{name = <<"payload-type">>,
           attrs = [{<<"id">>, ID},
                    {<<"name">>, Name},
                    {<<"clockrate">>, ClockRate},
                    {<<"channels">>, Channels}],
          children = Parameters}.

parse_codec_rtpmap(RTPMap) ->
    case binary:split(RTPMap, <<$/>>, [global]) of
        [Name, ClockRate] ->
            {Name, ClockRate, <<"1">>};
        [Name, ClockRate, Channels] ->
            {Name, ClockRate, Channels}
    end.

codec_attr_to_params({<<"rtcp-fb">>, Value}) ->
    Attrs = rtcp_fb_value_to_xml_attrs(Value),
    [#xmlel{name = <<"rtcp-fb">>,
            attrs = [{<<"xmlns">>, <<"urn:xmpp:jingle:apps:rtp:rtcp-fb:0">>} | Attrs]}];
codec_attr_to_params({<<"fmtp">>, [Value]}) ->
    Params = binary:split(Value, <<$;>>, [global]),
    [basic_param_to_xml_el(Param) || Param <- Params];
codec_attr_to_params(_) ->
    [].

rtcp_fb_value_to_xml_attrs([Type]) ->
    [{<<"type">>, Type}];
rtcp_fb_value_to_xml_attrs([Type, SubType]) ->
    [{<<"type">>, Type}, {<<"subtype">>, SubType}].

basic_param_to_xml_el(Param) ->
    case binary:split(Param, <<$=>>) of
        [Name, Value] ->
            #xmlel{name = <<"parameter">>,
                   attrs = [{<<"name">>, Name},
                            {<<"value">>, Value}]};
        [Value] ->
            #xmlel{name = <<"parameter">>,
                   attrs = [{<<"value">>, Value}]}
    end.

parse_nksip_media_attrs(Attrs) ->
    parse_nksip_media_attrs(Attrs, #{}).

parse_nksip_media_attrs([], Acc) ->
    Acc;
parse_nksip_media_attrs([{AttrName, AttrValue} | Rest], Acc) ->
    NewAcc = parse_nksip_media_attr(AttrName, AttrValue, Acc),
    parse_nksip_media_attrs(Rest, NewAcc).

parse_nksip_media_attr(<<"sendrecv">>, _, Acc) ->
    Acc#{sender => <<"both">>};
parse_nksip_media_attr(<<"recvonly">>, _, Acc) ->
    Acc#{sender => <<"responder">>};
parse_nksip_media_attr(<<"sendonly">>, _, Acc) ->
    Acc#{sender => <<"initiator">>};
parse_nksip_media_attr(<<"inactive">>, _, Acc) ->
    Acc#{sender => <<"none">>};
parse_nksip_media_attr(<<"candidate">>, Params, Acc) ->
    [Foundation, Component, Protocol, Priority, IP, Port | ExtraArgs] = Params,
    Candidate = #{foundation => Foundation,
                  component => Component,
                  protocol => Protocol,
                  priority => Priority,
                  ip => IP,
                  port => Port,
                  network => <<"0">>, %% no SDP equivalent
                  id => base16:encode(crypto:strong_rand_bytes(5))},
    CompleteCandidate = parse_candidate_extra_args(ExtraArgs, Candidate),
    Transport = maps:get(transport, Acc, #{}),
    Candidates = maps:get(candidates, Transport, []),
    NewTransport = Transport#{candidates => [CompleteCandidate | Candidates]},
    Acc#{transport => NewTransport};
parse_nksip_media_attr(<<"ice-ufrag">>, [Value], Acc) ->
    Transport = maps:get(transport, Acc, #{}),
    Acc#{transport => Transport#{ufrag => Value}};
parse_nksip_media_attr(<<"ice-pwd">>, [Value], Acc) ->
    Transport = maps:get(transport, Acc, #{}),
    Acc#{transport => Transport#{pwd => Value}};
parse_nksip_media_attr(<<"fingerprint">>, [Hash, Fingerprint], Acc) ->
    Transport = maps:get(transport, Acc, #{}),
    Acc#{transport => Transport#{fingerprint => {Hash, Fingerprint}}};
parse_nksip_media_attr(<<"setup">>, [Value], Acc) ->
    Transport = maps:get(transport, Acc, #{}),
    Acc#{transport => Transport#{setup => Value}};
parse_nksip_media_attr(<<"rtcp-mux">>, _, Acc) ->
    Acc#{rtcp_mux => true};
parse_nksip_media_attr(<<"extmap">>, [IDWithSender, URI], Acc) ->
    {ID, Sender} = decode_extmap_id(IDWithSender),
    RTPHdrExts = maps:get(rtphdr_ext, Acc, []),
    Acc#{rtphdr_ext => [{ID, Sender, URI} | RTPHdrExts]};
parse_nksip_media_attr(<<"ssrc">>, [ID | Parameter], Acc) ->
    SourceMap = maps:get(source_map, Acc, #{}),
    SourceParams = maps:get(ID, SourceMap, []),
    Param = decode_ssrc_sdp_param(Parameter),
    NewSourceParams = [Param | SourceParams],
    NewSourceMap = SourceMap#{ID => NewSourceParams},
    Acc#{source_map => NewSourceMap};
parse_nksip_media_attr(<<"mid">>, [Name], Acc) ->
    Acc#{name => Name};
parse_nksip_media_attr(_, _, Acc) ->
    Acc.

decode_extmap_id(IDWithSender) ->
    case binary:split(IDWithSender, <<$/>>) of
        [ID] ->
            {ID, <<"both">>};
        [ID, Sender] ->
            {ID, decode_sender(Sender)}
    end.

decode_sender(<<"sendrecv">>) -> <<"both">>;
decode_sender(<<"recvonly">>) -> <<"responder">>;
decode_sender(<<"sendonly">>) -> <<"initiator">>;
decode_sender(<<"inactive">>) -> <<"none">>.

description_elements_from_content(Content) ->
    RTPHdrExts = [rtphdr_ext_to_element(Ext) || Ext <- maps:get(rtphdr_ext, Content, [])],
    ElsWithRtcpMux = maybe_add_rtcp_mux(Content, RTPHdrExts),
    maybe_add_sources(Content, ElsWithRtcpMux).

rtphdr_ext_to_element({ID, Sender, URI}) ->
    #xmlel{name = <<"rtp-hdrext">>,
           attrs = [{<<"xmlns">>, <<"urn:xmpp:jingle:apps:rtp:rtp-hdrext:0">>},
                    {<<"id">>, ID},
                    {<<"uri">>, URI},
                    {<<"senders">>, Sender}]}.

maybe_add_rtcp_mux(#{rtcp_mux := true}, Els) ->
    El = #xmlel{name = <<"rtcp-mux">>},
    [El | Els];
maybe_add_rtcp_mux(_, Els) ->
    Els.

parse_candidate_extra_args([], Candidate) ->
    Candidate;
parse_candidate_extra_args([V], Candidate) ->
    ?WARNING_MSG("Unrecognised candidate extra arg: ~p", [V]),
    Candidate;
parse_candidate_extra_args([<<"typ">>, Value | Rest], Candidate) ->
    NewCandidate = Candidate#{type => Value},
    parse_candidate_extra_args(Rest, NewCandidate);
parse_candidate_extra_args([<<"raddr">>, Value | Rest], Candidate) ->
    NewCandidate = Candidate#{raddr => Value},
    parse_candidate_extra_args(Rest, NewCandidate);
parse_candidate_extra_args([<<"rport">>, Value | Rest], Candidate) ->
    NewCandidate = Candidate#{rport => Value},
    parse_candidate_extra_args(Rest, NewCandidate);
parse_candidate_extra_args([<<"generation">>, Value | Rest], Candidate) ->
    NewCandidate = Candidate#{generation => Value},
    parse_candidate_extra_args(Rest, NewCandidate);
parse_candidate_extra_args([<<"tcptype">>, Value | Rest], Candidate) ->
    NewCandidate = Candidate#{tcptype => Value},
    parse_candidate_extra_args(Rest, NewCandidate);
parse_candidate_extra_args(Rest, Candidate) ->
    ?WARNING_MSG("Unrecognised candidate extra args: ~p", [Rest]),
    Candidate.


make_transport_el(Transport) ->
    CandidateElements = [make_candidate_el(Candidate) || Candidate <- maps:get(candidates, Transport, [])],
    AttrsWithUfrag = maybe_add_ice_ufrag(maps:get(ufrag, Transport, undefined), []),
    ICEAttrs = maybe_add_ice_pwd(maps:get(pwd, Transport, undefined), AttrsWithUfrag),

    El = #xmlel{name = <<"transport">>,
                attrs = [{<<"xmlns">>, <<"urn:xmpp:jingle:transports:ice-udp:1">>} |
                         ICEAttrs],
                children = CandidateElements},
    maybe_add_fingerprint_el(Transport, El).

make_candidate_el(Candidate) ->
    Attrs = maps:fold(fun candidate_kv_to_attr/3, [], Candidate),
    #xmlel{name = <<"candidate">>,
           attrs = Attrs}.

candidate_kv_to_attr(raddr, Value, Acc) ->
    [{<<"rel-addr">>, Value} | Acc];
candidate_kv_to_attr(rport, Value, Acc) ->
    [{<<"rel-port">>, Value} | Acc];
candidate_kv_to_attr(Key, Value, Acc) ->
    [{erlang:atom_to_binary(Key, utf8), Value} | Acc].

maybe_add_ice_ufrag(undefined, Attrs) ->
    Attrs;
maybe_add_ice_ufrag(Ufrag, Attrs) ->
    [{<<"ufrag">>, Ufrag} | Attrs].

maybe_add_ice_pwd(undefined, Attrs) ->
    Attrs;
maybe_add_ice_pwd(Pwd, Attrs) ->
    [{<<"pwd">>, Pwd} | Attrs].


maybe_add_fingerprint_el(#{fingerprint := {Hash, Fingerprint}} = Transport,
                         #xmlel{children = Children} = El) ->
    Attrs = [{<<"xmlns">>, <<"urn:xmpp:jingle:apps:dtls:0">>},
             {<<"hash">>, Hash}],
    AllAttrs = maybe_add_setup_attr(maps:get(setup, Transport, undefined), Attrs),
    FingerprintEl = #xmlel{name = <<"fingerprint">>,
                           attrs = AllAttrs,
                           children = [#xmlcdata{content = Fingerprint}]},
    El#xmlel{children = [FingerprintEl | Children]}.

maybe_add_setup_attr(undefined, Attrs) ->
    Attrs;
maybe_add_setup_attr(Setup, Attrs) ->
    [{<<"setup">>, Setup} | Attrs].

decode_ssrc_sdp_param(Parameter) ->
    Bin = list_to_binary(lists:join(" ", Parameter)),
    case binary:split(Bin, <<$:>>) of
        [Attr] ->
            Attr;
        [Attr, Value] ->
            {Attr, Value}
    end.

maybe_add_sources(#{source_map := SourceMap}, Els) ->
    SourceEls = [source_to_el(SSRC, Params) || {SSRC, Params} <- maps:to_list(SourceMap)],
    Els ++ SourceEls;
maybe_add_sources(_, Els) ->
    Els.

source_to_el(SSRC, Params) ->
    Parameters = [ssrc_attr_to_el(Attr) || Attr <- Params],
    #xmlel{name = <<"source">>,
           attrs = [{<<"xmlns">>, <<"urn:xmpp:jingle:apps:rtp:ssma:0">>},
                    {<<"ssrc">>, SSRC}],
           children = Parameters}.

ssrc_attr_to_el(Attr) ->
    Attrs = make_el_attrs_from_ssrc(Attr),
    #xmlel{name = <<"parameter">>,
           attrs = Attrs}.

make_el_attrs_from_ssrc({Attr, Value}) ->
    [{<<"name">>, Attr},
     {<<"value">>, Value}];
make_el_attrs_from_ssrc(Attr) ->
    [{<<"name">>, Attr}].


