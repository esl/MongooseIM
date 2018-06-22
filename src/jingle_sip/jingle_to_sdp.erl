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
-module(jingle_to_sdp).

%% @doc this modules translates jingle stanza to a sdp packet
%%

-export([from_media/1]).
-export([parse_transport_element/1]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("nksip/include/nksip.hrl").

-type rtphdr_ext() :: {ID :: binary(), URI :: binary(), Senders :: binary()}.

-type source() :: {SSRC :: binary(), [{binary(), binary()}]}.

-type content() :: #{media := undefined | binary(),
                     name := undefined | binary(),
                     protocol := binary(),
                     description := description(),
                     transport := transport(),
                     senders := binary()}.

-type description() :: #{codecs := [codec()],
                         rtphdr_ext := [rtphdr_ext()],
                         rtcp_mux := boolean(),
                         sources := [source()]}.

-type transport() :: #{ufrag := undefined | binary(),
                       pwd := undefined | binary(),
                       fingerprint => fingerprint(),
                       candidates => candidate()}.

-type codec() :: #{id := binary(),
                   name := binary(),
                   clock_rate := binary(),
                   channels := binary(),
                   params := [param()], %% basic codec parameters.
                   rtcp_fb_params := rtcp_fb_param()}.

-type param() :: {binary(), binary()}.
-type rtcp_fb_param() :: [binary()].

-type fingerprint() :: {Hash :: undefined | binary(), Setup :: undefined | binary(),
                        Fingerprint :: binary()}.

-type candidate() :: any().

-export_type([content/0]).

-spec from_media(exml:element()) -> content().
from_media(#xmlel{name = <<"content">>} = JingleContent) ->
    Content = #{name => exml_query:attr(JingleContent, <<"name">>),
                protocol => <<"UDP/TLS/RTP/SAVPF">>,
                senders => decode_senders(JingleContent)},
    parse_content_children(Content, JingleContent#xmlel.children).

-spec parse_content_children(map(), list()) -> content().
parse_content_children(Content, []) ->
    Content;
parse_content_children(Content, [Child | Rest]) ->
    NewContent = parse_content_child(Child, Content),
    parse_content_children(NewContent, Rest).


parse_content_child(#xmlel{name = <<"description">>} = DescriptionEl, Content) ->
    Description = #{codecs => [],
                    rtphdr_ext => [],
                    rtcp_mux => false,
                    sources => []},
    Media = exml_query:attr(DescriptionEl, <<"media">>),
    NewDescription = parse_description_children(Description, DescriptionEl#xmlel.children),
    Content#{description => NewDescription, media => Media};
parse_content_child(#xmlel{name = <<"transport">>} = TransportEl, Content) ->
    NewTransport = parse_transport_element(TransportEl),
    Content#{transport => NewTransport};
parse_content_child(_, Content) ->
    Content.

-spec parse_transport_element(exml:element()) -> transport().
parse_transport_element(TransportEl) ->
    Ufrag = exml_query:attr(TransportEl, <<"ufrag">>),
    Pwd = exml_query:attr(TransportEl, <<"pwd">>),
    Transport = #{ufrag => Ufrag,
                  pwd => Pwd,
                  candidates => []},
    parse_transport_children(Transport, TransportEl#xmlel.children).

-spec parse_description_children(map(), list()) -> description().
parse_description_children(Desc, []) ->
    Desc;
parse_description_children(Desc, [Child | Rest]) ->
    NewDesc = parse_description_child(Child, Desc),
    parse_description_children(NewDesc, Rest).

parse_description_child(#xmlel{name = <<"payload-type">>} = Payload, Description) ->
    fill_codec(Description, Payload);
parse_description_child(#xmlel{name = <<"rtp-hdrext">>} = RTPHdrExtEl, Description) ->
    RTPHdrExts = maps:get(rtphdr_ext, Description),
    RTPHdrExt = decode_rtp_hdr_ext(RTPHdrExtEl),
    Description#{rtphdr_ext := [RTPHdrExt | RTPHdrExts]};
parse_description_child(#xmlel{name = <<"rtcp-mux">>}, Description) ->
    Description#{rtcp_mux := true};
parse_description_child(#xmlel{name = <<"source">>} = SourceEl, Description) ->
    fill_source(Description, SourceEl);
parse_description_child(_, Content) ->
    Content.

fill_codec(#{codecs := Codecs} = Desc, Payload) ->
    ID = exml_query:attr(Payload, <<"id">>),
    Name = exml_query:attr(Payload, <<"name">>),
    ClockRate = exml_query:attr(Payload, <<"clockrate">>),
    Channels = exml_query:attr(Payload, <<"channels">>, <<"1">>),
    Codec = #{id => ID,
              name => Name,
              clock_rate => ClockRate,
              channels => Channels,
              params => [],
              rtcp_fb_params => []},
    FinalCodec = parse_payload_children(Codec, Payload#xmlel.children),
    Desc#{codecs := [FinalCodec | Codecs]}.

parse_payload_children(Codec, []) ->
    Codec;
parse_payload_children(Codec, [Child | Rest]) ->
    UpdatedCodec = parse_payload_child(Child, Codec),
    parse_payload_children(UpdatedCodec, Rest).

parse_payload_child(#xmlel{name = <<"parameter">>} = BasicParam,
                    #{params := Params} = Codec) ->
    Name = exml_query:attr(BasicParam, <<"name">>),
    Value = exml_query:attr(BasicParam, <<"value">>),
    NewParams = [{Name, Value} | Params],
    Codec#{params := NewParams};
parse_payload_child(#xmlel{name = <<"rtcp-fb">>} = RTCPParam,
                    #{rtcp_fb_params := Params} = Codec) ->
    Param = decode_rtcp_fb_param(RTCPParam),
    NewParams = [Param | Params],
    Codec#{rtcp_fb_params := NewParams};
parse_payload_child(_, Codec) ->
    Codec.

decode_rtcp_fb_param(RTCPParam) ->
    Type = exml_query:attr(RTCPParam, <<"type">>),
    case exml_query:attr(RTCPParam, <<"subtype">>) of
        undefined ->
            [Type];
        SubType ->
            [Type, SubType]
    end.

decode_rtp_hdr_ext(RTPHdrExtEl) ->
    ID = exml_query:attr(RTPHdrExtEl, <<"id">>),
    URI = exml_query:attr(RTPHdrExtEl, <<"uri">>),
    Senders = exml_query:attr(RTPHdrExtEl, <<"senders">>, <<"both">>),
    {ID, URI, sender_to_sdp_attr(Senders)}.

fill_source(#{sources := Sources} = Desc, SourceEl) ->
    Params = exml_query:subelements(SourceEl, <<"parameter">>),
    KV = [source_param_to_kv(Param) || Param <- Params],
    ID = exml_query:attr(SourceEl, <<"ssrc">>),
    Desc#{sources := [{ID, KV} | Sources]}.

source_param_to_kv(El) ->
    Name = exml_query:attr(El, <<"name">>),
    Value = exml_query:attr(El, <<"value">>),
    {Name, Value}.

decode_senders(ContentEl) ->
    SenderValue = exml_query:attr(ContentEl, <<"senders">>, <<"both">>),
    sender_to_sdp_attr(SenderValue).

sender_to_sdp_attr(<<"both">>) -> <<"sendrecv">>;
sender_to_sdp_attr(<<"initiator">>) -> <<"sendonly">>;
sender_to_sdp_attr(<<"responder">>) -> <<"recvonly">>;
sender_to_sdp_attr(<<"none">>) -> <<"inactive">>.

parse_transport_children(Transport, []) ->
    Transport;
parse_transport_children(Transport, [Child | Rest]) ->
    NewTransport = parse_transport_child(Child, Transport),
    parse_transport_children(NewTransport, Rest).

parse_transport_child(#xmlel{name = <<"fingerprint">>} = FingerprintEl, Transport) ->
    Hash = exml_query:attr(FingerprintEl, <<"hash">>),
    Setup = exml_query:attr(FingerprintEl, <<"setup">>),
    Fingerprint = exml_query:cdata(FingerprintEl),
    Transport#{fingerprint => {Hash, Setup, Fingerprint}};
parse_transport_child(#xmlel{name = <<"candidate">>, attrs = Attrs},
                      #{candidates := Candidates} = Transport) ->
    NewCandidate = lists:foldl(fun parse_candidate_attr/2, #{}, Attrs),
    Transport#{candidates := [NewCandidate | Candidates]};
parse_transport_child(_, Transport) ->
    Transport.

-spec parse_candidate_attr({binary(), any()}, map()) -> map().
parse_candidate_attr({<<"foundation">>, Val}, Map) ->
    Map#{foundation => Val};
parse_candidate_attr({<<"component">>, Val}, Map) ->
    Map#{component => Val};
parse_candidate_attr({<<"generation">>, Val}, Map) ->
    Map#{generation => Val};
parse_candidate_attr({<<"id">>, Val}, Map) ->
    Map#{id => Val};
parse_candidate_attr({<<"ip">>, Val}, Map) ->
    Map#{ip => Val};
parse_candidate_attr({<<"network">>, Val}, Map) ->
    Map#{network => Val};
parse_candidate_attr({<<"port">>, Val}, Map) ->
    Map#{port => Val};
parse_candidate_attr({<<"priority">>, Val}, Map) ->
    Map#{priority => Val};
parse_candidate_attr({<<"protocol">>, Val}, Map) ->
    Map#{protocol => Val};
parse_candidate_attr({<<"rel-addr">>, Val}, Map) ->
    Map#{raddr => Val};
parse_candidate_attr({<<"rel-port">>, Val}, Map) ->
    Map#{rport => Val};
parse_candidate_attr({<<"tcptype">>, Val}, Map) ->
    Map#{tcptype => Val};
parse_candidate_attr({<<"type">>, Val}, Map) ->
    Map#{type => Val};
parse_candidate_attr(_, Map) ->
    Map.
