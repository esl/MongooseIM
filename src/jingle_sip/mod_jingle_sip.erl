%% @doc Enables Jingle to SIP and SIP to Jingle translator.
%% @author Michal Piotrowski <michal.piotrowski@erlang-solutions.com>
%%
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
-module(mod_jingle_sip).

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-include_lib("nksip/include/nksip.hrl").
-include_lib("nksip/include/nksip_call.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

-include_lib("nklib/include/nklib.hrl").

-define(SERVICE, "mim_sip").

%% gen_mod callbacks
-export([start/2, stop/1]).

-export([intercept_jingle_stanza/2]).

-export([content_to_nksip_media/1]).

%% this is because nksip has wrong type specs
-dialyzer({nowarn_function, [translate_to_sip/3,
                             get_proxy_uri/1,
                             prepare_initial_sdp/2,
                             jingle_content_to_media/1,
                             content_to_nksip_media/1]}).

%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------
%%

-spec start(ejabberd:server(), list()) -> ok.
start(Host, Opts) ->
    start_nksip_service_or_error(Opts),
    mod_jingle_sip_backend:init(Host, Opts),
    ejabberd_hooks:add(hooks(Host)),
    ok.

start_nksip_service_or_error(Opts) ->
    application:ensure_all_started(nksip),
    ListenPort = gen_mod:get_opt(listen_port, Opts, 5600),
    NkSipBasicOpts = #{sip_listen => "sip:all:" ++ integer_to_list(ListenPort),
                       callback => jingle_sip_callbacks,
                       plugins => [nksip_outbound, nksip_100rel]},
    NkSipOpts = maybe_add_udp_max_size(NkSipBasicOpts, Opts),
    case nksip:start(?SERVICE, NkSipOpts) of
        {ok, _SrvID} ->
            ok;
        {error, already_started} ->
            ok;
        Other ->
            erlang:error(Other)
    end.

maybe_add_udp_max_size(NkSipOpts, Opts) ->
    case gen_mod:get_opt(udp_max_size, Opts, undefined) of
        undefined ->
            NkSipOpts;
        Size ->
            NkSipOpts#{sip_udp_max_size => Size}
    end.

-spec stop(ejabberd:server()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(hooks(Host)),
    ok.

hooks(Host) ->
    [{c2s_preprocessing_hook, Host, ?MODULE, intercept_jingle_stanza, 75}].

intercept_jingle_stanza(Acc, _C2SState) ->
    case mongoose_acc:get(hook, result, undefined, Acc) of
        drop ->
            Acc;
        _ ->
            maybe_iq_stanza(Acc)
    end.

maybe_iq_stanza(Acc) ->
    case mongoose_acc:stanza_name(Acc) of
        <<"iq">> ->
            maybe_iq_to_other_user(Acc);
        _ ->
            Acc
    end.

maybe_iq_to_other_user(Acc) ->
    #jid{luser = StanzaTo} = mongoose_acc:to_jid(Acc),
    #jid{luser = LUser} = mongoose_acc:get(c2s, origin_jid, Acc),
    case LUser of
        StanzaTo ->
            QueryInfo = jlib:iq_query_info(mongoose_acc:element(Acc)),
            maybe_jingle_get_stanza_to_self(QueryInfo, Acc);
        _ ->
            QueryInfo = jlib:iq_query_info(mongoose_acc:element(Acc)),
            maybe_jingle_stanza(QueryInfo, Acc)
    end.

maybe_jingle_stanza(#iq{xmlns = ?JINGLE_NS, sub_el = Jingle, type = set} = IQ, Acc) ->
    JingleAction = exml_query:attr(Jingle, <<"action">>),
    From = mongoose_acc:from_jid(Acc),
    To = mongoose_acc:to_jid(Acc),
    maybe_translate_to_sip(JingleAction, From, To, IQ, Acc);
maybe_jingle_stanza(_, Acc) ->
    Acc.

maybe_jingle_get_stanza_to_self(#iq{xmlns = ?JINGLE_NS, sub_el = Jingle, type = get} = IQ, Acc) ->
    JingleAction = exml_query:attr(Jingle, <<"action">>),
    case JingleAction of
        <<"existing-session-initiate">> ->
            resend_session_initiate(IQ, Acc),
            mongoose_acc:set(hook, result, drop, Acc);
        _ ->
            Acc
    end;
maybe_jingle_get_stanza_to_self(_, Acc) ->
    Acc.

maybe_translate_to_sip(JingleAction, From, To, IQ, Acc)
  when JingleAction =:= <<"session-initiate">>;
       JingleAction =:= <<"session-accept">>;
       JingleAction =:= <<"session-terminate">>;
       JingleAction =:= <<"source-remove">>;
       JingleAction =:= <<"source-add">>;
       JingleAction =:= <<"source-update">>;
       JingleAction =:= <<"transport-info">> ->
    #iq{sub_el = Jingle} = IQ,
    try
      Result = translate_to_sip(JingleAction, Jingle, Acc),
      route_result(Result, From, To, IQ)
    catch Class:Error:StackTrace ->
            ejabberd_router:route_error_reply(To, From, Acc, mongoose_xmpp_errors:internal_server_error()),
            ?ERROR_MSG("error=~p, while translating to sip, class=~p, stack_trace=~p",
                       [Class, Error, StackTrace])
    end,
    mongoose_acc:set(hook, result, drop, Acc);
maybe_translate_to_sip(JingleAction, _, _, _, Acc) ->
    ?WARNING_MSG("Forwarding unknown action: ~p", [JingleAction]),
    Acc.

route_result(ok, From, To, IQ)  ->
    route_ok_result(From, To, IQ);
route_result({ok, _}, From, To, IQ) ->
    route_ok_result(From, To, IQ);
route_result({ok, _, _}, From, To, IQ) ->
    route_ok_result(From, To, IQ);
route_result({error, item_not_found}, From, To, IQ) ->
    Error = mongoose_xmpp_errors:item_not_found(),
    route_error_reply(From, To, IQ, Error);
route_result(Other, From, To, IQ) ->
    ?WARNING_MSG("Unknown result: ~p for IQ ~p", [Other, IQ]),
    Error = mongoose_xmpp_errors:internal_server_error(),
    route_error_reply(From, To, IQ, Error).

route_error_reply(From, To, IQ, Error) ->
    IQResult = IQ#iq{type = error, sub_el = [Error]},
    Packet = jlib:replace_from_to(From, To, jlib:iq_to_xml(IQResult)),
    ejabberd_router:route(To, From, Packet).

route_ok_result(From, To, IQ) ->
    IQResult = IQ#iq{type = result, sub_el = []},
    Packet = jlib:replace_from_to(From, To, jlib:iq_to_xml(IQResult)),
    ejabberd_router:route(To, From, Packet).

resend_session_initiate(#iq{sub_el = Jingle} = IQ, Acc) ->
    From = mongoose_acc:from_jid(Acc),
    To = mongoose_acc:to_jid(Acc),
    SID = exml_query:attr(Jingle, <<"sid">>),
    case mod_jingle_sip_backend:get_session_info(SID, From) of
        {ok, Session} ->
            maybe_resend_session_initiate(From, To, IQ, Acc, Session);
        _ ->
            ejabberd_router:route_error_reply(To, From, Acc, mongoose_xmpp_errors:item_not_found())
    end.

translate_to_sip(<<"session-initiate">>, Jingle, Acc) ->
    SID = exml_query:attr(Jingle, <<"sid">>),
    #jid{luser = ToUser} = ToJID = jingle_sip_helper:maybe_rewrite_to_phone(Acc),
    #jid{luser = FromUser} = FromJID = mongoose_acc:from_jid(Acc),
    From = jid:to_binary(jid:to_lus(FromJID)),
    To = jid:to_binary(jid:to_lus(ToJID)),
    LServer = mongoose_acc:lserver(Acc),
    SDP = prepare_initial_sdp(LServer, Jingle),
    ProxyURI = get_proxy_uri(LServer),
    RequestURI = list_to_binary(["sip:", ToUser, "@", ProxyURI]),
    ToHeader = <<ToUser/binary, " <sip:",To/binary, ">">>,
    LocalHost = gen_mod:get_module_opt(LServer, ?MODULE, local_host, "localhost"),

    {async, Handle} = nksip_uac:invite(?SERVICE, RequestURI,
                                       [%% Request options
                                        {to, ToHeader},
                                        {from, <<FromUser/binary, " <sip:", From/binary, ">">>},
                                        {call_id, SID},
                                        {body, SDP},
                                        {local_host, LocalHost},
                                        auto_2xx_ack,
                                        %% Internal options
                                        async,
                                        {callback, fun jingle_sip_callbacks:invite_resp_callback/1}]),
    mod_jingle_sip_backend:set_outgoing_request(SID, Handle, FromJID, ToJID),
    {ok, Handle};
translate_to_sip(<<"session-accept">>, Jingle, Acc) ->
    LServer = mongoose_acc:lserver(Acc),
    SID = exml_query:attr(Jingle, <<"sid">>),
    case mod_jingle_sip_backend:get_incoming_request(SID, mongoose_acc:get(c2s, origin_jid, Acc)) of
        {ok, ReqID} ->
            try_to_accept_session(ReqID, Jingle, Acc, LServer, SID);
        _ ->
            {error, item_not_found}
    end;
translate_to_sip(<<"source-remove">> = Name, Jingle, Acc) ->
    translate_source_change_to_sip(Name, Jingle, Acc);
translate_to_sip(<<"source-add">> = Name, Jingle, Acc) ->
    translate_source_change_to_sip(Name, Jingle, Acc);
translate_to_sip(<<"source-update">> = Name, Jingle, Acc) ->
    translate_source_change_to_sip(Name, Jingle, Acc);
translate_to_sip(<<"transport-info">>, Jingle, Acc) ->
    SID = exml_query:attr(Jingle, <<"sid">>),
    SDP = make_sdp_for_ice_candidate(Jingle),
    case mod_jingle_sip_backend:get_outgoing_handle(SID, mongoose_acc:get(c2s, origin_jid, Acc)) of
        {ok, undefined} ->
            ?ERROR_MSG("event=missing_sip_dialog sid=~p", [SID]),
            {error, item_not_found};
        {ok, Handle} ->
            nksip_uac:info(Handle, [{content_type, <<"application/sdp">>},
                                    {body, SDP}]);
        _ ->
            ?ERROR_MSG("event=missing_sip_session sid=~p", [SID]),
            {error, item_not_found}
    end;
translate_to_sip(<<"session-terminate">>, Jingle, Acc) ->
    SID = exml_query:attr(Jingle, <<"sid">>),
    ToJID = jingle_sip_helper:maybe_rewrite_to_phone(Acc),
    From = mongoose_acc:get(c2s, origin_jid, Acc),
    FromLUS = jid:to_lus(From),
    ToLUS = jid:to_lus(ToJID),
    case mod_jingle_sip_backend:get_session_info(SID, From) of
        {ok, Session} ->
            try_to_terminate_the_session(FromLUS, ToLUS, Session);
        _ ->
            {error, item_not_found}
    end.

translate_source_change_to_sip(ActionName, Jingle, Acc) ->
    SID = exml_query:attr(Jingle, <<"sid">>),
    LServer = mongoose_acc:lserver(Acc),
    #sdp{attributes = SDPAttrs} = RawSDP = prepare_initial_sdp(LServer, Jingle),

    SDPAttrsWithActionName = [{<<"jingle-action">>, [ActionName]}
                              | SDPAttrs],
    SDP = RawSDP#sdp{attributes = SDPAttrsWithActionName},

    case mod_jingle_sip_backend:get_outgoing_handle(SID, mongoose_acc:get(c2s, origin_jid, Acc)) of
        {ok, undefined} ->
            ?ERROR_MSG("event=missing_sip_dialog sid=~p", [SID]),
            {error, item_not_found};
        {ok, Handle} ->
            nksip_uac:invite(Handle, [auto_2xx_ack,
                                      {body, SDP}]);
        _ ->
            ?ERROR_MSG("event=missing_sip_session sid=~p", [SID]),
            {error, item_not_found}
    end.

try_to_terminate_the_session(FromLUS, ToLUS, Session) ->
    case maps:get(state, Session) of
        accepted ->
            DialogHandle = maps:get(dialog, Session),
            Node = maps:get(node, Session),
            nksip_uac_bye(Node, DialogHandle, [{to, make_user_header(ToLUS)},
                                         {from, make_user_header(FromLUS)}]);
        _ ->
            RequestHandle = maps:get(request, Session),
            case maps:get(direction, Session) of
                out ->
                    nksip_uac:cancel(RequestHandle, [{to, make_user_header(ToLUS)},
                                                     {from, make_user_header(FromLUS)}]);
                in ->
                    %% When reject incoming invite we need to reply with an error code
                    Node = maps:get(node, Session),
                    nksip_request_reply(busy, {Node, RequestHandle})
            end
    end.

try_to_accept_session(ReqID, Jingle, Acc, Server, SID) ->
    SDP = prepare_initial_sdp(Server, Jingle),
    LocalHost = gen_mod:get_module_opt(Server, ?MODULE, local_host, "localhost"),
    case nksip_request_reply({ok, [{body, SDP}, {local_host, LocalHost}]}, ReqID) of
        ok ->
           ok = mod_jingle_sip_backend:set_incoming_accepted(SID),
           terminate_session_on_other_devices(SID, Acc),
           ok;
        Other ->
           Other
    end.

terminate_session_on_other_devices(SID, Acc) ->
    #jid{lresource = Res} = From = mongoose_acc:from_jid(Acc),
    FromBin = jid:to_binary(From),
    ReasonEl = #xmlel{name = <<"reason">>,
                      children = [#xmlel{name = <<"cancel">>}]},
    JingleEl = jingle_sip_helper:jingle_element(SID, <<"session-terminate">>, [ReasonEl]),
    PResources = ejabberd_sm:get_user_present_resources(From),
    lists:foreach(
      fun({_, R}) when R /= Res ->
              ToJID = jid:replace_resource(From, R),
              IQEl = jingle_sip_helper:jingle_iq(jid:to_binary(ToJID), FromBin, JingleEl),
              ejabberd_router:route(From, ToJID, Acc, IQEl);
         (_) ->
              skip
      end, PResources).

make_user_header({User, _} = US) ->
    JIDBin = jid:to_binary(US),
    <<User/binary, " <sip:", JIDBin/binary, ">">>.


get_proxy_uri(Server) ->
    ProxyHost = gen_mod:get_module_opt(Server, ?MODULE, proxy_host, "localhost"),
    ProxyPort = gen_mod:get_module_opt(Server, ?MODULE, proxy_port, 5060),
    Transport = gen_mod:get_module_opt(Server, ?MODULE, transport, "udp"),
    PortStr = integer_to_list(ProxyPort),
    [ProxyHost, ":", PortStr, ";transport=", Transport].

make_sdp_for_ice_candidate(#xmlel{children = Children}) ->
    Content = parse_content(Children, []),
    list_to_binary(Content).

parse_content([], Acc) ->
    Acc;
parse_content([#xmlel{name = <<"content">>} = Content | Rest], Acc) ->
    SDPCandidate = parse_content(Content),
    NewAcc = [SDPCandidate | Acc],
    parse_content(Rest, NewAcc).

parse_content(Content) ->
    Name = exml_query:attr(Content, <<"name">>),
    MediaLine = [<<"m=">>, Name, <<"9 RTP/AVP 0">>, <<"\r\n">>,
                 <<"a=mid:1\r\n">>],
    Transport = jingle_to_sdp:parse_transport_element(exml_query:subelement(Content, <<"transport">>)),
    IceParamsLine = [<<"a=ice-pwd:">>, maps:get(pwd, Transport), <<"\r\n">>,
                     <<"a=ice-ufrag:">>, maps:get(ufrag, Transport), <<"\r\n">>],
    Candidates = [candidate_to_sdp_line(Candidate) || Candidate <- maps:get(candidates, Transport)],
    [IceParamsLine, MediaLine, Candidates].

candidate_to_sdp_line(Candidate) ->
    OptionalArgs = [generation, tcptype, raddr, rport],
    ExtraArgs = [map_field_to_candidate_arg(Field, Candidate) || Field <- OptionalArgs],
    [<<"a=candidate: ">>,
     maps:get(foundation, Candidate), " ",
     maps:get(component, Candidate), " ",
     maps:get(protocol, Candidate), " ",
     maps:get(priority, Candidate), " ",
     maps:get(ip, Candidate), " ",
     maps:get(port, Candidate), " ",
     "typ ", maps:get(type, Candidate),
     ExtraArgs,
     <<"\r\n">>].

map_field_to_candidate_arg(Field, Map) ->
    case maps:find(Field, Map) of
        {ok, Value} ->
            [" ", atom_to_binary(Field, utf8), " ", Value];
        _ ->
            []
    end.

prepare_initial_sdp(Server, Jingle) ->
    Medias = [jingle_content_to_media(Content) ||
              Content <- exml_query:subelements(Jingle, <<"content">>)],

    GroupingAttrs = add_group_attr_from_jingle(Jingle, []),

    OriginAddress = gen_mod:get_module_opt(Server, ?MODULE, sdp_origin, "127.0.0.1"),
    SDP = nksip_sdp:new(OriginAddress, []),
    SDP#sdp{medias = Medias,
            attributes = GroupingAttrs}.

jingle_content_to_media(ContentEl) ->
    Content = jingle_to_sdp:from_media(ContentEl),
    content_to_nksip_media(Content).

content_to_nksip_media(Content) ->
    {FMTS, DescAttrs} = description_to_nksip_attrs(maps:get(description, Content)),
    TransportAttrs = transport_to_nksip_attrs(maps:get(transport, Content), DescAttrs),
    AttrsWithMediaName = add_media_name_attr(maps:get(name, Content), TransportAttrs),
    AttrsWithSender = add_sender_attr(maps:get(senders, Content), AttrsWithMediaName),

    #sdp_m{media = maps:get(media, Content),
           port = 1024 + rand:uniform(1024),
           proto = maps:get(protocol, Content),
           fmt = FMTS,
           attributes = AttrsWithSender}.

description_to_nksip_attrs(Desc) ->
    AttrsWithSSRC = sources_to_nksip_attrs(maps:get(sources, Desc)),
    {FMTS, CodecAttrs} = codecs_to_nksip_attrs(maps:get(codecs, Desc), AttrsWithSSRC),
    AttrsWithRTPHeaderExt = add_rtp_header_ext(maps:get(rtphdr_ext, Desc), CodecAttrs),
    AttrsWithRTCPMUX = add_rtcp_mux(Desc, AttrsWithRTPHeaderExt),

    {FMTS, AttrsWithRTCPMUX}.

sources_to_nksip_attrs(Sources) ->
    lists:flatmap(fun source_to_nksip_attr/1, Sources).

source_to_nksip_attr({ID, KeyValues}) ->
    [{<<"ssrc">>, [ID, source_to_nksip_value(Name, Value)]} || {Name, Value} <- KeyValues].

source_to_nksip_value(Name, Value) ->
    <<Name/binary, $:, Value/binary>>.

codecs_to_nksip_attrs(Codecs, Attrs) ->
    codecs_to_nksip_attrs2(Codecs, {[], Attrs}).

codecs_to_nksip_attrs2([], Acc) ->
    Acc;
codecs_to_nksip_attrs2([Codec | Rest], {FMTS, Attrs}) ->
    ID = maps:get(id, Codec),
    NewFMTS = [ID | FMTS],
    RTCPFBAttrs = [rtcp_fb_param_to_attr(ID, Param) || Param <- maps:get(rtcp_fb_params, Codec)],
    AttrsWithFMTPParams = maybe_add_fmtp_attr(ID, maps:get(params, Codec), RTCPFBAttrs),
    RTPMapValue = prepare_rtp_map(maps:get(name, Codec), maps:get(clock_rate, Codec), maps:get(channels, Codec)),
    RTPAttr = {<<"rtpmap">>, [ID, RTPMapValue]},
    CodecAttrs = [RTPAttr | AttrsWithFMTPParams],

    codecs_to_nksip_attrs2(Rest, {NewFMTS, lists:append(CodecAttrs, Attrs)}).

rtcp_fb_param_to_attr(ID, RTCPParam) ->
    {<<"rtcp-fb">>, [ID | RTCPParam]}.

maybe_add_fmtp_attr(_ID, [], RTCPParams) ->
    RTCPParams;
maybe_add_fmtp_attr(ID, Params, RTCPParams) ->
    KV = [basic_parameter_to_fmtp_value(Param) || Param <- Params],
    Joined = lists:join($;, KV),
    FMTPValue = list_to_binary(Joined),
    [{<<"fmtp">>, [ID, FMTPValue]} | RTCPParams].

basic_parameter_to_fmtp_value({undefined, Value}) ->
    Value;
basic_parameter_to_fmtp_value({Name, Value}) ->
    <<Name/binary, $=, Value/binary>>.

prepare_rtp_map(Name, ClockRate, <<"1">>) ->
    <<Name/binary, $/, ClockRate/binary>>;
prepare_rtp_map(Name, ClockRate, Channels) ->
    <<Name/binary, $/, ClockRate/binary, $/, Channels/binary>>.

add_rtp_header_ext(RTPHdrExts, Attrs) ->
    RTPHdrextAttrs = [rtp_hdrext_to_sdp_attr(RTPHdrExt) || RTPHdrExt <- RTPHdrExts],
    lists:append(RTPHdrextAttrs, Attrs).

rtp_hdrext_to_sdp_attr({ID, URI, Senders}) ->
    {<<"extmap">>, rtp_hdrext_value(ID, URI, Senders)}.

rtp_hdrext_value(ID, URI, <<"sendrecv">>) ->
    [ID, URI];
rtp_hdrext_value(ID, URI, Sender) ->
    [<<ID/binary, $/, Sender/binary>>, URI].

add_rtcp_mux(#{rtcp_mux := true}, Attrs) ->
    [{<<"rtcp-mux">>, []} | Attrs];
add_rtcp_mux(_, Attrs) ->
    Attrs.

transport_to_nksip_attrs(Transport, Attrs) ->
    AttrsWithUfrag = maybe_add_ice_ufrag_attr(maps:get(ufrag, Transport), Attrs),
    AttrsWithPwd = maybe_add_ice_pwd_attr(maps:get(pwd, Transport), AttrsWithUfrag),
    maybe_add_fingerprint(maps:get(fingerprint, Transport), AttrsWithPwd).

maybe_add_fingerprint({Hash, Setup, Fingerprint}, Attrs) ->
    AttrsWithFingerprint = [{<<"fingerprint">>, [Hash, Fingerprint]} | Attrs],
    case Setup of
        undefined ->
            AttrsWithFingerprint;
        _ ->
            [{<<"setup">>, [Setup]} | AttrsWithFingerprint]
    end;
maybe_add_fingerprint(_, Attrs) ->
    Attrs.

maybe_add_ice_ufrag_attr(undefined, Attrs) ->
    Attrs;
maybe_add_ice_ufrag_attr(Ufrag, Attrs) ->
    [{<<"ice-ufrag">>, [Ufrag]} | Attrs].

maybe_add_ice_pwd_attr(undefined, Attrs) ->
    Attrs;
maybe_add_ice_pwd_attr(Pwd, Attrs) ->
     [{<<"ice-pwd">>, [Pwd]} | Attrs].

add_media_name_attr(Name, Attrs) ->
    [{<<"mid">>, [Name]} | Attrs].

add_sender_attr(Sender, Attrs) ->
    [{Sender, []} | Attrs].

add_group_attr_from_jingle(Jingle, Attrs) ->
    case exml_query:subelement(Jingle, <<"group">>) of
        #xmlel{} = El ->
            Attr = make_group_attr(El),
            [Attr | Attrs];
        _ ->
            Attrs
    end.

make_group_attr(#xmlel{children = Children} = Group) ->
    Semantic = exml_query:attr(Group, <<"semantics">>),
    Contents = [exml_query:attr(Content, <<"name">>) || Content <- Children],

    {<<"group">>, [Semantic | Contents]}.

maybe_resend_session_initiate(From, To, IQ, Acc,
                              #{meta := Meta, from := OriginFrom}) ->
    case maps:get(init_stanza, Meta, undefined) of
        undefined ->
            Error = mongoose_xmpp_errors:item_not_found(<<"en">>,
                                                        <<"no session-initiate for this SID">>),
            ejabberd_router:route_error_reply(To, From, Acc, Error);
        Stanza ->
            IQResult = IQ#iq{type = result, sub_el = []},
            Packet = jlib:replace_from_to(From, To, jlib:iq_to_xml(IQResult)),
            ejabberd_router:route(To, From, Acc, Packet),
            OriginFromBin = jid:to_binary(OriginFrom),
            IQSet = jingle_sip_helper:jingle_iq(jid:to_binary(From), OriginFromBin,
                                                Stanza),
            {U, S} = OriginFrom,
            OriginJID = jid:make_noprep(U, S, <<>>),
            ejabberd_router:route(OriginJID, From, Acc, IQSet)
    end.

nksip_request_reply(Reply, {Node, ReqID}) ->
    case node() of
        Node ->
            nksip_request:reply(Reply, ReqID);
        _ ->
            rpc:call(Node, nksip_request, reply, [Reply, ReqID], timer:seconds(5))
    end.

nksip_uac_bye(Node, DialogHandle, Args) ->
    case node() of
        Node ->
            nksip_uac:bye(DialogHandle, Args);
        _ ->
            rpc:call(Node, nksip_uac, bye, [DialogHandle, Args], timer:seconds(5))
    end.

