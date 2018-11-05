-module(jingle_sip_translator).

-export([data_in/2]).
-export([data_out/2]).
-export([message_in/2]).
-export([message_out/2]).
-export([request/2]).
-export([request/3]).
-export([response/2]).
-export([locate/1]).

-export([send_invite/3]).
-export([send_ack_for_200_ok/4]).
-export([send_message_to_test_on_resp/4]).
-export([back_invite_callbacks/7]).
-export([send_invite_back/5]).
-export([make_200_ok_for_phone_call/3]).
-export([make_200_ok_for_conference_call/4]).
-export([in_invite_transaction_callback/4]).
-export([dialog_callback/3]).

-include_lib("esip/include/esip.hrl").
-include_lib("esip/include/esip_lib.hrl").


data_in(_, _) -> ignore.
data_out(_, _) -> ignore.
locate(_SIPMsg) ->
    ok.

message_in(#sip{method = <<"CANCEL">>} = Sip, _Socket) ->
    Sip;
message_in(Sip, _Socket) ->
    Sip.

message_out(Sip, Socket) ->
    Sip.

request(Sip, Socket) ->
    Sip.

request(#sip{method = <<"INVITE">>, hdrs = Hdrs, uri = Uri} = Sip, Socket, Tr) ->
    {ReqBack, Contact} = make_forward_req(Sip),

    {To, ToURI, _} = esip:get_hdr(to, Hdrs),

    case To of
        <<"error.", ErrorCode/binary>> ->
            %% Make error response
            ErrorInt = binary_to_integer(ErrorCode),
            esip:make_response(Sip, #sip{type = response,
                                         status = ErrorInt,
                                         hdrs = [{contact, [Contact]}]}, %% contact headers is needed so that the dialog know where to send packets
                               esip:make_tag()); %% tag is needed to create dialog on the reciving side
        <<"+", Number/binary>> ->
            {_, Resp} = make_provisional_response(Sip, 183, Contact),
            timer:apply_after(200, ?MODULE, make_200_ok_for_phone_call, [Tr, Sip, Resp]),
            Resp;
        <<"*", Number/binary>> ->
            {_, Resp} = make_provisional_response(Sip, 183, Contact),
            timer:apply_after(200, ?MODULE, make_200_ok_for_conference_call, [Socket, Tr, Sip, Resp]),
            Resp;
        _ ->
            CorrespondingCallID = esip:get_hdr('call-id', ReqBack#sip.hdrs),
            {Tag, Resp} = make_provisional_response(Sip, 180, Contact, CorrespondingCallID),

            timer:apply_after(100, ?MODULE, send_invite_back, [Socket, Sip, ReqBack, Tag, Tr]),
            timer:apply_after(10, esip_transaction, reply, [Tr, Resp]),
            {?MODULE, in_invite_transaction_callback, [ReqBack]}
    end;
request(Sip, Socket, Tr) ->
    ct:pal("UNKNOWN req: ~p~nsock: ~p~ntr: ~p", [Sip, Socket, Tr]),
    error.

noop(Arg) ->
   ok.

make_provisional_response(Request, Status, Contact) ->
    make_provisional_response(Request, Status, Contact, undefined).

make_provisional_response(#sip{hdrs = Hdrs} = Sip, Status, Contact, CorrespondingCallID) ->
    %% Make provisional response to initiator
    Tag = esip:make_tag(),
    Resp = esip:make_response(Sip, #sip{type = response,
                                        status = Status,
                                        hdrs = [{contact, [Contact]}]},  %% contact headers is needed so that the dialog know where to send packets
                              Tag), %% tag is needed to create dialog on the reciving side
    RespHdrs = Resp#sip.hdrs,
    To = esip:get_hdr(to, RespHdrs),
    From = esip:get_hdr(from, RespHdrs),
    CallID = esip:get_hdr('call-id', Hdrs),
    {ok, DialogId}  = esip_dialog:open(Sip, Resp, uas, {?MODULE, dialog_callback, []}),
    ct:pal("Provisional Dialog: ~p", [DialogId]),
    Data = #{initiator => From,
             receiver => To,
             receiver_tag => Tag,
             direction => in,
             dialog_id => DialogId},
    ets:insert(?MODULE, {CallID, Data}),
    {Tag, Resp}.

send_invite_back(Socket, Sip, ReqBack, Tag, Tr) ->
    {ok, TrID} = esip:request(Socket, ReqBack,
                              {?MODULE, back_invite_callbacks, [Sip, Tr, ReqBack, Tag]}),
    InitialID = esip:get_hdr('call-id', Sip#sip.hdrs),
    BackID = esip:get_hdr('call-id', ReqBack#sip.hdrs),
    ets:insert(jingle_sip_translator_bindings, {InitialID, BackID}),
    ets:insert(jingle_sip_translator_bindings, {BackID, InitialID}),
    ok.

make_200_ok_for_phone_call(Tr, Sip, #sip{hdrs = RespHdrs} = Resp183) ->
    {_, _, Params} = esip:get_hdr(to, RespHdrs),
    Tag = esip:get_param(<<"tag">>, Params),
    Resp = esip:make_response(Sip, #sip{type = response,
                                        status = 200,
                                        body = prepared_sdp_for_phone_call()},
                              Tag),
    esip:reply(Tr, Resp).

make_200_ok_for_conference_call(Socket, Tr, #sip{hdrs = Hdrs} = Sip, #sip{hdrs = RespHdrs} = Resp183) ->
    make_200_ok_for_phone_call(Tr, Sip, Resp183),
    {_, #uri{user = ReceiverUser}, Params} = Receiver = esip:get_hdr(to, RespHdrs),
    {_, _, _} = Initiator = esip:get_hdr(from, RespHdrs),
    Branch = base16:encode(crypto:strong_rand_bytes(3)),
    URIBin = <<"sip:",ReceiverUser/binary,"@127.0.0.1:12345;ob;transport=tcp">>,
    ContactURI = esip_codec:decode_uri(URIBin),
    Contact = {<<>>, ContactURI, []},

    Via = #via{transport = <<"UDP">>, host = <<"127.0.0.1">>, port = 12345,
               params = [{<<"rport">>, <<>>},
                         {<<"branch">>, <<"z9hG4bK-", Branch/binary>>}]},

    Hdrs1 = esip:filter_hdrs(['call-id', 'cseq',
                              'route', 'max-forwards',
                              'authorization',
                              'proxy-authorization'], Hdrs),

    Hdrs2 = [{via, [Via]}, {contact, [Contact]}, {from, Receiver}, {to, Initiator} | Hdrs1],
    ReqBack = Sip#sip{uri = esip_codec:decode_uri(<<"sip:127.0.0.1:5600">>),
                      hdrs = Hdrs2,
                      body = prepared_sdp_for_phone_call()},
    timer:sleep(200),
    esip:request(Socket, ReqBack, {?MODULE, send_ack_for_200_ok, [ReqBack]}).


make_forward_req(#sip{hdrs = Hdrs} = Sip) ->
    %% Forward the request back to MongooseIM
    Branch = base16:encode(crypto:strong_rand_bytes(3)),
    {_, #uri{user = FromUser}, _} = esip:get_hdr(to, Hdrs),
    URIBin = <<"sip:",FromUser/binary,"@127.0.0.1:12345;ob;transport=tcp">>,
    ContactURI = esip_codec:decode_uri(URIBin),
    Contact = {<<>>, ContactURI, []},

    Via = #via{transport = <<"UDP">>, host = <<"127.0.0.1">>, port = 12345,
               params = [{<<"rport">>, <<>>},
                         {<<"branch">>, <<"z9hG4bK-", Branch/binary>>}]},

    Hdrs1 = esip:filter_hdrs(['cseq', 'route', 'max-forwards',
                              'authorization', 'to', 'from',
                              'proxy-authorization'], Hdrs),
    CallID = esip:make_callid(),
    Hdrs2 = [{via, [Via]}, {'call-id', CallID}, {contact, [Contact]} | Hdrs1],
    ReqBack = Sip#sip{uri = esip_codec:decode_uri(<<"sip:127.0.0.1:5600">>),
                      hdrs = Hdrs2,
                      body = prepared_sdp()},
    {ReqBack, Contact}.

back_invite_callbacks(#sip{type = response, method = <<"INVITE">>,
                         hdrs = Hdrs, status = 200} = Resp,
                     Socket, TrID, InitialReq, InitialTr, ForwardedRequest, Tag) ->
    send_ack_for_200_ok(Resp, Socket, TrID, ForwardedRequest),
    %% Forward reply to the initial req
    CallID = esip:get_hdr('call-id', Hdrs),

    Reply = esip:make_response(InitialReq, #sip{type = response,
                                                status = 200,
                                                body = prepared_sdp_inactive_video()},
                              Tag),
    esip:reply(InitialTr, Reply),

    ok;
back_invite_callbacks(#sip{type = response, status = 180, hdrs = Hdrs} = Sip, _, TrID,
                    InitialReq, InitialTr, ForwardedRequest, Tag) ->
    {_, #uri{user = ToUser}, _} = To = esip:get_hdr(to, Hdrs),
    {_, #uri{user = FromUser}, _} = From = esip:get_hdr(from, Hdrs),
    CallID = esip:get_hdr('call-id', Hdrs),
    {ok, DialogId}  = esip_dialog:open(ForwardedRequest, Sip, uac, {?MODULE, dialog_callback, []}),
    ct:pal("Back Dialog ~p for call ~p", [DialogId, CallID]),
    Data = #{initiator => From,
             receiver => To,
             receiver_tag => Tag,
             direction => out,
             dialog_id => DialogId},
    ets:insert(?MODULE, {CallID, Data}),
    ok;
back_invite_callbacks(#sip{type = response, status = 486}, _, _TrId, InitialReq, InitialTr, _, Tag) ->
    Reply = esip:make_response(InitialReq, #sip{type = response,
                                                status = 486
                                                },
                              Tag),
    esip:reply(InitialTr, Reply),
    ok;
back_invite_callbacks(_Sip, _, _TrId, _InitialReq, _InitialTr, _, Tag) ->
    ok.

in_invite_transaction_callback(#sip{type = request, method = <<"CANCEL">>, hdrs = Hdrs} = Sip,
                               Socket, Tr, #sip{hdrs = ReqBackHdrs}) ->

    %% We want to preserve From and To headers so that we know who CANCELed the INVITE
    {_, #uri{user = FromUserReq}, _} = esip:get_hdr(from, Hdrs),
    {_, #uri{user = ToUser}, _} = esip:get_hdr(to, Hdrs),

    Contact = esip:get_hdr(contact, ReqBackHdrs),
    CorrespondingCall = esip:get_hdr('call-id', ReqBackHdrs),
    Via = esip:get_hdr(via, ReqBackHdrs),
    %% It's important to use the save via as in the INVITE sent to Mongoose
    %% In the via header there is `branch` parameter defining the transaction
    %% we are going to CANCEL
    Hdrs1 = esip:filter_hdrs(['cseq', 'from', 'to',
                              'route', 'max-forwards',
                              'authorization',
                              'proxy-authorization'], Hdrs),

    Hdrs2 = [{via, Via}, {'call-id', CorrespondingCall}, {contact, Contact} | Hdrs1],
    ReqBack = Sip#sip{uri = esip_codec:decode_uri(<<"sip:127.0.0.1:5600">>),
                      hdrs = Hdrs2},
    {ok, TrID} = esip:request(Socket, ReqBack),
   esip:make_response(Sip, #sip{type = response,
                                status = 487});
in_invite_transaction_callback(Req, Socket, Tr, CorrespondingCall) ->
    ct:pal("in invite transaction callback:~n~p~n=========~n~p~n=========~n~p~n=========~n~p",
           [Req, Socket, Tr, CorrespondingCall]).

dialog_callback(#sip{type = request, method = <<"ACK">>}, _, _) ->
   ok;
%% Below function forwards any in-dialog request to correspoding jingle session
dialog_callback(#sip{type = request, hdrs = Hdrs} = Req, Socket, Tr) ->
    CallID = esip:get_hdr('call-id', Hdrs),
    [{_, OtherCallID}] = ets:lookup(jingle_sip_translator_bindings, CallID),

    ct:pal("Other call id: ~p", [OtherCallID]),

    [{_, Data}] = ets:lookup(?MODULE, OtherCallID),
    DialogID = maps:get(dialog_id, Data),
    ct:pal("Other dialog: ~p", [DialogID]),

    {_, #uri{user = FromUserReq}, _} = esip:get_hdr(from, Hdrs),
    {From, To} = get_from_and_to_for_request_and_call_id(OtherCallID, FromUserReq),
    {_, #uri{user = ToUser}, _} = To,
    {_, #uri{user = FromUser}, _} = From,

    Branch = base16:encode(crypto:strong_rand_bytes(3)),
    URIBin = <<"sip:",ToUser/binary,"@127.0.0.1:12345;ob;transport=tcp">>,
    ContactURI = esip_codec:decode_uri(URIBin),
    Contact = {<<>>, ContactURI, []},

    Via = #via{transport = <<"UDP">>, host = <<"127.0.0.1">>, port = 12345,
               params = [{<<"rport">>, <<>>},
                         {<<"branch">>, <<"z9hG4bK-", Branch/binary>>}]},

    Hdrs1 = esip:filter_hdrs(['cseq',
                              'route', 'max-forwards',
                              'authorization',
                              'proxy-authorization'], Hdrs),

    Hdrs2 = [{via, [Via]}, {contact, [Contact]}, {'call-id', OtherCallID}, {from, From},
                            {to, To} | Hdrs1],
    ReqBack = Req#sip{uri = esip_codec:decode_uri(<<"sip:127.0.0.1:5600">>),
                      hdrs = Hdrs2},
    esip:request(Socket, ReqBack),
    esip:make_response(Req, #sip{type = response,
                                        status = 200
                                        });
dialog_callback(SIP, Socket, Tr) ->
   ct:pal("unknown in-dialog sip msg: ~p", [SIP]).

send_invite(From, To, Pid) ->
    FromUser = escalus_client:username(From),
    FromJID = escalus_client:short_jid(From),

    FromURI = esip_codec:decode_uri(<<"sip:", FromJID/binary>>),
    FromSIP = {FromUser, FromURI,
               [{<<"tag">>, base16:encode(crypto:strong_rand_bytes(2))}]},

    ToUser = escalus_client:username(To),
    ToJID = escalus_client:short_jid(To),
    ToURI = esip_codec:decode_uri(<<"sip:", ToJID/binary>>),
    ToSIP = {ToUser, ToURI, []},
    CallID = base16:encode(crypto:strong_rand_bytes(6)),
    CSeq = 141501489,
    Branch = base16:encode(crypto:strong_rand_bytes(3)),
    ContactURI = esip_codec:decode_uri(<<"sip:",FromUser/binary,"@127.0.0.1:12345;ob">>),
    Contact = {<<>>, ContactURI, []},

    Via = #via{transport = <<"UDP">>, host = <<"127.0.0.1">>, port = 12345,
               params = [{<<"rport">>, <<>>},
                         {<<"branch">>, <<"z9hG4bK-", Branch/binary>>}]},

    Req = #sip{type = request,
               method = <<"INVITE">>,
               uri = esip_codec:decode_uri(<<"sip:127.0.0.1:5600">>),
               hdrs = [{via, [Via]},
                       {from, FromSIP},
                       {to, ToSIP},
                       {contact, [Contact]},
                       {'call-id', CallID},
                       {cseq, CSeq},
                       {'max-forwards', 70}]},

    {ok, Socket} = esip:connect(Req),
    {ok, TrID} = esip:request(Socket, Req, {?MODULE, send_message_to_test_on_resp, [Pid]}),

    ok.

send_message_to_test_on_resp(#sip{type = response, status = 100}, _Socket, _Tr, Pid) ->
   ok; %% test is not interested in this resp
send_message_to_test_on_resp(#sip{type = response, status = Status}, _Socket, _Tr, Pid) ->
   Pid ! {sip_resp, Status}.

send_ack_for_200_ok(#sip{type = response, method = <<"INVITE">>,
                         hdrs = Hdrs, status = 200} = Resp,
                     Socket, TrID, #sip{uri = ReqURI, hdrs = ReqHdrs} = InitialReq) ->
    Contact = esip:get_hdrs(contact, ReqHdrs),
    Hdrs1 = esip:filter_hdrs(['call-id', 'cseq',
                              'route', 'max-forwards',
                              'authorization', 'to', 'from',
                              'proxy-authorization'], Hdrs),
    Branch = base16:encode(crypto:strong_rand_bytes(3)),
    Via = #via{transport = <<"UDP">>, host = <<"127.0.0.1">>, port = 12345,
               params = [{<<"rport">>, <<>>},
                         {<<"branch">>, <<"z9hG4bK-", Branch/binary>>}]},
    [{_, #uri{user = ToUser}, _} | _] = esip:get_hdrs(to, Hdrs),
    ACK = #sip{type = request,
               uri = ReqURI#uri{user = ToUser},
               method = <<"ACK">>,
               hdrs = [{via, [Via]}, {contact, Contact} | Hdrs1]},
    esip_transport:send(Socket, ACK),
    ok;
send_ack_for_200_ok(_, _, _, _) ->
    ok.

response(Sip, Socket) ->
    Sip.


get_from_and_to_for_request_and_call_id(CallID, FromUserReq) ->
    [{_, Data}] = ets:lookup(?MODULE, CallID),
    {_, #uri{user = InitUser}, _} = Initiator = maps:get(initiator, Data),
    Receiver = maps:get(receiver, Data),
    case InitUser of
        FromUserReq ->
            {Initiator, Receiver};
        _ ->
            {Receiver, Initiator}
    end.

prepared_sdp() ->
    <<"v=0\r\n"
      "o=- 1519637887 1519637887 IN IP4 127.0.0.1\r\n"
      "s=nksip\r\n"
      "c=IN IP4 127.0.0.1\r\n"
      "t=0 0\r\n"
      "a=group:BUNDLE audio video\r\n"
      "m=audio 1879 UDP/TLS/RTP/SAVPF 111 103 104 9 0 8 106 105 13 110 112 113 126\r\n"
      "a=rtpmap:111 opus/48000/2\r\n"
      "a=rtpmap:103 ISAC/16000\r\n"
      "a=rtpmap:104 ISAC/32000\r\n"
      "a=rtpmap:9 G722/8000\r\n"
      "a=rtpmap:0 PCMU/8000\r\n"
      "a=rtpmap:8 PCMA/8000\r\n"
      "a=rtpmap:106 CN/32000\r\n"
      "a=rtpmap:105 CN/16000\r\n"
      "a=rtpmap:13 CN/8000\r\n"
      "a=rtpmap:110 telephone-event/48000\r\n"
      "a=rtpmap:112 telephone-event/32000\r\n"
      "a=rtpmap:113 telephone-event/16000\r\n"
      "a=rtpmap:126 telephone-event/8000\r\n"
      "a=fmtp:111 useinbandfec=1;minptime=10\r\n"
      "a=mid:audio\r\n"
      "a=ice-ufrag:7NlF\r\n"
      "a=ice-pwd:wFJ/qkEHGCQGenrkRiFm8qSe\r\n"
      "a=fingerprint:sha-256 4B:06:E3:6D:DB:73:A3:16:DD:49:ED:18:AA:65:2D:08:6B:C5:7D:5F:AF:AA:83:92:0F:50:07:C7:21:B4:F4:F3\r\n"
      "a=setup:actpass\r\n"
      "a=extmap:1 urn:ietf:params:rtp-hdrext:ssrc-audio-level\r\n"
      "a=rtcp-fb:111 transport-cc\r\n"
      "a=ssrc:1982546274 cname:qQQu7Fxk5w/3gjgT\r\n"
      "a=ssrc:1982546274 msid:VEcgUMo7jjgo7YknmmibIuoLdy2cammm9YIv 26b62176-071b-47b2-9afd-f1e9672feb9d\r\n"
      "a=rtcp-mux\r\n"
      "a=sendrecv\r\n"
      "a=candidate:6jP5NffoId0CdpiZ 1 UDP 16777215 52.91.166.238 50714 typ relay raddr 52.91.166.238 rport 50714\r\n"
      "a=candidate:6jP5NffoId0CdpiZ 2 UDP 16777214 52.91.166.238 50715 typ relay raddr 52.91.166.238 rport 50715\r\n"
      "m=video 1600 UDP/TLS/RTP/SAVPF 96 97 98 99 100 101 102 124 127 123 125 107 108\r\n"
      "a=rtpmap:96 VP8/90000\r\n"
      "a=rtpmap:97 rtx/90000\r\n"
      "a=rtpmap:98 VP9/90000\r\n"
      "a=rtpmap:99 rtx/90000\r\n"
      "a=rtpmap:100 H264/90000\r\n"
      "a=rtpmap:101 rtx/90000\r\n"
      "a=rtpmap:102 H264/90000\r\n"
      "a=rtpmap:124 rtx/90000\r\n"
      "a=rtpmap:127 H264/90000\r\n"
      "a=rtpmap:123 rtx/90000\r\n"
      "a=rtpmap:125 red/90000\r\n"
      "a=rtpmap:107 rtx/90000\r\n"
      "a=rtpmap:108 ulpfec/90000\r\n"
      "a=fmtp:97 apt=96\r\n"
      "a=fmtp:99 apt=98\r\n"
      "a=fmtp:100 profile-level-id=64001f;packetization-mode=1;level-asymmetry-allowed=1\r\n"
      "a=fmtp:101 apt=100\r\n"
      "a=fmtp:102 profile-level-id=42e01f;packetization-mode=1;level-asymmetry-allowed=1\r\n"
      "a=fmtp:124 apt=102\r\n"
      "a=fmtp:127 profile-level-id=42001f;packetization-mode=1;level-asymmetry-allowed=1\r\n"
      "a=fmtp:123 apt=127\r\n"
      "a=fmtp:107 apt=125\r\n"
      "a=mid:video\r\n"
      "a=ice-ufrag:7NlF\r\n"
      "a=ice-pwd:wFJ/qkEHGCQGenrkRiFm8qSe\r\n"
      "a=fingerprint:sha-256 4B:06:E3:6D:DB:73:A3:16:DD:49:ED:18:AA:65:2D:08:6B:C5:7D:5F:AF:AA:83:92:0F:50:07:C7:21:B4:F4:F3\r\n"
      "a=setup:actpass\r\n"
      "a=extmap:8 http://www.webrtc.org/experiments/rtp-hdrext/video-timing\r\n"
      "a=extmap:7 http://www.webrtc.org/experiments/rtp-hdrext/video-content-type\r\n"
      "a=extmap:6 http://www.webrtc.org/experiments/rtp-hdrext/playout-delay\r\n"
      "a=extmap:5 http://www.ietf.org/id/draft-holmer-rmcat-transport-wide-cc-extensions-01\r\n"
      "a=extmap:4 urn:3gpp:video-orientation\r\n"
      "a=extmap:3 http://www.webrtc.org/experiments/rtp-hdrext/abs-send-time\r\n"
      "a=extmap:2 urn:ietf:params:rtp-hdrext:toffset\r\n"
      "a=rtcp-fb:96 transport-cc\r\n"
      "a=rtcp-fb:96 goog-remb\r\n"
      "a=rtcp-fb:96 nack pli\r\n"
      "a=rtcp-fb:96 nack\r\n"
      "a=rtcp-fb:96 ccm fir\r\n"
      "a=rtcp-fb:98 transport-cc\r\n"
      "a=rtcp-fb:98 goog-remb\r\n"
      "a=rtcp-fb:98 nack pli\r\n"
      "a=rtcp-fb:98 nack\r\n"
      "a=rtcp-fb:98 ccm fir\r\n"
      "a=rtcp-fb:100 transport-cc\r\n"
      "a=rtcp-fb:100 goog-remb\r\n"
      "a=rtcp-fb:100 nack pli\r\n"
      "a=rtcp-fb:100 nack\r\n"
      "a=rtcp-fb:100 ccm fir\r\n"
      "a=rtcp-fb:102 transport-cc\r\n"
      "a=rtcp-fb:102 goog-remb\r\n"
      "a=rtcp-fb:102 nack pli\r\n"
      "a=rtcp-fb:102 nack\r\n"
      "a=rtcp-fb:102 ccm fir\r\n"
      "a=rtcp-fb:127 transport-cc\r\n"
      "a=rtcp-fb:127 goog-remb\r\n"
      "a=rtcp-fb:127 nack pli\r\n"
      "a=rtcp-fb:127 nack\r\n"
      "a=rtcp-fb:127 ccm fir\r\n"
      "a=rtcp-mux\r\n"
      "a=recvonly\r\n"
      "a=candidate:6jP5NffoId0CdpiZ 1 UDP 16777215 52.91.166.238 50750 typ relay raddr 52.91.166.238 rport 50750\r\n"
      "a=candidate:6jP5NffoId0CdpiZ 2 UDP 16777214 52.91.166.238 50751 typ relay raddr 52.91.166.238 rport 50751\r\n">>.

prepared_sdp_inactive_video() ->
    <<"v=0\r\n"
    "o=- 1519734860 1519734860 IN IP4 127.0.0.1\r\n"
    "s=nksip\r\n"
    "c=IN IP4 127.0.0.1\r\n"
    "t=0 0\r\n"
    "a=group:BUNDLE audio\r\n"
    "m=video 2003 UDP/TLS/RTP/SAVPF 120\r\n"
    "a=rtpmap:120 VP8/90000\r\n"
    "a=inactive\r\n"
    "a=mid:video\r\n"
    "a=fingerprint:sha-256 06:17:9A:74:3E:CD:97:98:31:E6:5C:D6:32:5F:5A:E4:E1:27:5D:14:FA:2C:2F:8A:BD:28:69:FD:84:3D:5F:1B\r\n"
    "a=candidate:dYJpVWb1MKfBt6dS 1 UDP 16777215 34.203.249.109 52488 typ relay raddr 34.203.249.109 rport 52488\r\n"
    "a=candidate:dYJpVWb1MKfBt6dS 2 UDP 16777214 34.203.249.109 52489 typ relay raddr 34.203.249.109 rport 52489\r\n"
    "m=audio 1257 UDP/TLS/RTP/SAVPF 111 126\r\n"
    "a=rtpmap:111 opus/48000/2\r\n"
    "a=rtpmap:126 telephone-event/8000\r\n"
    "a=fmtp:111 useinbandfec=1;stereo=1;maxplaybackrate=48000\r\n"
    "a=fmtp:126 0-15\r\n"
    "a=sendrecv\r\n"
    "a=mid:audio\r\n"
    "a=ice-ufrag:e277126c\r\n"
    "a=ice-pwd:7dd379b4f6d8b69ad32de8afbd70a5f3\r\n"
    "a=fingerprint:sha-256 06:17:9A:74:3E:CD:97:98:31:E6:5C:D6:32:5F:5A:E4:E1:27:5D:14:FA:2C:2F:8A:BD:28:69:FD:84:3D:5F:1B\r\n"
    "a=setup:active\r\n"
    "a=extmap:1/recvonly urn:ietf:params:rtp-hdrext:ssrc-audio-level\r\n"
    "a=ssrc:883505085 cname:{adae26f9-82e5-3343-83f5-44f6213e0781}\r\n"
    "a=ssrc:883505085 msid:{312f703a-61fa-8f4e-a2cf-2c652c280748} {358c475c-71f0-ac43-88b2-407dba1aab70}\r\n"
    "a=rtcp-mux\r\n"
    "a=rtcp-mux\r\n"
    "a=candidate:dYJpVWb1MKfBt6dS 1 UDP 16777215 34.203.249.109 52518 typ relay raddr 34.203.249.109 rport 52518\r\n">>.

prepared_sdp_for_phone_call() ->
    <<"v=0\r\n"
      "o=SBCSIPUAS 785727450 1 IN IP4 169.132.139.30\r\n"
      "s=SBCSIPUAS SIP STACK v1.0\r\n"
      "c=IN IP4 34.194.210.141\r\n"
      "t=0 0\r\n"
      "m=audio 55146 UDP/TLS/RTP/SAVPF 0 101\r\n"
      "a=rtpmap:0 PCMU/8000\r\n"
      "a=rtpmap:101 telephone-event/8000\r\n"
      "a=fmtp:101 0-15\r\n"
      "a=sendrecv\r\n"
      "a=ice-ufrag:qUZGm9va\r\n"
      "a=ice-pwd:8S4w0jP9kLKksupcqy8CJInEEm\r\n"
      "a=fingerprint:sha-256 06:17:9A:74:3E:CD:97:98:31:E6:5C:D6:32:5F:5A:E4:E1:27:5D:14:FA:2C:2F:8A:BD:28:69:FD:84:3D:5F:1B\r\n"
      "a=setup:passive\r\n"
      "a=candidate:AdCiOysi7BhJc6vq 1 UDP 2130706431 34.194.210.141 55146 typ host\r\n"
      "a=candidate:AdCiOysi7BhJc6vq 2 UDP 2130706430 34.194.210.141 55147 typ host\r\n">>.
