-module(jingle_helper).

-include_lib("exml/include/exml.hrl").

-export([content/1]).
-export([content_group/1]).

content(audio) ->
    escalus_stanza:from_xml(<<"
      <content creator='initiator' name='audio_1' senders='both'>
         <description xmlns='urn:xmpp:jingle:apps:rtp:1' media='audio' ssrc='948015790'>
            <payload-type id='111' name='opus' clockrate='48000' channels='2'>
               <parameter name='minptime' value='10' />
               <parameter name='useinbandfec' value='1' />
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='transport-cc' />
            </payload-type>
            <payload-type id='103' name='ISAC' clockrate='16000' channels='1' />
            <payload-type id='104' name='ISAC' clockrate='32000' channels='1' />
            <payload-type id='9' name='G722' clockrate='8000' channels='1' />
            <payload-type id='102' name='ILBC' clockrate='8000' channels='1' />
            <payload-type id='0' name='PCMU' clockrate='8000' channels='1' />
            <payload-type id='8' name='PCMA' clockrate='8000' channels='1' />
            <payload-type id='106' name='CN' clockrate='32000' channels='1' />
            <payload-type id='105' name='CN' clockrate='16000' channels='1' />
            <payload-type id='13' name='CN' clockrate='8000' channels='1' />
            <payload-type id='110' name='telephone-event' clockrate='48000' channels='1' />
            <payload-type id='112' name='telephone-event' clockrate='32000' channels='1' />
            <payload-type id='113' name='telephone-event' clockrate='16000' channels='1' />
            <payload-type id='126' name='telephone-event' clockrate='8000' channels='1' />
            <rtp-hdrext xmlns='urn:xmpp:jingle:apps:rtp:rtp-hdrext:0' id='1' uri='urn:ietf:params:rtp-hdrext:ssrc-audio-level' senders='both' />
            <rtcp-mux />
            <source xmlns='urn:xmpp:jingle:apps:rtp:ssma:0' ssrc='948015790'>
               <parameter name='cname' value='3dYy6Ys3wP//8AoS' />
               <parameter name='msid' value='c5f700e1-1897-41c2-9421-697103982067 a4423a33-ffb9-40e2-a300-8317d9d00a46' />
            </source>
         </description>
         <transport xmlns='urn:xmpp:jingle:transports:ice-udp:1' ufrag='TIxp' pwd='MkQXObfhEelTbQdRV1e0ADGh'>
            <fingerprint xmlns='urn:xmpp:jingle:apps:dtls:0' hash='sha-256' setup='actpass'>08:D7:8E:6D:A6:40:77:4C:CC:F8:46:68:80:F2:2A:B1:7B:A0:AF:02:02:CA:2A:2A:F4:35:1A:95:11:75:B2:F7</fingerprint>
         </transport>
      </content>">>);
content(audio_source_remove) ->
    escalus_stanza:from_xml(<<"
    <content creator='initiator' name='sdparta_0' senders='both'>
        <description media='audio' ssrc='1922778502' xmlns='urn:xmpp:jingle:apps:rtp:1'>
            <payload-type channels='2' clockrate='48000' id='109' name='opus'>
                <parameter name='maxplaybackrate' value='48000' xmlns='urn:xmpp:jingle:apps:rtp:1'/>
                <parameter name='stereo' value='1' xmlns='urn:xmpp:jingle:apps:rtp:1'/>
                <parameter name='useinbandfec' value='1' xmlns='urn:xmpp:jingle:apps:rtp:1'/>
            </payload-type>
            <payload-type channels='1' clockrate='8000' id='9' name='G722'/>
            <payload-type channels='1' clockrate='8000' id='0' name='PCMU'/>
            <payload-type channels='1' clockrate='8000' id='8' name='PCMA'/>
            <payload-type channels='1' clockrate='8000' id='101' name='telephone-event'>
                <parameter value='0-15' xmlns='urn:xmpp:jingle:apps:rtp:1'/>
            </payload-type>
            <rtp-hdrext id='1' senders='both' uri='urn:ietf:params:rtp-hdrext:ssrc-audio-level' xmlns='urn:xmpp:jingle:apps:rtp:rtp-hdrext:0'/>
            <rtp-hdrext id='2' senders='both' uri='urn:ietf:params:rtp-hdrext:sdes:mid' xmlns='urn:xmpp:jingle:apps:rtp:rtp-hdrext:0'/>
            <rtcp-mux/>
            <source ssrc='1922778502' xmlns='urn:xmpp:jingle:apps:rtp:ssma:0'>
                <parameter name='cname' value='{00ab4d0f-d151-ed49-ac18-6a46f77772f6}' xmlns='urn:xmpp:jingle:apps:rtp:ssma:0'/>
                <parameter name='msid' value='{5a53b85e-a9e5-984f-8c1c-1e413d46289e} {dfde9cf1-6bb9-3b44-ad9d-249482c2e05a}' xmlns='urn:xmpp:jingle:apps:rtp:ssma:0'/>
            </source>
        </description>
        <transport pwd='63ac00c43b2e66edad6a49ff54517a6e' ufrag='85c05eb0' xmlns='urn:xmpp:jingle:transports:ice-udp:1'>
            <fingerprint hash='sha-256' setup='actpass' xmlns='urn:xmpp:jingle:apps:dtls:0'>3B:91:AE:B4:C6:01:2E:A6:31:33:5B:AF:9F:F0:CF:52:7E:1E:F7:83:F2:DB:A8:D2:F4:5B:59:72:2E:F0:70:94</fingerprint>
        </transport>
    </content>">>);
content(video_source_remove) ->
    escalus_stanza:from_xml(<<"
    <content creator='initiator' name='sdparta_1' senders='responder'>
        <description media='video' ssrc='1329556979' xmlns='urn:xmpp:jingle:apps:rtp:1'>
            <payload-type channels='1' clockrate='90000' id='120' name='VP8'>
                <parameter name='max-fs' value='12288' xmlns='urn:xmpp:jingle:apps:rtp:1'/>
                <parameter name='max-fr' value='60' xmlns='urn:xmpp:jingle:apps:rtp:1'/>
                <rtcp-fb type='nack' xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0'/>
                <rtcp-fb subtype='pli' type='nack' xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0'/>
                <rtcp-fb subtype='fir' type='ccm' xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0'/>
                <rtcp-fb type='goog-remb' xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0'/>
            </payload-type>
            <payload-type channels='1' clockrate='90000' id='121' name='VP9'>
                <parameter name='max-fs' value='12288' xmlns='urn:xmpp:jingle:apps:rtp:1'/>
                <parameter name='max-fr' value='60' xmlns='urn:xmpp:jingle:apps:rtp:1'/>
                <rtcp-fb type='nack' xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0'/>
                <rtcp-fb subtype='pli' type='nack' xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0'/>
                <rtcp-fb subtype='fir' type='ccm' xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0'/>
                <rtcp-fb type='goog-remb' xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0'/>
            </payload-type>
            <payload-type channels='1' clockrate='90000' id='126' name='H264'>
                <parameter name='profile-level-id' value='42e01f' xmlns='urn:xmpp:jingle:apps:rtp:1'/>
                <parameter name='level-asymmetry-allowed' value='1' xmlns='urn:xmpp:jingle:apps:rtp:1'/>
                <parameter name='packetization-mode' value='1' xmlns='urn:xmpp:jingle:apps:rtp:1'/>
                <rtcp-fb type='nack' xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0'/>
                <rtcp-fb subtype='pli' type='nack' xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0'/>
                <rtcp-fb subtype='fir' type='ccm' xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0'/>
                <rtcp-fb type='goog-remb' xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0'/>
            </payload-type>
            <payload-type channels='1' clockrate='90000' id='97' name='H264'>
                <parameter name='profile-level-id' value='42e01f' xmlns='urn:xmpp:jingle:apps:rtp:1'/>
                <parameter name='level-asymmetry-allowed' value='1' xmlns='urn:xmpp:jingle:apps:rtp:1'/>
                <rtcp-fb type='nack' xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0'/>
                <rtcp-fb subtype='pli' type='nack' xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0'/>
                <rtcp-fb subtype='fir' type='ccm' xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0'/>
                <rtcp-fb type='goog-remb' xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0'/>
            </payload-type>
            <rtp-hdrext id='1' senders='both' uri='http://www.webrtc.org/experiments/rtp-hdrext/abs-send-time' xmlns='urn:xmpp:jingle:apps:rtp:rtp-hdrext:0'/>
            <rtp-hdrext id='2' senders='both' uri='urn:ietf:params:rtp-hdrext:toffset' xmlns='urn:xmpp:jingle:apps:rtp:rtp-hdrext:0'/>
            <rtp-hdrext id='3' senders='both' uri='urn:ietf:params:rtp-hdrext:sdes:mid' xmlns='urn:xmpp:jingle:apps:rtp:rtp-hdrext:0'/>
            <rtcp-mux/>
            <source ssrc='1329556979' xmlns='urn:xmpp:jingle:apps:rtp:ssma:0'>
                <parameter name='cname' value='{00ab4d0f-d151-ed49-ac18-6a46f77772f6}' xmlns='urn:xmpp:jingle:apps:rtp:ssma:0'/>
            </source>
        </description>
        <transport pwd='63ac00c43b2e66edad6a49ff54517a6e' ufrag='85c05eb0' xmlns='urn:xmpp:jingle:transports:ice-udp:1'>
            <fingerprint hash='sha-256' setup='actpass' xmlns='urn:xmpp:jingle:apps:dtls:0'>3B:91:AE:B4:C6:01:2E:A6:31:33:5B:AF:9F:F0:CF:52:7E:1E:F7:83:F2:DB:A8:D2:F4:5B:59:72:2E:F0:70:94</fingerprint>
        </transport>
    </content>">>);
content(video) ->
    escalus_stanza:from_xml(<<"
      <content creator='initiator' name='video_1' senders='responder'>
         <description xmlns='urn:xmpp:jingle:apps:rtp:1' media='video'>
            <payload-type id='96' name='VP8' clockrate='90000' channels='1'>
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='ccm' subtype='fir' />
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='nack' />
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='nack' subtype='pli' />
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='goog-remb' />
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='transport-cc' />
            </payload-type>
            <payload-type id='98' name='VP9' clockrate='90000' channels='1'>
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='ccm' subtype='fir' />
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='nack' />
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='nack' subtype='pli' />
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='goog-remb' />
               <rtcp-fb xmlns='urn:xmpp:jingle:apps:rtp:rtcp-fb:0' type='transport-cc' />
            </payload-type>
            <payload-type id='100' name='red' clockrate='90000' channels='1' />
            <payload-type id='127' name='ulpfec' clockrate='90000' channels='1' />
            <payload-type id='97' name='rtx' clockrate='90000' channels='1'>
               <parameter name='apt' value='96' />
            </payload-type>
            <payload-type id='99' name='rtx' clockrate='90000' channels='1'>
               <parameter name='apt' value='98' />
            </payload-type>
            <payload-type id='101' name='rtx' clockrate='90000' channels='1'>
               <parameter name='apt' value='100' />
            </payload-type>
            <rtp-hdrext xmlns='urn:xmpp:jingle:apps:rtp:rtp-hdrext:0' id='2' uri='urn:ietf:params:rtp-hdrext:toffset' senders='both' />
            <rtp-hdrext xmlns='urn:xmpp:jingle:apps:rtp:rtp-hdrext:0' id='3' uri='http://www.webrtc.org/experiments/rtp-hdrext/abs-send-time' senders='both' />
            <rtp-hdrext xmlns='urn:xmpp:jingle:apps:rtp:rtp-hdrext:0' id='4' uri='urn:3gpp:video-orientation' senders='both' />
            <rtp-hdrext xmlns='urn:xmpp:jingle:apps:rtp:rtp-hdrext:0' id='5' uri='http://www.ietf.org/id/draft-holmer-rmcat-transport-wide-cc-extensions-01' senders='both' />
            <rtp-hdrext xmlns='urn:xmpp:jingle:apps:rtp:rtp-hdrext:0' id='6' uri='http://www.webrtc.org/experiments/rtp-hdrext/playout-delay' senders='both' />
            <rtcp-mux />
         </description>
         <transport xmlns='urn:xmpp:jingle:transports:ice-udp:1' ufrag='TIxp' pwd='MkQXObfhEelTbQdRV1e0ADGh'>
            <fingerprint xmlns='urn:xmpp:jingle:apps:dtls:0' hash='sha-256' setup='actpass'>08:D7:8E:6D:A6:40:77:4C:CC:F8:46:68:80:F2:2A:B1:7B:A0:AF:02:02:CA:2A:2A:F4:35:1A:95:11:75:B2:F7</fingerprint>
         </transport>
      </content>">>);
content(video_disabled) ->
    escalus_stanza:from_xml(
      <<"<content creator='initiator' name='video' senders='none'>
      <description xmlns='urn:xmpp:jingle:apps:rtp:1' media='video'>
        <payload-type id='120' name='VP8' clockrate='90000' channels='1'/>
      </description>
      <transport xmlns='urn:xmpp:jingle:transports:ice-udp:1'>
        <fingerprint xmlns='urn:xmpp:jingle:apps:dtls:0' hash='sha-256'>99:A4:F2:DC:C0:9C:44:6E:29:7B:4C:4F:1A:00:5B:EA:24:2A:D9:3A:D1:6D:D8:C1:45:2D:E7:52:D8:E4:95:D1</fingerprint>
      </transport>
    </content>">>).

content_group(ContentEls) ->
    Contents = [#xmlel{name = <<"content">>,
                       attrs = [{<<"name">>, exml_query:attr(ContentEl, <<"name">>)}]}
                || ContentEl <- ContentEls],
    #xmlel{name = <<"group">>,
           attrs = [{<<"xmlns">>, <<"urn:xmpp:jingle:apps:grouping:0">>},
                    {<<"semantics">>, <<"BUNDLE">>}],
           children = Contents}.

