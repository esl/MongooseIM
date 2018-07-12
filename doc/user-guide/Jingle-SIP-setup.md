## Jingle/SIP setup proof of concept

In this document I'm going to describe how to configure MongooseIM, Routr (a SIP server) and client applications to show the Jingle/SIP integration.

### Prerequisites

We are going to use the following open source software:
* MongooseIM - https://github.com/esl/MongooseIM
    * see [How-to-build][How-to-build.md] for details on building.
      It's important to remember to run configure script with `with-jingle-sip` flag set, like below
      ```bash
      tools/configure with-jingle-sip
      ```
      Without this, 3rd party deps required by Jingle/SIP translator will not be included in the release.
* Routr (SIP server) - https://routr.io
    * I recommend downloading binaries for your system from https://routr.io/docs/getting-started-installation.html
* Jitsi (XMPP and SIP client application) - https://desktop.jitsi.org/
* Otalk - web based XMPP client - https://github.com/otalk/otalk-im-client
    * Folow instruction on https://github.com/otalk/otalk-im-client#installing to make it running

We will use 2 users `xmpp.user@xmpp.example` and `sip.user@sip.example`.

### Configuring Routr

First the domain `sip.example` needs to added to domains served by Routr.
This can be done by adding the following content to `config/domains.yml` in the directory where Routr was extracted.

```yml
- apiVersion: v1beta1
  kind: Domain
  metadata:
    name: SIP domain
  spec:
    context:
      domainUri: sip.example
```

Then the `sip.user@sip.example` needs to be added to `config/agents.yml` like below:

```yml
- apiVersion: v1beta1
  kind: Agent
  metadata:
    name: SIP User
  spec:
    credentials:
      username: 'sip.user'
      secret: '1234'
    domains: [sip.example]
```

Now Routr can be started with

    ./routr

If all goes well we'll see output like below, it's important to not the IP address as it'll be used in next point.

```
[INFO ] Starting Routr
[INFO ] Listening  on 10.152.1.27:5060 [udp]
[INFO ] Listening  on 10.152.1.27:5060 [tcp]
[INFO ] Starting Location service
[INFO ] Starting Registry service
[INFO ] Starting Restful service (port: 4567, apiPath: '/api/v1beta1')
```

### Configuring /etc/hosts

In my case the IP reported by Routr was `10.152.1.27`.
Now we need to use this to update `/etc/hosts` file like below:

    10.152.1.27     sip.example xmpp.example

### Configuring MongooseIM

At this point I assume that MongooseIM was build with `make rel`, is running and the CWD is `_build/prod/rel/mongooseim`.
Similar to Routr, MongooseIM also needs to know which hosts to server.
Please replace default host defined in line

```erlang
{hosts, ["localhost"] }.
```

```erlang
{hosts, ["xmpp.example", "sip.example"] }.
```

Now we need to enable `mod_jingle_sip`, please add the following line in modules list (somewhere around line 740)

```erlang
{mod_jingle_sip, [{proxy_host, "sip.example"}]},
```

Now we are registering both users in MongooseIM by calling following commands:

    bin/mongooseimctl register xmpp.user xmpp.example test_pass
    bin/mongooseimctl register sip.user sip.example test_pass

Yes, we need to have the `sip.user@sip.example` registered in MongooseIM.
This is needed because Jingle call by regular XMPP client can be initiate only when the other user is online.
In order to know the other user is online we need to get a presence from it.
The easiest way to achieve this is to have 2 xmpp users added to each other's roster.

The roster can be set by us with the following commands:

    bin/mongooseimctl add_rosteritem sip.user sip.example xmpp.user xmpp.example xmpp.user none both
    bin/mongooseimctl add_rosteritem xmpp.user xmpp.example sip.user sip.example sip.user none both

Now describe how to add sip user (for SIP and XMPP) to Jitsi

Then describe how to start otalk

* Remember that Jingle needs a full JID to work with so we need to hack on SIP user.

wss://localhost:5285/ws-xmpp


SIP INVITE from MongooseIM to SIP Proxy:

```
INVITE sip:sip.user@sip.example:5060 SIP/2.0
Via: SIP/2.0/TCP localhost:5600;rport;branch=z9hG4bK1HMB3o-3mbahM
From: xmpp.user <sip:xmpp.user@xmpp.example>;tag=aVEBue
To: sip.user <sip:sip.user@sip.example>
Call-ID: ae602f16-d57d-4452-b83e-36e54bb6d325
CSeq: 159913767 INVITE
Max-Forwards: 70
Content-Length: 2243
Contact: <sip:xmpp.user@localhost:5600;ob;transport=tcp>;+sip.instance="<urn:uuid:f45950f1-70cd-229d-6c2b-8c85903ce14e>"
Content-Type: application/sdp
Supported: outbound,100rel,path
Allow: PRACK,INVITE,ACK,CANCEL,BYE,OPTIONS,INFO,UPDATE,SUBSCRIBE,NOTIFY,REFER,MESSAGE

v=0
o=- 1531401304 1531401304 IN IP4 127.0.0.1
s=nksip
c=IN IP4 127.0.0.1
t=0 0
a=group:BUNDLE sdparta_0 sdparta_1
m=audio 1436 UDP/TLS/RTP/SAVPF 109 9 0 8 101
a=sendrecv
a=mid:sdparta_0
a=setup:actpass
a=fingerprint:sha-256 44:84:41:8F:B7:A3:B7:37:BA:00:26:5E:B1:D6:AB:D0:56:56:CF:53:F2:05:DB:99:DE:D4:1C:63:A4:68:58:EA
a=ice-pwd:49ad0f02b4f5181c9af3c4006575e071
a=ice-ufrag:a3cc96e2
a=rtcp-mux
a=extmap:3 urn:ietf:params:rtp-hdrext:sdes:mid
a=extmap:2/recvonly urn:ietf:params:rtp-hdrext:csrc-audio-level
a=extmap:1 urn:ietf:params:rtp-hdrext:ssrc-audio-level
a=rtpmap:109 opus/48000/2
a=fmtp:109 useinbandfec=1;stereo=1;maxplaybackrate=48000
a=rtpmap:9 G722/8000
a=rtpmap:0 PCMU/8000
a=rtpmap:8 PCMA/8000
a=rtpmap:101 telephone-event/8000
a=fmtp:101 0-15
a=ssrc:1698222108 cname:{ce7fa171-069e-db4f-ba41-cfa4455c1033}
a=ssrc:1698222108 msid:{788b64bb-c4fc-b644-89b0-89f69c78f8b0} {2ba61f91-abca-3e48-84b7-85b57e8fdfb5}
m=video 1031 UDP/TLS/RTP/SAVPF 120 121 126 97
a=sendrecv
a=mid:sdparta_1
a=setup:actpass
a=fingerprint:sha-256 44:84:41:8F:B7:A3:B7:37:BA:00:26:5E:B1:D6:AB:D0:56:56:CF:53:F2:05:DB:99:DE:D4:1C:63:A4:68:58:EA
a=ice-pwd:49ad0f02b4f5181c9af3c4006575e071
a=ice-ufrag:a3cc96e2
a=rtcp-mux
a=extmap:5 urn:ietf:params:rtp-hdrext:toffset
a=extmap:4 http://www.webrtc.org/experiments/rtp-hdrext/abs-send-time
a=extmap:3 urn:ietf:params:rtp-hdrext:sdes:mid
a=rtpmap:120 VP8/90000
a=fmtp:120 max-fr=60;max-fs=12288
a=rtcp-fb:120 goog-remb
a=rtcp-fb:120 ccm fir
a=rtcp-fb:120 nack pli
a=rtcp-fb:120 nack
a=rtpmap:121 VP9/90000
a=fmtp:121 max-fr=60;max-fs=12288
a=rtcp-fb:121 goog-remb
a=rtcp-fb:121 ccm fir
a=rtcp-fb:121 nack pli
a=rtcp-fb:121 nack
a=rtpmap:126 H264/90000
a=fmtp:126 packetization-mode=1;level-asymmetry-allowed=1;profile-level-id=42e01f
a=rtcp-fb:126 goog-remb
a=rtcp-fb:126 ccm fir
a=rtcp-fb:126 nack pli
a=rtcp-fb:126 nack
a=rtpmap:97 H264/90000
a=fmtp:97 level-asymmetry-allowed=1;profile-level-id=42e01f
a=rtcp-fb:97 goog-remb
a=rtcp-fb:97 ccm fir
a=rtcp-fb:97 nack pli
a=rtcp-fb:97 nack
a=ssrc:823938224 cname:{ce7fa171-069e-db4f-ba41-cfa4455c1033}
a=ssrc:823938224 msid:{788b64bb-c4fc-b644-89b0-89f69c78f8b0} {a7f87c8d-6002-fd4c-badb-13383c759e48}
```

SIP RINGING from SIP Proxy to MongooseIM

```
SIP/2.0 180 Ringing
CSeq: 159913767 INVITE
Call-ID: ae602f16-d57d-4452-b83e-36e54bb6d325
From: "xmpp.user" <sip:xmpp.user@xmpp.example>;tag=aVEBue
To: "sip.user" <sip:sip.user@sip.example>;tag=9b4c72a3
Via: SIP/2.0/TCP localhost:5600;rport=54071;branch=z9hG4bK1HMB3o-3mbahM;received=10.152.1.27
Contact: "sip.user" <sip:sip.user@10.152.1.27:53697;transport=tcp;registering_acc=sip_example>
User-Agent: Jitsi2.10.5550Mac OS X
Content-Length: 0

```
