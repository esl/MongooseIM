### Module Description

This module enables Jingle to SIP and SIP to Jingle translation.
When this module is enabled, MongooseIM will intercept any Jingle IQ set stanza with action:
* session-initiate
* session-terminate
* session-accept
* transport-info

and translates it to SIP messages with appropriate SDP content based on the details in the Jingle stanza.

The translation back from SIP to Jingle is done for following SIP messages:

* `INVITE` - with additional callback for following response codes:
   * `200`
   * `180` and `183`
   * `486` when the call's recipient rejects it
   * from `400` to `600` - other error codes indicating session terminate
* `re-INVITE` - `INVITE` message sent for established session
* `CANCEL`
* `BYE`
* `INFO`

### Prerequisites

By default, MongooseIM is built without SIP support.
In order to build the server with SIP support, please use `tools/configure` script before the release generation.
You may either pick only certain drivers (with SIP included) or simply use `with-all` option. Examples:

```
tools/configure with-mysql with-jingle-sip
tools/configure with-all without-odbc
tools/configure with-all
```

MongooseIM 2.2.x packages are built with OTP 19.3, so they include Jingle/SIP support.

### Options

* `proxy_host` (default: "localhost") name or IP address of the SIP Proxy to which MongooseIM will send SIP messages
* `proxy_port` (default: 5600) port of the SIP Proxy
* `listen_port` (default: 5600) the port on which MongooseIM will listen for incomming SIP messages
* `local_host` (default: "localhost") value used to create SIP URIs (including VIA headers)
* `sdp_origin` (default: "127.0.0.1") value of the `c=` SDP attribute

The simplest configuration is the following:

```erlang
{mod_jingle_sip, []}
```

With this configuration MongooseIM will try sending any SIP message to a SIP proxy
listening on `localhost` and port `5060`.

### Use cases covered by tests

Currently to test the functionality we use a SIP Proxy mock written in Erlang.
The following scenarios are covered by our tests in `big_tests/tests/jingle_SUITE.erl`

#### 1. Establishing a session with other XMPP user

With the `mod_jingle_sip` enabled all Jingle IQ set stanzas listed above are intercepted,
translated to SIP packets and sent to a SIP Proxy.
This means that current implementation will also translate stanzas addressed to a user
in the same domain.
Below there are sequence diagrams showing the communication between XMPP users, MongooseIM and SIP Proxy.

<!---
All the diagrams where generated with https://textart.io/sequence
The source code is embedded here below every diagram
-->

##### 1.1 Signaling session-initiate to other XMPP user via SIP Proxy

```
+-------+                       +-------------+       +-----------+                   +-------+
| UserA |                       | MongooseIM  |       | SIPProxy  |                   | UserB |
+-------+                       +-------------+       +-----------+                   +-------+
    |                                  |                    |                             |
    | session-initiate to UserB        |                    |                             |
    |--------------------------------->|                    |                             |
    | -------------------------\       |                    |                             |
    |-| Jingle stanza          |       |                    |                             |
    | | action:session-initate |       |                    |                             |
    | | sid: 123               |       |                    |                             |
    | |------------------------|       | SIP INVITE         |                             |
    |                                  |------------------->|                             |
    |                                  | -------------\     |                             |
    |                                  |-| from:UserA |     |                             |
    |                                  | | to:UserB   |     |                             |
    |                                  | | sid: 123   |     |                             |
    |                                  | |------------|     | create new call             |
    |                                  |                    |----------------             |
    |                                  |                    |               |             |
    |                                  |                    |<---------------             |
    |                                  |                    | ------------------------\   |
    |                                  |                    |-| SDP content can be    |   |
    |                                  |                    | | changed for instance  |   |
    |                                  |                    | | to inject a transport |   |
    |                                  |         SIP INVITE | | canidate              |   |
    |                                  |<-------------------| |-----------------------|   |
    |                                  |     -------------\ |                             |
    |                                  |     | from:UserA |-|                             |
    |                                  |     | to:UserB   | |                             |
    |            --------------------\ |     | sid:456    | |                             |
    |            | yes, new SID: 456 |-|     |------------| |                             |
    |            |-------------------| |                    |                             |
    |                                  |                    |                             |
    |                                  | session-initiate to UserB                        |
    |                                  |------------------------------------------------->|
    |                                  |                    |                             |
```

<!---
object UserA MongooseIM SIPProxy UserB
UserA->MongooseIM: session-initiate to UserB
note right of UserA: Jingle stanza \n action:session-initate\nsid: 123
MongooseIM->SIPProxy: SIP INVITE
note right of MongooseIM: from:UserA\nto:UserB\nsid: 123
SIPProxy->SIPProxy: create new call
note right of SIPProxy: SDP content can be\nchanged for instance\n to inject a transport\n canidate
SIPProxy->MongooseIM: SIP INVITE
note left of SIPProxy: from:UserA\nto:UserB\nsid:456
note left of MongooseIM: yes, new SID: 456
MongooseIM->UserB: session-initiate to UserB
-->
