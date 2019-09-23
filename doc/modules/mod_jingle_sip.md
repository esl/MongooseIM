### Module Description

This module enables Jingle to SIP and SIP to Jingle translation.
When this module is enabled, MongooseIM will intercept any Jingle IQ set stanza with action:

* session-initiate
* session-terminate
* session-accept
* transport-info

and translate it to SIP messages with appropriate SDP content based on the details in the Jingle stanza.

The translation back from SIP to Jingle is done for the following SIP requests:

* `INVITE`
* `re-INVITE` - `INVITE` message sent for an accepted session
* `CANCEL`
* `BYE`
* `INFO`

and following responses to the INVITE request:

* `200` when the call invite was accepted
* `180` and `183` to indicate that the invitation was sent to the device
* `486` when the call's recipient rejects it
* from `400` to `600` - other error codes indicating session termination

#### Jingle to SIP translation

The table below summarises the bilateral translation for standard Jingle and SIP messages:

| Jingle action | SIP message | comment |
| ------------- | ----------- | ------- |
| `session-initiate` | `INVITE` request | |
| `session-accept` | `200 OK` response | |
| `session-terminate` with reason `success` | `BYE` request | Only for accepted session |
| `session-terminate` with reason `decline` | `CANCEL` request | When sent by call's initiator |
| `session-terminate` with reason `decline` | `486 Busy Here` response | When sent by the invite user |
| `transport-info` | `INFO` request | |

##### Ringing notification

Both Jingle and SIP have the `ringing` notification.
It's generated as a response code `180 Ringing` by a SIP entity when the INVITE is sent to the device.
In SIP world a `183 Session Progress` response code is also generated in some cases.
Both `180` and `183` codes are translated as `session-info` Jingle stanza with `ringing` sub element.
MongooseIM generates only `180 Ringing` response code the `INVITE` request, if the recipient's online.
If the recipient is online, MongooseIM generates the `180 Ringing` response code to the `INVITE` request.

##### Recipient unavailable

When MongooseIM receives a SIP `INVITE` request addressed to an offline user,
it replies with a `480 Temporarily Unavailable` code.
The same code is expected from the SIP Proxy when MongooseIM sends the `INVITE` request.

##### Other error codes

When an error response to the `INVITE` request is from the range `400` to `699` but not `486`,
MongooseIM will send a Jingle `session-terminate` stanza to the call's initiator.
The stanza has reason `general-error` with the SIP error code in the `sip-error` sub element.

##### Non-standard Jingle stanzas used by jingle.js

The following non-standard Jingle stanzas were integrated with https://github.com/softwarehutpl/jingle.js

* `source-remove`
* `source-add`
* `source-update`

When MongooseIM observes the above Jingle stanzas, it will translate them to a SIP in-dialog `INVITE` request.
In the SDP content of the request, there will be a custom attribute `a=jingle-action`.
The value of the custom attribute is one of the three presented above.

Similarly when MongooseIM gets a SIP in-dialog `INVITE` request,
it will check if there is a custom attribute and use it as the `action` attribute of the Jingle stanza sent to the user.
If there is no such attribute, the action will be set to regular Jingle `transport-info`.

##### Non-stadard Jingle existing-session-initiate stanza

MongooseIM allows a user to ask for an unanswered `session-initiate` request.
This may be useful in web applications when there is a need to handle the call in a new browser window.

In order to get the `session-initiate`, which was not answered yet, the user can send a `get` Jingle stanza to self with action set to `existing-session-initiate`.
As a result, MongooseIM will resend the original `session-initiate` request to the device which sent the query.


### Prerequisites

By default, MongooseIM is built without SIP support.
In order to build the server with SIP support, please use `tools/configure` script before the release generation.
You may either pick only certain drivers (with SIP included) or simply use `with-all` option. Examples:

```
tools/configure with-mysql with-jingle-sip
tools/configure with-all without-odbc
tools/configure with-all
```

MongooseIM packages are built with Jingle/SIP support.

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

With this configuration MongooseIM will try sending SIP messages to a SIP proxy listening on localhost and port 5060.

### Use cases covered by tests

Currently to test the functionality we use a SIP Proxy mock written in Erlang.
The following scenarios are covered by our tests in `big_tests/tests/jingle_SUITE.erl`


All the sequence diagrams where generated with [textart.io/sequence](https://textart.io/sequence).
The source code is embedded in the markdown file below every diagram inside a comment `<!--- --->`


#### 1. Establishing a session with another XMPP user

With the mod_jingle_sip enabled, all Jingle IQ set stanzas listed above are intercepted, translated to SIP packets and sent to a SIP Proxy.
This means that the current implementation will also translate stanzas addressed to a user in the same domain.
This allows the SIP entity to control how the call between XMPP users is established.
Below there are sequence diagrams showing the communication between XMPP users, MongooseIM and SIP Proxy as in our tests.
It's possible that the SIP Proxy or other SIP entity decides that the call needs to be forked
and delivered to the user's phone number instead of generating a corresponding call back to MongooseIM.


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

##### 1.2 Signaling session-accept to other XMPP user via SIP Proxy

When the other user accepts the call invite sent by the first, the following sequence is executed.
This is a continuation of the [previous example](#11-signaling-session-initiate-to-other-xmpp-user-via-sip-proxy)

```
+-------+                       +-------------+        +-----------+                   +-------+
| UserA |                       | MongooseIM  |        | SIPProxy  |                   | UserB |
+-------+                       +-------------+        +-----------+                   +-------+
    |                                  |                     |                             |
    |                                  |                     |     session-accpet to UserA |
    |                                  |<--------------------------------------------------|
    |                                  |                     |   ------------------------\ |
    |                                  |                     |   | Jingle stanza         |-|
    |                                  |                     |   | action:session-accept | |
    |                                  |                     |   | sid: 456              | |
    |                                  | 200 OK              |   |-----------------------| |
    |                                  |-------------------->|                             |
    |                                  | --------------\     |                             |
    |                                  |-| from: UserA |     |                             |
    |                                  | | to: UserB   |     |                             |
    |                                  | | sid: 456    |     |                             |
    |                                  | |-------------|     | find corresponding call     |
    |                                  |                     |------------------------     |
    |                                  |                     |                       |     |
    |                                  |                     |<-----------------------     |
    |                                  |                     |                             |
    |                                  |              200 OK |                             |
    |                                  |<--------------------|                             |
    |                                  |     --------------\ |                             |
    |                                  |     | from: UserA |-|                             |
    |                                  |     | to: UserB   | |                             |
    |                                  |     | sid: 123    | |                             |
    |        session-accept from UserB |     |-------------| |                             |
    |<---------------------------------|                     |                             |
    |                                  |                     |                             |
```

<!---
object UserA MongooseIM SIPProxy UserB
UserB->MongooseIM: session-accpet to UserA
note left of UserB: Jingle stanza \n action:session-accept\nsid: 456
MongooseIM->SIPProxy: 200 OK
note right of MongooseIM:from: UserA\nto: UserB\nsid: 456
SIPProxy->SIPProxy: find corresponding call
SIPProxy->MongooseIM: 200 OK
note left of SIPProxy: from: UserA\nto: UserB\nsid: 123
MongooseIM->UserA: session-accept from UserB
-->

##### 1.3 Terminating a call

Any Jingle session (accepted or not) can be terminated by sending a Jingle stanza with action `session-terminate` and a reason.
In the SIP world it's more complex.
See the following examples for more information.

###### 1.3.1 Terminating an accepted call

The easiest scenario is when the call was accepted as in [1.2](#12-signaling-session-accept-to-other-xmpp-user-via-sip-proxy).
In this case one of the users sends a `session-terminate` Jingle action with a reason `success`.
This is translated to a SIP `BYE` request with `to` and `from` headers set appropriately -
`from` is the user who wants to terminate the call and `to` is the user on the other end of the session.
The `BYE` request is sent to the SIP Proxy and then to the other user in a similar way to session acceptance.

###### 1.3.2 Terminating an unanswered call by initiator

To terminate the call before it's accepted, the initiator sends a Jingle `session-terminate` stanza with a reason `decline`.
Then MongooseIM translates this to a SIP `CANCEL` request which is sent to the SIP Proxy.

###### 1.3.3 Rejecting the call

When the invitee wants to terminate the call, on the XMPP level this is also a Jingle `session-terminate` stanza with a reason `decline`.
MongooseIM translates this to SIP `486 Busy Here` **Response** (because this is a response to the invite request).

#### 2. Establishing a session with a SIP user

Establishing a session with a SIP user (or a SIP entity) works the same as in the previous section.
The only difference is that the SIP Proxy will not call MongooseIM back (as it may happen for call to other XMPP user).
Instead the SIP message sent by MongooseIM to SIP Proxy will be delivered directly to the SIP user's device.


