### Module Description

This module enabled Jingle to SIP and SIP to Jingle translator.
When this module is enalbed, MongooseIM will intercept any iq set jingle stanza with action:
* session-initiate
* session-terminate
* session-accept
* transport-info

and translate it to SIP messages with approprite SDP content based on the details in the Jingle stanza.

The translation back from SIP to Jingle is done for following SIP messages:

* `INVITE` - with additionall callback for following response codes:
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
