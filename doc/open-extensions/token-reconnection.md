# Introduction

Automatic reconnection after spurious disconnection is a must-have feature in modern IM applications. 
One way of providing this feature is storing the user login information on the disk. 
Here you need to balance two values - security and convienience for the end-user. 
To put it simply: storing passowords in plaintext is inherently insecure while protecting the XMPP password with a master-password is damages the user experience.
With a token-based authentication mechanism, the user has to provide login information only once, for the initial connection to the XMPP server, and can later rely on the application's automatic use of tokens for subsequent reconnections.


Reconnecting to the XMPP server, usually means that the client has to go through the same long process of SASL challenge-response exchange which may cause noticable lags, especially while using SCRAM-based mechanisms. 
Providing a token to the XMPP server is secure and doesn't require multiple challenge-response roundtrips, therefore might significantly speed up reconnection times.

# Requirements

This extension requires the client application to authenticate to the XMPP server using a regular XMPP authentication mechanism like SCRAM-SHA-1 at least once.

After that, the following authentications may be done using X-OAUTH SASL mechanism with a token obtained from the server.

To enable the feature, modules `mod_auth_token` and `mod_keystore` have to be enabled on the server. For more details regarding the configuration see [mod_auth_token documentation](../modules/mod_auth_token.md) and [mod_keystore](../modules/mod_keystore.md).

# Token types

| Token Type | Description |
| ---------  | ----------- |
| Access token | These are short lived tokens whose grants aren't tracked by the server (i.e. there's no need to store anything in a database). Access tokens can be used as a payload for the X-OAUTH authentication mechanism and grant access to the system. Access tokens can't be revoked. An access token is valid only until its expiry date is reached. |
| Refresh token | These are longer lived tokens which are tracked by the server, and therefore require persistent storage. Refresh tokens can be used as a payload for the X-OAUTH authentication mechanism and grant access to the system, as well as result in a new set of tokens being returned upon successful authentication. Refresh tokens can be revoked. A refresh token is valid until it has expired, unless it has been revoked. On revocation, it immediately becomes invalid. As the server stores information about granted tokens, it can also persistently mark them as revoked. |

While only two token types have been described above, implementations might use other token types for specific purposes. 
For example, a particular token type could limit the access privileges of a user logged into the system or denote an affiliation with a Multi User Chat room. 
None of such capability grants are a subject of this specification though.

## Use cases

### Obtaining a token

After authenticating with some other mechanism like SCRAM-SHA-1, a client may request a token from the server by sending the following iq get to its own bare JID:

**Client requests tokens**

```xml
<iq type='get' to='alice@wonderland.com' id='123'>
    <query xmlns='erlang-solutions.com:xmpp:token-auth:0'/>
</iq>
```

**Server responds with a tokens**

```xml
<iq from="alice@wonderland.com" type="result" to="alice@wonderland.com/resource" id="123">
  <items xmlns="erlang-solutions.com:xmpp:token-auth:0">
    <access_token>YWNjZXNzAGFsaWNlQHdvbmRlcmxhbmQuY29tL01pY2hhbC1QaW90cm93c2tpcy1NYWNCb29rLVBybwA2MzYyMTg4Mzc2NAA4M2QwNzNiZjBkOGJlYzVjZmNkODgyY2ZlMzkyZWM5NGIzZjA4ODNlNDI4ZjQzYjc5MGYxOWViM2I2ZWJlNDc0ODc3MDkxZTIyN2RhOGMwYTk2ZTc5ODBhNjM5NjE1Zjk=</access_token>
    <refresh_token>cmVmcmVzaABhbGljZUB3b25kZXJsYW5kLmNvbS9NaWNoYWwtUGlvdHJvd3NraXMtTWFjQm9vay1Qcm8ANjM2MjMwMDYxODQAMQAwZGQxOGJjODhkMGQ0N2MzNTBkYzAwYjcxZjMyZDVmOWIwOTljMmI1ODU5MmNhN2QxZGFmNWFkNGM0NDQ2ZGU2MWYxYzdhNTJjNDUyMGI5YmIxNGIxNTMwMTE4YTM1NTc=</refresh_token>
  </items>
</iq>
```

### Authentication with an access token

**Client authenticates with an access token**

```xml
<auth xmlns="urn:ietf:params:xml:ns:xmpp-sasl" mechanism="X-OAUTH">
YWNjZXNzAGFsaWNlQHdvbmRlcmxhbmQuY29tL01pY2hhbC1QaW90cm93c2tpcy1NYWNCb29rLVBybwA2MzYyMTg4Mzc2NAA4M2QwNzNiZjBkOGJlYzVjZmNkODgyY2ZlMzkyZWM5NGIzZjA4ODNlNDI4ZjQzYjc5MGYxOWViM2I2ZWJlNDc0ODc3MDkxZTIyN2RhOGMwYTk2ZTc5ODBhNjM5NjE1Zjk=
</auth>
```
**Server responds with a success**

```xml
<success xmlns="urn:ietf:params:xml:ns:xmpp-sasl"/>
```

### Authentication with a refresh token

In this situation server will respond with a new refresh token which SHOULD be used in future authentication.

**Client authenticates with a refresh token**

```xml
<auth xmlns="urn:ietf:params:xml:ns:xmpp-sasl" mechanism="X-OAUTH">
cmVmcmVzaABhbGljZUB3b25kZXJsYW5kLmNvbS9NaWNoYWwtUGlvdHJvd3NraXMtTWFjQm9vay1Qcm8ANjM2MjMwMDYxODQAMQAwZGQxOGJjODhkMGQ0N2MzNTBkYzAwYjcxZjMyZDVmOWIwOTljMmI1ODU5MmNhN2QxZGFmNWFkNGM0NDQ2ZGU2MWYxYzdhNTJjNDUyMGI5YmIxNGIxNTMwMTE4YTM1NTc=
</auth>
```

**Server responds with a success and a new refresh token**

```xml
<success xmlns="urn:ietf:params:xml:ns:xmpp-sasl">
cmVmcmVzaABhbGljZUB3b25kZXJsYW5kLmNvbS9NaWNoYWwtUGlvdHJvd3NraXMtTWFjQm9vay1Qcm8ANjM2MjMwMDYxODQAMgAwZGQxOGJjODhkMGQ0N2MzNTBkYzAwYjcxZjMyZDVmOWIwOTljMmI1ODU5MmNhN2QxZGFmNWFkNGM0NDQ2ZGU2MWYxYzdhNTJjNDUyMGI5YmIxNGIxNTMwMTE4YTM1NTc=
</success>
```

## Token format

All tokens are exchanged as Base64 encoded binary data. 
Serialization format of the token before encoding with Base64 is dependent on its type. 
Common parts in every token are `BARE_JID` and `EXPIRES_AT`. `EXPIRES_AT` is a timestamp saying when a given token will expire. `\0` stands for the ASCII null character (i.e. byte 0).
Text in single quotes ('example') is literal. 
`ALL_CAPS` denote parameters.

### Access token format

```
BASE64_encode
        ('access', \0, BARE_JID, \0, EXPIRES_AT, \0, DATA)
```

Example (please note the line break was added only for readability):

```
'access' \0 Q8@wonderland.com \0 64875466454
    \0 0acd0a66d06934791d046060cf9f1ad3c2abb3274cc7e7d7b2bc7e2ac4453ed774b6c6813b40ebec2bbc3774d59d4087
```

### Refresh token format

```
BASE64_encode
        ('refresh', \0, BARE_JID, \0, EXPIRES_AT, \0, SEQUENCE_NO, \0, DATA)
```

Example (please note the line break was added only for readability):

```
'refresh' \0 qp@wonderland.com \0 64875466457 \0 6
    \0 8f57cb019cd6dc6e7779be165b9558611baf71ee4a40d03e77b78b069f482f96c9d23b1ac1ef69f64c1a1db3d36a96ad
```
