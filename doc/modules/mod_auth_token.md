### Module Description

This module implements handling of tokens in oAuth-like authentication scheme. It provides necessary services to:

* deserialize/serialize binary tokens received and issued by server,
* validate incoming binary tokens, eg:
    * integrity checking based on Message Authentication Codes (MAC) with use of server-side stored user keys,
    * checking validity against configured validity duration times,
    * checking validity against `sequence_no` - a sequence number stored in `auth_token` table.

The module itself does not implement protocol related details - these are implemented in `cyrsasl.erl`.
Generation of keys necessary to sign binary tokens is delegated to module `mod_keystore.erl`.

### Configuration
#### Validity periods

Access and refresh tokens validity periods can be defined independently.

Allowed units are:

* days
* hours
* minutes
* seconds

unless defined, the default values for tokens are:

* 1 hour for access token
* 25 days for refresh token

Example configuration from *ejabberd.cfg* - inside ~modules~ section:

```
{modules, [
    {mod_auth_token, [{{validity_period, access}, {13, minutes}},
                      {{validity_period, refresh}, {13, days}}]
]}.
```

#### Key names

Key names are used for signing binary tokens using HMAC with SHA-2 family function SHA-384.
Minimal configuration consists of at least one entry - if more key storage methods are configured -
they will be used to generate key.

*Important*

If more entries are to be used - they *must* share the same key name, eg "token_secret"

Example configuration from *ejabberd.cfg*

Keys stored only in RAM, key name: token_secret

```
{mod_keystore, [{keys, [{token_secret, localhost, ram}
                    %%  {token_psk, localhost,  {file, "priv/token_psk"}},
                    %%  {token_provision_psk, localhost, {file, "priv/provision_psk"}}
                    ]}]}
```

Keys stored in RAM and on disk, key name: token_secret. Resulting keys is a sum of the two.

```
{mod_keystore, [{keys, [{token_secret, localhost, ram},
                        {token_secret, localhost,  {file, "priv/token_psk"}}
                    %%  {token_provision_psk, localhost, {file, "priv/provision_psk"}}
                    ]}]},
```

### Token format description.

Both tokens (access/refresh) are to be exchanged as *base64 encoded* binary buffers.
Each binary token consists of a number of fields described in this section.

On erlang/server side the a token is represented as a record of following structure:

```
-record (token, { type              :: mod_auth_token:token_type(),
                  expiry_datetime   :: calendar:datetime(),
                  user_jid          :: ejabberd:jid(),
                  sequence_no       :: mod_auth_token:sequence_no() | undefined,
                  mac_signature     :: binary() | undefined,
                  token_body        :: binary() | undefined }).
```

Token consists of two parts:

* token body - internal fields are separated with use of binary zero separator,
* token MAC (message authentication code) appended with use of binary zero.

----
#### ** Fields description **
----

** `type` **

Erlang term (atom). Allowed/handled values:

* refresh
* access
* provision

** `expiry_datetime` **

Seconds from the beginning of the epoch, eg: 63610072334

** `user_jid` **

String of following form :

username@servername.ext

*Comment*:
Server sends /normalized/ form of user's bare jid. No resource is allowed at the end of the string. (this is not 
enforced though - server just doesn't append user's resource identifier because it should not be used in context of
this implementation by neither side).

** `sequence_no` **

Makes sense only in context of *refresh* token. A positive integer number. 

*Comment*:
Generated and tracked by server to handle revocation.

** `mac_signature` **
HMAC (hashed message authentication code) Generated out of all the fields described above using *sha384* hashing
algorithm and a secret key generated and stored on server side only. Used to check token integrity and authenticity of a sender.

----
#### Token layout 
----

Order of fields in a body should follow the following format - fields are binaries:

`[type]0[expiry_datetime]0[user_jid]0[sequence_no]`

Token, before sending, gets binary MAC appended at the end and the resulting format looks as follows:

`[type]0[expiry_datetime]0[user_jid]0[sequence_no]0[MAC]`


### Token request format 

To request access and refresh tokens for the first time client should generate and send the following **IQ** stanza -
**after he successfuly authenticated for the first time using scram-sha1 method (preferred)**.

IQ wrapper stanza fields required:

- "to" (bare user JID)
- "type" (aways "get")
- "id" (use is not obligatory - server overwrites this value and sends back to client)

Content of IQ query:

- "query" xml node with "xmlns" attribute `urn:xmpp:tmp:auth-token`

Please note, that *we'll change namespace value* soon to keep it consistent with all the other namespaces 
(the final value is not decided at the time of writing this document).

### Token response format

Requested tokens are being returned by server wrapped in /IQ/ stanza with the following fields:

- `from` (bare user JID)
- `to` (full user JID)
- `id` (value taken from request IQ stanza)
- `type` (result)

Tokens will be returned using base64 encoding - in an xml body of following, example format:

```
 <items xmlns:urn:xmpp:tmp:auth-token>
  <access_token> <![CDATA[ACCESS_TOKEN_BODY_WITH_MAC]]> </access_token>
  <refresh_token> <![CDATA[REFRESH_TOKEN_BODY_WITH_MAC]]> </refresh_token>
 </items>
  
```

Once client obtained an acccess token - he may start authenticating himself next time choosing **X-OAUTH** SASL mechanism
by issuing following SASL command:

```
<auth xmlns="urn:ietf:params:xml:ns:xmpp-sasl" mechanism="X-OAUTH"/>
```

### Login with access/refresh token

In order to log into XMPP server using previously requested binary access token - client should issue following
stanza:

```
<auth xmlns="urn:ietf:params:xml:ns:xmpp-sasl" mechanism="X-OAUTH" <![CDATA[ACCESS_TOKEN_BODY_WITH_MAC]]>
```

or

```
<auth xmlns="urn:ietf:params:xml:ns:xmpp-sasl" mechanism="X-OAUTH" <![CDATA[REFRESH_TOKEN_BODY_WITH_MAC]]>
```


Both of the above will result in successful authentication and assigning a resource (binding) to a client by
the server - unless used tokens are expired or/and the keys could not be retrieved/generated by the server.

### Login with refresh token

When using refresh token to authenticate with the server - server will respond with new **access token** - the
token will be issued as body of `response` as follows;

```
<response xmlns="urn:ietf:params:xml:ns:xmpp-sasl">
<![CDATA[ACCESS_TOKEN_BODY_WITH_MAC]]>
</response>

```

Above response is to be expected unless refresh token used is expired or there were some problems with key
processing on server side.

### Token revocation using command line tool

Refresh tokens issued by the server serve as :

* authentication valet - to login user
* to request issuing of a **new access token** - in principle - with refreshed expiry date

Administrator may **revoke** refresh tokens - which means that client can no longer use them neither for
authentication nor requesting new access tokens. For client, in order to obtain a new refresh token - it's
necessary to log in first once it's been revoked.

` mongooseimctl revoke_token owner@xmpphost`

