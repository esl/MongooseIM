# MongooseIM's REST API for frontend or client

In addition to the regular XMPP connection methods such as TCP (with TLS/STARTTLS),
WebSockets and BOSH, MongooseIM provides parts of its functionality over a REST API.

## Assumptions

1. Every request has to be authenticated.
Please see the [Authentication](#authentication) section for more details.
1. We strongly advise that this API is served over HTTPS.
1. User registration has to be done via other methods (f.e. using the
[REST API for backend services](Administration-backend.md)).
1. The relevant endpoint has to be configured on the server side.
See the [configuration section](#configuration).
1. A list of provided actions is documented with Swagger.
See the [specification](https://esl.github.io/MongooseDocs/latest/swagger/index.html?client=true).

## Authentication

MongooseIM uses *Basic Authentication* as an authentication method for the REST API.

*Basic authentication* is a simple authentication scheme built into the HTTP protocol.
Each HTTP request to the client REST API has to contain the Authorization header
with the word `Basic` followed by a space and a base64-encoded string
`username@host:password`, where:

- `username@host` is the user's *bare JID*,
- `password` is the password used to register the user's account.

For example, to authorize as `alice@localhost` with the password `secret`, the
client would send a header:

```
Authorization: Basic YWxpY2VAbG9jYWxob3N0OnNlY3JldA==
```

## Configuration

Handlers have to be configured as shown in the [REST API configuration example](../configuration/listen.md#example-6-client-rest-api)
to enable REST API.

In order to get the client REST API up and running simply copy the provided example.
For more details about possible configuration parameters please see the relevant
documentation of the [listeners](../configuration/listen.md),
in particular the [client REST API handlers](../configuration/listen.md#handler-types-rest-api-client)
section.

## Smack library support
REST API can fetch messages for [Smack](https://github.com/igniterealtime/Smack/blob/master/documentation/extensions/properties.md#stanza-properties) Stanza Properties.

For example if we have properties in the stanza like:
```xml
    <message xml:lang='en' to='alice@localhost' id='123' type='chat'>
      <body xml:lang='en_US'>Hi!</body>
      <thread parent='7edac73ab41e45c4aafa7b2d7b749080'>
        e0ffe42b28561960c6b12b944a092794b9683a38
      </thread>
      <properties xmlns="http://www.jivesoftware.com/xmlns/xmpp/properties">
          <property>
              <name>some_number</name>
              <value type='integer'>123</value>
          <property>
          <property>
              <name>some_string</name>
              <value type='string'>abc</value>
          <property>
      </properties>
    </message>
```
then in the final json message these properties will be converted to json map without tag names and all types will be taken as string:
```json
    {   "to": "alice@localhost",
        "timestamp": 1531329049949,
        "id": "123",
        "from": "bob@localhost",
        "body": "Hi!",
        "thread": "e0ffe42b28561960c6b12b944a092794b9683a38",
        "parent": "7edac73ab41e45c4aafa7b2d7b749080",
        "properties":{
            "some_number":"123",
            "some_string":"abc"
        }
    }
```

## OpenAPI specifications

See the [Swagger documentation](https://esl.github.io/MongooseDocs/latest/swagger/index.html?client=true) for more information.

[![Swagger](https://nordicapis.com/wp-content/uploads/swagger-Top-Specification-Formats-for-REST-APIs-nordic-apis-sandoval-e1441412425742-300x170.png)](https://esl.github.io/MongooseDocs/latest/swagger/index.html?client=true)

<iframe src="https://esl.github.io/MongooseDocs/latest/swagger/index.html?client=true"
height="800" width="800" id="swagger-ui-iframe"></iframe>

<script>

$(document).ready(function() {
  if (window.location.host.match("github")){
    path = window.location.pathname.match("(.*)/REST-API/")[1]
    url = window.location.protocol + "//" + window.location.hostname
    finalURL = url + path + "/swagger/index.html?client=true"
    $('a[href$="swagger/index.html?client=true"]').attr('href', finalURL)
    $('#swagger-ui-iframe').attr('src', finalURL)
  }
})

</script>
