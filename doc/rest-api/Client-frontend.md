# MongooseIM's REST API for frontend or client

In addition to the regular XMPP connection methods such as TCP (with TLS/STARTTLS),
WebSockets and BOSH, MongooseIM provides parts of its functionality over a REST API.

## Assumptions

1. Every request has to be authenticated.
Please see the [Authentication](#authentication) section for more details.
1. We advise that this API is served over HTTPS.
1. User registration has to be done via other methods (f.e. using the
[REST API for backend services](Administration-backend.md)).
1. The relevant endpoint has to be configured on the server side.
See the [configuration section](#configuration).
1. A list of provided actions is documented with Swagger.
See the beatiful [specification](http://mongooseim.readthedocs.io/en/latest/swagger/index.html?client=true).

## Authentication

The only possible authentication method for the time being is *Basic Authentication*.
The *userid* part is user's *bare JID* and the password is the same as that used to
register the user's account.

### Bare JID

To ilustrate what bare JIDs are, let's assume your MongooseIM server's hostname is
*wonderland.com* and the user is *alice*.
In this case the bare JID for her is just: *alice@wonderland.com*.
This value should be used as the *userid* in the Basic Authentication method for all the REST API calls.

## Configuration

In order to enable the REST API, the following configuration should be added to the
*listen* section in *mongooseim.cfg* file.

```erlang
  { 8089 , ejabberd_cowboy, [
      {num_acceptors, 10},
      {max_connections, 1024},
      {compress, true},
      {ssl, [{certfile, "priv/ssl/fake_cert.pem"}, {keyfile, "priv/ssl/fake_key.pem"}, {password, ""}]},
      {modules, [
          {"_", "/api/messages/[:with]", mongoose_client_api_messages, []},
          {"_", "/api/rooms/:id/messages",    mongoose_client_api_rooms_messages, []},
          {"_", "/api/rooms/:id/users/[:user]",    mongoose_client_api_rooms_users, []},
          {"_", "/api/rooms/[:id]",    mongoose_client_api_rooms, []}
      ]}
  ]}
```

The most important part of the above example is the *modules* lists where the relevant
REST API functionalities are enabled and exposed on the given paths.
By default the REST API is exposed on port 8089 but this can be changed to whatever is more convenient.

For more details about possible `ejabberd_cowboy` configuration parameters please
see the relevant documentation in the [Listener modules](../advanced-configuration/Listener-modules.md#http-based-services-bosh-websocket-rest-ejabberd_cowboy).


## Smack library support
REST API can fetch messages for [Smack](https://github.com/igniterealtime/Smack/blob/master/documentation/extensions/properties.md#stanza-properties) Stanza Properties.

For example if we have properties in the stanza like:
  ```
      <message xml:lang='en' to='alice@localhost' id='123' type='chat'>
        <body xml:lang='en_US'>Hi!</body>
        <properties xmlns="http://www.jivesoftware.com/xmlns/xmpp/properties"
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
```
    {   "to": "alice@localhost",
        "timestamp": 1531329049949,
        "id": "123",
        "from": "bob@localhost",
        "body": "Hi!",
        "properties":{
            "some_number":"123",
            "some_string":"abc"
        }
    }
```

## OpenAPI specifications

See the beautiful [Swagger documentation](http://mongooseim.readthedocs.io/en/latest/swagger/index.html?client=true) for more information.

[![Swagger](http://nordicapis.com/wp-content/uploads/swagger-Top-Specification-Formats-for-REST-APIs-nordic-apis-sandoval-e1441412425742-300x170.png)](http://mongooseim.readthedocs.io/en/latest/swagger/index.html?client=true)

<iframe src="http://mongooseim.readthedocs.io/en/latest/swagger/index.html?client=true"
height="800" width="800" style="margin-left: -45px;" id="swagger-ui-iframe"></iframe>

<script>

$(document).ready(function() {
  if (window.location.host.match("readthedocs")){
    path = window.location.pathname.match("(.*)/REST-API/")[1]
    url = window.location.protocol + "//" + window.location.hostname
    finalURL = url + path + "/swagger/index.html?client=true"
    $('a[href$="swagger/index.html?client=true"]').attr('href', finalURL)
    $('#swagger-ui-iframe').attr('src', finalURL)
  }
})

</script>
