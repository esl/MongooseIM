In addition to the regular XMPP connection methods such as TCP (with TLS/STARTTLS),
WebSockets and BOSH, MongooseIM provides parts of its functionality over REST API.

### Assumptions

1. Every request has to be authenticated.
Please see the [Authentication](#authentication) section for more details.
1. We advise to serve this API over HTTPS.
1. User registration has to be done via other methods (f.e using the
[REST API for backend services](http-api/http-administration-api-documentation.md)).
1. Relevant endpoint has to be configured on the server side.
See the [configuration section](#configuration).
1. List of provided actions is documented in Swagger.
See the documentation at the [bottom](#specification) of this page or under
[this link](http://mongooseim.readthedocs.io/en/latest/swagger/index.html?client=true)

### Authentication

The only possible authentication method now is *Basic Authentication*.
The *userid* part is user's *bare JID* and password is the same as used to
register the user's account.

#### Bare JID

A word of explanation what is *bare JID*. Provide your MongooseIM servers host
`wonderland.com` and the user is `alice`, the bare JID for her is just: `alice@wonderland.com`.
This value should be used as *userid* in the *Basic Authentication* method for
all the REST API calls.

### Configuration

In order to enable the REST API, following configuration should be added to the
*listen* section in *ejabberd.cfg* file.

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

The most important part from above example is the *modules* lists where relevant
REST API functionalities are enabled and exposed on given path.
By default the REST API is exposed on port 8089 but this can be changed to anything
which is more convenient.

For more details about possible `ejabberd_cowboy` configuration parameters plese
see the relevant documentation in [Listener-modules](../advanced-configuration/Listener-modules/#http-based-services-bosh-websocket-rest-ejabberd_cowboy)

### Specification

Find the beautiful Swagger documentation below

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


