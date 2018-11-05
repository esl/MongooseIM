# MongooseIM's REST API for backend administration

## Configuration

Commands used by the REST API are provided by modules:

`mod_commands` - provides general purpose commands: both user-like (f.e. sending a message and retrieving messages from the archive) and administration-like (f.e. create/delete a user and change the password)

`mod_muc_commands` - commands related to Multi-user Chat rooms: create a room, invite users, send a message etc.

`mod_muc_light_commands` - same but for rooms based on the muc-light protocol.

To activate those commands, put modules you need into the mongooseim.cfg file:

```
  {mod_commands, []},
  {mod_muc_commands, []},
  {mod_muc_light_commands, []},

```

You also have to hook `mongoose_api_admin` module to an HTTP endpoint:

```
  { {8088, "127.0.0.1"} , ejabberd_cowboy, [
      {num_acceptors, 10},
      {transport_options, [{max_connections, 1024}]},
      {modules, [
          {"localhost", "/api", mongoose_api_admin, []}
      ]}
  ]},
```

## OpenAPI specifications

Read the beautiful [Swagger documentation](http://mongooseim.readthedocs.io/en/latest/swagger/index.html) for more information.

[![Swagger](http://nordicapis.com/wp-content/uploads/swagger-Top-Specification-Formats-for-REST-APIs-nordic-apis-sandoval-e1441412425742-300x170.png)](http://mongooseim.readthedocs.io/en/latest/swagger/index.html)

<iframe src="http://mongooseim.readthedocs.io/en/latest/swagger/index.html"
height="800" width="800" style="margin-left: -45px;" id="swagger-ui-iframe"></iframe>

<script>

$(document).ready(function() {
  if (window.location.host.match("readthedocs")){
    path = window.location.pathname.match("(.*)/rest-api/Administration-backend")[1]
    url = window.location.protocol + "//" + window.location.hostname
    finalURL = url + path + "/swagger/index.html"
    $('a[href$="swagger/index.html"]').attr('href', finalURL)
    $('#swagger-ui-iframe').attr('src', finalURL)
  }
})

</script>
