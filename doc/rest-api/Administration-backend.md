# MongooseIM's REST API for backend administration

## Configuration

Commands used by the REST API are provided by modules:

`mod_commands` - provides general purpose commands: both user-like (e.g. sending a message and retrieving messages from the archive) and administration-like (e.g. create/delete a user and change the password).

`mod_muc_commands` - commands related to Multi-user Chat rooms: create a room, invite users, send a message etc.

`mod_muc_light_commands` - same but for rooms based on the muc-light protocol.

To activate those commands, put the modules you need into the `mongooseim.toml` file:

```toml
  [modules.mod_commands]

  [modules.mod_muc_commands]

  [modules.mod_muc_light_commands]

```

You also have to hook the `mongoose_api_admin` module to an HTTP endpoint as described
in the [admin REST API handlers configuration](../advanced-configuration/listen.md#handler-types-rest-api-admin-mongoose_api_admin)
section of the [listeners](../advanced-configuration/listen.md) documentation.

## OpenAPI specifications

Read the beautiful [Swagger documentation](https://esl.github.io/MongooseDocs/latest/swagger/index.html) for more information.

[![Swagger](https://nordicapis.com/wp-content/uploads/swagger-Top-Specification-Formats-for-REST-APIs-nordic-apis-sandoval-e1441412425742-300x170.png)](https://esl.github.io/MongooseDocs/latest/swagger/index.html)

<iframe src="https://esl.github.io/MongooseDocs/latest/swagger/index.html"
height="800" width="800" style="margin-left: -45px;" id="swagger-ui-iframe"></iframe>

<script>

$(document).ready(function() {
  if (window.location.host.match("github")){
    path = window.location.pathname.match("(.*)/rest-api/Administration-backend")[1]
    url = window.location.protocol + "//" + window.location.hostname
    finalURL = url + path + "/swagger/index.html"
    $('a[href$="swagger/index.html"]').attr('href', finalURL)
    $('#swagger-ui-iframe').attr('src', finalURL)
  }
})

</script>
