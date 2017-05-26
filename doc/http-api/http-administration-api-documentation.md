# MongooseIM's HTTP Administration API

## Configuration

Commands used by the REST API are provided by modules:

`mod_commands` - provides general purpose commands: both user-like (f.e. sending a message and retrieving messages from the archive) and administration-like (f.e. create/delete a user and change the password)

`mod_muc_commands` - commands related to Multi-user Chat rooms: create a room, invite users, send a message etc.

`mod_muc_light_commands` - same but for rooms based on the muc-light protocol.

To activate those commands, put modules you need into the ejabberd.cfg file:

```
  {mod_commands, []},
  {mod_muc_commands, []},
  {mod_muc_light_commands, []},

```
## API

Find the beautiful Swagger documentation below or under [this link](http://mongooseim.readthedocs.io/en/latest/swagger/index.html)

<iframe src="http://mongooseim.readthedocs.io/en/latest/swagger/index.html"
height="800" width="800" style="margin-left: -45px;" id="swagger-ui-iframe"></iframe>

<script>

$(document).ready(function() {
  if (window.location.host.match("readthedocs")){
    path = window.location.pathname.match("(.*)/http-api/http-administration-api-documentation")[1]
    url = window.location.protocol + "//" + window.location.hostname
    finalURL = url + path + "/swagger/index.html"
    $('a[href$="swagger/index.html"]').attr('href', finalURL)
    $('#swagger-ui-iframe').attr('src', finalURL)
  }


})

</script>
