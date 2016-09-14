# MongooseIM's HTTP Administration API

## Prerequisites

Before you can use the HTTP Administration API, you must enable
`mod_mongoose_admin` and `mod_muc_admin` in the `modules` section of your
`ejabberd.cfg` file.

## API 

Find the beautiful Swagger documentation below or
under [this link](http://mongooseim.readthedocs.io/en/latest/swagger/index.html)

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
