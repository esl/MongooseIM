MongooseIM's REST API
---------------------

In addition to the regular XMPP connection methods such as TCP (with TLS/STARTTLS),
WebSockets and BOSH, MongooseIM provides parts of its functionality over REST API.

### Assumptions

1. Every request has to be authenticated.
1. User registration has to be done via other methods (f.e using the
[REST API for backend services](http-api/http-administration-api-documentation.md)).
1. Relevant endpoint has to be configured on the server side.
1. List of provided actions is documented in Swagger.
See the documentation at the bottom of this page or under
[this link](http://mongooseim.readthedocs.io/en/latest/swagger/index.html?client=true)



### Configuration

TODO add endpoint configuration

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


