# MongooseIM's REST API for backend administration

## Configuration

To enable the commands, you need to hook the `mongoose_admin_api` module to an HTTP endpoint as described
in the [admin REST API handlers configuration](../configuration/listen.md#handler-types-rest-api-admin-mongoose_admin_api)
section of the [listeners](../configuration/listen.md) documentation.

## OpenAPI specifications

Read the [Swagger documentation](https://esl.github.io/MongooseDocs/latest/swagger/index.html) for more information.

[![Swagger](https://nordicapis.com/wp-content/uploads/swagger-Top-Specification-Formats-for-REST-APIs-nordic-apis-sandoval-e1441412425742-300x170.png)](https://esl.github.io/MongooseDocs/latest/swagger/index.html)

<iframe src="https://esl.github.io/MongooseDocs/latest/swagger/index.html"
height="800" width="800" id="swagger-ui-iframe"></iframe>

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
