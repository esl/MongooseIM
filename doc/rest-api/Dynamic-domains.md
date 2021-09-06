# MongooseIM's REST API for dynamic domain management

Provides API for adding/removing and enabling/disabling domains over HTTP.
Implemented by `mongoose_domain_handler` module.

## Configuration

`mongoose_domain_handler` has to be configured as shown in the [REST API configuration example](../advanced-configuration/listen.md#example-4-domain-api)
to enable the REST API.

For details about possible configuration parameters please see the relevant
documentation of the [listeners](../advanced-configuration/listen.md),
in particular the [`mongoose_domain_handler`](../advanced-configuration/listen.md#handler-types-rest-api---domain-management---mongoose_domain_handler)
section.

## OpenAPI specifications

Read our [Swagger documentation](https://esl.github.io/MongooseDocs/latest/swagger/index.html?domains=true) for more information.

[![Swagger](https://nordicapis.com/wp-content/uploads/swagger-Top-Specification-Formats-for-REST-APIs-nordic-apis-sandoval-e1441412425742-300x170.png)](https://esl.github.io/MongooseDocs/latest/swagger/index.html?domains=true)

<iframe src="https://esl.github.io/MongooseDocs/latest/swagger/index.html?domains=true"
height="800" width="800" id="swagger-ui-iframe"></iframe>

<script>

$(document).ready(function() {
  if (window.location.host.match("github")){
    path = window.location.pathname.match("(.*)/rest-api/Dynamic-domains")[1]
    url = window.location.protocol + "//" + window.location.hostname
    finalURL = url + path + "/swagger/index.html?domains=true"
    $('a[href$="swagger/index.html?domains=true"]').attr('href', finalURL)
    $('#swagger-ui-iframe').attr('src', finalURL)
  }
})

</script>
