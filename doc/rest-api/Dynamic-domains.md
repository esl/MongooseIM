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

## Services

### Add domain

```bash
curl -v -X PUT "http://localhost:8088/api/domains/example.db" \
    --user admin:secret \
    -H 'content-type: application/json' \
    -d '{"host_type": "type1"}'
```

Result codes:

* 204 - inserted.
* 409 - domain already exists with a different host type.
* 403 - DB service disabled.
* 403 - unknown host type.
* 500 - other errors.

Example of the result body with a failure reason:

```
{"what":"unknown host type"}
```

Check the `src/domain/mongoose_domain_handler.erl` file for the exact values of the `what` field if needed.


### Delete domain

You must provide the domain's host type inside the body:

```bash
curl -v -X DELETE "http://localhost:8088/api/domains/example.db" \
    --user admin:secret \
    -H 'content-type: application/json' \
    -d '{"host_type": "type1"}'
```

Result codes:

* 204 - the domain is removed or not found.
* 403 - the domain is static.
* 403 - the DB service is disabled.
* 403 - the host type is wrong (does not match the host type in the database).
* 403 - the host type is unknown.
* 500 - other errors.


### Enable/disable domain

Provide `{"enabled": true}` as a body to enable a domain.
Provide `{"enabled": false}` as a body to disable a domain.

```bash
curl -v -X PATCH "http://localhost:8088/api/domains/example.db" \
    --user admin:secret \
    -H 'content-type: application/json' \
    -d '{"enabled": true}'
```

Result codes:

* 204 - updated.
* 404 - domain not found;
* 403 - domain is static;
* 403 - service disabled.
