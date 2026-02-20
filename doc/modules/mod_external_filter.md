## Module Description
This module enables message filtering through an external service.
It uses a configured HTTP pool to manage outgoing connections.

The external service has to be a GraphQL server with the schema
described in the next section.

### GraphQL server schema
```gql
type RootMutation {
    verify(
        messageBody: String!
        messageId: String!
        rawMessage: String
        receiver: String!
        sender: String!
    ): VerificationResult
}

type VerificationResult {
    action: Action!
}

enum Action {
    # The message can be routed
    ALLOW
    # The message should not be routed
    BLOCK
}
```

## Options

### `modules.mod_external_filter.pool_tag`
* **Syntax:** string
* **Default:** no default, this option is mandatory
* **Example:** `pool_tag = "graphql_filter"`

An http pool tag that is configured in [outgoing connections](../configuration/outgoing-connections.md#) section. E.g.
```
[outgoing_pools.http.filter_service]
  scope = "host_type"
  workers = 30

  [outgoing_pools.http.filter_service.connection]
    host = "http://localhost:8080"
    path_prefix = "/api"
```

## Example configuration
```toml
[modules.mod_external_filter]
  pool_tag = "graphql_filter"
```