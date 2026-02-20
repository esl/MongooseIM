## Module Description
This module enables message filtering through an external service.
It uses a configured HTTP pool to manage outgoing connections.

The external service has to be a GraphQL server with the following schema:

### Root query
```gql
type Query {
    verify(
        messageBody: String!
        messageId: String!
        rawMessage: String
        receiver: String!
        sender: String!
    ): VerificationResult
}

type VerificationResult {
    action: ActionEnum
}

enum ActionEnum {
    # The message can be routed
    ALLOW
    # The message should not be routed
    BLOCK
}


## Options

### `modules.mod_external_filter.pool_tag`
* **Syntax:** string
* **Default:** no default, this option is mandatory
* **Example:** `pool_tag = "graphql_filter"`

An http pool tag that is configured in [outgoing connections](../configuration/outgoing-connections.md#) section.

## Example configuration
```toml
[modules.mod_external_filter]
  pool_tag = "graphql_filter"
```