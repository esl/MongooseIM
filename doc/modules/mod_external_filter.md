## Module Description
This module allows to do filtering of messages based on external service.
It uses a configured outgoing http pool to handle the connection.

The external service has to be a GraphQL server with the following schema:

### Root query
```
verify(
    messageBody: String!
    messageId: String!
    rawMessage: String
    receiver: String!
    sender: String!
): VerificationResult
```

### VerificationResult
```
VerificationResult {
    action: ActionEnum
}
```

### ActionEnum

| Enum value | Description |
| ---------- | ----------- |
| ALLOW | The message can be routed |
| BLOCK | The message should not be routed |

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