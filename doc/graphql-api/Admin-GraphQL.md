# MongooseIM's GraphQL API for the administrator

The new GraphQL admin API contains all the commands available through the REST API, and the vast majority of the CLI (`mongooseimctl`) commands. Only commands that wouldn't have worked well with GraphQL style have been omitted.

We can distinguish two levels of the administration. A global admin (has access to all commands), and the admin per domain (has access only to the own domain). Each of them is handled by a different endpoint. Please see the configuration [Listen](../../configuration/listen/#handler-types-graphql-api-mongoose_graphql_handler) section for more details.

There is only one schema for both admin types. Admin per domain simply has no permissions to execute global commands or commands with not owned domain. The API documentation clearly says which commands are global.

**Queries** and **mutations** can be executed with the POST or GET method, as specified in the [GraphQL documentation](https://graphql.org/learn/serving-over-http/). The endpoint URL is as configured in the Listen section, e.g. `http://localhost:5551/api/graphql` for the global admin.

**Subscriptions** can be executed with the GET method, and are handled with [Server-Sent Events (SSE)](https://html.spec.whatwg.org/multipage/server-sent-events.html). The endpoint URL is the same as for regular queries with the addition of `/sse`, e.g. `http://localhost:5551/api/graphql/sse` for the global admin.

## Domain admin configuration

Out of the box, domains are created with a disabled admin account. Admin per domain can be enabled only by the global admin with the command
<a href="../admin-graphql-doc.html#definition-DomainAdminMutation" target="_blank" rel="noopener noreferrer">mutation.domains.setDomainPassword</a>. Afterward, the domain admin can change the password with the same command.

The admin per domain can be disabled by the global admin with the command <a href="../admin-graphql-doc.html#definition-DomainAdminMutation" target="_blank" rel="noopener noreferrer">mutation.domains.removeDomainPassword</a>.

## Authentication

MongooseIM uses *Basic Authentication* as an authentication method for the GraphQL API.

*Basic authentication* is a simple authentication scheme built into the HTTP protocol.
Each HTTP request to the GraphQL API has to contain the Authorization header
with the word `Basic` followed by a space and a base64-encoded string.

### Global admin endpoint

The authentication for global admin is optional because this endpoint shouldn't be exposed outside. The credentials set in the handler section in the config enables the authentication. Please see the [GraphQL handler](../configuration/listen.md#handler-types-graphql-api-mongoose_graphql_handler) section for more details.

The base64-encoded string should have the form
`LOGIN:PASSWORD`, where:

- `LOGIN` is the login set in the config,
- `PASSWORD` is the password set in the config.

### Domain admin endpoint

The authorization as a domain admin the base64-encoded string should have the form
`admin@DOMAIN:PASSWORD`, where:

- `DOMAIN` is the domain to authorize,
- `PASSWORD` is the password for the given domain.

## GraphiQL

GraphiQL is the GraphQL integrated development environment (IDE). It allows to experiment with API and run queries with ease. The GraphiQL page is automatically served with each GraphQL endpoint. For example:

`http://localhost:5551/api/graphql`

Open the above address in your browser and try to use it.

### Authorization

Executing some of the queries requires authorization. Just add the following JSON into the header tab. Remember to update the credentials.

```json
{
   "Authorization": "Basic YWxpY2VAbG9jYWxob3N0OnNlY3JldA=="
}
```

## Static documentation

<a style="float: right; padding: 5px" href="../admin-graphql-doc.html" target="_blank" rel="noopener noreferrer">Open GraphQL documentation as a full page</a>

<iframe src="../admin-graphql-doc.html"
height="800" width="800" style="border: 1px solid black;"></iframe>
