# MongooseIM's GraphQL API for the user

The new GraphQL user API contains all commands from the client REST API and provides plenty of new ones. Multiple commands previously available only for the admin have their counterparts for the user.

**Queries** and **mutations** can be executed with the POST or GET method, as specified in the [GraphQL documentation](https://graphql.org/learn/serving-over-http/). The endpoint URL is as configured in the Listen section, e.g. `http://localhost:5561/api/graphql`.

**Subscriptions** can be executed with the GET method, and are handled with [Server-Sent Events (SSE)](https://html.spec.whatwg.org/multipage/server-sent-events.html). The endpoint URL is the same as for regular queries with the addition of `/sse`, e.g. `http://localhost:5561/api/graphql/sse`.

## Authentication

MongooseIM uses *Basic Authentication* as the authentication method for the GraphQL API.

*Basic authentication* is a simple authentication scheme built into the HTTP protocol.
Each HTTP request to the client REST API has to contain the Authorization header
with the word `Basic` followed by a space and a base64-encoded string
`username@host:password`, where:

- `username@host` is the user's *bare JID*,
- `password` is the password used to register the user's account.

For example, to authorize as `alice@localhost` with the password `secret`, the
client would send a header:

```
Authorization: Basic YWxpY2VAbG9jYWxob3N0OnNlY3JldA==
```

## GraphiQL

GraphiQL is the GraphQL integrated development environment (IDE). It allows to experiment with API and run queries with ease. The GraphiQL page is automatically served with each GraphQL endpoint. For example:

`http://localhost:5561/api/graphql`

Open the above address in your browser and try to use it.

### Authorization

Executing some of the queries requires authorization. Just add the following JSON into the header tab. Remember to update the credentials.

```json
{
   "Authorization": "Basic YWxpY2VAbG9jYWxob3N0OnNlY3JldA=="
}
```

## Static documentation

<a style="float: right; padding: 5px" href="../user-graphql-doc.html" target="_blank" rel="noopener noreferrer">Open GraphQL documentation as a full page</a>

<iframe src="../user-graphql-doc.html"
height="800" width="800" style="border: 1px solid black;"></iframe>
