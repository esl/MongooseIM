## Module description

This module is a backend of [mod_event_pusher] that enables forwarding certain events (messages, presence, etc.) via HTTP to external services such as push (by mobile, email or SMS), big data, or analytics services.

### How it works

The module hooks on all packets sent by connected users.
When the hook is triggered, the module:

* runs a callback module's `should_make_req/3` function to see if a notification should be sent
* sends a POST request composed of `{Host::binary(), Sender::binary(), Receiver::binary(), Message::binary()}` to the http notification server

### Callback module

To find out what should be sent to the HTTP server, MongooseIM calls `Mod:should_make_req(Packet::xmlel(), From::jid(), To::jid())`.
By default it uses the function in `mod_event_pusher_http` itself, which ships all non-empty chat messages and nothing else. The module can be substituted here, the method should return `true | false`.

## Prerequisites

This module uses a connection pool created by mongoose_http_client. It must be defined in the `http_connections` settings.

## Options

* `pool_name`: name of the pool to use (as defined in http_connections)
* `path`: path part of an URL to which a request should be sent (will be appended to the pool's prefix path).
* `callback_module`: name of a module which should be used to check whether a notification should be sent.

## Example configuration

`{http_connections, [{http_pool, [{server, "http://localhost:8000"},
                             {pool_size, 50}, {path_prefix, "/webservice"}]}
                   ]}.`

```erlang
{mod_event_pusher, [
    {backends, [
        {http, [
            {pool_name, http_pool},
            {path, "/notifications"}
        ]}
    ]}
]}
```

Notifications will be POSTed to `http://localhost:8000/webservice/notifications`.

## Payload format
The HTTP event pusher sends a POST request with Content-Type `application/x-www-form-urlencoded`. The form has the following fields:
* `author`: username of the user who authored the message
* `server`: name of the server from where the message originates
* `receiver`: username of the user who the message is for
* `message`: content of `<body>` element of the message

The contents of the author, server and receiver fields are processed by `stringprep`. As a result, these values are all lower case.

### Example
Below is an example of what the body of an HTTP POST request can look like:
```bash
"author=alice&server=localhost&receiver=bob&message=Hi, Bob!"
```

## Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md** page.

> **Warning:** the metrics' names may change once the deprecated `mod_http_notification` is removed from MongooseIM.

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[Host, mod_http_notifications, sent]` | spiral | An HTTP notification is sent successfully. |
| `[Host, mod_http_notifications, failed]` | spiral | An HTTP notification failed. |
| `[Host, mod_http_notifications, response_time]` | histogram | Does not include timings of failed requests. |

[mod_event_pusher]: ./mod_event_pusher.md
