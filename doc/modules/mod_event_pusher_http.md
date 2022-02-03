## Module description

This module is a backend of [mod_event_pusher] that enables forwarding certain events (messages, presence, etc.) via HTTP to external services such as push (by mobile, email or SMS), big data, or analytics services.

### How it works

The module hooks on all packets sent by connected users.
When the hook is triggered, the module:

* runs a callback module's `should_make_req/6` function to see if a notification should be sent
* runs a callback module's `prepare_headers/7` to get http headers to be used
* runs a callback module's `prepare_body/7`
* sends a POST request composed of `{Host::binary(), Sender::binary(), Receiver::binary(), Message::binary()}` to the http notification server

You can make multiple configuration entries for this backend to handle more complicated pushing scenarios (e.g. sending various types of messages to different backends).

### Callback module


To find out what and how to send MongooseIM calls the following callback module's functions:
`Mod:should_make_req(Acc::mongoose_acc:t(), Dir::in|out, Packet::xmlel(), From::jid(), To::jid(), Opts :: [{atom(), term()}])`.

`Mod:prepare_headers(Acc::mongoose_acc:t(), Dir::in|out, Host::jid:lserver(), Message::binary(), Sender::jid:luser(), Receiver::luser(), Opts :: [{atom(), term()}])`.

`Mod:prepare_body(Acc::mongoose_acc:t(), Dir::in|out, Host::jid:lserver(), Message::binary(), Sender::jid:luser(), Receiver::luser(), Opts :: [{atom(), term()}])`.

By default it uses the function in `mod_event_pusher_http` itself, which ships all non-empty chat messages.

## Prerequisites

This module uses a connection pool created by mongoose_http_client.
It must be defined in the [`outgoing_pools` settings](../configuration/outgoing-connections.md#http-options).

## Options

### `modules.mod_event_pusher_http.pool_name`
* **Syntax:** non-empty string
* **Default:** `"http_pool"`
* **Example:** `pool_name = "http_pool"`

Name of the pool to use (as defined in outgoing_pools).

### `modules.mod_event_pusher_http.path`
* **Syntax:** string
* **Default:** `""`
* **Example:** `path = "/notifications"`

Path part of an URL to which a request should be sent (will be appended to the pool's prefix path).

### `modules.mod_event_pusher_http.callback_module`
* **Syntax:** string
* **Default:** `"mod_event_pusher_http_defaults"`
* **Example:** `callback_module = "mod_event_pusher_http_notifications"`

Name of a module which should be used to check whether a notification should be sent.

## Example configuration

```toml
[outgoing_pools.http.http_pool]
  scope = "global"
  workers = 50

  [outgoing_pools.http.http_pool.connection]
    host = "https://localhost:8000"
    path_prefix = "/webservice"
    request_timeout = 2000

[modules.mod_event_pusher.backend.http]
  pool_name = "http_pool"
  path = "/notifications"
```

Notifications will be POSTed to `http://localhost:8000/webservice/notifications`.

```toml
[[modules.mod_event_pusher.backend.http]]
  pool_name = "http_pool"
  path = "/notifications"
  callback_module = "mod_event_pusher_http_notifications"

[[modules.mod_event_pusher.backend.http]]
  pool_name = "http_pool"
  path = "/alerts"
  callback_module = "mod_event_pusher_http_alerts"
```

Here, some notifications will be POSTed to `http://localhost:8000/webservice/notifications` and some to `http://localhost:8000/webservice/alerts`, depending on implementation of `should_make_req/6` in the two callback modules.

## Default payload format
The default HTTP event pusher sends a POST request with Content-Type `application/x-www-form-urlencoded`. The form has the following fields:

* `author`: username of the user who authored the message
* `server`: name of the server from where the message originates
* `receiver`: username of the user who the message is for
* `message`: content of `<body>` element of the message

The contents of the author, server and receiver fields are processed by `stringprep`.
As a result, these values are all lower case.

### Example
Below is an example of what the body of an HTTP POST request can look like:
```bash
"author=alice&server=localhost&receiver=bob&message=Hi, Bob!"
```

## Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/MongooseIM-metrics.md) page.

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[Host, mod_event_pusher_http, sent]` | spiral | An HTTP notification is sent successfully. |
| `[Host, mod_event_pusher_http, failed]` | spiral | An HTTP notification failed. |
| `[Host, mod_event_pusher_http, response_time]` | histogram | Does not include timings of failed requests. |

[mod_event_pusher]: ./mod_event_pusher.md
