# Module description

This module enables forwarding certain events (messages, presence, etc.) to an
external service via HTTP, for example push (by mobile, email or SMS), big data,
or analytics services.

## How it works

The module registers a `user_send_packet` hook every time a user sends anything
the hook is triggered, the module does the following:

* checks whether http_notification is enabled (the `host` param is set)
* runs a callback module's `should_make_req/3` function to see if a notification should be sent
* sends a POST request composed of `{Host::binary(), Sender::binary(), Receiver::binary(), Message::binary()}` to the http notification server

## Callback module

To find out what should be sent to the HTTP server, MongooseIM calls `Mod:should_make_req(Packet::xmlel(), From::jid(), To::jid())`.
Default is to use function in mod_http_notification itself, which ships all non-empty chat messages
and nothing else. The module can be substituted here, the method should return `true | false`.

# Prerequisites

This module uses a connection pool created by mongoose_http_client. It must be defined
in `http_connections` setting.

# Options

* `pool_name`: name of the pool to use (as defined in http_connections)
* `path`: path part of URL to which a request should be sent (will be appended to the pool's prefix path).
* `callback_module`: name of a module which should be used to check whether a
notification should be sent.

# Example configuration

`{http_connections, [{http_pool, [{server, "http://localhost:8000"},
                             {pool_size, 50}, {path_prefix, "/webservice"}]}
                   ]}.`

  `{mod_http_notification, [{pool_name, http_pool}, {path, "/notifications"}]}`

Notifications will be POSTed to `http://localhost:8000/webservice/notifications`.
