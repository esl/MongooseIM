# Module description

This module enables forwarding certain events (messages, presence etc) to an
external service via http.

# How it works

The module registers a `user_send_packet` hook - every time a user sends anything
the hook is triggered, the module does the following:

* checks whether http_notification is enabled (the `host` param is set)
* runs a callback module's `should_make_req/3` function to see if a notification should be sent
* asks poolboy for a worker process (this is to limit a number of concurrent requests)
* sends a POST request composed of {Host::binary(), Sender::binary(), Receiver::binary(), Message::binary()} to the http notification serwer

# Configuration

## Sample config

`  {mod_http_notification, [{pool_size, 5}, {host, "http://localhost:8000"}]}`

## Config params explained

* `host` - rather self-explanatory, this is where the http notification server should listen
* `prefix_path` - path part of url to which a request should be sent
* `callback_module` - name of a module which should be used to check whether a
notification should be sent
* `pool_size` - size of a worker pool, imposes a limit of the number of concurrent requests
to http server; default is 100
* `pool_timeout` - how long to wait for an available worker; default is 200ms (has to be
very short so that we don't run into problems if there is a message flood)
* `worker_timeout` - how much time do we give http server to process requests before we
bail out; default is 5000ms

## Callback module
To find out what should be sent to http server, MongooseIM calls `Mod:should_make_req(Packet::xmlel(), From::jid(), To::jid())`.
Default is to use function in mod_http_notification itself, which ships all non-empty chat messages
and nothing else. The module can be substituted here, the method should return `true | false`.
