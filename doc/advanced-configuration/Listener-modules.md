### HTTP module: `mod_cowboy`

This module provides an additional routing layer on top of HTTP(s) or WS(S) protocols.
It allows other HTTP/WS modules to coexist under the same URL on the single port.
Packets are forwarded to them based on the protocol.
This mechanism is transparent to actual handlers so the path sharing does not require any additional code.

Example: If you wish, you can use BOSH and WS XMPP handlers (mod_bosh, mod_websockets) on a single port and a URL without any code modifications.


Here's an example of its configuration (added to ejabberd_cowboy modules list described above):

```Erlang
{"_", "/[...]", mod_cowboy, [{http, mod_revproxy,
                               [{timeout, 5000},
                                % time limit for upstream to respond
                                {body_length, 8000000},
                                % maximum body size (may be infinity)
                                {custom_headers, [{<<"header">>,<<"value">>}]}
                                % list of extra headers that are send to upstream
                               ]},
                               {ws, xmpp, mod_websockets}
                              ]},
```

According to this configuration, all HTTP requests will go through the `mod_revproxy` module (see [mod_revproxy](../modules/mod_revproxy.md) for more details).
As for now, all WebSocket connections with the `Sec-WebSocket-Protocol: xmpp` header, will go through the mod_websockets connection.
This is the MongooseIM's regular websocket connection handler.
