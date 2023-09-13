## Module Description
Implements [XEP-0215: External Service Discovery](http://xmpp.org/extensions/xep-0215.html) for discovering information about services external to the XMPP network.
The main use-case is to help discover STUN/TURN servers to allow for negotiating media exchanges.

## Options

### `modules.mod_extdisco.iqdisc.type`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** `"no_queue"`

Strategy to handle incoming IQ stanzas. For details, please refer to
[IQ processing policies](../configuration/Modules.md#iq-processing-policies).

### `modules.mod_extdisco.service`
* **Syntax:** TOML array with one table for each advertised service - see below for details.
* **Default:** `[]` - no services advertised

### Service options

Each advertised service is specified as a TOML table containing the following options listed below.

#### `modules.mod_extdisco.service.type`
* **Syntax:** string
* **Default:** none, this option is required
* **Example:** `type = "stun"`

Service type, common values are `"stun"`, `"turn"`, `"ftp"`.

#### `modules.mod_extdisco.service.host`
* **Syntax:** string
* **Default:** none, this option is required
* **Example:** `host = "192.168.0.2"`

Hostname or an IP address where the service is hosted.

#### `modules.mod_extdisco.service.port`
* **Syntax:** integer, between 0 and 65535, non-inclusive
* **Default:** none, this option is recommended
* **Example:** `port = 3478`

The communications port to be used at the host.

#### `modules.mod_extdisco.service.transport`
* **Syntax:** string, one of `"udp"`, `"tcp"`
* **Default:** none, this option is optional
* **Example:** `transport = "udp"`

The underlying transport protocol to be used when communicating with the service.

#### `modules.mod_extdisco.service.username`
* **Syntax:** string
* **Default:** none, this option is optional
* **Example:** `username = "username"`

A service-generated username for use at the service.

#### `modules.mod_extdisco.service.password`
* **Syntax:** string
* **Default:** none, this option is optional
* **Example:** `password = "password"`

A service-generated password for use at the service.

## Example Configuration

```toml
[modules.mod_extdisco]

  [[modules.mod_extdisco.service]]
    type = "stun"
    host = "127.0.0.1"
    port = 3478
    transport = "udp"
    username = "username"
    password = "password"

  [[modules.mod_extdisco.service]]
    type = "stun"
    host = "stun.host.com"
    port = 3478
    transport = "tcp"
    username = "username2"
    password = "password2"

  [[modules.mod_extdisco.service]]
    type = "turn"
    host = "turn.host.com"
```
