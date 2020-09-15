### Module Description
Implements [XEP-0215: External Service Discovery](http://xmpp.org/extensions/xep-0215.html) for discovering information about services external to the XMPP network.
The main use-case is to help discover STUN/TURN servers to allow for negotiating media exchanges.

### Options
* **type** (atom, default: unset) - service type

#### Service Options
* **host** (string, required, default: unset): Hostname or an IP address where the service is hosted.
* **port** (string, recommended, default: unset): The communications port to be used at the host.
* **transport** (string, optional, default: unset): The underlying transport protocol to be used when communicating with the service.
     * **Valid values:** `udp, tcp`
* **username** (string, optional, default: unset): A service-generated username for use at the service.
* **password** (string, optional, default: unset): A service-generated password for use at the service.


### Example Configuration

```Erlang
{mod_extdisco, [
    {stun, [
        {host, "127.0.0.1"},
        {port, "3478"},
        {transport, "udp"},
        {username, "username"},
        {password, "secret"}
    ]},
    {turn, [
        {host, "hostname"},
        {port, "3478"},
        {transport, "tcp"},
    ]}
]}.
```
