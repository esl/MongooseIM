# STUN / TURN Server Configuration

## NAT and the Role of STUN/TURN servers

Most devices on the internet operate behind Network Address Translation (NAT). NAT allows multiple devices within a private network (for example home or corporate networks) to share a single public IP address.

While NAT conserves public IPs, it introduces a limitation: devices behind NAT typically cannot accept incoming connections directly. This makes establishing peer-to-peer (P2P) connections more difficult.

Two protocols are commonly used to solve this problem:

* STUN (Session Traversal Utilities for NAT) allows a client to discover its public IP address and NAT behavior. With this information, two peers can often establish a direct P2P connection.
* TURN (Traversal Using Relays around NAT) serves as a fallback mechanism. When a direct P2P connection cannot be established due to NAT or firewall restrictions, TURN relays traffic through a server.

## How Often TURN Is Required

Typical connection statistics vary depending on network environments:

* 50-90% of connections succeed via direct P2P using STUN
* 10-50% require TURN relay due to NAT or firewall restrictions

Since TURN relays the entire media stream, it is bandwidth heavy and should be used only as a fallback.

## Recommended Deployment Strategy

STUN/TURN servers have networking requirements that make them less suitable for Kubernetes deployments.

Key reasons:

* STUN/TURN servers require a public IP address
* TURN servers need a large range of UDP ports
* Kubernetes networking and load balancers complicate exposing large port ranges

For these reasons, the recommended approach is to deploy the TURN server on a dedicated virtual machine with a public IP address.

If you run the server using `Docker`, ensure the container uses host networking (`--network=host`).

This avoids port mapping overhead and ensures the TURN server can use the full relay port range.

!!! Warning
    TURN servers need a large number of file descriptors to operate properly.

    Each TURN allocation consumes at least one file descriptor, often more. If the limit is set too low, the server may be unable to create new allocations and could produce errors like “Too many open files.”

    In production environments, it’s recommended to raise the file descriptor limit (ulimit -n) for the TURN service and verify that system-wide limits can handle the expected number of concurrent connections.

## Which STUN/TURN server to choose?

The most widely used implementation is Coturn.

Coturn is an open-source STUN/TURN server that:

* Fully implements STUN and TURN protocols
* Is actively maintained
* Supports multiple authentication mechanisms
* Is widely adopted in WebRTC infrastructures

Because of its stability, performance, and strong community adoption, Coturn is typically the recommended choice.

## Important Coturn Configuration Options

Below are several key options commonly used when configuring a Coturn server.

* `--use-auth-secret`

    Enables TURN REST API authentication using a shared secret.

    This allows credentials to be generated dynamically by an application server instead of configuring users directly on the TURN server.

* `--static-auth-secret`

    Defines the shared secret used to generate temporary TURN credentials.

    Your application backend uses this same secret to generate usernames and passwords provided to clients.

* `--lt-cred-mech`

    Enables long-term credential authentication, which is the standard authentication mechanism defined in the TURN specification.

* `--user`

    Defines a static username/password pair for the long-term credential authentication method.

    This method stores credentials directly on the TURN server and is mainly used for:

    * testing
    * small deployments
    * debugging

    In production environments, TURN REST API authentication is generally preferred.

* `--realm`

    The realm identifies the authentication domain used by the TURN server.

    It is used during credential generation and validation. Typically, this value matches your application's domain.

* `--listening-ip`

    Specifies the IP address that the TURN server should use for incoming connections.

    By default, Coturn listens on all available network interfaces. Using `--listening-ip` restricts the server to a specific interface.

    Related option: `--relay-ip`

* `--external-ip`

    Specifies the public IP address of the TURN server.

    This option is required when the server is behind NAT or running in cloud infrastructure.

    !!!Warning
        When setting `--external-ip`, use the `external_ip/internal_ip` format, or explicitly whitelist the internal relay IP(s) using the `--allowed-peer-ip` option. Otherwise, the server may fail in double-relay scenarios (ClientA <-> TURN <-> TURN <-> ClientB). For more details, see [this issue](https://github.com/coturn/coturn/issues/1407#issuecomment-4079033944)

* `--listening-port`

    Defines the primary port for TCP and UDP STUN/TURN connections (default is 3478).

    Related option: `--alt-listening-port`

*  `--tls-listening-port`

    Defines the primary port for TLS and DTLS TURN connections (default is 5349).

    Media streams are normally protected using Secure Real-time Transport Protocol (SRTP), so enabling TLS for TURN does not usually provide additional media security. The main purpose of TLS support in TURN is to encapsulate traffic within TLS, allowing it to pass through restrictive firewalls. For this reason, deployments often configure the TLS listening port to 443, which is commonly allowed in most networks.

    Related option: `--alt-tls-listening-port`, `--cert`, `--pkey`, `--no-tls`, `--no-dtls`


* `--alternate-server`

    Defines an alternative TURN server that clients can use.

    This option can be used for load balancing across multiple TURN servers.

    Related option: `--tls-alternate-server`

* `--verbose`, `--Verbose` and `--log-file`

    Options to configure logging

* `--denied-peer-ip=<IPaddr[-IPaddr]>` and `--allowed-peer-ip=<IPaddr[-IPaddr]>`

    Options to allow or deny specific IP addresses or IP ranges. These settings can be used to prevent the TURN server from relaying TCP connections or UDP packets to hosts within internal or otherwise restricted networks.

    Starting with Coturn version 4.5.2, peering with 127.0.0.1 or ::1 is disabled by default. In some older versions, this behavior can be enforced using the `--no-loopback-peers` option.

    For versions prior to 4.9.0, an additional workaround is required for CVE-2026-27624:

    ```
    denied-peer-ip=::ffff:0.0.0.0-::ffff:255.255.255.255
    ```

    For more information, visit:

    * https://www.enablesecurity.com/blog/cve-2020-26262-bypass-of-coturns-access-control-protection/
    * https://www.enablesecurity.com/blog/slack-webrtc-turn-compromise-and-bug-bounty/
    * https://github.com/coturn/coturn/security/advisories/GHSA-6g6j-r9rf-cm7p
    * https://github.com/coturn/coturn/security/advisories/GHSA-j8mm-mpf8-gvjg

    Related option:  `--no-multicast-peers`

* `--min-port` and `--max-port`

    These options define the UDP relay port range used for relayed TURN traffic.

    Default range is 49152-65535 (according to RFC 5766).
    A larger port range allows the server to support more concurrent relay sessions.
    In most cases, the default range should not be modified.

* `--max-bps`

    Limits the maximum bandwidth per TURN session.

    This can be used to prevent abuse and control bandwidth consumption.

    Related option: `--bps-capacity`

* `--user-quota` and `--total-quota`

    These options limit the number of concurrent TURN allocations.

    Note that users are ephemeral when using TURN REST API authentication, which affects the meaning of the `--user-quota` limit.

* `--max-allocate-lifetime`

    Sets the maximum value for the allocation lifetime. Default is 3600 seconds.

* `--max-allocate-timeout`

    Sets the maximum time allowed for full allocation establishment. Default is 60 seconds.

* `--no-rfc5780`

    Disables RFC5780 (NAT behavior discovery) support.

    NAT behavior discovery is not required for Interactive Connectivity Establishment (ICE).

* `--no-cli`

    Turns off the CLI support. The CLI is not needed for a static configuration, so it can be disabled.

* `--prometheus`

    Enables prometheus metrics.

    Related option: `--prometheus-port`

## Authentication mechanisms

Coturn supports two main authentication methods.

#### TURN REST API Authentication (Recommended)

This is the preferred method for modern WebRTC deployments.

Instead of storing users on the TURN server:

* The application server generates temporary TURN credentials.
* The username contains an expiration timestamp.
* The password is generated using HMAC with a shared secret.
* The TURN server verifies the credentials using the same secret.

This approach has the following advantages:

* No user database on the TURN server
* Credentials expire automatically
* Easy integration with backend services

### Classic long-term credentials

This older method stores static usernames and passwords directly on the TURN server.

While simpler, it has some drawbacks:

* credentials must be managed manually
* harder to rotate securely
* less flexible than TURN REST API authentication

For these reasons, TURN REST API authentication is typically recommended.

## Example Coturn Configuration

Below is a sample configuration that uses TURN REST API authentication with a static shared secret:

```ini
realm=example.com

## Local IP address
listening-ip=192.168.0.1
relay-ip=192.168.0.1

## Public IP address associated with this server (e.g. AWS Elastic IP)
external-ip=1.2.3.4/192.168.0.1

## Authentication (TURN REST API)
use-auth-secret
static-auth-secret="put_your_shared_secret_here"

## if you're wondering what those IP ranges are, check these wiki pages:
##    https://en.wikipedia.org/wiki/IPv4#Special-use_addresses
##    https://en.wikipedia.org/wiki/IPv6_address#Special_addresses
no-multicast-peers
denied-peer-ip=0.0.0.0-0.255.255.255
denied-peer-ip=10.0.0.0-10.255.255.255
denied-peer-ip=100.64.0.0-100.127.255.255
denied-peer-ip=127.0.0.0-127.255.255.255
denied-peer-ip=169.254.0.0-169.254.255.255
denied-peer-ip=172.16.0.0-172.31.255.255
denied-peer-ip=192.0.0.0-192.0.0.255
denied-peer-ip=192.0.2.0-192.0.2.255
denied-peer-ip=192.88.99.0-192.88.99.255
denied-peer-ip=192.168.0.0-192.168.255.255
denied-peer-ip=198.18.0.0-198.19.255.255
denied-peer-ip=198.51.100.0-198.51.100.255
denied-peer-ip=203.0.113.0-203.0.113.255
denied-peer-ip=240.0.0.0-255.255.255.255
denied-peer-ip=::1
denied-peer-ip=64:ff9b::-64:ff9b::ffff:ffff
denied-peer-ip=::ffff:0.0.0.0-::ffff:255.255.255.255
denied-peer-ip=100::-100::ffff:ffff:ffff:ffff
denied-peer-ip=2001::-2001:1ff:ffff:ffff:ffff:ffff:ffff:ffff
denied-peer-ip=2002::-2002:ffff:ffff:ffff:ffff:ffff:ffff:ffff
denied-peer-ip=fc00::-fdff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
denied-peer-ip=fe80::-febf:ffff:ffff:ffff:ffff:ffff:ffff:ffff

no-rfc5780
no-cli

## Bandwidth limit per session - 5 Mbps
max-bps=5000000

## disable TLS/DTLS
no-tls
no-dtls

## Prometheus metrics on port 9641
prometheus
```

This configuration can be quickly deployed using a Docker-based approach:

```bash
mkdir --parents ~/coturn/logs
chmod a+w ~/coturn/logs/
vi ~/coturn/turnserver.conf

docker run -d --restart=always --name=coturn \
           --network=host --ulimit nofile=1000000 \
           -v "${HOME}/coturn/turnserver.conf:/turnserver.conf:ro" \
           -v "${HOME}/coturn/logs:/var/log" \
           coturn/coturn:4.9.0 -c /turnserver.conf
```

## Testing the STUN/TURN Server

Testing a STUN/TURN server can be complicated.

### Generating Temporary Credentials

If you are using TURN REST API authentication, you can generate time-limited credentials with `openssl`:

```bash
secret="put_your_shared_secret_here"

## credentials will expire in 1 hour (3600 seconds)
expiration_timestamp="$(($(date "+%s") + 3600))"

username="${expiration_timestamp}:dummy_user"
password="$(echo -n "$username" | openssl dgst -hmac "$secret" -sha1 -binary | openssl base64)"

echo -e "username: '${username}'"
echo -e "password: '${password}'"

```

### Using the Trickle ICE WebRTC Testing Tool

A simple way to validate your setup is with the Trickle ICE WebRTC testing tool, which checks STUN and TURN connectivity directly from your browser.

Steps:

* Open the test page: https://webrtc.github.io/samples/src/content/peerconnection/trickle-ice/
* Enter your STUN/TURN server details
* Start ICE candidate gathering
* Confirm that both server reflexive and relay candidates are returned

If you see server reflexive and relay candidates, your TURN credentials and listeners are configured correctly.

## Testing Traffic Relaying

Verifying candidate gathering alone does not guarantee that traffic can be forwarded through the TURN server. A dedicated test is required to ensure relaying works properly.

One approach is to use the browser based test suite available at [test.8x8.vc], specifically the Connectivity section, which attempts to establish real media paths through the TURN server.

Alternatively, you can use the `turnutils_uclient` command-line tool to simulate client behavior and validate relaying:

```bash
turnutils_uclient -y -n 30 -u <username> -w <password> <TURN-server-IP-address>
```

This tool is bundled with Coturn, so you don’t need to install it separately. You can run it directly from the official Docker image:

```bash
docker run -it --rm --entrypoint /bin/bash coturn/coturn
```

## Configuring MongooseIM

To provision XMPP clients with STUN/TURN credentials, enable and configure the [mod_extdisco] module as shown below:

```toml
[modules.mod_extdisco]

  [[modules.mod_extdisco.service]]
    type = "stun"
    host = "1.2.3.4"
    port = 3478
    transport = "udp"

  [[modules.mod_extdisco.service]]
    type = "turn"
    host = "1.2.3.4"
    port = 3478
    transport = "udp"
    secret = "put_your_shared_secret_here"

  [[modules.mod_extdisco.service]]
    type = "turn"
    host = "1.2.3.4"
    port = 3478
    transport = "tcp"
    secret = "put_your_shared_secret_here"
```

For verification purposes, you can generate a username and password using the `mongooseimctl` command.

```bash
mongooseimctl externalServices getServices --domain "domain.with.mod_extdisco.configured" --type turn
```

Alternatively, you can use the [externalServices] admin GraphQL mutation:

```graphql
mutation MyMutation {
  externalServices {
    getServices(domain: "domain.with.mod_extdisco.configured" , type: "turn") {
      expires
      password
      port
      transport
      username
      host
      type
    }
  }
}
```

[test.8x8.vc]: https://test.8x8.vc/
[mod_extdisco]: ../modules/mod_extdisco.md
[externalServices]: ../graphql-api/admin-graphql-doc.html#mutation-externalServices
