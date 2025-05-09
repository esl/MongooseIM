The `shaper` section specifies **traffic shapers** used to limit the incoming XMPP traffic, providing a safety valve to protect the server. It can be used to prevent DoS attacks or to calm down too noisy clients.

* **Syntax:** each shaper is specified in a subsection starting with `[shaper.name]` where `name` is used to uniquely identify the shaper.
* **Default:** no default - each shaper needs to be specified explicitly.
* **Example:** the `normal` shaper is used for the C2S connections.

```toml
[shaper.normal]
  max_rate = 1000
```

## Traffic shaper options

### `shaper.maxrate`
* **Syntax:** positive integer
* **Default:** no default, this option is mandatory
* **Example:** `maxrate = 1000`

Defines the maximum accepted rate. For the shapers used by XMPP listeners this is the number of bytes per second, but there are shapers that use different units, e.g. [MAM shapers](#mam-shapers).

## Examples

The following examples show the typical shaper definitions.

### C2S Shaper

This is the typical definition of an XMPP shaper, which accepts the maximum data rate of 1 kbps. When the rate is exceeded, the receiver pauses before processing the next packet.

```toml
[shaper.normal]
  max_rate = 1000
```

To make use of it, the C2S listener has to be configured to use the defined shaper - see the [C2S Example](../listeners/listen-c2s.md#c2s-listener-configuration-example).

### S2S Shaper

For S2S connections we need to increase the limit as they receive the accumulated traffic from multiple users - e.g. to 50 kbps:

```toml
[shaper.fast]
  max_rate = 50_000
```

To make use of it, the S2S listener has to be configured to use the defined shaper - see the [S2S Example](../listeners/listen-s2s.md#s2s-listener-configuration-example).

### MAM Shapers

These shapers limit the number of MAM operations per second (rather than bytes per second).

```toml
[shaper.mam_shaper]
  max_rate = 1

[shaper.mam_global_shaper]
  max_rate = 1000
```

To make use of them, the [corresponding rules](access.md#mam-shapers) should be defined in the `access` section.
