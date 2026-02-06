The XMPP standard itself does not provide any means of transmitting audio/video data. However, XMPP can be used as a signaling layer to manage [RTP] sessions for audio and video between clients. The set of XEPs that defines the protocol for this is commonly referred to as the [Jingle protocol].

# Support of the Jingle protocol

There is no need for any specific support of the Jingle protocol on the XMPP server side.

Nevertheless, the successful deployment of the Jingle-based solution may require some additional server-side infrastructure:

* [STUN]/[TURN] server(s) for [NAT traversal].

* `RTP relay` for redirecting incoming [RTP] streams to all participants, excluding the sender. It can be useful for small video group calls.

* `Content mixer` for processing multiple input [RTP] streams into a single output stream, required for video conferences with a larger number of participants.

* XMPP `focus agent` for organising conference calls. All participants establish a Jingle session only with the agent and receive conference information updates from it. It can be either a custom extension module, an external XMPP component, or even a client-side functionality.

We will take a closer look at every component later in the tutorial.

# The Jingle Framework

The main XEP is [XEP-0166], which introduces the concept of multimedia sessions and the protocol for establishing, managing, and tearing them down. The negotiation of the sessions happens over XMPP, while media transfer typically takes place outside of XMPP. During the initialisation of the session, the peers must come to an agreement on the following aspects:

* The application format  - media type, what is to be transferred.

* The transport method - communication channel, how the media is transferred.

* Optionally, the peers can agree on a security precondition for a transport method.

However, neither data transport methods nor application formats are defined in this XEP, leaving that up to separate specifications.

## Jingle application formats

At the moment, there are two major application formats defined:

* The first one is [XEP-0167], which defines a protocol for Jingle [RTP] sessions.

* And [XEP-0234], application format for negotiating Jingle file transfer sessions.

In this tutorial, we will focus solely on Jingle [RTP] sessions.

## Jingle transport method

There are multiple XEPs defining various transfer methods, but only 2 of them are considered suitable for Audio/Video data:

* [XEP-0177]: Jingle Raw UDP Transport Method.

* [XEP-0176]: Jingle ICE-UDP Transport Method.

The Raw UDP transport does not provide end-to-end traversal of NAT(s), or even basic connectivity checks. If [NAT traversal] is needed, Jingle clients should use the Jingle ICE-UDP Transport Method.

# Various video/audio call scenarios

As mentioned above, depending on the call scenario, we require different types of additional server-side infrastructure. Let's take a quick look at the most common scenarios.

## One-to-one call scenario

Probably the easiest scenario to implement, which is supported by many interoperable XMPP clients. Moreover, most interoperable XMPP clients support only this kind of audio/video connection. There are several reasons for this:

* It requires little to no additional server-side infrastructure. In particular, to overcome the [NAT traversal] problem, only a [STUN]/[TURN] server is needed.

* [STUN] and [TURN] are open protocols, and the way to use a [STUN]/[TURN] server is clearly defined by the [ICE] standard and [XEP-0176] (Jingle ICE-UDP Transport Method).

* [XEP-0215] (External Service Discovery) provides an easy way to configure an XMPP client with custom [STUN]/[TURN] servers.

## Small group call scenario (mesh topology)

One method for handling calls in small groups (up to 4 participants, when video is involved) is to set up separate jingle sessions between each participant. [XEP-0272] (Multiparty Jingle, also known as MuJi) describes a protocol for this scenario, but this approach has several obvious drawbacks:

* It requires a large connection bandwidth; every client must send data N times and receive it from N sources (where N is the number of participants).

* The parties must agree on the audio/video format, as we do not want to encode the data multiple times for different participants.

* Video/audio mixing must be done by each participant individually.

![full_mesh][full_mesh]

A [STUN]/[TURN] server can be used to set up individual peer-to-peer sessions (as in a one-to-one scenario).

[XEP-0272] suggests that optimisation of the upstream bandwidth can be done with the help of `RTP relay`.

![relay_example][relay_example]

while the `content mixer` helps to reduce downstream bandwidth and CPU usage:

![mixer_example][mixer_example]

In order to ensure clients' interoperability, they must allocate `RTP relays` and `content mixers` individually:

![complex_mesh_example][complex_mesh_example]

This approach is quite inefficient. The same server components are sufficient to optimize the call for each participant, but it requires switching from a mesh to a star topology.

## Large conference call (star topology)

This type of connection requires the allocation of a `content mixer` and an `RTP relay` (in reality, it's usually just one software component) for the call. The XMPP entity that does this allocation is called the `focus agent`. All participants establish individual jingle sessions with the `focus agent` in a similar manner to a one-to-one connection.

[XEP-0298] (Delivering Conference Information to Jingle Participants, also known as COIN) and [XEP-0340] (Conferences with Lightweight Bridging, also known as COLIBRI) define certain aspects of the protocol for conference call management. It’s expected that one of the participants would become a `focus agent`.

Theoretically, the `content mixer` and `RTP relay` could also be placed on the participant (`focus agent`) device, but this configuration may exhibit poor performance.

![conference_example][conference_example]

With the introduction of the `content mixer`, we also gain some additional benefits:

* Fewer restrictions on audio/video format compatibility. E.g., Peer 4 can now provide data in a format that is not supported by Peer 2.

* Support for multiple output formats. For example, we can allow users to choose between high-resolution and low-resolution video depending on their connection bandwidth.

* Introduction of a server-side component for call recording.

From another perspective, we lose the ability to have end-to-end encryption.

# FAQ

### The listed XEPs do not provide the required functionality. What can I do?

As mentioned earlier, it is difficult to create an interoperable standard for group calling functionality. There is no single standard/protocol for the `content mixer` or `RTP relay` components, so XEPs are trying to push custom (vendor-dependent) logic on the client side. In any case, group audio/video calling is not widely supported by interoperable XMPP clients. And if interoperability is not a concern for you, then consider XEP as a point of inspiration.

### Is there an alternative to a [STUN]/[TURN] server?

Yes, there is [XEP-0278] (Jingle relay nodes). However, it hasn’t gained much popularity.

It is also worth noting that [TURN] Server is a more versatile solution.

### In regard to the `focus agent`, what does the XMPP entity mean?

There is no strict requirement for the `focus agent` to be a call participant; it can also be implemented as an extension to an XMPP server or as an external XMPP component. Importantly, it must have a valid JID so that participants can interact with it.

### Do I need a [STUN]/[TURN] server if I use a `content mixer` and/or `RTP relay` with a public IP address?

It is still recommended to have a [STUN]/[TURN] server in your setup. Clients can use it as a fallback connection option, not necessarily for [NAT traversal], but to overcome strict firewall settings.

### Given that [TURN] is a relay server, why is a separate `RTP relay` server needed?

Note that a [TURN] server is not designed for multicasting of the outgoing data stream; if you want to send the same data to N peers, you still have to send it N times from the client to the [TURN] server.

### For more information about [STUN]/[TURN] servers, see the [dedicated page][TURN tutorial].

[complex_mesh_example]: jingle/complex_mesh_example.png
[conference_example]: jingle/conference_example.png
[full_mesh]: jingle/full_mesh.png
[mixer_example]: jingle/mixer_example.png
[relay_example]: jingle/relay_example.png
[XEP-0166]: https://xmpp.org/extensions/xep-0166.html
[XEP-0167]: https://xmpp.org/extensions/xep-0167.html
[XEP-0234]: https://xmpp.org/extensions/xep-0234.html
[XEP-0177]: https://xmpp.org/extensions/xep-0177.html
[XEP-0176]: https://xmpp.org/extensions/xep-0176.html
[XEP-0215]: https://xmpp.org/extensions/xep-0215.html
[XEP-0272]: https://xmpp.org/extensions/xep-0272.html
[XEP-0298]: https://xmpp.org/extensions/xep-0298.html
[XEP-0340]: https://xmpp.org/extensions/xep-0340.html
[XEP-0278]: https://xmpp.org/extensions/xep-0278.html
[Jingle protocol]: https://en.wikipedia.org/wiki/Jingle_(protocol)
[RTP]: https://en.wikipedia.org/wiki/Real-time_Transport_Protocol
[NAT traversal]: https://en.wikipedia.org/wiki/NAT_traversal
[ICE]: https://en.wikipedia.org/wiki/Interactive_Connectivity_Establishment
[STUN]: https://en.wikipedia.org/wiki/Session_Traversal_Utilities_for_NAT
[TURN]: https://en.wikipedia.org/wiki/Traversal_Using_Relays_around_NAT
[TURN tutorial]: ICE_tutorial.md
