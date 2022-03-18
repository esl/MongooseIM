This module allows the client to query for the most recent [chat markers][chat-markers].

When a client enters a conversation after being offline for a while, such client might want to know what was the last message-id that was marked according to the rules defined in [XEP-0333 - Chat Markers][chat-markers], in order to know where he left of, and build an enhanced UI.

MongooseIM provides such functionality, using [mod_smart_markers](../modules/mod_smart_markers.md)

## Namespace
```
esl:xmpp:smart-markers:0
```

## Fetching a conversation's latest markers

Given a peer, i.e., another user or a muc room, we can fetch the marker we last sent, to the main thread or any other sub-thread, with an IQ like the following:
```xml
<iq id='iq-unique-id' type='get'>
  <query xmlns='esl:xmpp:smart-markers:0' peer='<peer-bare-jid>' [thread='<thread-id>' after='<RFC3339-timestamp>'] />
</iq>
```
where:

* `<peer-bare-jid>` MUST be the bare jid of the peer whose last marker wants to be checked. It can be the bare jid of a user, or of MUC room.
* `<thread>` is an optional attribute that indicates if the check refers to specific a thread in the conversation. If not provided, defaults to the main conversation thread.
* `<after>` is an optional attribute indicating whether markers sent only after a certain timestamp are desired. This most often makes sense for big groupchats, as a potential filter to reduce the amount of markers that will be returned.

Then the following would be received, was there to be any marker:
```xml
<iq from='user-bare-jid' to='user-jid' id='iq-unique-id' type='result'>
  <query xmlns='esl:xmpp:smart-markers:0' peer='peer-bare-jid'>
    <marker from='<sender-bare-jid>' id='<message-id>' type='<type>' timestamp='<RFC3339>' [thread='<thread-id>']/>
  </query>
</iq>
```
where `peer-bare-jid` matches the requested bare jid and the subelements are `marker` xml payloads with the following attributes:

* `<id>` is the message id associated to this marker.
* `<type>` is a marker as described in [XEP-0333][chat-markers].
* `<timestamp>` contains an RFC3339 timestamp indicating when the marker was sent
* `<thread>` is an optional attribute that indicates if the marker refers to specific a thread in the conversation, or the main conversation if absent.
* `<sender-bare-jid>` is the bare jid of the peer who sent the marker, which can be the requester itself, or any peer in the conversation, for both 1:1 chats or groupchats.

### Example: 1:1

```xml
<!-- Alice fetches markers in her conversation with Bob -->
<iq id='iq-unique-id' type='get'>
  <query xmlns='esl:xmpp:smart-markers:0' peer='bob@localhost' />
</iq>

<!-- She receives as an answer -->
<iq from='alice@localhost' to='alice@localhost/res1' id='iq-unique-id' type='result'>
  <query xmlns='esl:xmpp:smart-markers:0' peer='bob@localhost'>
    <marker from='alice@localhost' id='ABCDEFGHIJ' type='displayed' timestamp='2022-02-26T09:11:05.634232Z'/>
    <marker from='bob@localhost' id='KLMNOPQRST' type='displayed' timestamp='2022-02-26T09:11:07.382923Z'/>
  </query>
</iq>
```

### Example: groupchats

```xml
<!-- Alice fetches markers in a groupchat -->
<iq id='iq-unique-id' type='get'>
  <query xmlns='esl:xmpp:smart-markers:0' peer='room@muc.localhost' />
</iq>

<!-- She receives as an answer -->
<iq from='alice@localhost' to='alice@localhost/res1' id='iq-unique-id' type='result'>
  <query xmlns='esl:xmpp:smart-markers:0' peer='room@muc.localhost'>
    <marker from='alice@localhost' id='XOLWEMUNTO' type='displayed' timestamp='2022-02-26T09:11:05.634232Z'/>
    <marker from='bob@localhost' id='NNTMWMKSOE' type='displayed' timestamp='2022-02-26T09:11:07.382923Z'/>
    <marker from='mike@localhost' id='OSNTETNHUR' type='displayed' timestamp='2022-02-26T09:13:07.382923Z'/>
    <marker from='kate@localhost' id='SNWMENSTUH' type='displayed' timestamp='2022-02-26T09:12:07.382923Z'/>
  </query>
</iq>
```

[chat-markers]: https://xmpp.org/extensions/xep-0333.html
