When a messaging client starts, it typically builds a UI showing a list of recent chats, with metadata attached to them like, whether any chat has new messages and how many, or if it is fully read, or if they are for example muted and until when.
In MongooseIM this functionality is provided by [mod_inbox](../modules/mod_inbox.md).

## Terminology:

### The Inbox
It is personal to a given user and represents the current status of the conversations of that user. It's the front-page of the chat feature.

### Inbox entry
It is a specific conversation, that the user can identify by the recipient jid, that is, the user jid in case of a one-to-one chat, or the room jid in case of a group-chat.

### Box (also referred to as "folder")
A category where entries can be classified. The default box is the active box, simply called _inbox_. There is a second box, called _archive_, where entries can be thrown into and not displayed by default. More boxes can be created through [configuration][inbox boxes].


## Entity Use Cases

### Discovering Inbox Services
An entity can discover the inbox service via a Features Discovery request:
```xml
<!-- Client -->
<iq type='get' id='a96d4244760853af7b3ae84faa1a40fb' to='localhost'>
	<query xmlns='http://jabber.org/protocol/disco#info'/>
</iq>

<!-- Server -->
<iq from='localhost' to='alice@localhost/res1' id='a96d4244760853af7b3ae84faa1a40fb' type='result'>
	<query xmlns='http://jabber.org/protocol/disco#info'>
		<identity category='server' type='im' name='MongooseIM'/>
		<feature var='erlang-solutions.com:xmpp:inbox:0'/>
	</query>
</iq>
```

## Fetching the inbox

### Querying
The inbox is fetched using regular XMPP [Data Forms]. To request the supported form, the client should send:
```xml
<!-- Client -->
<iq type='get' id='some_unique_id'>
  <query xmlns='erlang-solutions.com:xmpp:inbox:0'/>
</iq>

<!-- Server -->
<iq from='alice@localhost' to='alice@localhost/res1' id='some_unique_id' type='result'>
  <query xmlns='erlang-solutions.com:xmpp:inbox:0'>
    <x xmlns='jabber:x:data' type='form'>
      <field type='hidden' var='FORM_TYPE'><value>erlang-solutions.com:xmpp:inbox:0</value></field>
      <field var='start' type='text-single'/>
      <field var='end' type='text-single'/>
      <field var='order' type='list-single'>
        <value>desc</value>
        <option label='Ascending by timestamp'><value>asc</value></option>
        <option label='Descending by timestamp'><value>desc</value></option>
      </field>
      <field var='hidden_read' type='text-single' value='false'/>
      <field var='box' type='list-simple' value='all'>
        <option label='all'><value>all</value></option>
        <option label='inbox'><value>inbox</value></option>
        <option label='archive'><value>archive</value></option>
        <option label='bin'><value>bin</value></option>
      </field>
      <field var='archive' type='boolean'/>
    </x>
  </query>
</iq>
```

To fetch the inbox, the client should send:
```xml
<iq type='set' id='10bca'>
  <inbox xmlns='erlang-solutions.com:xmpp:inbox:0' queryid='b6'/>
</iq>
```

Then the client should receive:
```xml
<message from="alice@localhost" to="alice@localhost/res1" id="9b759">
  <result xmlns="erlang-solutions.com:xmpp:inbox:0" unread="0" queryid="b6">
    <forwarded xmlns="urn:xmpp:forward:0">
      <delay xmlns="urn:xmpp:delay" stamp="2018-07-10T23:08:25.123456Z"/>
      <message xml:lang="en" type="chat" to="bob@localhost/res1" from="alice@localhost/res1" id=”123”>
        <body>Hello</body>
      </message>
    </forwarded>
    <box>inbox</box>
    <archive>false</archive>
    <mute>0</mute>
  </result>
</message>

<iq from="alice@localhost" to="alice@localhost/res1" id="b6" type="result">
  <fin xmlns='erlang-solutions.com:xmpp:inbox:0'>
    <count>1</count>
    <unread-messages>0</unread-messages>
    <active-conversations>0</active-conversations>
  </fin>
</iq>
```
where none-or-many `message` stanzas are sent to the requesting resource describing each inbox entry, and a final `iq-fin` stanza marks the end of the inbox query,
Inbox query result IQ stanza returns the following values:

* `count`: the total number of conversations (if `hidden_read` value was set
  to true, this value will be equal to `active_conversations`)
* `unread-messages`: total number of unread messages from all
  conversations
* `active-conversations`: the number of conversations with unread
  message(s)

Note that the `queryid` field is optional, and if not provided, the answers will fall back to the `id` field of the IQ query.


### Filtering and ordering
Inbox query results may be filtered by time range and box, and sorted by timestamp.
By default, `mod_inbox` returns all conversations, listing the ones updated most recently first.

A client may specify the following parameters:

* variable `start`: Start date for the result set (value: ISO timestamp)
* variable `end`: End date for the result set (value: ISO timestamp)
* variable `order`: Order by timestamp (values: `asc`, `desc`)
* variable `hidden_read`: Show only conversations with unread messages (values: `true`, `false`)
* variable `box`: Indicate which box is desired. Supported are `all`, `inbox`, `archive` and `bin`. More boxes can be implemented, see [mod_inbox – Boxes](../modules/mod_inbox.md#modulesmod_inboxboxes). If not provided, all except the bin are returned.
* variable `archive` [deprecated, prefer `box`]: whether to query the archive inbox. `true` means querying only the archive box, `false` means querying only the active box. If the flag is not set, it is assumed all entries are requested. This is kept for backwards compatibility reasons, use the `box` flag instead.

They are encoded inside a standard XMPP [Data Forms] format.
Dates must be formatted according to [XMPP Date and Time Profiles](https://xmpp.org/extensions/xep-0082.html).
It is not mandatory to add an empty data form if a client prefers to use default values (`<inbox/>` element may be empty).
However, the IQ type must be "set", even when the data form is missing.

### Limiting the query
It can happen that the amount of inbox entries is too big for a given user, even after filtering by `start` and `end` as already available in [mod_inbox]. Hence, we need to set a fixed limit of the number of entries that are requested. For this, we can use a `<max>` attribute as defined in [XEP-0059: #2.1 Limiting the Number of Items](https://xmpp.org/extensions/xep-0059.html#limit):
```xml
<iq type='set' id='10bca'>
  <inbox xmlns='erlang-solutions.com:xmpp:inbox:0' queryid='b6'>
    <x xmlns='jabber:x:data' type='form'>
      <field type='hidden' var='FORM_TYPE'><value>erlang-solutions.com:xmpp:inbox:0</value></field>
      <field type='list-single' var='order'><value>asc</value></field>
      <field type='text-single' var='hidden_read'><value>true</value></field>
      <field type='list-single' var='box'><value>inbox</value></field>
    </x>
    <set xmlns='http://jabber.org/protocol/rsm'>
      <max>Max</max>
    </set>
  </inbox>
</iq>
```
where `Max` is a non-negative integer.

Inbox also has partial support for pagination as described in [XEP-0059](https://xmpp.org/extensions/xep-0059.html). Note that therefore there are two ways to denote pages, the standard RSM mechanism and the custom inbox form. If both are used, the RSM marker will override the respective inbox form, as in, `before` will override `start`, and `after` will override `end`.

!!! Note
    Inbox pagination does not support total count nor indexes as described in [XEP-0059: #2.6 Retrieving a Page Out of Order](https://xmpp.org/extensions/xep-0059.html#jump).

## Properties of an entry
Given an entry, certain properties are defined for such an entry:

### Box
Clients usually have two different boxes for the inbox: the regular one, simply called the inbox (or the active inbox), and an archive box, where clients can manually throw conversations they don't want displayed in the default UI. A third box is the trash bin, where deleted entries go and are cleaned up in regular intervals.

It is expected that entries will reside in the archive until they're either manually moved back to the active box, or they receive a new message: in such case the entry should jump back to the active box automatically.

More boxes can be implemented, see [mod_inbox#boxes](../modules/mod_inbox.md#modulesmod_inboxboxes). Movement between boxes can be achieved through the right XMPP IQ, no more automatic movements are developed as in the case of inbox-archive.

### Read
Entries keep a count of unread messages that is incremented automatically upon receiving a new message, and (in the current implementation) set to zero upon receiving either a message by one-self, or an appropriate chat marker as defined in [XEP-0333](https://xmpp.org/extensions/xep-0333.html) (which markers reset the count is a matter of configuration, see [doc](../modules/mod_inbox.md#modulesmod_inboxreset_markers)).

This property can also be manually set to zero or to one using the appropriate requests as explained below.

### Muted
Entries can be muted for given periods of time, and likewise, unmuted. This changes the UI representation, and also, means that the user won't get PNs (Push Notifications) for this entry, until the time set expires, or the user sets otherwise. Knowledge of this is necessary to help build the UI.

Expected times can be extended before the period has expired, without the need to first unmuting. When muting a conversation, the final timestamp will be calculated by the server as the current time plus the requested period, in seconds, to centralise knowledge of UTC clocks. When muting an already muted conversation, the timestamp is simply overridden following the previous specification.

### Other properties
No more properties are expected, but one could envisage notions of flagging conversations with different colours, for example according to their urgency, or a client-specific category (work, personal, fitness, and whatnot), or pins to denote an entry should be always displayed (possibly in a special format, like on top of the box). The design of the protocol, and the implementation, aims to leave room for future extensions.

## Getting properties
To fetch all supported properties, a classic Data Form is used. Upon the client sending an iq-get without a jid:
```xml
<iq id='some_unique_id' type='get'>
  <query xmlns='erlang-solutions.com:xmpp:inbox:0#conversation'/>
</iq>
```
The server would respond with:
```xml
<iq from='alice@localhost' to='alice@localhost/res1' id='some_unique_id' type='result'>
  <query xmlns='erlang-solutions.com:xmpp:inbox:0#conversation'>
    <x xmlns='jabber:x:data' type='form'>
      <field type='hidden' var='FORM_TYPE'><value>erlang-solutions.com:xmpp:inbox:0</value></field>
      <field var='archive' type='boolean' value='false'/>
      <field var='read' type='boolean' value='false'/>
      <field var='mute' type='text-single' value='0'/>
      <field var='box' type='list-simple' value='all'>
        <option label='all'><value>all</value></option>
        <option label='inbox'><value>inbox</value></option>
        <option label='archive'><value>archive</value></option>
        <option label='bin'><value>bin</value></option>
      </field>
    </x>
  </query>
</iq>
```

If the properties of a certain entry were to be fetched, it can easily be done with:
```xml
<iq id='some_unique_id' type='get'>
  <query xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='bob@localhost'/>
</iq>
```
To which the server will reply, just like before, with:
```xml
<iq id='some_unique_id' to='alice@localhost/res1' type='result'>
  <query xmlns='erlang-solutions.com:xmpp:inbox:0#conversation'>
    <box>inbox</box>
    <archive>false</archive>
    <mute>0</mute>
    <read>true</read>
  </query>
</iq>
```

If an entire entry wanted to be queried, and not only its attributes, a `complete='true'` can be provided:
```xml
<iq id='some_unique_id' type='get'>
  <query xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='bob@localhost' complete='true'/>
</iq>
```
To which the server will reply, just like before, with:
```xml
<iq id='some_unique_id' to='alice@localhost/res1' type='result'>
  <query xmlns='erlang-solutions.com:xmpp:inbox:0#conversation'>
    <forwarded xmlns="urn:xmpp:forward:0">
      <delay xmlns="urn:xmpp:delay" stamp="2018-07-10T23:08:25.123456Z"/>
      <message xml:lang="en" type="chat" to="bob@localhost/res1" from="alice@localhost/res1" id=”123”>
        <body>Hello</body>
      </message>
    </forwarded>
    <archive>false</archive>
    <mute>0</mute>
    <read>true</read>
  </query>
</iq>
```


## Setting properties
Setting properties is done using the standard XMPP pattern of `iq-query` and `iq-result`, as below:
```xml
<iq id='some_unique_id' type='set'>
  <query xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='bob@localhost'>
    <Property>Value</Property>
    <!-- Possibly other properties -->
  </query>
</iq>
```
where `Property` and `Value` are a list of key-value pairs as follows:

- `box`: `inbox`, `archive`, or a custom value if this has been extended.
- `archive`: `true` or `false`
- `mute`: number of _seconds_ to mute for. Choose `0` for unmuting.
- `read` (adjective, not verb): `true` or `false`. Setting to true essentially sets the unread-count to `0`, `false` sets the unread-count to `1` (if it was equal to `0`, otherwise it lefts it unchanged). No other possibilities are offered, to reduce the risk of inconsistencies or problems induced by a faulty client.

*Note* that resetting the inbox count will not be forwarded. While a chat marker will be forwarded to the interlocutor(s), (including the case of a big groupchat with thousands of participants), this reset stanza will not.

If the query was successful, the server will answer with two stanzas, following the classic pattern of broadcasting state changes. First, it would send a message with a `<x>` children containing all new configuration, to the bare-jid of the user: this facilitates broadcasting to all online resources to successfully synchronise their interfaces.
```xml
<message from='alice@localhost' to='alice@localhost' id='some_unique_id'>
  <x xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='bob@localhost'>
    <box>inbox</box>
    <archive>false</archive>
    <mute>0</mute>
    <read>true</read>
  </x>
</message>
```
where `<mute>` may contain either a zero, to denote unmuted, or a RFC3339 timestamp, as in `2021-02-25T08:44:14.323836Z`.

To the requesting resource, a simple iq-result would be then sent to notify of success, as required by the iq directives of the XMPP RFCs:
```xml
<iq id='some_unique_id' to='alice@localhost/res1' type='result'/>
```

If the request was not successful, the server would then answer as in:
```xml
<iq to='alice@localhost/res1' type='error'>
  <error type='Type'>
    <Condition xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
  </error>
</iq>
```
Where `Type` will usually be `modify` or `cancel`, as explained in <https://xmpp.org/rfcs/rfc6120.html#stanzas-error-syntax>, and `Condition` is as explained in <https://xmpp.org/rfcs/rfc6120.html#stanzas-error-conditions>, `bad-request` being the most common.

This final syntax for the protocol has been chosen as it allows for better pipelining of requests, and it remains consistent with how, for example, rooms are configured for MUC-Light.


### Examples: archiving an entry
To put an entry into the archived box, the client can send:
```xml
<iq id='some_unique_id' type='set'>
  <query xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='bob@localhost'>
    <box>archive</box>
  </query>
</iq>
```
On success, the server would return (considering the entry has no unread messages and is not muted):
```xml
<iq id='some_unique_id' to='alice@localhost/res1' type='result'>
  <query xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='bob@localhost'>
    <box>archive</box>
    <archive>true</archive>
    <mute>0</mute>
    <read>true</read>
  </query>
</iq>
```
If the client had sent an invalid number (negative, or NaN), the server would answer:
```xml
<iq to='alice@localhost/res1' type='error'>
  <error type='modify'>
    <bad-request xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
  </error>
</iq>
```

### Examples: emptying the trash bin
A user can empty his trash bin, through the following request:
```xml
<iq id='some_unique_id' type='set'>
  <empty-bin xmlns='erlang-solutions.com:xmpp:inbox:0'/>
</iq>
```
On success, the server would return how many entries where dropped as in:
```xml
<iq id='some_unique_id' to='alice@localhost/res1' type='result'>
  <empty-bin xmlns='erlang-solutions.com:xmpp:inbox:0'>
    <num>2</num>
  </empty-bin>
</iq>
```
The server might answer with a corresponding error message, might anything go wrong.

### Examples: muting an entry
To mute an entry for a full day (86400 seconds in a day, 604800 in a week, for example), a client can send:
```xml
<iq id='some_unique_id' type='set'>
  <query xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='bob@localhost'>
    <mute>86400</mute>
  </query>
</iq>
```
On success, the server would return (considering the server receives the timestamp on "2021-02-26T09:11:05.634232Z", and the entry is on the active box and completely read):
```xml
<iq id='some_unique_id' to='alice@localhost/res1' type='result'>
  <query xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='bob@localhost'>
    <box>inbox</box>
    <archive>false</archive>
    <mute>2021-02-27T09:11:05.634232Z</mute>
    <read>true</read>
  </query>
</iq>
```
If the client had sent an invalid number (negative, or NaN), the server would answer:
```xml
<iq to='alice@localhost/res1' type='error'>
  <error type='modify'>
    <bad-request xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
  </error>
</iq>
```
To unmute, similarly, the client can send:
```xml
<iq id='some_unique_id' type='set'>
  <query xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='bob@localhost'>
    <mute>0</mute>
  </query>
</iq>
```
And server responses will be similar.

### Examples: reading an entry
To set an entry as read, the client can send:
```xml
<iq id='some_unique_id' type='set'>
  <query xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='bob@localhost'>
    <read>true</read>
  </query>
</iq>
```
On success, the server would return (considering the entry is not archived and not muted):
```xml
<iq id='some_unique_id' to='alice@localhost/res1' type='result'>
  <query xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='bob@localhost'>
    <box>inbox</box>
    <archive>false</archive>
    <mute>0</mute>
    <read>true</read>
  </query>
</iq>
```
On error, as usual, the client would get:
```xml
<iq to='alice@localhost/res1' type='error'>
  <error type='modify'>
    <bad-request xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
  </error>
</iq>
```
And similarly, to set a conversation as unread:
```xml
<iq id='some_unique_id' type='set'>
  <query xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='bob@localhost'>
    <read>false</read>
  </query>
</iq>
```

### Deprecated reset entry stanza:
You can reset the inbox with the following stanza:
```xml
<iq type='set'>
    <reset xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='interlocutor_bare_jid'/>
</iq>
```
Here `jid` is the bare jid of the user whose inbox we want to reset. This action
does not change the last message stored in inbox; meaning that neither this
stanza nor anything given within will be stored; the only change is the inbox
`unread_count` is set to zero.


## Example request

```xml
<!-- Alice sends: -->
<message type="chat" to="bob@localhost/res1" id=”123”>
  <body>Hello</body>
</message>

<!-- Bob receives: -->
<message from="alice@localhost/res1" to="bob@localhost/res1" id=“123” xml:lang="en" type="chat">
  <body>Hello</body>
</message>

<!-- Alice sends: -->
<iq type="set" id="10bca">
  <inbox xmlns="erlang-solutions.com:xmpp:inbox:0" queryid="b6">
    <x xmlns='jabber:x:data' type='form'>
      <field type='hidden' var='FORM_TYPE'><value>erlang-solutions.com:xmpp:inbox:0</value></field>
      <field type='text-single' var='start'><value>2018-07-10T12:00:00Z</value></field>
      <field type='text-single' var='end'><value>2018-07-11T12:00:00Z</value></field>
      <field type='list-single' var='order'><value>asc</value></field>
    </x>
  </inbox>
</iq>

<!-- Alice receives: -->
<message from="alice@localhost" to="alice@localhost" id="9b759">
  <result xmlns="erlang-solutions.com:xmpp:inbox:0" unread="0" queryid="b6">
    <forwarded xmlns="urn:xmpp:forward:0">
      <delay xmlns="urn:xmpp:delay" stamp="2018-07-10T23:08:25.123456Z"/>
      <message xml:lang="en" type="chat" to="bob@localhost/res1" from="alice@localhost/res1" id=”123”>
        <body>Hello</body>
      </message>
    </forwarded>
    <box>inbox</box>
    <archive>false</archive>
    <mute>0</mute>
  </result>
</message>

<iq from="alice@localhost" to="alice@localhost/res1" id="10bca" type="result">
  <fin xmlns='erlang-solutions.com:xmpp:inbox:0'>
    <count>1</count>
    <unread-messages>0</unread-messages>
    <active-conversations>0</active-conversations>
  </fin>
</iq>
```

## Example error response
```xml
<!--Alice sends request with invalid value of start field: -->
<iq type='set' id='a78478f20103ff8354d7834d0ba2fdb2'>
  <inbox xmlns='erlang-solutions.com:xmpp:inbox:0'>
    <x xmlns='jabber:x:data' type='submit'>
      <field type='text-single' var='start'>
        <value>invalid</value>
      </field>
    </x>
  </inbox>
</iq>

<!--Alice receives an error with description of the first encountered invalid value: -->
<iq from='alice@localhost' to='alice@localhost/res1'
    id='a78478f20103ff8354d7834d0ba2fdb2' type='error'>
  <error code='400' type='modify'>
    <bad-rquest xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
    <text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>
      Invalid inbox form field value, field=start, value=invalid
    </text>
  </error>
</iq>
```

[mod_inbox]: ../modules/mod_inbox.md
[Data Forms]: https://xmpp.org/extensions/xep-0004.html
[inbox boxes]: ../modules/mod_inbox.md#modulesmod_inboxboxes
