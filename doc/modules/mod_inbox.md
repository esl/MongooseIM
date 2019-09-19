### Module Description

`Inbox` is an experimental feature implemented as a few separate modules.
To use it, enable mod\_inbox in the config file.

### Options

* **backend** (atom, default: `rdbms`) - Database backend to use. For now, only `rdbms` is supported.
* **reset_markers** (list, default: `[displayed]`) - List of atom chat markers that when sent, will reset the unread message counter for a conversation.
This works when [Chat Markers](https://xmpp.org/extensions/xep-0333.html) are enabled on the client side.
Possible values are from the set: `displayed`, `received`, `acknowledged`. Setting as empty list (not recommended) means that no chat marker can decrease the counter value.
* **groupchat** (list, default: `[muclight]`) - The list indicating which groupchats will be included in inbox.
Possible values are `muclight` [Multi-User Chat Light](https://xmpp.org/extensions/inbox/muc-light.html) or `muc` [Multi-User Chat](https://xmpp.org/extensions/xep-0045.html).
* **aff_changes** (boolean, default: `true`) - use this option when `muclight` is enabled.
Indicates if MUC Light affiliation change messages should be included in the conversation inbox.
Only changes that affect the user directly will be stored in their inbox.
* **remove_on_kicked** (boolean, default: `true`) - use this option when `muclight` is enabled.
If true, the inbox conversation is removed for a user when they are removed from the groupchat.
* **iqdisc** (atom, default: `no_queue`)

### Note about supported RDBMS

`mod_inbox` executes upsert queries, which have different syntax in every supported RDBMS.
Inbox currently supports the following DBs:

* MySQL via native driver
* PgSQL via native driver
* MSSQL via ODBC driver

### Legacy MUC support
Inbox comes with support for the legacy MUC as well. It stores all groupchat messages sent to
room in each sender's and recipient's inboxes and private messages. Currently it is not possible to
configure it to store system messages like [subject](https://xmpp.org/extensions/xep-0045.html#enter-subject) 
or [affiliation](https://xmpp.org/extensions/xep-0045.html#affil) change.

### Filtering and ordering

Inbox query results may be filtered by time range and sorted by timestamp.
By default, `mod_inbox` returns all conversations, listing the ones updated most recently first.

A client may specify three parameters:

* Start date for the result set (variable `start`, value: ISO timestamp)
* End date for the result set (variable `end`, value: ISO timestamp)
* Order by timestamp (variable `order`, values: `asc`, `desc`)
* Show only conversations with unread messages (variable `hidden_read`,
  values: `true`, `false`)

They are encoded inside a standard XMPP [Data Forms](https://xmpp.org/extensions/xep-0004.html) format.
Dates must be formatted according to [XMPP Date and Time Profiles](https://xmpp.org/extensions/xep-0082.html).
It is not mandatory to add an empty data form if a client prefers to use default values (`<query/>` element may be empty).
However, the IQ type must be "set", even when data form is missing.

Your client application may request the currently supported form with IQ get:

```
Client:

<iq type='get' id='c94a88ddf4957128eafd08e233f4b964'>
  <query xmlns='erlang-solutions.com:xmpp:inbox:0'/>
</iq>

Server:

<iq from='alicE@localhost' to='alicE@localhost/res1' id='c94a88ddf4957128eafd08e233f4b964' type='result'>
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
    </x>
  </query>
</iq>
```

### Reseting inbox

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

Resetting the inbox count will also skip the forwarding of messages. While a
typical chat marker will be forwarded to the interlocutor(s), (including the
case of a big groupchat with thousands of participants!), this reset stanza will
not.

### Example Request

```
Alice sends:

<message type="chat" to="bOb@localhost/res1" id=”123”>
  <body>Hello</body>
</message>

Bob receives:

<message from="alicE@localhost/res1" to="bOb@localhost/res1" id=“123” xml:lang="en" type="chat">
  <body>Hello</body>
</message>

Alice sends:

<iq type="set" id="10bca">
  <inbox xmlns=”erlang-solutions.com:xmpp:inbox:0” queryid="b6">
    <x xmlns='jabber:x:data' type='form'>
      <field type='hidden' var='FORM_TYPE'><value>erlang-solutions.com:xmpp:inbox:0</value></field>
      <field type='text-single' var='start'><value>2018-07-10T12:00:00Z</value></field>
      <field type='text-single' var='end'><value>2018-07-11T12:00:00Z</value></field>
      <field type='list-single' var='order'><value>asc</value></field>
      <field type='text-single' var='hidden_read'><value>true</value></field>
    </x>
  </inbox>
</iq>


Alice receives:

<message from="alicE@localhost" to="alicE@localhost" id="9b759">
  <result xmlns="erlang-solutions.com:xmpp:inbox:0" unread="0" queryid="b6">
    <forwarded xmlns="urn:xmpp:forward:0">
      <delay xmlns="urn:xmpp:delay" stamp="2018-07-10T23:08:25.123456Z"/>
      <message xml:lang="en" type="chat" to="bOb@localhost/res1" from="alicE@localhost/res1" id=”123”>
        <body>Hello</body>
      </message>
    </forwarded>
  </result>
</message>

<iq from="alicE@localhost" to="alicE@localhost/res1" id="b6" type="result">
  <fin xmlns='erlang-solutions.com:xmpp:inbox:0'>
    <count>1</count>
    <unread-messages>0</unread-messages>
    <active-conversations>0</active-conversations>
  </fin>
</iq>

```


Inbox query result IQ stanza returns the following values:

* `count`: the total number of conversations (if `hidden_read` value was set
  to true, this value will be equal to `active_conversations`)
* `unread-messages`: total number of unread messages from all
  conversations
* `active-conversations`: the number of conversations with unread
  message(s)

### Example error response

```
Alice sends request with invalid value of start field:

<iq type='set' id='a78478f20103ff8354d7834d0ba2fdb2'>
  <inbox xmlns='erlang-solutions.com:xmpp:inbox:0'>
    <x xmlns='jabber:x:data' type='submit'>
      <field type='text-single' var='start'>
        <value>invalid</value>
      </field>
    </x>
  </inbox>
</iq>

Alice receives an error with description of the first encountered invalid
value: 

<iq from='alicE@localhost' to='alicE@localhost/res1'
    id='a78478f20103ff8354d7834d0ba2fdb2' type='error'>
  <error code='400' type='modify'>
    <bad-rquest xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
    <text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>
      Invalid inbox form field value, field=start, value=invalid
    </text>
  </error>
</iq>
```

### Example Configuration

```
{mod_inbox, [{backend, rdbms},
             {reset_markers, [displayed]},
             {aff_changes, true},
             {remove_on_kicked, true},
             {groupchat, [muclight]}
            ]},
```

