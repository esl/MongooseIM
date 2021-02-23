## Extensions to [mod_inbox](../modules/mod_inbox.md)

### Archived Inbox

Beekeeper has two different mailboxes for the inbox: the regular one, simply called the inbox, and an archive mailbox, where clients can manually throw conversations they don't want displayed in the default UI. When quering "the inbox", the expected default behaviour is to get the regular inbox, and no knowledge of what was archived. If the archive was to be fetched, a flag requesting either the archive, or both together with each conversation being flagged as being archived or not, will be required.

The expected behaviour is that, upon receiving a new message, any conversation in the archived inbox should jump back to the regular inbox.

As in [mod_inbox](../modules/mod_inbox.md), to request the currently supported form, the client can:

```xml
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
      <field var='archived' type='boolean' value='false'/>
    </x>
  </query>
</iq>
```

#### Setting a conversation as archived and restoring it

To set a conversation as archived the client can send:
```xml
<iq type='set' id='some_unique_id'>
    <archive xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='bob@localhost'/>
</iq>
```
Or to restore from the archive:
```xml
<iq type='set' id='some_unique_id'>
    <restore xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='bob@localhost'/>
</iq>
```

If the request completed successfully, an iq-result of the corresponding element will be returned:
```xml
<iq from='alice@localhost' to='alice@localhost/res1' id='some_unique_id' type='result'>
    <archive xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='bob@localhost'/>
</iq>
```
Or when restoring
```xml
<iq from='alice@localhost' to='alice@localhost/res1' id='some_unique_id' type='result'>
    <restore xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='bob@localhost'/>
</iq>
```

If the request failed, for example if the jid wasn't valid or the status was already set, Alice would receive, as expected:
```xml
<iq from='alice@localhost' to='alice@localhost/res1' id='some_unique_id' type='error'>
    <error code='400' type='modify'>
        <bad-request xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
        <text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>Error</text>
    </error>
</iq>
```
Where `Error` can be `invalid-jid`, `already-archived`, `already-restored`, predictably.


#### Fetching the inbox

To fetch the regular inbox, as explained in [mod_inbox/fetching](../modules/mod_inbox.md/#filtering-and-ordering), the client sends:

```xml
<iq type='set' id='10bca'>
  <inbox xmlns='erlang-solutions.com:xmpp:inbox:0' queryid='b6'>
    <x xmlns='jabber:x:data' type='form'>
      <field type='hidden' var='FORM_TYPE'><value>erlang-solutions.com:xmpp:inbox:0</value></field>
      <field type='list-single' var='order'><value>asc</value></field>
      <field type='text-single' var='hidden_read'><value>true</value></field>
    </x>
  </inbox>
</iq>
```
where the `archived` flag is assumed to be false and hence it is not needed
```xml
      <field type='boolean' var='archived'><value>false</value></field>
```


#### Fetching the archive

To fetch just the archived inbox, an 'archived' flag must be provided to the previous form (false is assumed the default, so the previous didn't require a value for it)

```xml
<iq type='set' id='10bca'>
  <inbox xmlns='erlang-solutions.com:xmpp:inbox:0' queryid='b6'>
    <x xmlns='jabber:x:data' type='form'>
      <field type='hidden' var='FORM_TYPE'><value>erlang-solutions.com:xmpp:inbox:0</value></field>
      <field type='list-single' var='order'><value>asc</value></field>
      <field type='boolean' var='archived'><value>true</value></field>
    </x>
  </inbox>
</iq>
```


### Setting a conversation as unread

How does it work? Alice wants to reset her inbox's count with Bob, so Alice sends an iq-set with attribute `set-unread` and jid, that one she wants to set the counter. This will effectively do a +1 to whatever the counter is at a given time, to work against race conditions.

```xml
<iq type='set' id='some_unique_id'>
    <set-unread xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='bob@localhost'/>
</iq>
```

If the count was incremented successfully, the following result is expected
```xml
<iq from='alice@localhost' to='alice@localhost/res1' id='some_unique_id' type='result'>
    <set-unread xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' unread-count='1' jid='bob@localhost'/>
</iq>
```

If the request failed, for example if there was no conversation with Bob with Alice's inbox, or the jid wasn't a valid jid in general, Alice would receive:
```xml
<iq from='alice@localhost' to='alice@localhost/res1' id='some_unique_id' type='error'>
    <error code='400' type='modify'>
        <bad-request xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
        <text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>Error</text>
    </error>
</iq>
```

Where `Error` can be `invalid-jid`, or `already-set`, predictably.


### Muting a conversation

To set a conversation as muted, the client will supply the number of seconds he desires this conversation to be muted for. The server will then calculate the final timestamp from the time the request was received, adding the number of requested seconds, and store this final timestamp in the database. This is the timestamp that will be provided to the clients upon inbox building. It is also assumed that muting a muted conversation simply restarts the timer.

For an example of one year, the client would submit this request:
```xml
<iq type='set' id='some_unique_id'>
    <mute xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='bob@localhost' seconds=31557600/>
</iq>
```
or to unmute:
```xml
<iq type='set' id='some_unique_id'>
    <unmute xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='bob@localhost'/>
</iq>
```


If the conversation was muted successfully, a timestamp under the `muted_until` attribute will be provided:
```xml
<iq from='alice@localhost' to='alice@localhost/res1' id='some_unique_id' type='result'>
    <mute xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' muted_until='2021-02-25T08:44:14.323836Z' jid='bob@localhost'/>
</iq>
```
or when unmuting:
```xml
<iq from='alice@localhost' to='alice@localhost/res1' id='some_unique_id' type='result'>
    <unmute xmlns='erlang-solutions.com:xmpp:inbox:0#conversation' jid='bob@localhost'/>
</iq>
```

If the request failed, for example if the jid or the timestamp weren't valid, Alice would receive:
```xml
<iq from='alice@localhost' to='alice@localhost/res1' id='some_unique_id' type='error'>
    <error code='400' type='modify'>
        <bad-request xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
        <text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>Error</text>
    </error>
</iq>
```

Where `Error` can be `invalid-jid`, `invalid-time`, or `already-set`, predictably.
