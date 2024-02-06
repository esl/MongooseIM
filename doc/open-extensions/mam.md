# Message Archive Management extensions

## New MAM filtering fields

The new fields allow to improve the performance of the counting queries for very big archives
by changing how count and index functions work.

- `from-id` - returns and counts messages with ids `id >= from-id` only (`from-id` is included into the set).
- `to-id` - returns and counts messages with ids `id <= to-id` only (`to-id` is included into the set).
- `after-id` - returns and counts messages with ids `id > after-id` only (`after-id` is not included into the set).
- `before-id` - returns and counts messages with ids `id < before-id` only (`before-id` is not included into the set).
- `simple` - do not return count and offset fields in the result.

The fields could be combined together. If two filters are provided, both would
be applied to the result.

## Get new messages, oldest first

Example from `pagination_first_page_after_id4` testcase:

The client has downloaded his archive and got disconnected.
He knows, that the last message he has on his device has id=BO7CH1JOF801.
He wants to receive new messages that were sent while he has been disconnected
using a page size 5.

In this mode, the client would get the oldest messages first.

Testcase: the client has messages 1-15 in his archive.

```xml
<!-- Client sends -->
<iq type='set' id='req1'>
    <query xmlns='urn:xmpp:mam:1' queryid='first_page_after_id4'>
        <x xmlns='jabber:x:data'>
            <field var='after-id'>
                <value>BO7CH1JOF801</value> <!-- id of the Message #4 -->
            </field>
        </x>
        <set>
            <max>5</max>
        </set>
    </query>
</iq>

<!-- Server sends -->
<message from='alice@localhost' to='alice@localhost/res1' id='323372af-7d69-4f36-803d-110272066373'>
    <result queryid='first_page_after_id4' xmlns='urn:xmpp:mam:1' id='BO7CH1JQR9O1'>
        <forwarded xmlns='urn:xmpp:forward:0'>
            <delay xmlns='urn:xmpp:delay' stamp='2022-06-08T09:43:08.952999Z' from='alice@localhost/res1'/>
            <message from='alice@localhost/res1' xmlns='jabber:client' xml:lang='en' to='bob@localhost/res1' type='chat'>
                <body>Message #5</body>
            </message>
        </forwarded>
    </result>
</message>
<!-- ... Messages 6, 7, 8  ... -->
<message from='alice@localhost' to='alice@localhost/res1' id='a44d83f3-de47-4e71-a1e6-62100437fe2c'>
    <result queryid='first_page_after_id4' xmlns='urn:xmpp:mam:1' id='BO7CH1K3TU01'>
        <forwarded xmlns='urn:xmpp:forward:0'>
            <delay xmlns='urn:xmpp:delay' stamp='2022-06-08T09:43:08.990200Z' from='alice@localhost/res1'/>
            <message from='alice@localhost/res1' xmlns='jabber:client' xml:lang='en' to='bob@localhost/res1' type='chat'>
                <body>Message #9</body>
            </message>
        </forwarded>
    </result>
</message>

<iq from='alice@localhost' to='alice@localhost/res1' id='req1' type='result'>
    <fin xmlns='urn:xmpp:mam:1'>
        <set xmlns='http://jabber.org/protocol/rsm'>
            <first index='0'>BO7CH1JQR9O1</first> <!-- Id of the message #5 -->
            <last>BO7CH1K3TU01</last> <!-- Id of the message #9 -->
            <count>11</count> <!-- messages 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 -->
        </set>
    </fin>
</iq>
```

Messages 1-4 are completely ignored in the count and in the index fields.
If the client asked for 5 messages, but count is 11, he should ask for
more messages.

```xml
<!-- Client sends -->
<iq type='set' id='req2'>
    <query xmlns='urn:xmpp:mam:1' queryid='first_page_after_id9'>
        <x xmlns='jabber:x:data'>
            <field var='after-id'>
                <value>BO7CH1K3TU01</value> <!-- id of the Message #9 -->
            </field>
        </x>
        <set>
            <max>5</max>
        </set>
    </query>
</iq>
...
```


## Get new messages, newest first

Sometimes we want to render the newest messages as fast as possible.

Though, if the client caches messages, he has to track which pages
are still need to be requested, when using this method.

Example `pagination_last_page_after_id4`.

```xml
<!-- Client sends -->
<iq type='set' id='req3'>
    <query xmlns='urn:xmpp:mam:1' queryid='last_page_after_id4'>
        <x xmlns='jabber:x:data'>
            <field var='after-id'>
                <value>BO7CUCVVS6O1</value>
            </field>
        </x>
        <set>
            <max>5</max>
            <before/>
        </set>
    </query>
</iq>

<!-- Server sends -->
<message from='alice@localhost' to='alice@localhost/res1' id='4917656e-a5cb-4f4a-9718-ed525a1202ee'>
    <result queryid='last_page_after_id4' xmlns='urn:xmpp:mam:1' id='BO7CUD0L8B81'>
        <forwarded xmlns='urn:xmpp:forward:0'>
            <delay xmlns='urn:xmpp:delay' stamp='2022-06-08T10:13:01.601837Z' from='alice@localhost/res1'/>
            <message from='alice@localhost/res1' xmlns='jabber:client' xml:lang='en' to='bob@localhost/res1' type='chat'>
                <body>Message #11</body>
            </message>
        </forwarded>
    </result>
</message>

...

<message from='alice@localhost' to='alice@localhost/res1' id='09987901-d53d-4b57-8b3c-5f3aaa2de99b'>
    <result queryid='last_page_after_id4' xmlns='urn:xmpp:mam:1' id='BO7CUD0U4301'>
        <forwarded xmlns='urn:xmpp:forward:0'>
            <delay xmlns='urn:xmpp:delay' stamp='2022-06-08T10:13:01.638156Z' from='alice@localhost/res1'/>
            <message from='alice@localhost/res1' xmlns='jabber:client' xml:lang='en' to='bob@localhost/res1' type='chat'>
                <body>Message #15</body>
            </message>
        </forwarded>
    </result>
</message>
<iq from='alice@localhost' to='alice@localhost/res1' id='req3' type='result'>
    <fin xmlns='urn:xmpp:mam:1'>
        <set xmlns='http://jabber.org/protocol/rsm'>
            <first index='6'>BO7CUD0L8B81</first> <!-- id of the message 11 -->
            <last>BO7CUD0U4301</last> <!-- id of the message 15 -->
            <count>11</count> <!-- messages 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 -->
        </set>
    </fin>
</iq>
```

Because index is not zero, the client would have to send more queries to get
all missing messages.

## Disable message counting

Sometimes, we don't want to count messages at all.
It would improve performance.

For example, if we want to request another page of the result set,
we already would know the total number of messages from the first query.

Sometimes, total and offset values are not visible in the UI.

```xml
<!-- Client sends -->
<iq type='set' id='req5'>
    <query xmlns='urn:xmpp:mam:1' queryid='before10'>
        <x xmlns='jabber:x:data'>
            <field var='simple'>
                <value>true</value>
            </field>
        </x>
        <set>
            <max>5</max>
            <before>BO7DD6KDP0O1</before>
        </set>
    </query>
</iq>

...skip messages...
<!-- Server returns messages and the final IQ -->
<iq from='alice@localhost' to='alice@localhost/res1' id='req5' type='result'>
    <fin xmlns='urn:xmpp:mam:1'>
        <set xmlns='http://jabber.org/protocol/rsm'>
            <first>BO7DD6K1E8G1</first>
            <last>BO7DD6KBAAG1</last>
        </set>
    </fin>
</iq>
```
