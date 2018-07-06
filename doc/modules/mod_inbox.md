### Module Description

`Inbox` is an experimental feature implemented as a few seperate modules.
To use it, enable mod_inbox in the config file.

### Options

* **backend** (atom, default: `odbc`) - Database backend to use. For now, only `odbc` is supported.
* **reset_markers** (list, default: `[displayed]`) - List of atom chat markers that when sent, will reset the unread message counter for a conversation.
This works when [Chat Markers](https://xmpp.org/extensions/xep-0333.html) are enabled on the client side.
Possible values are from the set: `displayed`, `received`, `acknowledged`. Setting as empty list (not recommended) means that no chat marker can decrease the counter value.
* **groupchat** (list, default: `[muclight]`) - The list indicating which groupchats will be included in inbox.
Possible value is `muclight` [Multi-User Chat Light](https://xmpp.org/extensions/inbox/muc-light.html).
Soon the classic [Multi-User Chat](https://xmpp.org/extensions/xep-0045.html) will be supported.
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

<iq type="get" id="10bca">
<inbox xmlns=”erlang-solutions.com:xmpp:inbox:0” queryid="b6"/>
</iq>


Alice receives:

<message from="alicE@localhost" to="alicE@localhost" id="9b759">
<result xmlns=”erlang-solutions.com:xmpp:inbox:0” unread="0" queryid="b6">
<forwarded xmlns=”urn:xmpp:forward:0”>
<message xml:lang="en" type="chat" to="bOb@localhost/res1" from="alicE@localhost/res1" id=”123”>
<body>Hello</body>
</message>
</forwarded>
</result>
</message>

<iq from="alicE@localhost" to="alicE@localhost/res1" id="10bca" type="result">
<count xmlns='erlang-solutions.com:xmpp:inbox:0'>1</count>
</iq>

```


### Example Configuration

```
{mod_inbox, [{backend, odbc},
             {reset_markers, [displayed]},
             {aff_changes, true},
             {remove_on_kicked, true},
             {groupchat, [muclight]}
            ]},
```

