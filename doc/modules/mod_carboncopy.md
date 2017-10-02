### Module Description

##### Discovering Support
The server uses a disco query to inform if carbons are enabled.

##### Enabling and disabling Carbons from the client

Carbons are not enabled by default. 
Every client app has to enable carbons to get messages sent to other clients of the user. 
Carbons are enabled and disabled with an iq stanza with a child element -  `<enable xmlns='urn:xmpp:carbons:2'/>` or `<disable xmlns='urn:xmpp:carbons:2'/>`.

##### Receiving messages to a bare JID
Each message to a bare JID is forked and sent to all carbon enabled resources of the recipient, and not just to the highest priority resource. 
Sending multiple copies to same resource is avoided.

##### Receiving messages to full JID
Each directed message to a full JID is also forwarded to all carbon enabled resources of the recipient. 
The message is wrapped in the `<forwarded xmlns='urn:xmpp:forward:0'></forwarded>` tag and directed towards each carbon enabled resource.

##### Sending Messages
Just as when receiving messages to a full JID, each sent message is forwarded to all carbon enabled resources of recipient.
The message is wrapped in the `<forwarded xmlns='urn:xmpp:forward:0'></forwarded>` tag and is directed towards each carbon enabled resource.

##### Private Messages
Private messages are tagged `<private/>` and are not forwarded to any carbon enabled resource of the sender and recipient if the `to` attribute contains a full JID. 
However, if the message is sent to a bare JID, it is forked to all highest priority resources. 
This is not done through `mod_carboncopy` but is an expected outcome.

##### Multiple enable/disable requests
Multiple enable/disable requests are not treated as an error even if they come from the same resource.

##### Behavior with other modules
  * **mod_offline**: Offline messages are delivered as they are. 
   Since, only one resource can connect at a time and there will be a finite time delay between login from two resources, `mod_carboncopy` has no role to play and only one resource can receive offline messages. 
   Other resources can retrieve old messages from the archive.
  *  **mod_mam**: `mod_mam` covers only direct messages from one user to another. 
  All the forked messages for a message sent with a bare JID are ignored by `mod_mam`. 
  Similarly, all the carbon messages are also ignored by `mod_mam`.

##### Retrieving archive from multiple resources
A resource can retrieve archives of messages sent to a specific resource of a friend which will not contain any carbon messages. 
It will only contain messages directed towards that resource or messages sent with a bare jid when that resource was at the highest priority.
A request to `mod_mam` with a bare JID of the chosen user will retrieve all messages to them from any resource. 
There are no instances of copies of same messages being sent by `mod_mam`. 
This is because `mod_mam` does not archive carbon messages.

##### Testing with a client
The module and its behavior have been tested with `mod_offline` and `mod_mam` using a desktop client made in Java using the Smack library. 
The standard Smack library for carbons is able to unpack and read the carbon messages. 
Also, the standard library supports checking for carbon support by the server using disco and sending enable and disable requests for carbon messages.
A client needs to synchronize with `mod_offline` and `mod_mam`. 
Once a client is online and enables carbons, it will not receive all the messages. 
`mod_mam` does not capture any carbon messages so it does not send any duplicates during any archive request. 
Only the simple chat messages are archived and they can be accessed by using the bare JID of the user for whom the archive is requested.
For an Erlang-based test suite, please see [/esl/ejabberd_tests/blob/master/tests/carboncopy_SUITE.erl].

### Options

* **iqdisc** (default: no_queue)

### Example Configuration
` {mod_carboncopy, []} `
