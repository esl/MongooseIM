### Module Description

##### Discovering Support
Server informs in disco query if carbons are active on server.

##### Enabling and disabling Carbons from the client
Each client for a user can enable/disable carbons for itself by sending an iq stanza with a child element respectively `<enable xmlns='urn:xmpp:carbons:2'/>` or `<disable xmlns='urn:xmpp:carbons:2'/>`

##### Receiving messages to bare JID
Each message to bare JID is forked and sent to all carbon enabled resources of recipient apart from sending it to the highest priority resource. Sending multiple copies to same resource is avoided.

##### Receiving messages to full JID
Each directed message to a full JID is also forwarded to all carbon enabled resources of recipient. The message is wrapped in `<forwarded xmlns='urn:xmpp:forward:0'></forwarded>` tag and is directed towards each carbon enabled resource.

##### Sending Messages
Similar to receiving messages to full JID, each sent message is forwarded to all carbon enabled resources of recipient. The message is wrapped in `<forwarded xmlns='urn:xmpp:forward:0'></forwarded>` tag and is directed towards each carbon enabled resource.

##### Private Messages
Private messages with tag `<private/>` are not forwarded to any carbon enabled resource of sender and recipient if the `to` attribute contains a full JID. However, if the message is sent to bare JID then it is forked to all highest priority resources. This is not done through `mod_carboncopy` but is an expected outcome.

##### Multiple enable/disable requests
Multiple enable/disable requests even from same resource are not treated as error.

##### Behavior with other modules
  * **mod_offline**: Offline messages are delivered as it is. Since, only one resource can connect at a time and there will be a finite time delay between login from two resources, `mod_carboncopy` has no role to play and only one resource can receive offline messages. Other resources can retrieve old messages from the archive.
  *  **mod_mam**: `mod_mam` covers only direct messages from one user to another. All the forked messages for message that is sent with bare JID are ignored by `mod_mam`. Similarly, all the carbon messages are also ignored by `mod_mam`.

##### Retrieving archive from multiple resources
A resource can retrieve archives of messages sent to a specific resource of a friend which will not contain any carbon message. It will only contain message directed towards that resource or message sent with bare jid when that resource was at highest priority.
To retrieve all messages to a certain friend user from any resource, a request to `mod_mam` with bare JID of friend will retrieve all messages. There are no instances of copies of same messages being sent by `mod_mam`. This is because `mod_mam` does not archive carbon messages.

##### Testing with client
The module has been tested along with its behavior with `mod_offline` and `mod_mam` using a desktop client made in Java using Smack library. The standard Smack library for carbons is able to unpack and read the carbon messages. Also, the standard library also supports check for carbon support by server using disco and sending enable and disable requests for carbon messages.
A client needs to synchronize with `mod_offline` and `mod_mam`. Once a client is online and enables carbon, it will not receive all the messages. `mod_mam` does not capture any carbon messages so it does not send any duplicates during any archive request. Only the simple chat messages are archived and they can be accessed by using the bare JID of the friend for whom the archive is requested.
For an Erlang-based test suite, please see [/esl/ejabberd_tests/blob/master/tests/carboncopy_SUITE.erl].

### Options

None.

### Example Configuration
` {mod_carboncopy, []} `
