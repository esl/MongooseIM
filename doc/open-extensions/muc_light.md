## 1. Introduction

Classic Multi-User chat, as described in XEP-0045, adds an IRC-like functionality to XMPP.
It distinguishes between the affiliation list and the occupant list, where the latter is based on presences routed to the room from the client resource.
While perfectly sufficient for desktop applications and relatively stable network connection, it does not exactly meet the challenges the mobile world it is facing.
Modern mobile applications do not rely on presence information, as it can frequently change. The expected user experience not only differs from the IRC model, but also uses only a small subset of XEP-0045 features.
The service described in this specification attempts to provide a complete solution for all common use cases of mobile group chats.

## 2. Requirements

Here are some high-level features required from a new variant of MUC

1. The service allows any user to create a room for group communication.
1. Users cannot join rooms on their own. They have to be added by the room owner or (if configured by service administrator) any other occupant.
1. Only the owner can remove other occupants from the room.
1. Every occupant can leave the room.
1. A user may block the attempts of being added to the specific room or by specific user.
1. The message sent in the room is always broadcasted to every occupant.
1. The full occupant list is always available to all occupants.
1. The occupant is always visible on the list, even if they do not have any resources online.
1. Occupants can only have two affiliations: owner and member.
1. There MUST be at least one owner in the room (the service can choose to treat all users equally).
1. If the room becomes empty, it is destroyed.
1. Occupants cannot hide behind nicks. Their real bare JID is always visible to everyone.
1. No exchange of any `<presence/>` stanza inside the room.
1. The user MUST be able to retrieve the list of rooms they occupy.
1. The owner can modify the room configuration at any time; members may also be allowed to set configuration.
1. All occupants can get the full room configuration at any time.
1. Room history is available only in Message Archive Management.

## 3. Entity Use Cases

### 3.1. Discovering a MUC Light Service

An entity often discovers a MUC service by sending a Service Discovery items ("disco#items") request to its own server.

**Entity Queries the Server for Associated Services**

```xml
<iq from='hag66@shakespeare.lit/pda'
    id='h7ns81g'
    to='shakespeare.lit'
    type='get'>
    <query xmlns='http://jabber.org/protocol/disco#items'/>
</iq>
```

The server then returns the services that are associated with it.

**Server Returns a Disco Items Result**

```xml
<iq from='shakespeare.lit'
    id='h7ns81g'
    to='hag66@shakespeare.lit/pda'
    type='result'>
    <query xmlns='http://jabber.org/protocol/disco#items'>
        <item jid='muclight.shakespeare.lit' name='MUC Light Service'/>
    </query>
</iq>
```

### 3.2. Discovering the Features Supported by a MUC Light Service

An entity may wish to discover if a service implements the Multi-User Chat protocol; in order to do so, it sends a service discovery information ("disco#info") query to the MUC service's JID.

**Entity Queries Chat Service for MUC Light Support via Disco**

```xml
<iq from='hag66@shakespeare.lit/pda'
    id='lx09df27'
    to='muclight.shakespeare.lit' type='get'>
    <query xmlns='http://jabber.org/protocol/disco#info'/>
</iq>
```

The service MUST return its identity and the features it supports.

**Service Returns a Disco Info Result**

```xml
<iq from='muclight.shakespeare.lit'
    id='lx09df27'
    to='hag66@shakespeare.lit/pda'
    type='result'>
    <query xmlns='http://jabber.org/protocol/disco#info'>
        <identity category='conference' name='Shakespearean Chat Service' type='text'/>
        <feature var='urn:xmpp:muclight:0'/>
    </query>
</iq>
```

### 3.3. Discovering Occupied Rooms

The service discovery items ("disco#items") protocol enables an entity to query a service for a list of associated items, which in the case of a chat service would consist of the specific chat rooms the entity occupies.

**Entity Queries Chat Service for Rooms**

```xml
<iq from='hag66@shakespeare.lit/pda'
    id='zb8q41f4'
    to='muclight.shakespeare.lit'
    type='get'>
    <query xmlns='http://jabber.org/protocol/disco#items'/>
</iq>
```

The service MUST return a full list of the rooms the entity occupies. The server SHOULD include room name and version in each item.

**Service Returns a Disco Items Result**

```xml
<iq from='muclight.shakespeare.lit'
    id='zb8q41f4'
    to='hag66@shakespeare.lit/pda'
    type='result'>
    <query xmlns='http://jabber.org/protocol/disco#items'>
        <item jid='heath@muclight.shakespeare.lit' name='A Lonely Heath' version='1'/>
        <item jid='coven@muclight.shakespeare.lit' name='A Dark Cave' version='2'/>
        <item jid='forres@muclight.shakespeare.lit' name='The Palace' version='3'/>
        <item jid='inverness@muclight.shakespeare.lit'
              name='Macbeth&apos;s Castle'
              version='4'/>
    </query>
</iq>
```

If the full list of rooms is large (see XEP-0030 for details), the service MAY return only a partial list of rooms. If it does, it MUST include a `<set/>` element qualified by the 'http://jabber.org/protocol/rsm' namespace (as defined in Result Set Management (XEP-0059) [1]) to indicate that the list is not the full result set.

**Service Returns a Limited List of Disco Items Result**

```xml
<iq from='muclight.shakespeare.lit'
    id='hx51v49s'
    to='hag66@shakespeare.lit/pda'
    type='result'>
    <query xmlns='http://jabber.org/protocol/disco#items'>
        <item jid='alls-well-that-ends-well@muclight.shakespeare.lit'
              name='Everybody dies'
              version='1'/>
        <item jid='as-you-like-it@muclight.shakespeare.lit'
              name='As you like it'
              version='2'/>
        <item jid='cleopatra@muclight.shakespeare.lit' name='Cleo fans' version='3'/>
        <item jid='comedy-of-errors@muclight.shakespeare.lit'
              name='404 Comedy not found'
              version='4'/>
        <item jid='coriolanus@muclight.shakespeare.lit'
              name='What is Coriolanus?'
              version='5'/>
        <item jid='cymbeline@muclight.shakespeare.lit' name='Music room' version='6'/>
        <item jid='hamlet@muclight.shakespeare.lit'
              name='To chat or not to chat?'
              version='7'/>
        <item jid='henry-the-fourth-one@muclight.shakespeare.lit'
              name='Royal Room 1'
              version='8'/>
        <item jid='henry-the-fourth-two@muclight.shakespeare.lit'
              name='Royal Room 2'
              version='9'/>
        <item jid='henry-the-fifth@muclight.shakespeare.lit'
              name='Royal Room Prime'
              version='10'/>
        <set xmlns='http://jabber.org/protocol/rsm'>
            <first index='0'>alls-well-that-ends-well@muclight.shakespeare.lit</first>
            <last>henry-the-fifth@muclight.shakespeare.lit</last>
            <count>37</count>
        </set>
    </query>
</iq>
```

## 4. Occupant Use Cases

### 4.1. Sending a message to a room

Every occupant in the room MAY broadcast messages to other occupants. In order to do so, the client MUST send a groupchat message to the room bare JID.

The room automatically assumes that occupants' nicks are equal to their bare JIDs. MUC light is designed for applications where it is not important to hide behind nicknames. On the contrary - it is up to the client to replace pure JIDs with user-friendly names like phone numbers or full names if necessary.

The room MUST route all messages of the 'groupchat' type.

**Client sends a message to the room**

```xml
<message from='hag66@shakespeare.lit/pda'
         id='msg111'
         to='coven@muclight.shakespeare.lit'
         type='groupchat'>
    <body>Harpier cries: 'tis time, 'tis time.</body>
</message>
```

**Server broadcasts a groupchat message**

```xml
<message id='msg111' type='groupchat'
    from='coven@muclight.shakespeare.lit/hag66@shakespeare.lit'
    to='crone1@shakespeare.lit'>
    <body>Harpier cries: 'tis time, 'tis time.</body>
</message>
```

```xml
<message id='msg111' type='groupchat'
    from='coven@muclight.shakespeare.lit/hag66@shakespeare.lit'
    to='crone2@shakespeare.lit'>
    <body>Harpier cries: 'tis time, 'tis time.</body>
</message>
```

Note the message is sent to all the room occupants including the original sender.

```xml
<message id='msg111' type='groupchat'
    from='coven@muclight.shakespeare.lit/hag66@shakespeare.lit'
    to='hag66@shakespeare.lit'>
    <body>Harpier cries: 'tis time, 'tis time.</body>
</message>
```

### 4.2. Changing a room subject

The service MAY allow room occupants to set the room subject by changing the "subject" configuration field. A standard configuration stanza is used in this case. Subject change is announced like an ordinary configuration change.

**Client sends a message to the room**

```xml
<iq from='hag66@shakespeare.lit/pda'
    id='subject1'
    to='coven@muclight.shakespeare.lit'
    type='set'>
    <query xmlns='urn:xmpp:muclight:0#configuration'>
        <subject>To be or not to be?</subject>
    </query>
</iq>
```

```xml
<message from='coven@muclight.shakespeare.lit'
         to='crone1@shakespeare.lit'
         type='groupchat'
         id='newsubject'>
    <x xmlns='urn:xmpp:muclight:0#configuration'>
        <prev-version>asdfghj000</prev-version>
        <version>asdfghj</version>
        <subject>To be or not to be?</subject>
    </x>
    <body />
</message>
```

```xml
<message from='coven@muclight.shakespeare.lit'
         to='hag66@shakespeare.lit'
         type='groupchat'
         id='newsubject'>
    <x xmlns='urn:xmpp:muclight:0#configuration'>
        <prev-version>asdfghj000</prev-version>
        <version>asdfghj</version>
        <subject>To be or not to be?</subject>
    </x>
    <body />
</message>
```

```xml
<iq to='hag66@shakespeare.lit/pda'
    id='subject1'
    from='coven@muclight.shakespeare.lit'
    type='result' />
```

### 4.3. Requesting room information

Room occupants may request room information (configuration and/or occupants list) by an information version. It is up to the service to define the version string, the only requirement for it, is to be unique per room. Please note there are no separate versions for configuration and occupant list alone.

If the server side version does not match the one provided by the client (or if the client does not provide one, i.e. the 'version' element is empty), the service MUST respond with a current version string and full configuration and/or occupant list.

If the version strings match, server MUST reply with an empty result.

Only room occupants can get room information.

**Matching versions**

```xml
<iq from='crone1@shakespeare.lit/desktop'
    id='config0'
    to='coven@muclight.shakespeare.lit'
    type='get'>
    <query xmlns='urn:xmpp:muclight:0#configuration'>
        <version>abcdefg</version>
    </query>
</iq>
```

```xml
<iq from='coven@muclight.shakespeare.lit'
    id='config0'
    to='crone1@shakespeare.lit/desktop'
    type='result' />
```

#### 4.3.1. Getting the room configuration

**Client gets configuration from the server**

```xml
<iq from='crone1@shakespeare.lit/desktop'
    id='getconfig1'
    to='coven@muclight.shakespeare.lit'
    type='get'>
    <query xmlns='urn:xmpp:muclight:0#configuration'>
        <version>abcdefg</version>
    </query>
</iq>
```

```xml
<iq from='coven@muclight.shakespeare.lit'
    id='getconfig1'
    to='crone1@shakespeare.lit/desktop'
    type='result'>
    <query xmlns='urn:xmpp:muclight:0#configuration'>
        <version>123456</version>
        <roomname>A Dark Cave</roomname>
    </query>
</iq>
```

#### 4.3.2. Requesting a user list

**Client requests a user list**

```xml
<iq from='crone1@shakespeare.lit/desktop'
    id='getmembers'
    to='coven@muclight.shakespeare.lit'
    type='get'>
    <query xmlns='urn:xmpp:muclight:0#affiliations'>
        <version>abcdefg</version>
    </query>
</iq>
```

```xml
<iq from='coven@muclight.shakespeare.lit'
    id='getmembers'
    to='crone1@shakespeare.lit/desktop'
    type='result'>
    <query xmlns='urn:xmpp:muclight:0#affiliations'>
        <version>123456</version>
        <user affiliation='owner'>user1@shakespeare.lit</user>
        <user affiliation='member'>user2@shakespeare.lit</user>
        <user affiliation='member'>user3@shakespeare.lit</user>
    </query>
</iq>
```

#### 4.3.3. Requesting full room information

Room occupants may request both lists (configuration + occupants) with a single request.

**Client requests room information**

```xml
<iq from='crone1@shakespeare.lit/desktop'
    id='getinfo1'
    to='coven@muclight.shakespeare.lit'
    type='get'>
    <query xmlns='urn:xmpp:muclight:0#info'>
        <version>abcdefg</version>
    </query>
</iq>
```

```xml
<iq from='coven@muclight.shakespeare.lit'
    id='getinfo1'
    to='crone1@shakespeare.lit/desktop'
    type='result'>
    <query xmlns='urn:xmpp:muclight:0#info'>
        <version>123456</version>
        <configuration>
            <roomname>A Dark Cave</roomname>
        </configuration>
        <occupants>
            <user affiliation='owner'>user1@shakespeare.lit</user>
            <user affiliation='member'>user2@shakespeare.lit</user>
            <user affiliation='member'>user3@shakespeare.lit</user>
        </occupants>
    </query>
</iq>
```

### 4.4. Leaving the room

Every occupant is allowed to leave the room at any time. It is done by modifying their own affiliation.

**Occupant leaves the room**

```xml
<iq from='crone1@shakespeare.lit/desktop'
    id='leave1'
    to='coven@muclight.shakespeare.lit'
    type='set'>
    <query xmlns='urn:xmpp:muclight:0#affiliations'>
        <user affiliation='none'>crone1@shakespeare.lit</user>
    </query>
</iq>
```

```xml
<message from='coven@muclight.shakespeare.lit'
         to='crone1@shakespeare.lit'
         type='groupchat'
         id='leave1'>
    <x xmlns='urn:xmpp:muclight:0#affiliations'>
        <user affiliation='none'>crone1@shakespeare.lit</user>
    </x>
    <body />
</message>
```

```xml
<message from='coven@muclight.shakespeare.lit'
         to='hag77@shakespeare.lit'
         type='groupchat'
         id='leave1'>
    <x xmlns='urn:xmpp:muclight:0#affiliations'>
        <prev-version>1111111</prev-version>
        <version>aaaaaaa</version>
        <user affiliation='none'>crone1@shakespeare.lit</user>
    </x>
    <body />
</message>
```

```xml
<message from='coven@muclight.shakespeare.lit'
         to='hag88@shakespeare.lit'
         type='groupchat'
         id='leave1'>
    <x xmlns='urn:xmpp:muclight:0#affiliations'>
        <prev-version>1111111</prev-version>
        <version>aaaaaaa</version>
        <user affiliation='none'>crone1@shakespeare.lit</user>
    </x>
    <body />
</message>
```

```xml
<iq to='crone1@shakespeare.lit/desktop'
    id='leave1'
    from='coven@muclight.shakespeare.lit'
    type='result' />
```

### 4.5. Blocking functionality

A user MAY choose to automatically deny being added to the room. All stanzas must be directed to MUC Light service. User MAY send more than one item in a single request and mix both 'user' and 'room' elements.

If the occupant tries to add another user to the room, and this user has set a blocking policy, the server MUST ignore the attempt. No error is returned, this user is simply skipped when processing affiliation change query.

**Service denies adding blocking user**

```xml
<iq from='crone2@shakespeare.lit/desktop'
    id='blocked1'
    to='coven@muclight.shakespeare.lit'
    type='set'>
    <query xmlns='urn:xmpp:muclight:0#affiliations'>
        <user affiliation='member'>crone1@shakespeare.lit</user>
        <user affiliation='member'>crone3@shakespeare.lit</user>
    </query>
</iq>
```

```xml
<message from='coven@muclight.shakespeare.lit'
         to='crone2@shakespeare.lit'
         type='groupchat'
         id='blockedadd1'>
    <x xmlns='urn:xmpp:muclight:0#affiliations'>
        <user affiliation='member'>crone3@shakespeare.lit</user>
    </x>
    <body />
</message>
```

```xml
<message from='coven@muclight.shakespeare.lit'
         to='hag88@@shakespeare.lit'
         type='groupchat'
         id='blockedadd1'>
    <x xmlns='urn:xmpp:muclight:0#affiliations'>
        <user affiliation='member'>crone3@shakespeare.lit</user>
    </x>
    <body />
</message>
```


```xml
<iq to='crone2@shakespeare.lit/desktop'
    id='blocked1'
    from='coven@muclight.shakespeare.lit'
    type='result' />
```

#### 4.5.1. Requesting a blocking list

In order to get the current blocking list in the MUC Light service, the client sends an empty IQ get query with a proper namespace.

The list includes only items with a 'deny' action, since the 'allow' behaviour is default for MUC Light and is only used for the list modification.

**User retrieves a blocking list**

```xml
<iq from='crone1@shakespeare.lit/desktop'
    id='getblock1'
    to='muclight.shakespeare.lit'
    type='get'>
    <query xmlns='urn:xmpp:muclight:0#blocking'>
    </query>
</iq>
```

```xml
<iq type='result'
    id='getblock1'
    to='crone1@shakespeare.lit/desktop'
    from='muclight.shakespeare.lit'>
    <query xmlns='urn:xmpp:muclight:0#blocking'>
        <room action='deny'>coven@muclight.shakespeare.lit</room>
        <user action='deny'>hag77@shakespeare.lit</user>
    </query>
</iq>
```

#### 4.5.2. Blocking a room

In order to block a room, a query must contain at least one 'room' item with a 'deny' action and a room bare JID in the content.

**User blocks a room**

```xml
<iq from='crone1@shakespeare.lit/desktop'
    id='block1'
    to='muclight.shakespeare.lit'
    type='set'>
    <query xmlns='urn:xmpp:muclight:0#blocking'>
        <room action='deny'>coven@muclight.shakespeare.lit</room>
        <room action='deny'>chapel@shakespeare.lit</room>
    </query>
</iq>
```

```xml
<iq type='result'
    id='block1'
    to='crone1@shakespeare.lit/desktop'
    from='muclight.shakespeare.lit' />
```

#### 4.5.3. Blocking a user

In order to block a user, a query must contain at least one 'user' item with a 'deny' action and a user bare JID in the content.

**User blocks another user**

```xml
<iq from='crone1@shakespeare.lit/desktop'
    id='block2'
    to='muclight.shakespeare.lit'
    type='set'>
    <query xmlns='urn:xmpp:muclight:0#blocking'>
        <user action='deny'>hag66@shakespeare.lit</user>
        <user action='deny'>hag77@shakespeare.lit</user>
    </query>
</iq>
```

```xml
<iq type='result'
    id='block2'
    to='crone1@shakespeare.lit/desktop'
    from='muclight.shakespeare.lit' />
```

#### 4.5.4. Unblocking

In order to cancel a blocking, a query must contain at least one 'room' or 'user' item with an 'allow' action and an appropriate bare JID in the content.

Unblocking a JID that is not blocked does not trigger any error. The server MUST return an empty IQ result in such case.

**User cancels blocking**

```xml
<iq from='crone1@shakespeare.lit/desktop'
    id='unblock1'
    to='muclight.shakespeare.lit'
    type='set'>
    <query xmlns='urn:xmpp:muclight:0#blocking'>
        <room action='allow'>coven@muclight.shakespeare.lit</room>
        <user action='allow'>hag66@shakespeare.lit</user>
    </query>
</iq>
```

```xml
<iq type='result'
    id='unblock1'
    to='crone1@shakespeare.lit/desktop'
    from='muclight.shakespeare.lit' />
```

## 5. Owner Use Cases

### 5.1. Creating a new room

A room is created by submitting a dedicated stanza. The client application should pick a random room node name, since a human-readable room name is in configuration.

For rules that apply to the configuration options, please see "Setting room configuration" chapter.

The client MAY include initial configuration and occupant list (the list MUST NOT include the creator). The server MAY allow sending an incomplete configuration form. In such case the server MUST use the default values for missing fields. The server MAY enforce a minimal occupant list length.

The service MAY either give the creator the 'owner' or 'member' status. In the latter case all users are equal.

Upon room creation success, the service MUST reply with an empty IQ result.

The following rules (similar to the ones relevant to the affiliation change request) apply to the occupant list:

* 'none' affiliation cannot be used.
* All user bare JIDs must be unique
* At most one owner can be chosen. If none is chosen, the room creator will become "just" a 'member'.

After the room is created (but before receiving IQ result), new occupants (including the creator) receive `<message/>` from the room with their affiliations (the stanza MUST include only recipient's affiliation) and the initial room version. `<prev-version/>` element MUST NOT be included.

**Client requests room creation**

```xml
<iq from='crone1@shakespeare.lit/desktop'
    id='create1'
    to='coven@muclight.shakespeare.lit'
    type='set'>
    <query xmlns='urn:xmpp:muclight:0#create'>
        <configuration>
            <roomname>A Dark Cave</roomname>
        </configuration>
        <occupants>
            <user affiliation='member'>user1@shakespeare.lit</user>
            <user affiliation='member'>user2@shakespeare.lit</user>
        </occupants>
    </query>
</iq>
```

```xml
<message from='coven@muclight.shakespeare.lit'
         to='crone1@shakespeare.lit'
         type='groupchat'
         id='createnotif'>
    <x xmlns='urn:xmpp:muclight:0#affiliations'>
        <version>aaaaaaa</version>
        <user affiliation='owner'>crone1@shakespeare.lit</user>
    </x>
    <body />
</message>
```

```xml
<message from='coven@muclight.shakespeare.lit'
         to='user1@shakespeare.lit'
         type='groupchat'
         id='createnotif'>
    <x xmlns='urn:xmpp:muclight:0#affiliations'>
        <version>aaaaaaa</version>
        <user affiliation='member'>user1@shakespeare.lit</user>
    </x>
    <body />
</message>
```

```xml
<message from='coven@muclight.shakespeare.lit'
         to='user2@shakespeare.lit'
         type='groupchat'
         id='createnotif'>
    <x xmlns='urn:xmpp:muclight:0#affiliations'>
        <version>aaaaaaa</version>
        <user affiliation='member'>user2@shakespeare.lit</user>
    </x>
    <body />
</message>
```

```xml
<iq to='crone1@shakespeare.lit/desktop'
    id='create1'
    from='coven@muclight.shakespeare.lit'
    type='result' />
```

#### 5.1.1. Requesting a new room with a unique name

If a client would like to avoid a room JID conflict, it MAY request creating a new room with a server-side generated name, that is verfied to be unique. In order to do so, the client MUST send a creation request to service JID, not room bare JID. The IQ result will originate from the new room bare JID

The messages with affiliation change notifications MUST have the same ID as IQ set and result.

**Client requests room creation**

```xml
<iq from='crone1@shakespeare.lit/desktop'
    id='createrandom'
    to='muclight.shakespeare.lit'
    type='set'>
    <query xmlns='urn:xmpp:muclight:0#create'>
        <configuration>
            <roomname>Random Cave</roomname>
        </configuration>
    </query>
</iq>
```

```xml
<message from='randomcave@muclight.shakespeare.lit'
         to='crone1@shakespeare.lit'
         type='groupchat'
         id='createrandom'>
    <x xmlns='urn:xmpp:muclight:0#affiliations'>
        <version>aaaaaaa</version>
        <user affiliation='owner'>crone1@shakespeare.lit</user>
    </x>
    <body />
</message>
```


```xml
<iq to='crone1@shakespeare.lit/desktop'
    id='createrandom'
    from='muclight.shakespeare.lit'
    type='result' />
```

#### 5.1.2. Room already exists

If the chosen room name already exists, the service MUST return a 'conflict' error.

**Client requests room creation with existing name**

```xml
<iq from='crone1@shakespeare.lit/desktop'
    id='conflict1'
    to='castle@muclight.shakespeare.lit'
    type='set'>
    <query xmlns='urn:xmpp:muclight:0#create'>
        <configuration>
            <roomname>A Dark Cave</roomname>
        </configuration>
    </query>
</iq>
```

```xml
<iq to='crone1@shakespeare.lit/desktop'
    id='conflict1'
    from='castle@muclight.shakespeare.lit'
    type='error'>
    <error type='cancel'>
        <conflict xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
    </error>
</iq>
```

### 5.2. Destroying a room

A room is automatically destroyed when its occupant list becomes empty or the room owner explicitly sends an IQ with a room destroy request.

Before sending an IQ result, every occupant is notified that its affiliation has changed to 'none'. These notifications include an `<x/>` element qualified with a "urn:xmpp:muclight:0#destroy" namespace.

Only the room owner is allowed to destroy it.

Room destruction notification SHOULD NOT contain version (or "prev-version" information).

**Client requests room destruction**

```xml
<iq from='crone1@shakespeare.lit/desktop'
    id='destroy1'
    to='coven@muclight.shakespeare.lit'
    type='set'>
    <query xmlns='urn:xmpp:muclight:0#destroy' />
</iq>
```

```xml
<message from='coven@muclight.shakespeare.lit'
         to='crone1@shakespeare.lit'
         type='groupchat'
         id='destroynotif'>
    <x xmlns='urn:xmpp:muclight:0#affiliations'>
        <user affiliation='none'>crone1@shakespeare.lit</user>
    </x>
    <x xmlns='urn:xmpp:muclight:0#destroy' />
    <body />
</message>
```

```xml
<message from='coven@muclight.shakespeare.lit'
         to='hag77@shakespeare.lit'
         type='groupchat'
         id='destroynotif'>
    <x xmlns='urn:xmpp:muclight:0#affiliations'>
        <user affiliation='none'>hag77@shakespeare.lit</user>
    </x>
    <x xmlns='urn:xmpp:muclight:0#destroy' />
    <body />
</message>
```

```xml
<message from='coven@muclight.shakespeare.lit'
         to='hag88@shakespeare.lit'
         type='groupchat'
         id='destroynotif'>
    <x xmlns='urn:xmpp:muclight:0#affiliations'>
        <user affiliation='none'>hag88@shakespeare.lit</user>
    </x>
    <x xmlns='urn:xmpp:muclight:0#destroy' />
    <body />
</message>
```

```xml
<iq to='crone1@shakespeare.lit/desktop'
    id='create1'
    from='coven@muclight.shakespeare.lit'
    type='result' />
```

### 5.3. Setting room configuration

Only room owners can modify the room configuration but the service MAY allow members to change it too.

All room occupants MUST be notified about a configuration change and both the new and old room version string (`<version />` and `<prev-version />` respectively).

"version" and "prev-version" configuration field names are NOT ALLOWED - they are reserved for room versioning.

The service MAY allow the client to set the configuration fields with any name but it is NOT RECOMMENDED.

The Data Forms are not used for the configuration. Instead, the config fields are encoded in XML elements with names equal to the key and content equal to the value.

**Client configuration request to the server**

```xml
<iq from='crone1@shakespeare.lit/desktop'
    id='conf2'
    to='coven@muclight.shakespeare.lit'
    type='set'>
    <query xmlns='urn:xmpp:muclight:0#configuration'>
        <roomname>A Darker Cave</roomname>
    </query>
</iq>
```

```xml
<message from='coven@muclight.shakespeare.lit'
         to='crone1@shakespeare.lit'
         type='groupchat'
         id='configchange'>
    <x xmlns='urn:xmpp:muclight:0#configuration'>
        <prev-version>zaqwsx</prev-version>
        <version>zxcvbnm</version>
        <roomname>A Darker Cave</roomname>
    </x>
    <body />
</message>
```

```xml
<message from='coven@muclight.shakespeare.lit'
         to='hag66@shakespeare.lit'
         type='groupchat'
         id='configchange'>
    <x xmlns='urn:xmpp:muclight:0#configuration'>
        <prev-version>zaqwsx</prev-version>
        <version>zxcvbnm</version>
        <roomname>A Darker Cave</roomname>
    </x>
    <body />
</message>
```

```xml
<iq to='crone1@shakespeare.lit/desktop'
    id='conf2'
    from='coven@muclight.shakespeare.lit'
    type='result' />
```

The server SHOULD accept incomplete (i.e. delta) configuration forms. In such case, values of the missing fields SHOULD be preserved.

### 5.4. Changing the occupant list

The occupant list is modified by a direct affiliation change. Following rules apply:

1. There are only 3 affiliations.
    * owner - can do everything in the room
    * member - can send messages to the room and if the service allows it, can also change configuration or change others' affiliations
    * none - not in the room; it's a keyword for marking a user for removal from a room
1. Every occupant can change its own affiliation to none in order to leave the room.
1. The only way to join the room is being added by other occupant.
1. The owner can change affiliations at will.
1. If the owner leaves, the server MAY use any strategy to choose a new one or reject the change.
1. The room MAY be configured to have either a single owner or multiple owners. When configured for a single owner, giving someone else the 'owner' status effectively causes the current owner to lose it.
1. The owner can choose a new owner when leaving by including both 'none' and 'owner' items in affiliation change request.
1. Every user JID can be used in the request at most once.
1. A single request MAY change multiple affiliations.
1. All changes must be meaningful, e.g. setting member's affiliation to 'member' is considered a bad request.
1. Server MAY allow members to add new members but they still cannot make anyone an 'owner' or remove other users from the room.
1. On success the server will reply with a result IQ with all the changed items. BEFORE returning the IQ result, the service MUST route a message with the affiliation change to all relevant users.

Newcomers, i.e. users that were not occupants before the change, SHOULD receive only their own affiliation and SHOULD NOT receive a `<prev-version />` element.

The notifications must include both the new and old room version (`<version />` and `<prev-version />` respectively) string (except for the ones directed to users that have been removed from the room).

The notifications contain a list of items. The item list may be different from the list in the IQ set, because some of the changes may require additional operations, e.g. choosing new owner when the old one leaves. Users, that are still in the room after the change, will receive the full change list. Users, that have been removed from the room with the request, will get only one item: themselves with affiliation 'none'.

**Affiliations change request**

Let's consider a room `coven` with following members:

* `crone1` - `owner`
* `hag77` - `member`
* `hag88` - `member`

`hag66` is not in the room yet.

User `crone1` wants to add `hag66` to the room, kick `hag88` out and make `hag77` the room owner.

```xml
<iq from='crone1@shakespeare.lit/desktop'
    id='member1'
    to='coven@muclight.shakespeare.lit'
    type='set'>
    <query xmlns='urn:xmpp:muclight:0#affiliations'>
        <user affiliation='member'>hag66@shakespeare.lit</user>
        <user affiliation='owner'>hag77@shakespeare.lit</user>
        <user affiliation='none'>hag88@shakespeare.lit</user>
    </query>
</iq>
```

Now each user will receive an update.
As you can see, affiliations have changed accordingly to `crone1` request.
However, this request implies one more update.
Since `hag77` has been promoted to a new owner, `crone1` is automatically degraded to `member`.

```xml
<message from='coven@muclight.shakespeare.lit'
         to='crone1@shakespeare.lit'
         type='groupchat'
         id='memberchange'>
    <x xmlns='urn:xmpp:muclight:0#affiliations'>
        <prev-version>njiokm</prev-version>
        <version>qwerty</version>
        <user affiliation='member'>crone1@shakespeare.lit</user>
        <user affiliation='member'>hag66@shakespeare.lit</user>
        <user affiliation='owner'>hag77@shakespeare.lit</user>
        <user affiliation='none'>hag88@shakespeare.lit</user>
    </x>
    <body></body>
</message>
```

Because `hag66` was not a member of this room before, they only receive **their own affiliation** and **no prev-version** element.

```xml
<message from='coven@muclight.shakespeare.lit'
         to='hag66@shakespeare.lit'
         type='groupchat'
         id='memberchange'>
    <x xmlns='urn:xmpp:muclight:0#affiliations'>
        <version>qwerty</version>
        <user affiliation='member'>hag66@shakespeare.lit</user>
    </x>
    <body></body>
</message>
```

`hag77` receives an ordinary update, just like `crone1`.

```xml
<message from='coven@muclight.shakespeare.lit'
         to='hag77@shakespeare.lit'
         type='groupchat'
         id='memberchange'>
    <x xmlns='urn:xmpp:muclight:0#affiliations'>
        <prev-version>njiokm</prev-version>
        <version>qwerty</version>
        <user affiliation='member'>crone1@shakespeare.lit</user>
        <user affiliation='member'>hag66@shakespeare.lit</user>
        <user affiliation='owner'>hag77@shakespeare.lit</user>
        <user affiliation='none'>hag88@shakespeare.lit</user>
    </x>
    <body></body>
</message>
```

`hag88` has been kicked out of the room and therefore gets only their own affiliation change of type 'none'.

```xml
<message from='coven@muclight.shakespeare.lit'
         to='hag88@shakespeare.lit'
         type='groupchat'
         id='memberchange'>
    <x xmlns='urn:xmpp:muclight:0#affiliations'>
        <user affiliation='none'>hag88@shakespeare.lit</user>
    </x>
    <body></body>
</message>
```

`crone1` gets the result IQ after the change.
```xml
<iq to='crone1@shakespeare.lit/desktop'
    id='member1'
    from='coven@muclight.shakespeare.lit'
    type='result' />
```

## 6. Interactions with RFCs and other XEPs

### 6.1. User rosters

The service MAY add user's rooms to its roster. It allows the client to skip the separate Disco request to the service.
Roster items with rooms MUST belong to the group "urn:xmpp:muclight:0" (MUC Light namespace) and include the `<version/>` element.
Their subscription type MUST be 'to'.

**Entity requests the roster and receives a reply that includes a room item**

```xml
<iq type='get' id='roster1' to='shakespeare.lit'>
    <query xmlns='jabber:iq:roster'/>
</iq>
```

```xml
<iq id='roster1' to='hag66@shakespeare.lit/tablet' type='result'>
    <query xmlns='jabber:iq:roster' ver='ver7'>
        <item jid='hag77@shakespeare.lit' subscription='both'/>
        <item jid='hag88@shakespeare.lit' subscription='both'/>
        <item jid='coven@muclight.shakespeare.lit' name='The Coven' subscription='to'>
            <group>urn:xmpp:muclight:0</group>
            <version>1234345</version>
        </item>
    </query>
</iq>
```

### 6.2. XEP-0313 Message Archive Management

This section defines the rules for archiving MUC Light events and messages.
Stanzas described in the subsections below MUST be archived by the server.
The stanzas not included here MUST NOT be archived.

The `<message/>` element inside `<forwarded/>` MUST include a "from" attribute and MUST NOT include a "to" attribute.
"id" SHOULD be archived as well.

In case of regular groupchat messages,
the "from" attribute MUST consist of a room full JID with a sender bare JID in the resource part.
As for room notification, e.g. create event, "from" MUST be equal to room bare JID.

Examples below use MAM v0.4 protocol.
The archive can be fetched only from a specific room, the client MUST NOT query MUC Light service directly.

#### 6.2.1. Groupchat message from occupant

Message from a user MUST be archived with all child elements.

**Occupant queries MAM and receives regular groupchat message**

```xml
<iq type='set' id='mamget1' to='coven@muclight.shakespeare.lit'>
    <query xmlns='urn:xmpp:mam:1' queryid='f27' />
</iq>
```

```xml
<message id='aeb213' to='hag66@shakespeare.lit/pda'>
    <result xmlns='urn:xmpp:mam:1' queryid='f27' id='28482-98726-73623'>
        <forwarded xmlns='urn:xmpp:forward:0'>
            <delay xmlns='urn:xmpp:delay' stamp='2010-07-10T23:08:25Z'/>
            <message from="coven@muclight.shakespeare.lit/hag77@shakespeare.lit"
                     id="msgid11">
                <body>Welcome!</body>
                <x xmlns="elixir:ingredient">bat-wing</x>
            </message>
        </forwarded>
    </result>
</message>
```

```xml
<iq type='result' id='mamget1' from='coven@muclight.shakespeare.lit'/>
```

#### 6.2.2. Affiliation change

Every archived affiliation change notification MUST include the `<version/>` element and MUST NOT contain the `<prev-version/>` element.

**Occupant queries MAM and receives an affiliation change notification**

```xml
<iq type='set' id='mamget2' to='muclight.shakespeare.lit'>
    <query xmlns='urn:xmpp:mam:1' queryid='f37' />
</iq>
```

```xml
<message id='aef2133' to='hag66@shakespeare.lit/pda'>
    <result xmlns='urn:xmpp:mam:1' queryid='f37' id='21482-98726-71623'>
        <forwarded xmlns='urn:xmpp:forward:0'>
            <delay xmlns='urn:xmpp:delay' stamp='2013-07-10T21:08:25Z'/>
            <message from="coven@muclight.shakespeare.lit" id="notifid11">
                <x xmlns='urn:xmpp:muclight:0#affiliations'>
                    <version>b9uf13h98f13</version>
                    <user affiliation='owner'>hag66@shakespeare.lit</user>
                    <user affiliation='member'>user1@shakespeare.lit</user>
                    <user affiliation='member'>user2@shakespeare.lit</user>
                </x>
            </message>
        </forwarded>
    </result>
</message>
```

```xml
<iq type='result' id='mamget12'/>
```

#### 6.2.3. Room creation

Room creation is archived as an affiliation change that includes ALL initial occupants (including the room creator).

## 7. General Error Cases

### 7.1. Client sends an unauthorized stanza to a room

If a client sends a stanza to the room, that it does not occupy, the service MUST reply with the 'item-not-found' error.

**Unauthorized IQ**

```xml
<iq from='crone1@shakespeare.lit/desktop'
    id='member1'
    to='coven@muclight.shakespeare.lit'
    type='set'>
    <query xmlns='urn:xmpp:muclight:0#affiliations'>
        <user affiliation='member'>hag66@shakespeare.lit</user>
    </query>
</iq>
```

```xml
<iq to='crone1@shakespeare.lit/desktop'
    id='member1'
    from='coven@muclight.shakespeare.lit'
    type='error'>
    <error type='cancel'>
        <item-not-found xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
    </error>
</iq>
```

**Unauthorized message**

```xml
<message from='hag66@shakespeare.lit/pda'
         id='unauth2'
         to='coven@muclight.shakespeare.lit'
         type='groupchat'>
    <body>Harpier cries: 'tis time, 'tis time.</body>
</message>
```

```xml
<message to='hag66@shakespeare.lit/pda'
         id='unauth2'
         from='coven@muclight.shakespeare.lit'
         type='error'>
    <error type='cancel'>
        <item-not-found xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
    </error>
</message>
```

### 7.2. Client sends a &lt;presence/> stanza to the service

The service MUST ignore all `<presence/>` stanzas sent by the client.

### 7.3. Client sends an invalid stanza to the service

If service receives an invalid stanza it MUST reply with a 'bad-request' error.

**Invalid IQ**

```xml
<iq from='crone1@shakespeare.lit/desktop'
    id='bad1'
    to='coven@muclight.shakespeare.lit'
    type='set'>
    <query xmlns='urn:xmpp:muclight:0#affiliations'>
        <item role='participant'>hag66@shakespeare.lit</item>
    </query>
</iq>
```

```xml
<iq to='crone1@shakespeare.lit/desktop'
    id='bad1'
    from='coven@muclight.shakespeare.lit'
    type='error'>
    <error type='modify'>
        <bad-request xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
    </error>
</iq>
```

**Invalid message**

```xml
<message from='hag66@shakespeare.lit/pda'
         id='bad2'
         to='coven@muclight.shakespeare.lit'
         type='chat'>
    <body>Harpier cries: 'tis time, 'tis time.</body>
</message>
```

```xml
<message to='hag66@shakespeare.lit/pda'
         id='bad2'
         from='coven@muclight.shakespeare.lit'
         type='error'>
    <error type='modify'>
        <bad-request xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
    </error>
</message>
```

### 7.4. Request sender has insufficient privileges

If the request sender does not have sufficient privileges (but is a room occupant),
the service MUST reply with a 'not-allowed' error.

It occurs in the following cases:

* A member tries to change the configuration but the service is not configured to allow it.
  It does not apply to the subject change,
  although it has to be performed by sending `<message/>` with `<subject/>`,
  not configuration `<iq/>`.
* A member tries to change anyone's affiliation to 'none' or 'owner'.
* A member tries to change someone's affiliation to 'member' but the service is not configured to allow it.

**Prohibited IQ**

```xml
<iq from='minion@shakespeare.lit/desktop'
    id='privileges1'
    to='coven@muclight.shakespeare.lit'
    type='set'>
    <query xmlns='urn:xmpp:muclight:0#affiliations'>
        <user role='owner'>minion@shakespeare.lit</user>
    </query>
</iq>
```

```xml
<iq to='minion@shakespeare.lit/desktop'
    id='privileges1'
    from='coven@muclight.shakespeare.lit'
    type='error'>
    <error type='cancel'>
        <not-allowed xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
    </error>
</iq>
```

## 8. Implementation Notes

The MUC Light service may be abused by a malicious users,
e.g. due to replicating a single message for every room occupant.
The list below contains suggested configurable limits that SHOULD be implemented.

The service features that might vary depending on a specific application are included as well.

* Maximum number of rooms the user occupies.
* Blocking feature enabled/disabled.
* Room creator's initial affiliation: owner/member.
* Room configuration may be changed by owner/occupants.
* New members can be invited by owner/occupants.
* Maximal room size.
