# Build a complete iOS messaging app using XMPPFramework

Read our blog posts:
* [Build a complete iOS messaging app using XMPPFramework - Tutorial Part 1](https://www.erlang-solutions.com/blog/build-a-complete-ios-messaging-app-using-xmppframework-tutorial-part-1.html)
* [Build a complete iOS messaging app using XMPPFramework - Part 2](https://www.erlang-solutions.com/blog/build-a-complete-ios-messaging-app-using-xmppframework-part-2.html)

## YAXT??! Yet another XMPP tutorial?

Well, this is going to be another tutorial, but I’m going to try to make it a little bit different.
This is an XMPP tutorial from an iOS developer’s perspective.
I’ll try to answer all the questions I had when I started working in this area.
This journey is going to go from no XMPP knowldege at all
to having a fully functional instant messaging iOS appusing this cool protocol.
We are going to be using the super awesome (yet overwhelming at the beginning…) [XMPPFramework](https://github.com/robbiehanson/XMPPFramework) library,
and the idea is also to also mix in some iOS concepts that you are going to need for your app.

## What’s XMPP?

From [Wikipedia](https://en.wikipedia.org/wiki/XMPP):
Extensible Messaging and Presence Protocol (XMPP) is a communications protocol for message-oriented middleware based on XML.

This basically means XMPP is a protocol for exchanging stuff.
What kind of stuff? Messages and presences.
We all know what messages are, but what about presences?
A presence is just a way of sharing a “status”, that’s it.
You can be ‘online’, 'offline’, 'having lunch’, or whatever you want.
Also there’s another important word: Extensible meaning it can grow.
It started as an instant messaging protocol
and it has grown into multiple fields for example IoT (Internet of Things).
And last, but not least:
every piece of information we are going to exchange under this protocol is going to be XML.
I can heard you complaining but…
Come on, it’s not that bad!

## Why do we need XMPP? Why not just REST?

Well what other options do we have?
On the one hand, a custom solution means building everything from scratch, that takes time.
On the other hand, we have XMPP, a super tested technology broadly used by millions of people every day,
so we can say that’s an advantage over a custom approach.

Everytime I talk about XMPP, someone asks me 'Why not just REST?’.
Well, there is a misconception here.
REST is not a protocol, it’s just a way of architecting a networked application;
it’s just a standarized way of doing something (that I love btw).
So let’s change the question to something that makes more sense:
“Why not just build a custom REST chat application?”.
The first thing that comes to my mind is what I already explained in the previous paragraph,
but there is something else.
How do I know when someone has sent me a message?
For XMPP this is trivial:
we have an open connection all the time so,
as soon as a message arrives to the server,
it will send us the message.
We have a full-duplex.
On the other hand, the only solution with REST is polling.
We will need to ask the server for new messages from time to time to see if there is something new for us.
That sucks.
So, we will have to add a mechanism that allows us to receive the messages as soon as they are created,
like SSE or WebSockets.

There is one more XMPP advantage over a custom REST chat application.
REST uses HTTP, an application level protocol that is built on top of a transport level protocol: TCP.
So everytime you want to use your REST solution,
you will need HTTP,
a protocol that is not always available everywhere (maybe you need to embed this in a cheap piece of hardware?).
Besides, we have XMPP built on top of TCP that’s going to be always available.

## What’s the basic stuff I need to know to get started?

Well, you know a lot already but let’s make a list. Lists are always good:

* XMPP is built on top of TCP. It keeps an open connection all the time.
* Client/Server architecture. Messages always go through a server.
* Everything we send and receive is going to be XML and it’s called Stanza.
* We have three different types of stanzas: iq, message and presence.
* Every individual on the XMPP network is univocally identified by a JID (Jabber ID).
* All the stanzas are cointained in a Stream. Let’s imagine the Stream as a white canvas where you and the server write the stanzas.
* Stream, iq, message and presence are the core of XMPP. You can find everything perfectly detailed in RFC6120
XMPP can be extended to accomplish different stuff. Each extension is called XEP (XMPP Extension Protocol).

## What’s a JID?

Jabber ID (JID) is how we univocally identify each individual in XMPP.
It is the address to where we are going to send our stanzas.

This is how a JID looks like:

* **localpart**: This is your username.
* **domainpart**: Server name where the **localpart** resides.
* **resourcepart**: This is optional, and it identifies a particular client for the user. For example: I can be logged in with `andres@erlang-solutions.com` on my iPhone, on my Android and on my mac at the same time… So all these will be the same **localpart** + **domainpart** but different **resourcepart**

I’m sure you have already noticed how similar the JID looks to a standard email address.
This is because you can connect multiple servers together and the messages are rooted to the right user in the right server,
just as email works.
Pretty cool, right?

Sometimes you will see we have a JID with just the domain part.
Why?!
Because it’s also possible to send stanzas to a service instead of a user.
A service?
What’s a service?!
Services are different pieces of an XMPP server that offer you some special functionality,
but don’t worry about this right now, just remember: you can have JIDs without a localpart.

## What’s a Stanza?

Stanza is the name of the XML pieces that we are going to be sending and receiving.
The defined stanzas are: `<message/>`, `<presence/>` and `<iq/>`.

### `<message/>`

This is a basic `<message/>` stanza.
Everytime you want to send a message to someone (a JID), you will have to send this stanza:

```xml
<message from='andres@erlang-solutions.com/iphone' to='juana@erlang-solutions.com' type='chat'>
    <body>Hey there!</body>
</message>
 ```
 
### `<iq/>`

It stands for Info/Query.
It’s a query-action mechanism, you send an `iq` and you will get a response to that query.
You can pair the `iq-query` with the `iq-response` using the stanza id.

For example, we send an `iq` to the server to do something
(don’t pay attention to what we want to do… you just need to know there is an `iq` stanza and how the mechanism works):

```xml
<iq to='erlang-solutions.com' type='get' id='1'>
  <query xmlns='http://jabber.org/protocol/disco#items'/>
</iq>
```

And we get back another `iq` with the same id with the result of the previous query:

```xml
<iq from='erlang-solutions.com' to='ramabit@erlang-solutions.com/Andress-MacBook-Air' id='1' type='result'>
    <query xmlns='http://jabber.org/protocol/disco#items'>
        <item jid='muc.erlang-solutions.com'/>
        <item jid='muclight.erlang-solutions.com'/>
        <item jid='pubsub.erlang-solutions.com'/>
    </query>
</iq>
```

### `<presence/>`

Used to exchange presence information, as you could have imagined.
Usually presences are sent from the client to the server and broadcasted by it.
The most basic, yet valid presence, to indicate to the server that a user is avaiable is:

```xml
<presence/>
```

After a sucessfull connection,
you are not going to receive any `<message/>` until you make yourself available sending the previous presence.

If you want to make yourself unavailable, you just have to send:

```xml
<presence type="unavailable"></presence>
```

If we want to make the presences more useful, we can send something like this:

```xml
<presence>
      <status>On vacation</status>
</presence>
```

## What’s a Stream?

Before answering this, let’s refresh our mind.
What’s a Unix socket?
From Wikipedia: A socket is a special file used for inter-process communication.
These allows communication between two processes.
So a socket is a file that can be written by two processes
(in the same computer or in different computers in the same network).
So the client is going to write to this file and server too.

Ok, but how is a socket related to a Stream?
Well, we are going to be connected to a server using a socket,
therefore we are going to have a 'shared file’ between the client and the server.
This shared file is a white canvas where we are going to start writting our XML stanzas.
The first thing we are going to write to this file is an opening `<stream>` tag! 
And there you go… that’s our stream.

Perfect, I understand what a stream is, but I still don’t understand how to send a message to the server.
Well, the only thing we need to do to send a message is writting a <message/> stanza in our shared file.
But what happens when the server wants to send me a message?
Simple: it will write the message in the 'shared file’.

## Are we ok so far?

I’m sure at this point you have questions like:

* “What?! An active TCP connection open all the time? I’m used to REST! How am I going to do that?!” 
  * Easy, you don’t have to care about that any more! That’s why we are going to use the library, and it will take care of that.
* “You said nothing about how to connect to the server!”
  * Believe me, you don’t have to care about this either. If we start adding all this info, we are going to get crazy. Trust me, I’ve been there.
* “What about encrypted messages? We need security! How are we going to handle this?”
  * Again, you don’t have to care about this at this point. Baby steps!

You just need to be able to answer:
“What’s XMPP?”,
“How do you send a message?”,
“How do you change your status in XMPP?”,
“How do you ask something to the server?”,
“What’s a Stream?”.
If you can answer all that, you are WAY better than me when I started.

