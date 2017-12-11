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

![This is how a JID looks like](./content_jid.png)

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

## First steps: installing the XMPPFramework library

Let’s create a brand new Xcode project and install the library.
In this tutorial we are going to be using `Swift 3`.
The easiest way to integrate XMPPFramework to the project is using [CocoaPods](https://cocoapods.org/).

Let’s create our `Podfile` using the `pod init` command in the folder where our `.xcodeproj` lives.
There are thousands of forks but the maintained one is the original: `robbiehanson/XMPPFramework`.

So let’s add the `pod` to our `Podfile` and remember to uncomment the `use_frameworks!`.

```
use_frameworks!

target 'CrazyMessages' do
    pod 'XMPPFramework', :git=> 'git@github.com:robbiehanson/XMPPFramework.git', :branch => 'master'
end
``` 

Then `pod install` and `CocoaPods` is going to do its magic and create a `.xcworkspace` with the library integrated.
Now we just need to `import XMPPFramework` in the files we want to use the library and that’s it.

## Starting to build our Instant Messaging app

The most important thing in an XMPP application is the stream,
that’s where we are going to “write” our stanzas, so we need an object that is going to hold it.
We are going to create an `XMPPController` class with an `XMPPStream`:

```
import Foundation
import XMPPFramework

class XMPPController: NSObject {
    var xmppStream: XMPPStream

    init() {
        self.xmppStream = XMPPStream()  
    }

}
``` 

We are dealing with a highly asynchronous library here.
For every action we are going to have a response some time in the future.
To handle this `XMPPFramework` defines the `XMPPStreamDelegate`.
So implementing that delegate is going to help us answer lots of different questions like:
“How do I know when XMPP has successfully connected?”,
“How do I know if I’m correctly authenticated?”,
“How do I know if I received a message?”.
`XMPPStreamDelegate` is your friend!

So we have our `XMPPController` and our `XMPPStream`,
what do we need to do now?
Configure our stream with the `hostName`, `port` and `ourJID`.
To provide all this info to the controller
we are going to make some changes to the `init` to be able to receive all these parameters:

```
enum XMPPControllerError: Error {
    case wrongUserJID
}

class XMPPController: NSObject {
    var xmppStream: XMPPStream

    let hostName: String
    let userJID: XMPPJID
    let hostPort: UInt16
    let password: String

    init(hostName: String, userJIDString: String, hostPort: UInt16 = 5222, password: String) throws {
        guard let userJID = XMPPJID(string: userJIDString) else {
            throw XMPPControllerError.wrongUserJID
        }

        self.hostName = hostName
        self.userJID = userJID
        self.hostPort = hostPort
        self.password = password

        // Stream Configuration
        self.xmppStream = XMPPStream()
        self.xmppStream.hostName = hostName
        self.xmppStream.hostPort = hostPort
        self.xmppStream.startTLSPolicy = XMPPStreamStartTLSPolicy.allowed
        self.xmppStream.myJID = userJID

        super.init()

        self.xmppStream.addDelegate(self, delegateQueue: DispatchQueue.main)
    }
}
```

Our next step is going to actually connect to a server and authenticate using our `userJID` and `password`,
so we are adding a `connect` method to our `XMPPController`.

```
func connect() {
    if !self.xmppStream.isDisconnected() {
        return
    }

   try! self.xmppStream.connect(withTimeout: XMPPStreamTimeoutNone)
}
```

But how do we know we have successfully connected to the server?
As I said earlier, we need to check for a suitable delegate method from `XMPPStreamDelegate`.
After we connect to the server we need to authenticate so we are going to do the following:

```
extension XMPPController: XMPPStreamDelegate {

    func xmppStreamDidConnect(_ stream: XMPPStream!) {
        print("Stream: Connected")
        try! stream.authenticate(withPassword: self.password)
    }

    func xmppStreamDidAuthenticate(_ sender: XMPPStream!) {
        self.xmppStream.send(XMPPPresence())
        print("Stream: Authenticated")
    }
}
```

We need to test this.
Let’s just create an instance of `XMPPController` in the `AppDelegate` to test how it works:

```
try! self.xmppController = XMPPController(hostName: "host.com",
                                     userJIDString: "user@host.com",
                                          password: "password")
self.xmppController.connect()
```

If everything goes fine we should see two messages in the logs but of course that’s not happening,
we missed something.
We never told to our `xmppStream` who was the delegate object!
We need to add the following line after the `super.init()`

```
self.xmppStream.addDelegate(self, delegateQueue: DispatchQueue.main)
```

If we run the app again:

```
Stream: Connected
Stream: Authenticated
```

Success! We have our own `XMPPController` with a fully functional and authenticated stream!

Something that may catch your attention is how we are setting our delegate, we are not doing:

```
self.xmppStream.delegate = self
```

Why not?
Because we can “broadcast” the events to multiple delegates,
we can have 10 different objects implementing those methods.
Also we can tell what’s the thread where we want to receive that call,
in the previous example we want it in the main thread.

## Getting a Log In

Our app is super ugly, let’s put on some makeup!
We have nothing but an `XMPPController` and a hardcoded call in the `AppDelegate`.
I’m going to create a `ViewController` that is going to be presented modally as soon as the app starts,
that `ViewController` will have the neccesary fields/info to log in to the server.

I’m going to create a `LogInViewControllerDelegate` that is going to tell to our `ViewController`
that the `Log in` button was pressed and that’s it.
In that delegate implementation we are going to create our `XMPPController`, add the `ViewControlleras` delegate of the `XMPPStream` and connect!

```
extension ViewController: LogInViewControllerDelegate {

    func didTouchLogIn(sender: LogInViewController, userJID: String, userPassword: String, server: String) {
        self.logInViewController = sender

        do {
            try self.xmppController = XMPPController(hostName: server,
                                                     userJIDString: userJID,
                                                     password: userPassword)
            self.xmppController.xmppStream.addDelegate(self, delegateQueue: DispatchQueue.main)
            self.xmppController.connect()
        } catch {
            sender.showErrorMessage(message: "Something went wrong")
        }
    }
}
```

Why are we adding `ViewController` as a delegate of `XMPPStream` if our `XMPPController` alreay has that delegate implemented?
Because we need to know if this connection and authentication was successfull or notin our `ViewController`
so we are able to dismiss the `LogInViewController` or show an error message if something failed.
This is why being able to add multiple delegates is so useful.

So as I said I’m going to make `ViewController` to comform to the `XMPPStreamDelegate`:

```
extension ViewController: XMPPStreamDelegate {

    func xmppStreamDidAuthenticate(_ sender: XMPPStream!) {
        self.logInViewController?.dismiss(animated: true, completion: nil)
    }

    func xmppStream(_ sender: XMPPStream!, didNotAuthenticate error: DDXMLElement!) {
        self.logInViewController?.showErrorMessage(message: "Wrong password or username")
    }

}
```

And that’s it! Our app can log in to our server as I’m showing here:

![App connecting and authenticating](./content_login.gif)

## Logging!

We’ve been talking a lot about XMPP, stanzas and streams… but is there a way I can see the stream?
Yes SR! XMPPFramework got us covered!

XMPPFramework ships with [CocoaLumberJack](https://github.com/CocoaLumberjack/CocoaLumberjack),
a pretty well known logging framework.
We just need to configure it, set the logging level we want and that’s it.
Logs are going to start showing up!

### Configuring CocoaLumberjack

This is a really simple task,
you just need to add to your `func application(application: UIApplication, didFinishLaunchingWithOptions ...` method the following line (remember to `import CocoaLumberjack`):

```
DDLog.add(DDTTYLogger.sharedInstance(), with: DDLogLevel.all)
```

I’m not going to paste here all the connection process log
because it makes no sense to try to understand what’s going on at this stage of our learning.
But I think showing what some stanzas look like is a good idea.
To do this I’m going to be sending messages from [Adium](https://adium.im/).

I’m going to send this `<message/>`:

```xml
<message to="test.user@erlang-solutions.com">
    <body>This is a message sent from Adium!</body>
</message>
```

Let’s see how it looks like when it reaches our app:

```xml
<message xmlns="jabber:client" from="iamadium@erlang-solutions.com/MacBook-Air" to="test.user@erlang-solutions.com">
   <body>This is a message sent from Adium!</body>
</message>
```

Let’s send a `<presence/>` from Adium:

```xml
<presence>
    <status>On vacation</status>
</presence>
```

We are receiving:

```xml
<presence xmlns="jabber:client" from="iamadium@erlang-solutions.com/MacBook-Air" to="test.user@erlang-solutions.com">
   <status>On vacation</status>
</presence>
```

No doubts at all right? We send something and we receive it on the other end! That’s it!

## Test Time!

I want to be sure that you are understanding and following everything
and not just copy and pasting from a tutorial (as I usually do 🙊).
So if you are able to answer these questions you are on a good track!

* Why am I sending a presence after successfully authenticating? What happens if I don’t send it?
* What happens if I write a wrong server URL in the Log In form? How do I fix this problem if there is a problem…
* How do I detect if suddenly the stream is disconnected from the server? (maybe a network outage?)
* How do I detect if the user/password was wrong?

If you need help leave a message!
