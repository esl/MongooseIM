# MongooseIM traffic tracer

This is made to address the need to watch the XMPP traffic flowing through the server while developing/debugging/testing code.
It is normally achievied by tracing with Recon, which is not very convenient and hardly readable, mostly because of all the other junk that is printed out alongside.
Thus, here is something more convenient.

## How to enable

Build your MongooseIM, then in mongooseim.cfg uncomment lines defining a listener using `mongoose_traffic` and `mongoose_traffic_channel` module (should be the top one),
andalso module `mongoose_traffic`.

If you want https, add the following to your listener definition:
```
  tls.certfile = "mycert.pem"
  tls.keyfile = "mykey.pem"
  tls.password =  "secret"
  +```

## How to use

In your web browser, open address `http://localhost:5111` (or some other port, depending on listener settings).

Click the "Tracing" button to start.
When you run a "big test", the left part will start showing jids of users which communicate with the server.
Click one of them, and you'll see the stanzas send by this user and to this user.
New stanzas will be appended in real time.
The black digits to the left from the stanzas is this user's timeline, in seconds.

You can use the "Clear all" button to flush the UI.

Things to keep in mind:

* if you enable tracing and then restart the server, client app will reconnect and re-enable tracing
* however, if you reload the page, tracing will be disabled
* there is a limit to jids you can trace at the same time (100), so if you run a number of tests while tracing all the time chances are tracing will stop, you may want to clear it from time to time
* under the hood, traces are indexed with pids, and jids are only for display; it is perfectly normal to see the same jid appear multiple times, if your test uses `escalus:story` (not `fresh_story`)

## How it works

There is a hook `c2s_debug` which is called every time `ejabberd_c2s` sends or receives anything.
`mongoose_traffic` handles these calls and forwards messages to a dedicated process.
Your websocket process registers with that process.
It handles all "debug" messages and stores them (if tracing is enabled), also handles all communication with you.

Stanza lists are indexed with c2s process id, so that we can collect them even before we know the user's jid.
The pid is first used for display, then it is maped to bare jid (after auth), and finally to full jid.

Stanzas are stored in a double-ended queue, limited to 1000 entries.

When you select a jid, you retrieve its trace from the server and also mark it as your currently traced account.
Then you start receiving new stanzas in real time, until you click another jid.

## How it is implemented

On server side, it is a simple Cowboy websocket.

Client app is written in Elm, compiled to js and installed in `web/traffic` dir together with a css, some js for communication and a static html page.
Rebuilding it is as simple as running `make` in this directory.


