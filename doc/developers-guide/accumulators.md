# Accumulators

XMPP stanza processing starts in `ejabberd_c2s` module, which receives the
stanza from a socket. The stanza is processed and eventually this stanza
and/or other messages are sent out, either to the original sender, to
another c2s process within the same MongooseIM installation, or to
another XMPP server.

Upon entering the main processing chain the stanza is converted into an
accumulator which contains the stanza and other values. The accumulator
is then passed through all stages, until it reaches the end of
its life. Throughout the process it is the very same accumulator; it is
therefor possible to store a value in it on one stage of processing
and retrieve the same value later on.

The main assumption is that whatever the MongooseIM does it is always
triggered by a stanza entering the system, one way or another (with an
exeption for REST methods and mongooseimctl, which eventuall shoul). The stanza should always
be packed into an accumulator and passed on, so that internally every
action is performed the same way.

Benefits from this approach are threefold:

a) performance - if we need to do something which involves inspecting
a stanza or more complicated operations (e.g. privacy check) we don't need
to do it multiple times on various stages of processing - instead we can
do it once and store result in an accumulator

b) debugging - it is now very easy to produce an exact track record
of a stanza

c) simplified implementation of modules which inherently involve
multi-stage processing (e.g. mod_amp)

## Implementation

The accumulator is implemented in `mongoose_acc` module, which exposes
a number of methods for creating and modifying stanza and retrieving
values from it. Under the hood an accumulator is just a map, but it
may change in the future.

Initially an accumulator should be created from an `#xmlel{}` using
`mongoose_acc:from_element/1` or `mongoose_acc:from_element/3`. It should
also contain `from`, `from_jid`, `to` and `to_jid` (those are set
automatically if `mongoose_acc::from_element/3` is used), `user` and
`server`. Instantiation functions also set `timestamp` and `ref`. The
`to` part is optional, since not every stanza is directed to a specified
jid.

Some attributes are calculated upon request and stored: these are
`command`, `xmlns` and `iq_query_info`. If they are needed, you should
first use `mongoose_acc:require/2` and then just get:

```
Acc1 = mongoose_acc:require(command, Acc),
Cmd = mongoose_acc:get(command, Acc1)
```

## Entry points

Stanza enters MongooseIM in the following places:

### `ejabberd_c2s:session_established/2`

This is the handler for a stanza received from an XMPP client, after the
session has been fully established (before there is only technical
communication between user and server, the main processing chain is
not started). In this place we initialise accumulator and pass it on.

### `ejabberd_c2s:handle_incoming_message/3`

This is called by `ejabberd_c2s:handle_info/3` when an accumulator
from another c2s process is received. It is a ready-made accumulator,
but stripped of all cached values; it preserves original `ref`, `timestamp`,
`from` and `to`, but doesn't have `user` and `server` which has to be set here
since they are likely to differ.

### `ejabberd_c2s_in:stream_established/2` (not done yet)

This is where a stanza from another MongooseIM server is received.

## Hooks

A large part of MongooseIM functionalities is implemented in submodules
which attach their handlers to hooks (this is covered in detail in another
chapter). With reference to accumulators the following rules apply:

* when you use `run_fold` always pass an accumulator as - well, as
the accumulator, since this is basically what it's been made for. Unless
it really doesn't make sense, which is true in some cases (for example
if handlers are supposed to modify the state then the state should be the
accumulator). Your handlers should stash their results in the acc, then
you can extract what you need and pass the modified acc on.
* avoid passing superfluous arguments to handlers - e.g. a `Server` in
hook args is redundant since it is already present in accumulator.
* do not use `run` - it is against the main design principles. All handlers
have been rewritten so that they accept an acc as the first arg, and
the `run` function passed on an empty acc. The `run` is deprecated now
and at some point will be removed.

Most handlers have already been modified so that they accept an instance
of `mongoose_acc:t()` as the first argument and return value by storing
it inside it. How the accumulator is used within a module is up to the
implementors of the module.

## Exit points

The notion of an exit point is a bit vague, because sending a stanza
out of MongooseIM doesn't necessarily mean that processing of the
accumulator is terminated. A single stanza entering the system creates
on acc, but then may result in multiple stanzas going out - for example
changing a privacy list triggers presence updates and roster push. It is
in fact hard to determine when the process has been really finished.
The approach is then to pass along one acc and, when something is
just about to be sent out, either send a text and return the acc
or pass on a clone of the acc and return the original one. This gives
us an option to record a fact that a stanza has been sent out; when
the last stanza in the whole process is leaving the system we have
a complete record of what we sent in the meantime.

The following picture should illustrate the idea - the Acc1 which goes
to routing is the same, in that it has the same `ref`, only it doesn't
return from there:

![You should see an image here; if you don't, use plantuml to generate it from accum_path.uml](accum_path.png)

Thus, an 'exit point' is a place where we call a function which sends
something out of the system and, if ok, return the original accumulator, possibly
with addition track record of what's just been done.

An 'exit point' function should be called with acc AND a stanza to
be sent out - sometimes a stanza is the original one (which is stored)
in accumulator as `element`, sometimes not. It should return an
accumulator.

Sending data out can be done in two ways:

* if we send it out of MongooseIM through socket, then we send a binary
representation of the stanza to be sent
* if we send to another c2s process within the same MongooseIM then
we create another accumulator with `element` replaced by the stanza
we are sending, but preserving some useful attributes. This is done
by `mongoose_acc:strip/1`.

### `ejabberd_c2s:send_element/3`

This is where we send a stanza to an XMPP client. It is often called
directly by a c2s process when it needs to respond to its "owner".

### `ejabberd_router:route/4`

This function is called when a stanza is ready to be sent. It makes an
attempt to route it out via appropriate channel - either locally to
the same or another c2s process, or somewhere else via s2s. The stanza
can be sent or dropped, or it may error out - whatever it does this
function returns the original accumulator, optionally with track
record of what it tried to send, how and to what effect (see commented)
out code in `mongoose_acc:record_sending/4`.

Under the hood some routing modules work by passing a raw stanza, some
clone the accumulator and store the outgoing stanza as `to_send` attr.
The acc is then passed on and either is sent to another c2s process
by `ejabberd_sm`, stored as offline, or sent out to another XMPP
server via `ejabberd_s2s`.

## Sample usage, actual and potential

### Privacy check

Stanzas are often checked against privacy lists. It is now implemented
in `mongoose_privacy:privacy_check_packet` - result is stored in accumulator
so if a check has to be repeated it is just one map read.

### Tracing hooks and handlers

`ejabberd_hooks:record` functions contain some (commented out for now)
code which adds a record to accumulator every time it is passed to
hook and every time it goes through hook handler - very valuable
for debugging.

### Performance measurement

Given that each accumulator has a timestamp denoting its creation time
it is now very easy to implement a processing time counter, even
splitting time into multiple processing stages.

### `mod_amp`
















