# Accumulators

XMPP stanza processing starts in the `ejabberd_c2s` module, which receives the stanza from a socket, or in `ejabberd_s2s_in` which receives stanzas from federated XMPP clusters.
The stanza is processed and eventually it and/or other messages are sent out, either to the original sender, to another c2s process within the same MongooseIM installation, or to another XMPP server.

At the beginning of the main processing chain an accumulator is created containing following set of keys:

* `ref` - A unique reference of the acc, useful for tracing.
* `timestamp` - An Erlang timestamp retrieved from `os:timestamp()`.
* `origin_pid` - A PID of the process that created the accumulator.
* `origin_location` - `{Module, Function Line}` - A place in the code where the accumulator was created.
* `origin_stanza` - Original stanza that triggered the processing (in a binary).
* `lserver` - Nameprepped domain of the processing context.
* `stanza` - A map with information about the stanza being routed. May be missing in some processing chains (when they are not triggered by a stanza)!
  * `element` - `exml:element()` with the current stanza being routed.
  * `from_jid`, `to_jid` - `jid:jid()` with the sender and the recipient.
  * `name` - A name of the top-level element in `element`.
  * `type` - A value of `type` attribute of the top-level element. If the attribute is missing, this field contains `undefined`.
  * `ref` - A reference of routed stanza.

It is then passed through all the stages until it reaches the end of its life.
Throughout the process it is the very same accumulator; it is therefore possible to store a value in it on one stage of the processing and retrieve the same value later on.

The main assumption is that whatever MongooseIM does, it is always triggered by a stanza entering the system, with some exceptions, such as a couple of `mongooseimctl` operations, which create stanza-less accumulators.
The stanza should always be packed into an accumulator and passed on, so that internally every action is performed the same way.

There are three main benefits from this approach:

a) performance - if we need to do something involving inspecting a stanza or more complicated operations (e.g. privacy check) we don't need to do it multiple times on various stages of processing - instead we can do it once and store the result in an accumulator

b) debugging - it is now very easy to produce an exact track record of a stanza

c) simplified implementation of modules which inherently involve multi-stage processing (e.g. `mod_amp`)

## API

`mongoose_acc` module exports `t()` type which is the accumulator type.

### `new(new_acc_params())`

A constructor for accumulators. `new_acc_params()` is a map with following supported keys:

* `location` - Should be a `{Module, Function, Line}` tuple (may be constructed with `?LOCATION` macro from `mongoose.hrl`). Its format is not enforced by the acc logic but Dialyzer will most probably complain about any other type.
* `lserver` - Nameprepped domain of a the processing context.
* `element` (optional) - If present, it will be used as a source for the `stanza` map.
* `from_jid`, `to_jid` (optional) - Values used to override `from` and `to` attributes of the `element`, respectively.

If `element` is provided, the sender and recipient JIDs are extracted, either from the element itself, or from `to_jid` and `from_jid` parameters.
The call will fail with an exception if it's not possible.

While allowed, stanza-less accumulators usage should be avoided.

### Getters for predefined fields

* `ref(t())`
* `timestamp(t())`
* `lserver(t())`
* `element(t())`
* `stanza_name(t())` - Returns `name` value from `stanza` map.
* `stanza_type(t())` - Returns `type` value from `stanza` map.
* `stanza_ref(t())` - Returns `ref` value from `stanza` map. This is not the same as `ref(t())`!

### `update_stanza(stanza_params(), t())`

Replaces the whole `stanza` field in accumulator with params provided in `stanza_params()`, which is a map of 3 fields: `element`, `from_jid`, `to_jid`.
The same rules apply as in the case of constructor (`new/1`) but this time `element` field is **mandatory**.

### Access to namespaced fields

It is possible to store and retrieve any data in the accumulator, that is related to the processing.
There is no scope protection, so every module may access all namespaces and keys inside them.

* `set(Namespace :: any(), Key :: any(), Value :: any(), t())`
* `set_permanent(Namespace :: any(), Key :: any(), Value :: any(), t())` - Upserts a field, which won't be removed during `strip` operation.
* `append(Namespace :: any(), Key :: any(), Value :: any(), t())` - In order to use this function, a `Namespace:Key` field must not exist or must be a list. `Value` is appended to the end of this list. If `Value` is a list, then a `OldValue ++ Value` operation is performed. In other cases `OldValue ++ [Value]` is used.
* `get(Namespace :: any(), Key :: any(), t())` - Returns a value of a specified field. Will crash if the `NS:Key` is not found.
* `get(Namespace :: any(), Key :: any(), Default :: any(), t())` - Returns a value of a specified field or `Default` if `NS:Key` is not found.
* `delete(Namespace :: any(), Key :: any(), t())` - Removes a specified field, no matter if it is permanent or not.

### Stripping

Accumulator is used mostly to cache values for reuse within a c2s process; when it goes out to somewhere else, it is stripped of all unnecessary attributes except for:

* `ref`
* `timestamp`
* `origin_pid`
* `origin_location`
* `origin_stanza`
* `non_strippable` - A set of permanent `NS:Key` pairs.

If you want it to carry some additional values along with it, please use a dedicated api for setting "permanent" fields:

```
Acc2 = mongoose_acc:set_permanent(myns, myprop, 123, Acc1),
```

Permanent fields may be retrieved with ordinary `get/3,4` functions.

The rationale behind stripping an accumulator is that some values stored in it are context-dependend.
For example, at the beginning `lserver` refers to the host of the sender C2S.
When an accumulator goes to the c2s of the recipient, the `lserver` attribute may change.
There are also many cached values which are not valid anymore when user changes (e.g. privacy checks).

In order to strip an accumulator, please use `strip(strip_params(), t())`, where `strip_params()` is a map of:

* `lserver` - New host context. Obviously, may be equal to the old value.
* `element`, `from_jid`, `to_jid` - The same rules apply as in `update_stanza/2`.

## Main principles of an accumulator processing

1. An accumulator is created when a stanza enters the server.
2. An XML stanza is never passed around as a pure `exml:element()`.
3. An accumulator is stripped when it is passed to a different context (e.g. another c2s process).
4. If a process produces more stanzas to be routed, they must reuse original acc but with stanza replaced with `update_stanza/2`.

## Hooks

Many of the MongooseIM functionalities are implemented in submodules which attach their handlers to hooks (this is covered in detail in ["Hooks and handlers"](Hooks-and-handlers.md).
When it comes to the accumulators, the following rules apply:

* If a hook is related to stanza processing and is executed with `run_fold`, a Mongoose accumulator should be provided. A hook handler may modify an accumulator in every permitted way (i.e. shouldn't directly modify acc fields, bypassing `mongoose_acc` API) and should return the execution result in the `hook:result` field. This is not enforced but should be followed by convention.
* Avoid passing superfluous arguments to handlers - e.g. an `LServer` in hook args is redundant since it is already present in the accumulator.
* Do not use `run` - it is still present in API but executes `run_fold` with `ok` as an initial accumulator anyway.
 Handlers have been rewritten so that they accept an acc as the first arg.
 Note that `run` is deprecated now and at some point will be removed.

Most handlers have already been modified so that they accept an instance of `mongoose_acc:t()` as the first argument and return value by storing it inside it.
How the accumulator is used within a module is up to the implementors of the module.

## IQs and accumulators

`mongoose_iq` module exposes a dedicated API for accessing IQ-related accumulator fields. These are:

* `info(Acc)` - Returns a `#iq{}` record produced from a stanza stored in the accumulator. May be `invalid` or `not_iq` if the stanza is not a valid IQ.
* `xmlns(Acc)` - Returns XMLNS of the first subelement inside an IQ. In most cases it is a namespace of `<query/>` subelement. May be `undefined`.
* `command(Acc)` - Returns the name of a first subelement inside an IQ. May be `undefined`.

These functions ensure that cached information matches the accumulator's stanza, so all of them return a tuple with a possibly updated acc as a second element.

## Sample usage, actual and potential

### Privacy check

Stanzas are often checked against privacy lists.
According to the current `mongoose_privacy:privacy_check_packet` implementation, the result is stored in an accumulator so if a check has to be repeated it is just one map read.

### Tracing

`origin_stanza` field is fully immutable for the lifespan of a single accumulator, so it's easier to correlate one of the stanzas sent by a client with some "unexpected" stanza routed from a completely different part of a server.
There are many places in the server, where an accumulator may be created, so `origin_location` makes it much easier to find out what event has triggered the processing.

### Performance measurement

Given that each accumulator has a timestamp denoting its creation time, it is now very easy to implement a metric showing the stanza processing time, or even multiple metrics splitting it into stages.

