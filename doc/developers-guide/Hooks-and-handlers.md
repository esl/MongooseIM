# Hooks, handlers and accumulators

The hooks and handlers mechanism is one of the core architectural features of MongooseIM.
It allows for loose coupling between components of the system by calling only those which are available and configured to be used at runtime.

It can be thought of as a simple eventing mechanism notifying about certain things happening in the server.
That results in an extensible system with pluggable extra functionality.

To focus our attention, we'll analyze `mod_offline` which is responsible for storing messages for delivery to users unavailable at the time of sending.
`mod_offline` is an implementation of [XEP-0203: Delayed Delivery][xep-0203].

## Running a hook

### Basic usage

`ejabberd_sm` (ejabberd/MongooseIM session manager) is the module discovering whether the recipient of a message is available or not.
That's where storing the message for later delivery takes place.
It is possible, but not recommended, to save a message in an offline storage by calling `mod_offline` directly:

```erlang
mod_offline:store_packet(Acc, From, To, Packet)
```

Note that in this example `ejabberd_sm` is coupled with `mod_offline`.
I.e. if `mod_offline` was not available, the code would simply crash; if it was misconfigured or turned off, the behaviour would be undefined.
To avoid that coupling and also to enable other ([possibly yet to be written](#sidenote-code-yet-to-be-written)) code to carry out some action at this particular moment, `ejabberd_sm` calls instead:

```erlang
mongoose_hooks:offline_message_hook(Acc, From, To, Packet);
```

`mongoose_hooks` is a module which serves as an API for calling hooks in the server. All such modules are placed in `src/hooks`.

For every hook, there needs to be a function in this module written beforehand which accepts the correct arity of arguments and makes the call to actual low-level hooks mechanism.
This means that there is some degree of coupling still - but this time between the `ejabberd_sm` module and `mongoose_hooks`, and the latter is always available.

The extra level of indirection introduced by this call gives the flexibility to determine at runtime what code actually gets run at this point.
This depends on which handlers are registered to process the event.

`offline_message_hook` is the name of the hook (in other words of the event that is being signalled);
`Acc` is the [Accumulator, described later](#using-accumulators);
`From`, `To` and `Packet` are the arguments passed to the handler, just as they would in case of the function being called directly.

!!! Note "Why do we even need the `mongoose_hooks` module?"
    Why is there a module in which we have to define the hook invocation beforehand?
    Could we not just use the low-level hooks mechanism directly and avoid this module altogether?

    This was actually the case before this module was introduced, and hooks' names were just atoms provided as an argument to this low-level API.
    However, we discovered it was causing problems and producing bugs, due to the lack of static code analysis.
    Now we can have some guarantees thanks to Dialyzer, and each hook invocation has a correct number of arguments.
    Thanks to this, writing handlers is easier - there is a single source of truth about how a hook is run.
    Remember that a given hook can be invoked from many places in many modules.

    With the new `mongoose_c2s` implementation we introduced a new hook API module, `mongoose_c2s_hooks`.
    All such API modules are placed in the `src/hooks` directory.

### Getting results from handlers

Hook handlers are called by "folding".
This means that each handler on a list is passed a set of arguments, and an initial value that it then modifies, returns and hands over to the next handler in line.
This modified data that is processed by the series of handlers is called an accumulator - because it accumulates the results.

A simple example would look like this:

```erlang
NewAcc = mongoose_hooks:a_certain_hook(Accumulator,
                                       StateData#state.user,
                                       StateData#state.server).
```

The initial value of the accumulator being passed through the sequence of handlers is provided with additional arguments required by the hook, as defined in the `mongoose_hooks` module.

!!! info "Folds"
    If you haven't encountered the term _fold_ before, think of it as _reduce_ (like `Array.reduce`) in Ruby-speak, roughly equivalent to the Reduce step in MapReduce, sometimes called _accumulate_, _aggregate_ or _compress_.
    [See Wikipedia for more.][wiki:fold]

### Using accumulators

MongooseIM uses a dedicated data structure to accumulate data related to stanza processing (see ["Accumulators"](accumulators.md)).
It is instantiated with an incoming stanza, passed along throughout the processing chain, supplied to and returned from certain hook calls, and terminated when the stanza is leaving MongooseIM.
There are some hooks which don't use this data structure.

If a Mongoose accumulator is passed to a hook, handlers should store their return values in one of 3 ways:

* If it is a one-off value which doesn't need to be passed on along with the accumulator (can be overwritten any time), use `mongoose_acc:set(hook, result, Value, Acc)`.
* If the value is to be passed on to be reused within the current processing context, use `mongoose_acc:set(Namespace, Key, Value, Acc)`.
* If the value should be passed on to the recipient's session, pubsub node etc. use `mongoose_acc:set_permanent(Namespace, Key, Value, Acc)`.

A real life example, then, with regard to `mod_offline` is the `resend_offline_messages_hook` run in `mod_presence`:

```erlang
Acc1 = mongoose_hooks:resend_offline_messages_hook(Acc, Jid),
Rs = mongoose_acc:get(offline, messages, [], Acc1),
```

### Error handling in hooks

Hooks are meant to decouple modules; in other words, the caller signals that some event took place or that it intends to use a certain feature or a set of features, but how and if those features are implemented is beyond its interest.
For that reason hooks don't use the "let it crash" approach. Instead, it is rather like "fire-and-forget", more similar in principle to the `Pid ! signal` way.

In practical terms: if a handler throws an error, the hook machine logs a message and proceeds to the next handler with an unmodified accumulator.
If there are no handlers registered for a given hook, the call simply has no effect.

### Sidenote: Code yet to be written

Let's imagine, that when building a [minimum viable product][mvp] we settle on using `mod_offline` for delayed delivery of messages to unavailable clients.
However, while the product evolves (or the relevant client software catches up) we might drop `mod_offline` in favour of a more sophisticated solution like [Message Archive Management][mam] which would require a different action to be taken at the same point.
Thanks to loose coupling and `mongoose_hooks`, it's possible to turn off `mod_offline` and turn on `mod_mam` without changing
a single line of code in `ejabberd_sm`.

The only required change is to the configuration (apart from deploying the new module) which can even be performed at runtime - without restarting the server.

### Sidenote: Multiple Domains

A MongooseIM cluster may serve more than one domain at the same time.
E.g. it is quite common that services like Multi User Chat or Publish-Subscribe are available as subdomains of the main XMPP domain served by an installation.

Moreover, each XMPP host is of a certain type, as defined in [`general.host_types`](../configuration/general.md#generalhost_types), and hooks can be called either globally (across all hosts/host types) or for one host type.
If you are not using [dynamic domains](../user-guide/Features.md#multi-tenancy-dynamic-domains) or grouping hosts under host types, then each host has a corresponding host type implicitly, and the two terms are interchangeable.
Whether a hook is called globally or per host type is depends on its purpose.
It is decided when creating a hook and can be checked in the `mongoose_hooks` module for existing hooks.

## Registering hook handlers

In order to store a packet when `ejabberd_sm` runs `offline_message_hook`, the relevant module must register a handler for this hook.
To attain the runtime configurability the module should register the handlers when it's loaded and unregister them when
it's unloaded.
That's usually done in, respectively, `start/2` and `stop/1` functions.
Here is the relevant snippet from `mod_offline:start/2`:

```erlang
gen_hook:add_handlers(hooks(HostType)),
```
and the `hooks/1` function returns a list of tuples describing hook handlers, like:
```erlang
{offline_message_hook, HostType, fun ?MODULE:inspect_packet/3, #{}, 50}
```

It is clearly visible that the handler `inspect_packet` is added to the `offline_message_hook`.

`HostType` is the one for which the handler will be executed.
In the case of statically defined domains, it is the same as the host, as configured in the [`general.hosts` section](../configuration/general.md#generalhosts).

The handler itself is specified as a fun expression;
the arity of the function is always 3 - more about actual arguments in the [`Writing handlers`](#writing-handlers) section.
If the handler expects an incorrect number of arguments, it will simply crash.

The 4th element of this tuple is a map of static parameters that will be passed to every invocation of the handler.
It allows to specify additional handler config at the moment of its registering.

Multiple handlers may be registered for the same hook.
The last argument, 50, is the sequence number of this handler in the handler chain.
The higher the number, the later in the sequence the handler will be executed.
It's reasonable to keep this number small (e.g. in the range 0-100), though there's no real limit other than the size of the integer type in the Erlang VM.

## Unregistering handlers

Pluggability also requires the components to be unpluggable at will.
For that purpose there's the option to unregister a hook handler.
It's done in `mod_offline:stop/1` in a similar fashion to:

```erlang
gen_hook:delete_handlers(hooks(Host)),
```

The function `hooks/1` function returns a list of hook tuples exactly the same as passed to `gen_hook:add_handlers/1`.
Both these functions accept either a list of tuples.
There also exist functions `gen_hook:add_handler/5` and `gen_hook:delete_handler/5` which register and unregister one handler at a time.

### Sidenote: Metrics

Every time a hook is run, a corresponding metric of the same name in the same host is incremented by one.
There are some exceptions though as some metrics were implemented before the generic hook metrics.
List of hooks not updating generic metrics can be found in the `mongoose_metrics:filter_hook/1` function.
Such skipped hooks update metrics are defined in the `mongoose_metrics_hooks` module.

## Writing handlers

The signature of a handler has to follow these rules:

* Accepts correct arguments:
    - Acc - accumulator which was passed from previous handler (or initial accumulator). May be `mongoose_acc` in particular
    - Params - map of hook parameters passed from `mongoose_hooks`. It is constant for every handler in one hook invocation.
    For exact structure check the hook function in `mongoose_hooks` module, as different hooks use different parameters.
    - Extra - map of additional hook parameters. It is constant for **every** hook invocation.
    It is created from the map described in [`Registering hook handlers`](#registering-hook-handlers) section with 3 additional parameters:
    `host_type`, `hook_tag`, `hook_name`. Parameter `host_type` can be particularly useful.
* Returns a tuple `{ok | stop, Acc}` where `Acc` is the accumulator of the same type as the input one,
that shall be passed to the next handler (or return value in case of last handler).

Let's look at this example, from MongooseIM codebase:

```erlang
in_subscription(Acc, #{to := ToJID, from := FromJID, type := Type}, _) ->
    case process_subscription(in, ToJID, FromJID, Type) of
        stop ->
            {stop, Acc};
        {stop, false} ->
            {stop, mongoose_acc:set(hook, result, false, Acc)};
        _ -> {ok, Acc}
    end.
```

As seen in this example, a handler receives an accumulator, parameters and extra parameters (in this case - ignored).
Then it matches to the result of `process_subscription/4` and can return 3 different values:

* `{ok, Acc}` - it allows further processing and does not change the accumulator.
* `{stop, mongoose_acc:set(hook, result, false, Acc)}` - it stops further processing and returns accumulator with a new value in it.
* `{stop, Acc}` - it stops further processing and does not change the accumulator.

This is an important feature to note: in some cases our handler returns a tuple  `{stop, Acc}`.
This skips calling later actions in the handler sequence, while the hook call returns the `Acc`.
Further processing is only performed if the first element of return tuple is `ok`.

Watch out! Different handlers may be registered for the same hook - the priority mechanism orders their execution.
If a handler returns `{stop, Acc}` but runs early in the handler chain, it may prevent some other handler from running at all!
That might or might not be intentional.
It may be especially surprising in case of handlers from different modules registered for the same hook.
Always ensure what handlers are registered for a given hook (`grep` is your friend) and that you understand their interdependencies.

## Hooks list and how to extract it

The following command should give you a list of all the hooks available in MongooseIM:

```bash
awk '/\-export\(\[/,/\]\)\./' src/hooks/*.erl | grep -oh "\w*/" | sed 's/.$//' | sort
```
It returns:
```bash
adhoc_local_commands
adhoc_sm_commands
...
...
...
xmpp_stanza_dropped
```

It just extracts the hooks exported from `mongoose_hooks` and other hook API modules.
Refer to `grep`/`ack` to find where they're used.

## Creating your own hooks

You should put the new hook inside `mongoose_hooks` with a correct type specification, which provides some security in places where the hooks are run.
This is the way all hooks are called in MongooseIM (see the examples in the [hooks description](hooks_description.md)).
You could run `gen_hook:run_fold` directly, providing the hook name, but this is advised against.

Of course, as long as no module registers handlers for a hook, calling it won't have any effects.

This is similar to the case when a module registers handlers for some hook, but that hook is never run in the code.
That won't have an effect either.

### Example of creating a new hook

The following is an example of a module which both runs and registers a few handlers for a completely new hook.
The handlers are run sequentially using disparate priorities and passing over an accumulator value.
One of the handlers stops the handler execution chain prematurely by returning `{stop, NewVal}`.
It's also possible to try out what happens when the same hook is run with different XMPP domains by passing an argument to `run_custom_hook/1` - we'll see that the handlers are registered for a particular domain only.

At the end, you can see a printout of an accumulator with some debugging info.

To cut the long story short:

#### 1. Add the hook with type specification to `mongoose_hooks`

```erlang
-spec custom_new_hook(HostType, Acc, Number) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Number :: integer(),
    Result :: mongoose_acc:t().
custom_new_hook(HostType, Acc, Number) ->
    Params = #{number => Number},
    run_hook_for_host_type(custom_new_hook, HostType, Acc, Params).
```

Don't forget about exporting the function:
```erlang
-export([custom_new_hook/3]).
```

#### 2. Create the `mod_hook_example` module

```erlang
-module(mod_hook_example).

-behaviour(gen_mod).

-include("mongoose.hrl").

%% API
-export([run_custom_hook/1]).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% Hook handlers
-export([first_handler/3,
         stopping_handler/3,
         never_run_handler/3]).

start(HostType, _Opts) ->
    gen_hook:add_handlers(hooks(HostType)).

stop(HostType) ->
    gen_hook:delete_handlers(hooks(HostType)).

hooks(HostType) ->
    [{custom_new_hook, HostType, fun ?MODULE:first_handler/3, #{extra_param => <<"ExtraParam">>}, 25},
     {custom_new_hook, HostType, fun ?MODULE:stopping_handler/3, #{}, 50},
     {custom_new_hook, HostType, fun ?MODULE:never_run_handler/3, #{}, 75}].

run_custom_hook(Host) ->
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(Host),
    Acc = mongoose_acc:new(#{ location => ?LOCATION, lserver => Host, host_type => HostType }),
    Acc1 = mongoose_acc:set(example, value, 5, Acc),
    ResultAcc = mongoose_hooks:custom_new_hook(HostType, Acc1, 2),
    ResultValue = mongoose_acc:get(example, value, ResultAcc),
    ?LOG_INFO(#{what => hook_finished, result => ResultValue, result_acc => ResultAcc}).

first_handler(Acc, #{number := Number}, #{extra_param := Extra}) ->
    V0 = mongoose_acc:get(example, value, Acc),
    Result = V0 + Number,
    ?LOG_INFO(#{what => first_handler, value => V0, argument => Number,
                result => Result, extra => Extra}),
    {ok, mongoose_acc:set(example, value, Result, Acc)}.

stopping_handler(Acc, #{number := Number}, _) ->
    V0 = mongoose_acc:get(example, value, Acc),
    Result = V0 + Number,
    ?LOG_INFO(#{what => stopping_handler, value => V0, argument => Number, result => Result}),
    {stop, mongoose_acc:set(example, value, Result, Acc)}.

never_run_handler(Acc, #{number := Number}, _) ->
    ?LOG_INFO(#{what => never_run_handler,
                text => <<"This handler won't run as it's registered with a priority bigger "
                          "than that of stopping_handler/2 is. "
                          "This text should never get printed.">>}),
    {ok, Acc * Number}.
```

The module is intended to be used from the shell for educational purposes:

```erlang
(mongooseim@localhost)1> gen_mod:is_loaded(<<"localhost">>, mod_hook_example).
false
(mongooseim@localhost)2> mongoose_modules:ensure_started(<<"localhost">>, mod_hook_example, #{}).
{started,ok}
(mongooseim@localhost)3> gen_mod:is_loaded(<<"localhost">>, mod_hook_example).
true
(mongooseim@localhost)4> mongoose_logs:set_module_loglevel(mod_hook_example, info).
ok
(mongooseim@localhost)5> mod_hook_example:run_custom_hook(<<"localhost">>).
when=2022-12-15T12:37:16.109544+00:00 level=info what=first_handler pid=<0.1081.0> at=mod_hook_example:first_handler/3:41 value=5 result=7 extra=ExtraParam argument=2 
when=2022-12-15T12:37:16.109809+00:00 level=info what=stopping_handler pid=<0.1081.0> at=mod_hook_example:stopping_handler/3:48 value=7 result=9 argument=2 
when=2022-12-15T12:37:16.110028+00:00 level=info what=hook_finished pid=<0.1081.0> at=mod_hook_example:run_custom_hook/1:36 result_acc_{example,value}=9 result_acc_timestamp=1671107836109517 result_acc_stanza=undefined result_acc_ref=#Ref<0.4046106046.1908670465.111816> result_acc_origin_pid=<0.1081.0> result_acc_origin_location_mfa={mod_hook_example,run_custom_hook,1} result_acc_origin_location_line=32 result_acc_origin_location_file=/Users/paweldlugosz/Dev/Repos/MongooseIM/src/mod_hook_example.erl result_acc_non_strippable= result_acc_mongoose_acc=true result_acc_lserver=localhost result_acc_host_type=localhost result=9 
ok
```

[mam]: http://xmpp.org/extensions/xep-0313.html
[mvp]: http://en.wikipedia.org/wiki/Minimum_viable_product
[wiki:fold]: http://en.wikipedia.org/wiki/Fold_%28higher-order_function%29
[xep-0203]: http://xmpp.org/extensions/xep-0203.html
