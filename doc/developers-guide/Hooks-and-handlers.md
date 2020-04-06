# Hooks, handlers and accumulators

The hooks and handlers mechanism is one of the core architectural features of MongooseIM.
It allows for loose coupling between components of the system by calling only those which are available and configured to be used at runtime.

It can be thought of as a simple eventing mechanism notifying about certain things happening in the server.
That results in an extensible system with pluggable extra functionality.

To focus our attention, we'll analyze `mod_offline` which is responsible for storing messages for delivery to users unavailable at the time of sending.
`mod_offline` is an implementation of [XEP-0203][xep-0203].

[xep-0203]: http://xmpp.org/extensions/xep-0203.html

## Running a hook

### Basic usage

`ejabberd_sm` (ejabberd/MongooseIM session manager) is the module discovering whether the recipient of a message is available or not.
That's where storing the message for later delivery takes place.
It is possible, but not recommended, to save a message in an offline storage by calling `mod_offline` directly:

```erlang
mod_offline:store_packet(From, To, Packet)
```

Note that in this example `ejabberd_sm` is coupled with `mod_offline`.
I.e. if `mod_offline` was not available, the code would simply crash; if it was misconfigured or turned off, the behaviour would be undefined.
To avoid that coupling and also to enable other ([possibly yet to be written](#sidenote-yet-to-be-written)) code to carry out some action at this particular moment, `ejabberd_sm` instead calls:

```erlang
Acc1 = ejabberd_hooks:run_fold(offline_message_hook,
                               LServer,
                               Acc,
                               [From, To, Packet])
```

The extra level of indirection introduced by this call gives the flexibility to determine at runtime what code actually gets run at this point.
`offline_message_hook` is just the name of the hook (in other words of the event that is being signalled);
`From`, `To` and `Packet` are the arguments passed to the handler just as they would in case of the function being called directly;
`LServer` is [the XMPP domain for which this hook is signalled](#sidenote-multiple-domains).

### Getting results from handlers

Hook handlers are called by "folding".
This means that each handler on a list is passed a set of arguments and an initial value that it then modifies, returns and hands over to the next handler in line.

A simple example would look like this:

```erlang
ListOfSomething = ejabberd_hooks:run_fold(a_certain_hook,
                                          StateData#state.server,
                                          [],
                                          [StateData#state.user,
                                           StateData#state.server])
```

The initial value of the accumulator being passed through the sequence of handlers (in this case an empty list `[]`) is inserted between the XMPP domain `StateData#state.server` and handler arguments.
In between the XMPP domain (`StateData#state.server`) and handler arguments is the initial value of the accumulator being passed through the sequence of handlers is inserted - in this case an empty list (`[]`).

#### Sidenote: Folds

If you haven't encountered the term _fold_ before, think of it as _reduce_ (like `Array.reduce`) in Ruby-speak, roughly equivalent to the Reduce step in MapReduce, sometimes called _accumulate_, _aggregate_ or _compress_.
[See Wikipedia for more.][wiki:fold]

[wiki:fold]: http://en.wikipedia.org/wiki/Fold_%28higher-order_function%29

### Using accumulators

MongooseIM uses a dedicated data structure to accumulate data related to stanza processing (see ["Accumulators"](accumulators.md)).
It is instantiated with an incoming stanza, passed along throughout the processing chain, supplied to and returned from certain hook calls, and terminated when stanza is leaving MongooseIM.

If a Mongoose accumulator is passed to a hook, handlers should store their return values in one of 3 ways:

* If it is a one-off value which doesn't need to be passed on along with the accumulator (can be overwritten any time), use `mongoose_acc:set(hook, result, Value, Acc)`.
* If the value is to be passed on to be reused within the current processing context, use `mongoose_acc:set(Namespace, Key, Value, Acc)`.
* If the value should be passed on to the recipient's session, pubsub node etc. use `mongoose_acc:set_permanent(Namespace, Key, Value, Acc)`.

A real life example, then, with regard to `mod_offline` is the `resend_offline_messages_hook` run in `ejabberd_c2s`:

```erlang
Acc1 = ejabberd_hooks:run_fold(resend_offline_messages_hook,
                               StateData#state.server,
                               Acc,
                               [StateData#state.user, StateData#state.server]),
Rs = mongoose_acc:get(offline, messages, Acc1, []),

```

### Sidenote: something deprecated

In the past you may have found some calls to `ejabberd_hooks:run/3` in the MongooseIM source code.
Under the hood it called the same handlers with `ok` as the initial accumulator.
This is deprecated and some day will be removed.

### Error handling in hooks

Hooks are meant to decouple modules; in other words, the caller signals that some event took place or that it intends to use a certain feature or a set of features, but how and if those features are implemented is beyond its interest.
For that reason hooks don't use the "let it crash" approach. Instead it is rather like "fire-and-forget", more similar in principle to the `Pid ! signal` way.

In practical terms: if a handler throws an error the hook machine logs a message and proceeds to the next handler with an unmodified accumulator.
If there are no handlers registered for a given hook, the `run_fold` call simply has no effect.

### Sidenote: Code yet to be written

Let's imagine, that when building a [minimum viable product][mvp] we settle on using `mod_offline` for delayed delivery of messages to unavailable clients.
However, while the product evolves (or the relevant client software catches up) we might drop `mod_offline` in favour of a more sophisticated solution like [Message Archive Management][mam] which would require a different action to be taken at the same point.
Thanks to loose coupling and `ejabberd_hooks` it's possible to turn off `mod_offline` and turn on `mod_mam` without changing
a single line of code in `ejabberd_sm`.

The only required change is to the configuration (apart from deploying the new module) which can even be performed at runtime - without restarting the server.

[mvp]: http://en.wikipedia.org/wiki/Minimum_viable_product
[mam]: http://xmpp.org/extensions/xep-0313.html

### Sidenote: Multiple Domains

A MongooseIM cluster may serve more than one domain at the same time.
E.g. it is quite common that services like Multi User Chat or Publish-Subscribe are available as subdomains of the main XMPP domain served by an installation.

## Registering hook handlers

In order to store a packet when `ejabberd_sm` runs `offline_message_hook` the relevant module must register a handler for this hook.
To attain the runtime configurability the module should register the handlers when it's loaded and unregister them when
it's unloaded.
That's usually done in, respectively, `start/2` and `stop/1` functions.
Here is the relevant snippet from `mod_offline:start/2`:

```erlang
ejabberd_hooks:add(offline_message_hook, Host,
                   ?MODULE, store_packet, 50)
```

It is clearly visible that the handler is added to the `offline_message_hook`.

`Host` corresponds to the `LServer` used in the aforementioned call to the `ejabberd_hooks:run_fold`, i.e. it's the XMPP domain for which the handler is to be executed.

The handler itself is specified as a module-function pair;
the arity of the function is not specified at the registration nor verified when calling the handler.
This may change in the future, but for now we recommend calling the hooks through the `mongoose_hooks` module and checking the arity there when writing a handler.
If the handler expects an incorrect number of arguments it will simply crash.

Multiple handlers may be registered for the same hook.
The last argument, 50, is the sequence number of this handler in the handler chain.
The higher the number, the later in the sequence the handler will be executed.
It's reasonable to keep this number small (e.g. in the range 0-100), though there's no real limit other than the size of the integer type in the Erlang VM.

## Unregistering handlers

Pluggability also requires the components to be unpluggable at will.
For that purpose there's the option to unregister a hook handler.
It's done in `mod_offline:stop/1` in a similar fashion to:

```erlang
ejabberd_hooks:delete(offline_message_hook, Host,
                      ?MODULE, store_packet, 50)
```

The arguments are exactly the same as passed to `ejabberd_hooks:add/5`.


### Sidenote: Metrics

Every time a hook is run, a corresponding metric of the same name in the same host is incremented by one.
There are some exceptions though as some metrics were implemented before the generic hook metrics.
List of hooks not updating generic metrics can be found in the `mongoose_metrics:hook_to_name/1` function.
Such skipped hooks update metrics defined in the `mongoose_metrics_hooks` module.

## Writing handlers

The signature of a handler has to follow three rules:

* Correct arity (the number of args passed to `run_fold` in `mongoose_hooks` + 1).
* The first arg is a mutable accumulator (may be `mongoose_acc` in particular).
* Returns an accumulator of the same type as the input one.

Let's look at this example, from MongooseIM codebase:

```erlang

process_iq_get(Acc, #jid{ lserver = FromS } = From, To, #iq{} = IQ, _ActiveList) ->
    MUCHost = gen_mod:get_module_opt_subhost(FromS, ?MODULE, default_host()),
    case {mod_muc_light_codec_backend:decode(From, To, IQ),
          gen_mod:get_module_opt_by_subhost(MUCHost, ?MODULE, blocking, ?DEFAULT_BLOCKING)} of
        {{ok, {get, #blocking{} = Blocking}}, true} ->
            Items = mod_muc_light_db_backend:get_blocking(jid:to_lus(From), MUCHost),
            mod_muc_light_codec_backend:encode(
              {get, Blocking#blocking{ items = Items }}, From, jid:to_lus(To),
              fun(_, _, Packet) -> put(encode_res, Packet) end),
            #xmlel{ children = ResponseChildren } = erase(encode_res),
            Result = {result, ResponseChildren},
            {stop, mongoose_acc:set(hook, result, Result, Acc)};
        {{ok, {get, #blocking{}}}, false} ->
            Result = {error, ?ERR_BAD_REQUEST},
            {stop, mongoose_acc:set(hook, result, Result, Acc)};
        _ ->
            Result = {error, ?ERR_BAD_REQUEST},
            mongoose_acc:set(hook, result, Result, Acc)
    end.
```

As seen in this example, a handler receives an accumulator, puts some value into it and returns it
for further processing.

There's also one important feature to note: in some cases our handler returns a tuple  `{stop, Acc}`.
This skips calling the latter actions in the handler sequence, while the call to `run_fold` returns the Acc.
If a handler returns just an atom `stop`, then all later actions are skipped and `run_fold` returns `stopped`.

Watch out! Different handlers may be registered for the same hook - the priority mechanism orders their execution.
If a handler returns `stop` but runs early in the handler chain, it may prevent some other handler from running at all!
That might or might not be intentional.
It may be especially surprising in case of handlers from different modules registered for the same hook.
Always ensure what handlers are registered for a given hook (`grep` is your friend) and that you understand their interdependencies.

## Hooks list and how to extract it

The following command should give you a list of all the hooks available in MongooseIM:

```bash
$ find src/ -name '*.erl' -print | xargs ./tools/find-hooks.awk \
> | sort | uniq
adhoc_local_commands
adhoc_local_items
...
...
...
webadmin_user_parse_query
```

Refer to `grep`/`ack` to find where they're used.

Here are the contents of `find-hooks.awk`:

```awk
#!/usr/bin/env awk -f
BEGIN {
    RS=")"
    ORS=""
    FS="[ (,]"
}

$0 ~ /ejabberd_hooks:run/ {
    found = -1
    for (i = 1; i < NF; i++) {
        if ($i ~ /ejabberd_hooks:run/) {
            found = i
        }
    }
    if (found != -1 && $(found+1) != "" && $(found+1) ~ /^[a-z]/)
        print $(found+1)"\n"
}
```

## Creating your own hooks

There's no special function or any setup necessary to create a new hook.
The only thing that needs to be done is calling `ejabberd_hooks:run/3` or `ejabberd_hooks:run_fold/4` with the name of the new hook and relevant arguments.
If you want static code analysis though, you should put the new hook inside `mongoose_hooks` with a correct type specification.

Of course, as long as no module registers handlers for this hook just running, it won't have any effects.

Similar is the case when a module registers handlers for some hook, but that hook is never run in the code.
That won't have an effect either.

The following is a self-contained example of a module which both runs and registers a few handlers for a completely new hook.
The handlers are run sequentially using disparate priorities and passing over an accumulator value.
One of the handlers stops the handler execution chain prematurely by returning `{stop, NewVal}`.
It's also possible to try out what happens when the same hook is run with different XMPP domains by passing an argument to `run_custom_hook/1` - we'll see that the handlers are registered for a particular domain only.

At the end, you can see a printout of an accumulator with some debugging info.

To cut the long story short:

```erlang
-module(mod_hook_example).
-author("bartek").

-behaviour(gen_mod).

-include("mongoose.hrl").

%% API
-export([run_custom_hook/1]).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% Hook handlers
-export([first_handler/2,
         stopping_handler/2,
         never_run_handler/2]).

start(Host, _Opts) ->
    ejabberd_hooks:add(custom_new_hook, Host, ?MODULE, first_handler, 25),
    ejabberd_hooks:add(custom_new_hook, Host, ?MODULE, stopping_handler, 50),
    ejabberd_hooks:add(custom_new_hook, Host, ?MODULE, never_run_handler, 75).

stop(Host) ->
    ejabberd_hooks:delete(custom_new_hook, Host, ?MODULE, first_handler, 25),
    ejabberd_hooks:delete(custom_new_hook, Host, ?MODULE, stopping_handler, 50),
    ejabberd_hooks:delete(custom_new_hook, Host, ?MODULE, never_run_handler, 75).

run_custom_hook(Host) ->
    Acc = mongoose_acc:new(#{ location => ?LOCATION, lserver => Host, element => undefined }),
    Acc1 = mongoose_acc:set(example, value, 5, Acc),
    ResultAcc = ejabberd_hooks:run_fold(custom_new_hook, Host, Acc1, [2]),
    ResultValue = mongoose_acc:get(example, value, ResultAcc),
    ?INFO_MSG("Final hook result: ~p", [ResultValue]),
    ?INFO_MSG("Returned accumulator: ~p", [ResultAcc]).

first_handler(Acc, Number) ->
    V0 = mongoose_acc:get(example, value, Acc),
    Result = V0 + Number,
    ?INFO_MSG("First handler~n"
    "  value: ~p~n"
    "  argument: ~p~n"
    "  will return: ~p",
        [V0, Number, Result]),
    mongoose_acc:set(example, value, Result, Acc).

stopping_handler(Acc, Number) ->
    V0 = mongoose_acc:get(example, value, Acc),
    Result = V0 + Number,
    ?INFO_MSG("Stopping handler~n"
    "  value: ~p~n"
    "  argument: ~p~n"
    "  will return: ~p",
        [V0, Number, Result]),
    {stop, mongoose_acc:set(example, value, Result, Acc)}.

never_run_handler(Acc, Number) ->
    ?INFO_MSG("This hook won't run as it's registered with a priority bigger "
    "than that of stopping_handler/2 is. "
    "It doesn't matter what it returns. "
    "This text should never get printed.", []),
    Acc * Number.
```

The module is intended to be used from the shell for educational purposes:

```erlang
(mongooseim@localhost)1> gen_mod:is_loaded(<<"localhost">>, mod_hook_example).
false
(mongooseim@localhost)2> gen_mod:start_module(<<"localhost">>, mod_hook_example, [no_opts]).
{ok,ok}
(mongooseim@localhost)3> gen_mod:is_loaded(<<"localhost">>, mod_hook_example).
true
(mongooseim@localhost)4> ejabberd_loglevel:set_custom(mod_hook_example, 4).
[{{lager_file_backend,"ejabberd.log"},ok},
 {lager_console_backend,ok},
 {lager_manager_killer,ok}]
(mongooseim@localhost)5> mod_hook_example:run_custom_hook(<<"localhost">>).
17:48:55.421 [info] First handler
  value: 5
  argument: 2
  will return: 7
ok
17:48:55.421 [info] Stopping handler
  value: 7
  argument: 2
  will return: 9
17:48:55.421 [info] Final hook result: 9
(mongooseim@localhost)6> 17:48:55.421 [info] Returned accumulator: #{lserver => <<"localhost">>,mongoose_acc => true,non_strippable => {set,0,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}},origin_location => {mod_hook_example,run_custom_hook,31},origin_pid => <0.1189.0>,origin_stanza => undefined,ref => #Ref<0.3348369203.60555266.196226>,stanza => undefined,timestamp => {1538,408935,405141},{example,value} => 9}

(mongooseim@localhost)6> mod_hook_example:run_custom_hook(<<"another-domain">>).
ok
17:49:11.672 [info] Final hook result: 5
(mongooseim@localhost)7> 17:49:11.673 [info] Returned accumulator: #{lserver => <<"another-domain">>,mongoose_acc => true,non_strippable => {set,0,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}},origin_location => {mod_hook_example,run_custom_hook,31},origin_pid => <0.1189.0>,origin_stanza => undefined,ref => #Ref<0.3348369203.60555270.195733>,stanza => undefined,timestamp => {1538,408951,672653},{example,value} => 5}

(mongooseim@localhost)7> gen_mod:stop_module(<<"localhost">>, mod_hook_example).
ok
```
