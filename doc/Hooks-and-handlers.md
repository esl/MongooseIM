_Radek Szymczyszyn <radoslaw.szymczyszyn@erlang-solutions.com>_

The hooks and handlers mechanism is one of the core architectural
features of ejabberd.
It allows for loose coupling between components of the system
by calling only those which are available and configured
to be used at runtime.
It can be thought of as a simple eventing mechanism notifying about
certain things happening in the server.
That results in an extensible system with pluggable extra functionality.

To focus our attention we'll analyze `mod_offline` which is responsible
for storing messages for delivery to users unavailable at the time
of sending.
`mod_offline` is an implementation of [XEP-0203][xep-0203].

[xep-0203]: http://xmpp.org/extensions/xep-0203.html

## Running a hook

`ejabberd_sm` (ejabberd session manager) is the module which discovers
whether the recipient of a message is available or not.
That's where storage of the message for later delivery takes place.
The simplest way of calling into `mod_offline` to store a message
would be to make a call like:

```erlang
mod_offline:store_packet(From, To, Packet)
```

However, that would couple `ejabberd_sm` with `mod_offline`.
(I.e. if `mod_offline` was not available the code would simply crash;
if it was misconfigured or turned off the behaviour would be undefined.)
To avoid that coupling and also to enable other
([possibly yet to be written](#sidenote-yet-to-be-written))
code to carry out some action at this particular
moment of the message processing pipeline `ejabberd_sm` instead calls:

```erlang
ejabberd_hooks:run(offline_message_hook, LServer,
                   [From, To, Packet])
```

The extra level of indirection introduced by this call gives the flexibility
to determine at runtime what code actually gets run at this point.
`offline_message_hook` is just the name of the hook (in other words of
the event that is being signalled); `From`, `To` and `Packet` are the
arguments passed to the handler just as they would in case of the
function being called directly; `LServer` is
[the XMPP domain for which this hook is signalled](#sidenote-multiple-domains).

So how does this runtime configuration actually look like?

### Sidenote: Yet to be written

Let's imagine, that when building a [minimum viable product][mvp]
we settle on using `mod_offline`
for delayed delivery of messages to unavailable clients.
However, while the product evolves (or the relevant client software
catches up) we might drop `mod_offline` in favour of a more
sophisticated solution like [Message Archive Management][mam]
which would require a different action to be taken at the same point.
Thanks to loose coupling and `ejabberd_hooks` it's possible
to turn off `mod_offline` and turn on `mod_mam` without changing
a single line of code in `ejabberd_sm`.
The only required change is to the configuration (apart from deploying
the new module) which can even be performed at runtime - without
restarting the server.

[mvp]: http://en.wikipedia.org/wiki/Minimum_viable_product
[mam]: http://xmpp.org/extensions/xep-0313.html

### Sidenote: Multiple Domains

An ejabberd cluster may serve more than one domain at the same time.
E.g. it's quite common that services such as Multi User Chat or
Publish-Subscribe are available as subdomains of the main XMPP domain
served by an installation.

## Registering hook handlers

In order to store a packet when `ejabberd_sm` runs `offline_message_hook`
the relevant module must register a handler for this hook.
To attain the runtime configurability the module
should register the handlers when it's loaded and unregister them when
it's unloaded.
That's usually done in, respectively, `start/2` and `stop/1` functions.
Here's the relevant snippet from `mod_offline:start/2`:

```erlang
ejabberd_hooks:add(offline_message_hook, Host,
                   ?MODULE, store_packet, 50)
```

It's clearly visible that the handler is added to `offline_message_hook`.

`Host` corresponds to `LServer` used in the aforementioned call to
`ejabberd_hooks:run`, i.e. it's the XMPP domain for which the handler is
to be executed.

The handler itself is specified as a module-function pair; the arity of
the function is neither specified at registration nor verified when
calling the handler, so be careful to pass the appropriate number of
arguments to `ejabberd_hooks:run` - otherwise the handler will crash.

Of course, multiple handlers may be registered for the same hook.
The last argument, 50, is the sequence number of this handler in the
handler chain.
The higher the number, the later in the sequence the handler will
be executed.
It's reasonable to keep this number small (e.g. in the range 0-100),
though there's no real limit other than the size of the integral type
in the Erlang VM.

## Unregistering handlers

Pluggability also requires the components to be unpluggable at will.
For that purpose there's also the option to unregister a hook handler.
It's exercised as follows in `mod_offline:stop/1`:

```erlang
ejabberd_hooks:delete(offline_message_hook, Host,
                      ?MODULE, store_packet, 50)
```

The arguments are exactly the same as passed to `ejabberd_hooks:add/5`.

## How to get results from handlers?

Apart from being able to notify the rest of the system that some event has
happened by running a hook with `ejabberd_hooks:run/3`, it's also possible
to [fold](#sidenote-folds) over a sequence
of handlers with `ejabberd_hooks:run_fold/4`.
Like an ordinary `lists:foldl/3`, `ejabberd_hooks:run_fold/4` also requires
an initial value to be passed to the function.
An example with regard to `mod_offline` is the
`resend_offline_messages_hook` run in `ejabberd_c2s`:

```erlang
ejabberd_hooks:run_fold(resend_offline_messages_hook,
                        StateData#state.server,
                        [],
                        [StateData#state.user,
                         StateData#state.server])
```

In between the XMPP domain (`StateData#state.server`)
and handler(s) arguments the initial value of the accumulator
being passed through the sequence of handlers is inserted - in this
case an empty list (`[]`).

Note, that `ejabberd_hooks:run/3` and `ejabberd_hooks:run_fold/4` **are not
interchangeable**.
You must decide whether the hook is to return some value
or only carry out an action when designing it.

### Sidenote: Folds

If you haven't encountered the term _fold_ before,
then it's _reduce_ (like `Array.reduce`) in Ruby-speak,
roughly equivalent to the Reduce step in MapReduce
and also sometimes called _accumulate_, _aggregate_ or _compress_.
[See Wikipedia for more.][wiki:fold]

[wiki:fold]: http://en.wikipedia.org/wiki/Fold_%28higher-order_function%29

## Writing handlers

Dependant on whether a handler is to be folded over
or not its signature varies slightly.
Let's compare `store_packet/3` and `get_sm_features/5`:

```erlang
store_packet(From, To, Packet) ->
    Type = xml:get_tag_attr_s(<<"type">>, Packet),
    if
        (Type /= <<"error">>) and (Type /= <<"groupchat">>) and
        (Type /= <<"headline">>) ->
            case check_event_chatstates(From, To, Packet) of
                true ->
                    #jid{luser = LUser, lserver = LServer} = To,
                    TimeStamp = now(),
                    {xmlelement, _Name, _Attrs, Els} = Packet,
                    Expire = find_x_expire(TimeStamp, Els),
                    gen_mod:get_module_proc(To#jid.lserver, ?PROCNAME)
                        ! #offline_msg{us = {LUser, LServer},
                                       timestamp = TimeStamp,
                                       expire = Expire,
                                       from = From,
                                       to = To,
                                       packet = Packet},
                    stop;
                _ ->
                    ok
            end;
        true ->
            ok
    end.


get_sm_features(Acc, _From, _To, <<"">>, _Lang) ->
    Feats = case Acc of
                {result, I} -> I;
                _ -> []
            end,
    {result, Feats ++ [?NS_FEATURE_MSGOFFLINE]};

get_sm_features(_Acc, _From, _To, ?NS_FEATURE_MSGOFFLINE, _Lang) ->
    %% override all lesser features...
    {result, []};

get_sm_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.
```

`store_packet/3` is not folded over - the arity of the handler matches
exactly the number of arguments passed to `ejabberd_hooks:run/3`.

That's not the case with `get_sm_features/5` which is folded over - the
extra parameter (inserted before the actual hook arguments) is the accumulator
value passed through the sequence of all handlers.
As seen in the example, a handler might either adjust the passed in
value, overwrite it completely or just pass unmodified.

There's also one important feature to note: `store_packet/3` returns the
atom `stop` once it delegates storing the message to the relevant process.
This skips calling the latter actions in the handler sequence.

The same can be achieved for handlers returning a value by returning
`stop` (then `ejabberd_hooks:run_fold/4` returns `stopped`)
or `{stop, NewVal}` (then `ejabberd_hooks:run_fold/4` returns
`NewVal`).

Watch out! Different handlers may be registered for the same
hook - the priority mechanism orders their execution.
If some handler returns `stop` but runs early in the handler chain it may
prevent some other handler from running at all!
That might or might not be intentional.
It may be especially surprising in case of handlers from different
modules registered for the same hook.
Always ensure what handlers are registered for a given hook (`grep` is your
friend) and that you understand their interdependencies.

## Hooks list and how to extract it

The following command should give you a list of all the hooks available in
ejabberd
(and some garbage filtering out automatically isn't worth the effort):

```bash
$ find ejabberd/src/ -name '*.erl' -print | xargs ./find-hooks.awk \
> | sort | uniq
# ... snip out the ~5 lines of garbage ...
adhoc_local_commands
adhoc_local_items
adhoc_sm_commands
adhoc_sm_items
anonymous_purge_hook
c2s_broadcast_recipients
c2s_loop_debug
c2s_presence_in
c2s_stream_features
c2s_unauthenticated_iq
c2s_update_presence
callbacks
caps_update
check_bl_c2s
disco_info
disco_local_features
disco_local_identity
disco_local_items
disco_sm_features
disco_sm_identity
disco_sm_items
ejabberd_ctl_process
filter_packet
find_s2s_bridge
forbidden_session_hook
handler # that's garbage too
http_request_debug
local_send_to_resource_hook
message_sent_to_user
offline_message_hook
presence_probe_hook
privacy_check_packet
privacy_get_user_list
privacy_iq_get
privacy_iq_set
privacy_updated_list
pubsub_create_node
pubsub_delete_node
pubsub_publish_item
register_user
remove_user
reopen_log_hook
resend_offline_messages_hook
resend_subscription_requests_hook
roster_get
roster_get_jid_info
roster_get_subscription_lists
roster_get_versioning_feature
roster_groups
roster_in_subscription
roster_out_subscription
roster_process_item
s2s_allow_host
s2s_connect_hook
s2s_loop_debug
s2s_stream_features
set_presence_hook
sm_register_connection_hook
sm_remove_connection_hook
unset_presence_hook
user_available_hook
user_ping_timeout
user_receive_packet
user_send_packet
vcard_set
webadmin_menu_host
webadmin_menu_hostnode
webadmin_menu_main
webadmin_menu_node
webadmin_page_host
webadmin_user
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

$0 ~ /ejabberd_hooks/ {
    found = -1
    for (i = 1; i < NF; i++) {
        if ($i ~ /ejabberd_hooks/) {
            found = i
        }
    }
    if (found != -1 && $(found+1) != "")
        print $(found+1)"\n"
}
```

## Creating your own hooks

There's no special function or any setup necessary to create a new hook.
The only thing that needs to be done is calling
`ejabberd_hooks:run/3` or `ejabberd_hooks:run_fold/4` with the name
of the new hook and relevant arguments.

Of course, as long as no module registers handlers for this hook just
running it won't have any effects.

Similar is the case when a module registers handlers for some hook,
but that hook is never run in the code.
That won't have an effect either.

The following is a self-contained example of a module which both runs and
registers a few handlers for a completely new hook.
The handlers are run sequentially using disparate priorities
and passing over an accumulator value.
One of the handlers stops the handler execution chain prematurely
by returning `{stop, NewVal}`.
It's also possible to try out what happens when the same hook is run with
different XMPP domains by passing an argument to `run_custom_hook/1` - we'll
see that the handlers are registered for a particular domain only.

To cut the long story short:

```erlang
-module(mod_hook_example).

-behaviour(gen_mod).

%% API
-export([run_custom_hook/1]).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% Hook handlers
-export([first_handler/2,
         stopping_handler/2,
         never_run_handler/2]).

-include("ejabberd.hrl").

start(Host, _Opts) ->
    ejabberd_hooks:add(custom_new_hook, Host, ?MODULE, first_handler, 25),
    ejabberd_hooks:add(custom_new_hook, Host, ?MODULE, stopping_handler, 50),
    ejabberd_hooks:add(custom_new_hook, Host, ?MODULE, never_run_handler, 75).

stop(Host) ->
    ejabberd_hooks:delete(custom_new_hook, Host, ?MODULE, first_handler, 25),
    ejabberd_hooks:delete(custom_new_hook, Host, ?MODULE, stopping_handler, 50),
    ejabberd_hooks:delete(custom_new_hook, Host, ?MODULE, never_run_handler, 75).

run_custom_hook(Host) ->
    Result = ejabberd_hooks:run_fold(custom_new_hook, Host, 5, [2]),
    ?INFO_MSG("Final hook result: ~p", [Result]).

first_handler(Acc, Number) ->
    Result = Acc + Number,
    ?INFO_MSG("First handler~n"
              "  accumulator: ~p~n"
              "  argument: ~p~n"
              "  will return: ~p",
             [Acc, Number, Result]),
    Result.

stopping_handler(Acc, Number) ->
    Result = {stop, Acc + Number},
    ?INFO_MSG("Stopping handler~n"
              "  accumulator: ~p~n"
              "  argument: ~p~n"
              "  will return: ~p",
             [Acc, Number, Result]),
    Result.

never_run_handler(Acc, Number) ->
    ?INFO_MSG("This hook won't run as it's registered with a priority bigger "
              "than that of stopping_handler/2 is. "
              "It doesn't matter what it returns. "
              "This text should never get printed.", []),
    Acc * Number.
```

The module is intended to be used from the shell for educational purposes:

```erlang
(ejabberd@localhost)1> gen_mod:is_loaded(<<"localhost">>, mod_hook_example).
false
(ejabberd@localhost)2> gen_mod:start_module(<<"localhost">>, mod_hook_example, [no_opts]).
ok
(ejabberd@localhost)3> gen_mod:is_loaded(<<"localhost">>, mod_hook_example).
true
(ejabberd@localhost)4> ejabberd_loglevel:set_custom(mod_hook_example, 4).
{module,ejabberd_logger}
(ejabberd@localhost)5> mod_hook_example:run_custom_hook(<<"localhost">>).
ok
(ejabberd@localhost)6>
=INFO REPORT==== 30-Oct-2013::15:33:36 ===
I(<0.47.0>:mod_hook_example:35) : First handler
accumulator: 5
argument: 2
will return: 7

=INFO REPORT==== 30-Oct-2013::15:33:36 ===
I(<0.47.0>:mod_hook_example:44) : Stopping handler
  accumulator: 7
  argument: 2
  will return: {stop,9}

=INFO REPORT==== 30-Oct-2013::15:33:36 ===
I(<0.47.0>:mod_hook_example:31) : Final hook result: 9

(ejabberd@localhost)6> mod_hook_example:run_custom_hook(<<"another-domain">>).
ok
(ejabberd@localhost)7>
=INFO REPORT==== 30-Oct-2013::15:33:41 ===
I(<0.47.0>:mod_hook_example:31) : Final hook result: 5

(ejabberd@localhost)7> gen_mod:stop_module(<<"localhost">>, mod_hook_example).
{atomic,ok}
(ejabberd@localhost)8>
```

## Selected hooks description

### `user_send_packet`

```erlang
ejabberd_hooks:run(user_send_packet, Server,
                   [FromJID, ToJID, Stanza])
```

This hook is run in `ejabberd_c2s` after the user sends a packet.
Some rudimentary verification of the stanza is done once it is received
from the socket:

- if present, the `from` attribute of the stanza is checked against
  the identity of the user whose session the process in question serves;
  in case of the identity not matching the contents of the attribute an error
  is returned,

- the recipient JID (`to` attribute) format is verified.

The hook is not run for stanzas which do not pass these basic validity
checks. Neither are such stanzas further processed by the server.

This hook won't be called for stanzas which arrive from a user served
by a federated server (i.e. on a server-to-server connection handled
by `ejabberd_s2s`) and are intended for a user served by the relevant
ejabberd instance.

### `user_receive_packet`

```erlang
ejabberd_hooks:run(user_receive_packet, Server,
                   [Jid, From, To, FixedPacket])
```

The hook is run just after a packet has been sent to the user.

Prior to sending, the packet is verified against any relevant
privacy lists (the mechanism is described in [XEP-0016][privacy-lists]).
The privacy list mechanism itself is not mandatory and requires
`mod_privacy` to be configured; otherwise all stanzas are allowed
to pass.

[privacy-lists]: http://xmpp.org/extensions/xep-0016.html

This hook won't run for stanzas which are destined to users of a different
XMPP domain served by a federated server, connection to which is handled
by `ejabberd_s2s`.

### `filter_packet`

```erlang
ejabberd_hooks:run_fold(filter_packet,
                        {OrigFrom, OrigTo, OrigPacket}, [])
```

This hook is run by `ejabberd_router` as soon as the packet is routed
via `ejaberd_router:route/3`.
This is the most general function used to route stanzas across the
entire cluster and its calls are scattered all over ejabberd code.
`ejabberd_c2s` calls it after it receives a packet from
`ejabberd_receiver` (i.e. the socket) and multiple modules use it for
sending replies and errors.

As seen in the example the handlers take no arguments.
The accumulator is the packet which may or may not be filtered out
(in case the handler chain returns `drop`) or modified.

Note that this hook is run with `ejabberd_hooks:run_fold/3`,
not the usual and already mentioned `ejabberd_hooks:run_fold/4`.
The ternary variant doesn't take the XMPP domain argument
and hence it's not possible to register per-domain handlers for this hook.
Keep that in mind when registering the handlers and
appropriately use `ejabberd_hooks:add/4` instead of `ejabberd_hooks:add/5`.

### `offline_message_hook`

```erlang
ejabberd_hooks:run(offline_message_hook,
                   Server,
                   [From, To, Packet])
```

`ejabberd_sm` runs this hook once it determines that a routed stanza
is a message and that no resource (i.e. device or desktop client
application) of its recipient is available online for delivery.

The conservative approach is to use `mod_offline` to store that message in
a persistent way until the recipient becomes online and the message can be
successfully delivered.
The handler in `mod_offline` stores the message and returns `stop`.
In case some handler is registered with a sequence number greater than 50
it will not be called if `mod_offline` successfully stores the message.

### `remove_user`

```erlang
ejabberd_hooks:run(remove_user, Server, [User, Server])
```

`remove_user` is run by `ejabberd_auth` - the authentication module - when
a request is made to remove the user from the database of the server.
The hook is used to allow modules clean up the user data in any relevant
way - in a process state, persistent storage or by notifying some external
service.

### Route of a message through the system

In case of a message sent from User A to User B, both of whom are served by
the same domain, the flow of the message through the system is as follows:

1.  User A's `ejabberd_receiver` receives the stanza and passes
    it to `ejabberd_c2s`.

2.  Upon some minimal validation of the stanza `user_send_packet` is
    called.

3.  The stanza is checked against any privacy lists in use and,
    in case of being allowed, routed by `ejabberd_router:route/3`.

4.  `ejabberd_router:route/3` runs the `filter_packet` hook.
    The stanza may be dropped or modified at this point.

5.  In case of a local route, that we assumed, the stanza is passed
    to `ejabberd_local` responsible for routing inside the local node.

    This step is hard to guess just from analyzing the code.
    What is helpful is peeking into the `route` Mnesia table used
    by `ejabberd_router` to determine the route:

    ```erlang
    (ejabberd@localhost)2> ets:tab2list(route).
    [{route,<<"vjud.localhost">>,
            {apply_fun,#Fun<ejabberd_router.2.123745223>}},
     {route,<<"muc.localhost">>,
            {apply_fun,#Fun<mod_muc.2.63726579>}},
     {route,<<"localhost">>,{apply,ejabberd_local,route}}]
    ```

    Here we see that for domain "localhost" the action to take
    is to call `ejabberd_local:route()`.

6.  `ejabberd_local` routes the stanza to `ejabberd_sm` given it's
    got at least a bare JID as the recipient.

7.  `ejabberd_sm` determines the available resources of User B,
    takes into account their priorities and whether the message is
    addressed to a particular resource or a bare JID and appropriately
    replicates (or not) the message and sends it to the recipient's
    `ejabberd_c2s` process(es).

    In case no resources are available for delivery
    (hence no `ejabberd_c2s` processes to pass the message to),
    `offline_message_hook` is run.

8.  `ejabberd_c2s` verifies the stanza against any relevant privacy lists
    and sends it one the socket.
    `user_receive_packet` hook is run to notify the rest of the system
    about stanza delivery to User B.
