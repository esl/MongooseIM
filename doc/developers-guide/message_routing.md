# Route of a message through the system

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