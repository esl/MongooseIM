-module(xmpp_router).
-export([route_wrap/4]).

-include("ejabberd.hrl").


-callback route(From :: ejabberd:jid(), To :: ejabberd:jid(),
                   Packet :: jlib:xmlel()) ->
    done | {ejabberd:jid(), ejabberd:jid(), jlib:xmlel()}.

-callback filter(From :: ejabberd:jid(), To :: ejabberd:jid(),
    Packet :: jlib:xmlel()) ->
    drop | {ejabberd:jid(), ejabberd:jid(), jlib:xmlel()}.



-spec route_wrap(Module :: module(),
            From :: ejabberd:jid(),
            To :: ejabberd:jid(),
            Packet :: jlib:xmlel() | ejabberd_c2s:broadcast()) ->
                done | {ejabberd:jid(), ejabberd:jid(), jlib:xmlel()}.
route_wrap(Module, From, To, Packet) ->
    case (catch Module:do_route(From, To, Packet)) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("error when routing from=~ts to=~ts in module=~p, reason=~p, packet=~ts, stack_trace=~p",
                       [jid:to_binary(From), jid:to_binary(To),
                        Module, Reason, exml:to_binary(Packet),
                        erlang:get_stacktrace()]);
        Res -> Res
    end.


