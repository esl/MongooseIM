-module(xmpp_router).
-export([route/4]).
-export([filter/4]).

-include("ejabberd.hrl").


-callback do_route(From :: ejabberd:jid(), To :: ejabberd:jid(),
                   Packet :: jlib:xmlel()) ->
    done | {ejabberd:jid(), ejabberd:jid(), jlib:xmlel()}.

-callback do_filter(From :: ejabberd:jid(), To :: ejabberd:jid(),
    Packet :: jlib:xmlel()) ->
    drop | {ejabberd:jid(), ejabberd:jid(), jlib:xmlel()}.

-spec route(Module :: module(),
            From :: ejabberd:jid(),
            To :: ejabberd:jid(),
            Packet :: jlib:xmlel() | ejabberd_c2s:broadcast()) ->
                done | {ejabberd:jid(), ejabberd:jid(), jlib:xmlel()}.
route(Module, From, To, Packet) ->
    case (catch Module:do_route(From, To, Packet)) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("error when routing from=~ts to=~ts in module=~p, reason=~p, packet=~ts, stack_trace=~p",
                       [jid:to_binary(From), jid:to_binary(To),
                        Module, Reason, exml:to_binary(Packet),
                        erlang:get_stacktrace()]);
        Res -> Res
    end.

-spec filter(Module :: module(),
    From :: ejabberd:jid(),
    To :: ejabberd:jid(),
    Packet :: jlib:xmlel() | ejabberd_c2s:broadcast()) ->
    drop | {ejabberd:jid(), ejabberd:jid(), jlib:xmlel()}.
filter(Module, From, To, Packet) ->
    case (catch Module:do_filter(From, To, Packet)) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("error when filtering from=~ts to=~ts in module=~p, reason=~p, packet=~ts, stack_trace=~p",
                [jid:to_binary(From), jid:to_binary(To),
                    Module, Reason, exml:to_binary(Packet),
                    erlang:get_stacktrace()]);
        Res -> Res
    end.
