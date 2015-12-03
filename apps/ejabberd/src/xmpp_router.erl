-module(xmpp_router).
-export([route/4]).

-include("ejabberd.hrl").


-callback do_route(From :: ejabberd:jid(), To :: ejabberd:jid(),
                   Packet :: jlib:xmlel()) -> ok.

-spec route(Module :: module(), From :: ejabberd:jid(), To :: ejabberd:jid(),
            Packet :: jlib:xmlel()) -> ok.
route(Module,From,To,Packet) ->
    case (catch Module:do_route(From,To,Packet)) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("error when routing from=~ts to=~ts in module=~p, reason=~p, packet=~ts, stack_trace=~p",
                       [jid:to_binary(From), jid:to_binary(To),
                        Module, Reason, exml:to_binary(Packet),
                        erlang:get_stacktrace()]);
        _ -> ok
    end.

