-module(xmpp_router).
-export([route/4]).
-include_lib("ejabberd/include/ejabberd.hrl").


-callback do_route(From :: ejabberd:jid(), To :: ejabberd:jid(), Packet :: jlib:xmlel()) -> ok.
route(Module,From,To,Packet) ->
    case (catch Module:do_route(From,To,Packet)) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p when routing ~ts from ~ts to ~ts in module: ~p",
                       [Reason, exml:to_binary(Packet),
                        jlib:jid_to_binary(From), jlib:jid_to_binary(To),
                        Module]);
        _ -> ok
    end.
