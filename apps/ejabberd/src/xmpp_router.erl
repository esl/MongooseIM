%%%-------------------------------------------------------------------
%%% @doc
%%% A behaviour which should be used by all modules being used in a
%%% routing pipeline. The pipeline, manage by ejabberd_router:route
%%% func, calls filter and route for each successful module.
%%%
%%% Module has to implement both functions, can be a no-op just returning
%%% a tuple of its args.
%%% @end
%%%-------------------------------------------------------------------
-module(xmpp_router).

-include("ejabberd.hrl").


-callback route(From :: ejabberd:jid(), To :: ejabberd:jid(),
                   Packet :: mongoose_acc:t()) ->
    done | {ejabberd:jid(), ejabberd:jid(), mongoose_acc:t()}.

-callback filter(From :: ejabberd:jid(), To :: ejabberd:jid(),
    Packet :: mongoose_acc:t()) ->
    drop | {ejabberd:jid(), ejabberd:jid(), mongoose_acc:t()}.



