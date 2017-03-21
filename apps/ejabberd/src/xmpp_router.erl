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
-include("jlib.hrl").


-callback route(From :: ejabberd:jid(), To :: ejabberd:jid(),
                   Acc :: mongoose_acc:t(), Packet :: xmlel()) ->
    done | {ejabberd:jid(), ejabberd:jid(), mongoose_acc:t(), xmlel()}.

-callback filter(From :: ejabberd:jid(), To :: ejabberd:jid(),
    Acc :: mongoose_acc:t(), Packet :: xmlel()) ->
    drop | {ejabberd:jid(), ejabberd:jid(), mongoose_acc:t(), xmlel()}.


-export([call_route/4, call_filter/4]).

-spec call_route(Module :: module(), From :: ejabberd:jid(),
    To :: ejabberd:jid(), Packet :: mongoose_acc:t()) ->
    done | {ejabberd:jid(), ejabberd:jid(), mongoose_acc:t()}.
call_route(Module, From, To, Packet) ->
    Module:route(From, To, Packet).


-spec call_filter(Module :: module(), From :: ejabberd:jid(),
    To :: ejabberd:jid(), Packet :: mongoose_acc:t()) ->
    drop | {ejabberd:jid(), ejabberd:jid(), mongoose_acc:t()}.
call_filter(Module, From, To, Packet) ->
    Module:filter(From, To, Packet).
