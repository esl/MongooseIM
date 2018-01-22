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

-include("mongoose.hrl").
-include("jlib.hrl").


-callback route(From :: jid:jid(), To :: jid:jid(),
                   Acc :: mongoose_acc:t(), Packet :: exml:element()) ->
    done | {jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()}.

-callback filter(From :: jid:jid(), To :: jid:jid(),
    Acc :: mongoose_acc:t(), Packet :: exml:element()) ->
    drop | {jid:jid(), jid:jid(), mongoose_acc:t(), exml:element()}.


-export([call_route/5, call_filter/5]).

-spec call_route(Module :: module(), From :: jid:jid(),
    To :: jid:jid(), Acc :: mongoose_acc:t(), Packet :: exml:element()) ->
    done | {jid:jid(), jid:jid(), mongoose_acc:t()}.
call_route(Module, From, To, Acc, Packet) ->
    Module:route(From, To, Acc, Packet).


-spec call_filter(Module :: module(), From :: jid:jid(),
    To :: jid:jid(), Acc :: mongoose_acc:t(), Packet :: exml:element()) ->
    drop | {jid:jid(), jid:jid(), mongoose_acc:t()}.
call_filter(Module, From, To, Acc, Packet) ->
    Module:filter(From, To, Acc, Packet).
