-module(mod_pubsub_router).

-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

-include("pubsub.hrl").
-include("mongoose_logger.hrl").
-include("jlib.hrl").

-import(mod_pubsub, [
    do_route/7
]).

init([State]) ->
    {ok, State}.

handle_call(Msg, _From, State) ->
    {reply, Msg, State}.

handle_cast({route, From, To, Acc,
            #state{server_host = ServerHost, access = Access, plugins = Plugins} = State},
            _) ->
    Packet = mongoose_acc:element(Acc),
    case catch do_route(ServerHost, Access, Plugins, To#jid.lserver, From, To, Packet) of
        {'EXIT', Reason} -> ?ERROR_MSG("Function do_route failed with ~p\n", [Reason]);
        _ ->
            ok
    end,
    {noreply, State}.
