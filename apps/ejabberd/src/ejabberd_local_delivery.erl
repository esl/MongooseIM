%%%-------------------------------------------------------------------
%%% @doc
%%% Completes delivery to local recipient or a component; called by
%%% main routing chain if it finds a handler to direct the message to.
%%% @end
%%%-------------------------------------------------------------------
-module(ejabberd_local_delivery).
-author("bartek").

-include("jlib.hrl").

%% API
-export([do_route/5]).


do_route(OrigFrom, OrigTo, OrigPacket, LDstDomain, Handler) ->
    %% Filter locally
    case ejabberd_hooks:run_fold(filter_local_packet, LDstDomain,
        {OrigFrom, OrigTo, OrigPacket}, []) of
        {From, To, Packet} ->
            case Handler of
                {apply_fun, Fun} ->
                    Fun(From, To, Packet);
                {apply, Module, Function} ->
                    Module:Function(From, To, Packet)
            end;
        drop ->
            ejabberd_hooks:run(xmpp_stanza_dropped,
                OrigFrom#jid.lserver,
                [OrigFrom, OrigTo, OrigPacket]),
            ok
    end.
