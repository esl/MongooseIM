%%%-------------------------------------------------------------------
%%% @doc
%%% Completes delivery to local recipient or a component; called by
%%% main routing chain if it finds a handler to direct the message to.
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_local_delivery).
-author('bartlomiej.gorny@erlang-solutions.com').

-include("jlib.hrl").

%% API
-export([do_route/5]).


do_route(OrigFrom, OrigTo, OigPacket, LDstDomain, Handler) ->
    %% Filter locally
    OrigPacket = packet:pass(OigPacket, do_route),
    case ejabberd_hooks:run_fold(filter_local_packet, LDstDomain,
        {OrigFrom, OrigTo, OrigPacket}, []) of
        {From, To, Packet} ->
            Np = packet:pass(Packet, Handler),
            case Handler of
                {apply_fun, Fun} ->
                    Fun(From, To, Np);
                {apply, Module, Function} ->
                    Module:Function(From, To, Np)
            end;
        drop ->
            ejabberd_hooks:run(xmpp_stanza_dropped,
                OrigFrom#jid.lserver,
                [OrigFrom, OrigTo, OrigPacket]),
            ok
    end.
