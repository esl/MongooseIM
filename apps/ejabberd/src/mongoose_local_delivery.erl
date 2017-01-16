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


do_route(From, To, OrigPacket, LDstDomain, Handler) ->
    Acc = mongoose_stanza:from_element(OrigPacket),
    Acc1 = mongoose_stanza:put(from_jid, From, Acc),
    Acc2 = mongoose_stanza:put(to_jid, To, Acc1),
    #jid{lserver=LServer} = To,
    Acc3 = mongoose_stanza:put(routing_decision, send, mongoose_stanza:put(lserver, LServer, Acc2)),
    %% Filter locally
    Acc4 = ejabberd_hooks:run_fold(filter_local_packet, LDstDomain, Acc3, []),
    case mongoose_stanza:get(routing_decision, Acc4) of
        send ->
            Packet = mongoose_stanza:get(element, Acc4),
            case Handler of
                {apply_fun, Fun} ->
                    Fun(From, To, Packet);
                {apply, Module, Function} ->
                    Module:Function(From, To, Packet)
            end;
        drop ->
            ejabberd_hooks:run(xmpp_stanza_dropped,
                From#jid.lserver,
                [From, To, OrigPacket]),
            ok
    end.
