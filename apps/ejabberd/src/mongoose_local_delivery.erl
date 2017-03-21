%%%-------------------------------------------------------------------
%%% @doc
%%% Completes delivery to local recipient or a component; called by
%%% main routing chain if it finds a handler to direct the message to.
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_local_delivery).
-author('bartlomiej.gorny@erlang-solutions.com').

-include("ejabberd.hrl").
-include("jlib.hrl").

%% API
-export([do_route/6]).


do_route(OrigFrom, OrigTo, OrigAcc, OrigPacket, LDstDomain, Handler) ->
    %% Filter locally
%%    ?ERROR_MSG(">>> OrigAcc: ~p~n", [OrigAcc]),
    case ejabberd_hooks:run_fold(filter_local_packet, LDstDomain,
        {OrigFrom, OrigTo, OrigAcc, OrigPacket}, []) of
        {From, To, Acc, Packet} ->
%%            ?ERROR_MSG(">>> Acc: ~p~n", [Acc]),
            NAcc = mongoose_acc:put(to_send, Packet, Acc), % so as not to rewrite everything at once
            mongoose_packet_handler:process(Handler, From, To, NAcc);
        drop ->
            ejabberd_hooks:run(xmpp_stanza_dropped,
                OrigFrom#jid.lserver,
                [OrigFrom, OrigTo, OrigPacket]),
            ok
    end.
