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


do_route(OrigFrom, OrigTo, OrigPacket, LDstDomain, Handler) ->
    %% Filter locally
    case ejabberd_hooks:run_fold(filter_local_packet, LDstDomain,
        {OrigFrom, OrigTo, OrigPacket}, []) of
        {From, To, Packet} ->
            mongoose_packet_handler:process(Handler, From, To, Packet);
        drop ->
            ejabberd_hooks:run(xmpp_stanza_dropped,
                OrigFrom#jid.lserver,
                [OrigFrom, OrigTo, OrigPacket]),
            ok
    end.
