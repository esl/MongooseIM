%%%-------------------------------------------------------------------
%%% @doc
%%% Completes delivery to local recipient or a component; called by
%%% main routing chain if it finds a handler to direct the message to.
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_local_delivery).
-author('bartlomiej.gorny@erlang-solutions.com').

-include("mongoose.hrl").
-include("jlib.hrl").

%% API
-export([do_route/5]).


do_route(OrigFrom, OrigTo, OrigAcc, OrigPacket, Handler) ->
    % strip acc from all sender-related values, from now on we are interested in the recipient
    LDstDomain = OrigTo#jid.lserver,
    case mongoose_domain_api:get_host_type(LDstDomain) of
        {error, not_found} ->
            %% This can happen e.g. for external components
            %% No handlers can be registered for filter_local_packet in this case
            Acc = mongoose_acc:strip(#{lserver => LDstDomain,
                                        from_jid => OrigFrom,
                                        to_jid => OrigTo,
                                        element => OrigPacket}, OrigAcc),
            mongoose_packet_handler:process(Handler, Acc, OrigFrom, OrigTo, OrigPacket);
        {ok, HostType} ->
            Acc0 = mongoose_acc:strip(#{lserver => LDstDomain,
                                        host_type => HostType,
                                        from_jid => OrigFrom,
                                        to_jid => OrigTo,
                                        element => OrigPacket}, OrigAcc),
            case mongoose_hooks:filter_local_packet({OrigFrom, OrigTo, Acc0, OrigPacket}) of
                {From, To, Acc, Packet} ->
                    Acc1 = mongoose_acc:update_stanza(#{from_jid => From,
                                                        to_jid => To,
                                                        element => Packet}, Acc),
                    mongoose_packet_handler:process(Handler, Acc1, From, To, Packet);
                drop ->
                    mongoose_hooks:xmpp_stanza_dropped(mongoose_acc:host_type(Acc0), OrigFrom,
                                                       OrigTo, OrigPacket),
                    ok
            end
    end.
