-module(mongoose_component_packet_handler).

-behaviour(mongoose_packet_handler).

-export([process_packet/5]).

-spec process_packet(
        mongoose_acc:t(), jid:jid(), jid:jid(), exml:element(), #{pid := pid()}) ->
    mongoose_acc:t().
process_packet(Acc, _From, _To, _El, #{pid := Pid}) ->
    mongoose_component_connection:route(Pid, Acc),
    Acc.
