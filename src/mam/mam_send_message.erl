%% Forwards extracted messages to the client.
%% The default implementation is in mod_mam_utils.
-module(mam_send_message).
-export([call_send_message/5]).

-ignore_xref([behaviour_info/1]).

-callback send_message(
            Row :: mod_mam:message_row(),
            ArcJID :: jid:jid(),
            From :: jid:jid(),
            Packet :: exml:element()) -> Acc :: mongoose_acc:t().

-spec call_send_message(
            SendModule :: module(),
            Row :: mod_mam:message_row(),
            ArcJID :: jid:jid(),
            From :: jid:jid(),
            Packet :: exml:element()) -> Acc :: mongoose_acc:t().
call_send_message(SendModule, Row, ArcJID, From, Packet) ->
    SendModule:send_message(Row, ArcJID, From, Packet).
