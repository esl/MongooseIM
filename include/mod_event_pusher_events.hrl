-include("jlib.hrl").

-record(user_status_event, {jid :: jid:jid(), status :: online | offline}).
-record(chat_event, {type :: headline | normal | chat | groupchat,
                     direction :: in | out,
                     from :: jid:jid(), to :: jid:jid(), packet :: exml:element()}).
-record(unack_msg_event, {user :: jid:user(), server :: jid:server(),
                          resource :: jid:resource()}).

-type event() :: #user_status_event{} | #chat_event{} | #unack_msg_event{}.
