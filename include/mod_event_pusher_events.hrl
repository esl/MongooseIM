-include("jlib.hrl").

-record(user_status_event, {jid :: jid:jid(), status :: online | offline}).
-record(chat_event, {type :: headline | normal | chat | groupchat,
                     direction :: in | out,
                     from :: jid:jid(), to :: jid:jid(), packet :: exml:element()}).

-type event() :: #user_status_event{} | #chat_event{}.
