-include("jlib.hrl").

-record(user_status_event, {jid :: jlib:jid(), status :: online | offline}).
-record(chat_event, {type :: headline | normal | chat | groupchat,
                     direction :: in | out,
                     from :: jlib:jid(), to :: jlib:jid(), packet :: exml:element()}).

-type event() :: #user_status_event{} | #chat_event{}.
