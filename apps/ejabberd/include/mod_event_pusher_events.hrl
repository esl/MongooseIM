-include("jlib.hrl").

-record(user_status_event, {jid :: jid(), status :: online | offline}).
-record(chat_event, {type :: chat | groupchat, direction :: in | out,
                     from :: jid(), to :: jid(), packet :: xmlel()}).

-type event() :: #user_status_event{} | #chat_event{}.
