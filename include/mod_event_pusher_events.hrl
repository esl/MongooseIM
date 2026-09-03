-include("jlib.hrl").

-record(user_status_event, {jid :: jid:jid(), status :: online | offline}).
-record(msg_event, {type :: headline | normal | chat | groupchat,
                     direction :: in | out,
                     from :: jid:jid(), to :: jid:jid(), packet :: exml:element(),
                     user_status :: ejabberd_sm:user_status()}).
-record(unack_msg_event, {type :: headline | normal | chat | groupchat,
                          to :: jid:jid(), packet :: exml:element()}).
