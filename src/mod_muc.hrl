-type room_host() :: ejabberd:simple_bare_jid().

-record(muc_room, {
    name_host,
    opts
}).

-type muc_room() :: #muc_room{
name_host    :: room_host(),
opts         :: list()
}.

-record(muc_online_room, {name_host,
    pid
}).

-type muc_online_room() :: #muc_online_room{
name_host :: room_host(),
pid       :: pid()
}.
