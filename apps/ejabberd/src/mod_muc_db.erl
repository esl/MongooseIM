-module(mod_muc_db).

-callback init(ejabberd:server(), list()) -> ok.
-callback store_room(ejabberd:server(), ejabberd:server(),
                     mod_muc:room(), list()) ->
    {'aborted', _} | {'atomic', _}.
-callback restore_room(ejabberd:server(), ejabberd:server(), mod_muc:room()) ->
    'error' | 'undefined' | [any()].
-callback forget_room(ejabberd:server(), ejabberd:server(),
                      mod_muc:room()) -> 'ok'.
-callback can_use_nick(ejabberd:server(), ejabberd:server(),
                       ejabberd:jid(), mod_muc:nick()) -> boolean().
-callback get_rooms(ejabberd:server(), ejabberd:server()) -> list().
-callback get_nick(ejabberd:server(), ejabberd:server(), ejabberd:jid()) ->
    error | mod_muc:nick().
-callback set_nick(ejabberd:server(), ejabberd:server(),
                   ejabberd:jid(), mod_muc:nick()) -> term().
