-module(ejabberd_gen_mam_user).

-callback get_archive_id(Host :: jid:server(), User :: jid:user()) ->
    undefined | mod_mam:archive_id().

