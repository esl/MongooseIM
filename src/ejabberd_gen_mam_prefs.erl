-module(ejabberd_gen_mam_prefs).

-callback get_behaviour(Default :: mod_mam:archive_behaviour(),
        Host :: ejabberd:server(), ArcID :: mod_mam:archive_id(),
        LocJID :: ejabberd:jid(), RemJID :: ejabberd:jid()) -> any().

-callback set_prefs(Result :: any(), Host :: ejabberd:server(),
        ArcID :: mod_mam:archive_id(), ArcJID :: ejabberd:jid(),
        DefaultMode :: mod_mam:archive_behaviour(),
        AlwaysJIDs :: [ejabberd:literal_jid()],
        NeverJIDs :: [ejabberd:literal_jid()]) -> any().

-callback get_prefs(mod_mam:preference(), _Host :: ejabberd:server(),
        ArcId :: mod_mam:archive_id(), ArcJID :: ejabberd:jid())
            -> mod_mam:preference().
