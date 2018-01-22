-module(ejabberd_gen_mam_prefs).

-callback get_behaviour(Default :: mod_mam:archive_behaviour(),
        Host :: jid:server(), ArcID :: mod_mam:archive_id(),
        LocJID :: jid:jid(), RemJID :: jid:jid()) -> any().

-callback set_prefs(Result :: any(), Host :: jid:server(),
        ArcID :: mod_mam:archive_id(), ArcJID :: jid:jid(),
        DefaultMode :: mod_mam:archive_behaviour(),
        AlwaysJIDs :: [jid:literal_jid()],
        NeverJIDs :: [jid:literal_jid()]) -> any().

-callback get_prefs(mod_mam:preference(), _Host :: jid:server(),
        ArcId :: mod_mam:archive_id(), ArcJID :: jid:jid())
            -> mod_mam:preference().
