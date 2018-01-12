-module(ejabberd_gen_mam_prefs).

-callback get_behaviour(Default :: mod_mam:archive_behaviour(),
        Host :: jlib:server(), ArcID :: mod_mam:archive_id(),
        LocJID :: jlib:jid(), RemJID :: jlib:jid()) -> any().

-callback set_prefs(Result :: any(), Host :: jlib:server(),
        ArcID :: mod_mam:archive_id(), ArcJID :: jlib:jid(),
        DefaultMode :: mod_mam:archive_behaviour(),
        AlwaysJIDs :: [jlib:literal_jid()],
        NeverJIDs :: [jlib:literal_jid()]) -> any().

-callback get_prefs(mod_mam:preference(), _Host :: jlib:server(),
        ArcId :: mod_mam:archive_id(), ArcJID :: jlib:jid())
            -> mod_mam:preference().
