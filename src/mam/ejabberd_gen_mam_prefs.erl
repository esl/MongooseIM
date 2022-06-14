-module(ejabberd_gen_mam_prefs).

-ignore_xref([behaviour_info/1]).

-callback get_behaviour(Default :: mod_mam_pm:archive_behaviour(),
        Host :: jid:server(), ArcID :: mod_mam_pm:archive_id(),
        LocJID :: jid:jid(), RemJID :: jid:jid()) -> any().

-callback set_prefs(Result :: any(), Host :: jid:server(),
        ArcID :: mod_mam_pm:archive_id(), ArcJID :: jid:jid(),
        DefaultMode :: mod_mam_pm:archive_behaviour(),
        AlwaysJIDs :: [jid:literal_jid()],
        NeverJIDs :: [jid:literal_jid()]) -> any().

-callback get_prefs(mod_mam_pm:preference(), _Host :: jid:server(),
        ArcId :: mod_mam_pm:archive_id(), ArcJID :: jid:jid())
            -> mod_mam_pm:preference().

