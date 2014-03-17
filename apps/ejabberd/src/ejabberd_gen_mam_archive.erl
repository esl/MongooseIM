-module(ejabberd_gen_mam_archive).

-callback archive_size(Size :: integer(), Host :: ejabberd:server(),
        ArchiveID :: mod_mam:archive_id(), ArchiveJID :: ejabberd:jid())
            -> integer().

-callback archive_message(_Result, ejabberd:server(),
        MessID :: mod_mam:message_id(), ArchiveID :: mod_mam:archive_id(),
        LocJID :: ejabberd:jid(), RemJID :: ejabberd:jid(),
        SrcJID :: ejabberd:jid(), Dir :: atom(), Packet :: any()) -> ok.

-callback lookup_messages(Result :: any(), Host :: ejabberd:server(),
        ArchiveID :: mod_mam:archive_id(), ArchiveJID :: ejabberd:jid(),
        RSM :: jlib:rsm_in() | undefined, Borders :: mod_mam:borders() | undefined,
        Start :: mod_mam:unix_timestamp() | undefined,
        End :: mod_mam:unix_timestamp() | undefined, Now :: mod_mam:unix_timestamp(),
        WithJID :: ejabberd:jid() | undefined, PageSize :: integer(),
        LimitPassed :: boolean() | opt_count, MaxResultLimit :: integer(),
        IsSimple :: boolean()) -> {ok, mod_mam:lookup_result()}
                                | {error, 'policy-violation'}.

-callback remove_archive(Host :: ejabberd:server(),
    ArchiveID :: mod_mam:archive_id(), ArchiveJID :: ejabberd:jid()) -> 'ok'.

-callback purge_single_message(Result :: any(), Host :: ejabberd:server(),
        MessID :: mod_mam:message_id(), ArchiveID :: mod_mam:archive_id(),
        ArchiveJID :: ejabberd:jid(), Now :: mod_mam:unix_timestamp())
            -> ok | {error, 'not-allowed' | 'not-found'}.

-callback purge_multiple_messages(Result :: any(), Host :: ejabberd:server(),
        ArchiveID :: mod_mam:archive_id(), ArchiveJID :: ejabberd:jid(),
        Borders :: mod_mam:borders() | undefined,
        Start :: mod_mam:unix_timestamp() | undefined,
        End :: mod_mam:unix_timestamp() | undefined,
        Now :: mod_mam:unix_timestamp(),
        WithJID :: ejabberd:jid() | undefined) -> ok | {error, 'not-allowed'}.
