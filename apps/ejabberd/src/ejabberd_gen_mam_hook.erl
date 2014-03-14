-module(ejabberd_gen_mam_hook).

-callback archive_size(Size :: integer(), Host :: ejabberd:server(),
        ArcId :: mod_mam:archive_id(), ArcJID :: ejabberd:jid()) -> integer().

-callback archive_message(_Result, ejabberd:server(), MessID :: mod_mam:message_id(),
        RoomID :: mod_mam:room_id(), LocJID :: ejabberd:jid(), RemJID :: ejabberd:jid(),
        SrcJID :: ejabberd:jid(), incoming, Packet :: any()) -> ok.

-callback lookup_messages(Result :: any(), Host :: ejabberd:server(),
        ArcID :: mod_mam:archive_id(), ArcJID :: ejabberd:jid(), RSM :: jlib:rsm_in(),
        Borders :: mod_mam:borders(), Start :: mod_mam:unix_timestamp(),
        End :: mod_mam:unix_timestamp(), Now :: mod_mam:unix_timestamp(),
        WithJID :: ejabberd:jid(), PageSize :: integer(), LimitPassed :: boolean(),
        MaxResultLimit :: integer(), IsSimple :: boolean()) -> any().

-callback remove_archive(Host :: ejabberd:server(), RoomId :: mod_mam:room_id(),
        RoomJID :: ejabberd:jid()) -> 'ok'.

-callback purge_single_message(Result :: any(), Host :: ejabberd:server(),
        MessID :: mod_mam:message_id(), RoomID :: mod_mam:room_id(),
        RoomJID :: ejabberd:jid(), Now :: mod_mam:unix_timestamp())
            -> ok | {error, 'not-allowed' | 'not-found'}.

-callback purge_multiple_messages(Result :: any(), Host :: ejabberd:server(),
        RoomID :: mod_mam:room_id(), RoomJID :: ejabberd:jid(),
        Borders :: mod_mam:borders() | undefined,
        Start :: mod_mam:unix_timestamp() | undefined,
        End :: mod_mam:unix_timestamp() | undefined,
        Now :: mod_mam:unix_timestamp(),
        WithJID :: ejabberd:jid() | undefined) -> ok | {error, 'not-allowed'}.
