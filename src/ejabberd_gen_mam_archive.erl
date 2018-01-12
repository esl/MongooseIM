-module(ejabberd_gen_mam_archive).

-callback archive_size(Size :: integer(), Host :: jlib:server(),
                       ArchiveID :: mod_mam:archive_id(), ArchiveJID :: jlib:jid())
                      -> integer().

-callback archive_message(_Result, jlib:server(),
                          MessID :: mod_mam:message_id(), ArchiveID :: mod_mam:archive_id(),
                          LocJID :: jlib:jid(), RemJID :: jlib:jid(),
                          SrcJID :: jlib:jid(), Dir :: atom(), Packet :: any()) ->
    ok | {error, timeout}.

-callback lookup_messages(Result :: any(), Host :: jlib:server(),
                          Params :: map()) -> Result when
      Result :: {ok, mod_mam:lookup_result()} | {error, 'policy-violation'}.

-callback remove_archive(Acc :: map(), Host :: jlib:server(),
    ArchiveID :: mod_mam:archive_id(), ArchiveJID :: jlib:jid()) -> map().

-callback purge_single_message(Result :: purge_single_message_result(), Host :: jlib:server(),
                               MessID :: mod_mam:message_id(), ArchiveID :: mod_mam:archive_id(),
                               ArchiveJID :: jlib:jid(), Now :: mod_mam:unix_timestamp())
                              -> purge_single_message_result().

-callback purge_multiple_messages(Result :: any(), Host :: jlib:server(),
                                  ArchiveID :: mod_mam:archive_id(), ArchiveJID :: jlib:jid(),
                                  Borders :: mod_mam:borders() | undefined,
                                  Start :: mod_mam:unix_timestamp() | undefined,
                                  End :: mod_mam:unix_timestamp() | undefined,
                                  Now :: mod_mam:unix_timestamp(),
                                  WithJID :: jlib:jid() | undefined) ->
    ok | {error, 'not-allowed'}.

-type purge_single_message_result() :: ok | {error, 'not-allowed' | 'not-found' | term()}.

-export_type([purge_single_message_result/0]).

