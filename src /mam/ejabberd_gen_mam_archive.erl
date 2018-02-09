-module(ejabberd_gen_mam_archive).

-callback archive_size(Size :: integer(), Host :: jid:server(),
                       ArchiveID :: mod_mam:archive_id(), ArchiveJID :: jid:jid())
                      -> integer().

-callback archive_message(_Result, jid:server(),
                          MessID :: mod_mam:message_id(), ArchiveID :: mod_mam:archive_id(),
                          LocJID :: jid:jid(), RemJID :: jid:jid(),
                          SrcJID :: jid:jid(), Dir :: atom(), Packet :: any()) ->
    ok | {error, timeout}.

-callback lookup_messages(Result :: any(), Host :: jid:server(),
                          Params :: map()) -> Result when
      Result :: {ok, mod_mam:lookup_result()} | {error, 'policy-violation'}.

-callback remove_archive(Acc :: map(), Host :: jid:server(),
    ArchiveID :: mod_mam:archive_id(), ArchiveJID :: jid:jid()) -> map().

-callback purge_single_message(Result :: purge_single_message_result(), Host :: jid:server(),
                               MessID :: mod_mam:message_id(), ArchiveID :: mod_mam:archive_id(),
                               ArchiveJID :: jid:jid(), Now :: mod_mam:unix_timestamp())
                              -> purge_single_message_result().

-callback purge_multiple_messages(Result :: any(), Host :: jid:server(),
                                  ArchiveID :: mod_mam:archive_id(), ArchiveJID :: jid:jid(),
                                  Borders :: mod_mam:borders() | undefined,
                                  Start :: mod_mam:unix_timestamp() | undefined,
                                  End :: mod_mam:unix_timestamp() | undefined,
                                  Now :: mod_mam:unix_timestamp(),
                                  WithJID :: jid:jid() | undefined) ->
    ok | {error, 'not-allowed'}.

-type purge_single_message_result() :: ok | {error, 'not-allowed' | 'not-found' | term()}.

-export_type([purge_single_message_result/0]).

