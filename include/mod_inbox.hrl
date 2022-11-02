-type id() :: binary().

-type marker() :: binary().

-type inbox_res() :: #{remote_jid := binary(),
                       msg_id := id(),
                       msg := exml:element(),
                       timestamp := integer(),
                       muted_until := integer(),
                       unread_count := integer(),
                       box := binary()}.

-type entry_properties() :: mod_inbox_entries:properties() | inbox_res().
