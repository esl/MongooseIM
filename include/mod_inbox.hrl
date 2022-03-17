-type content() :: binary().

-type count_bin() :: binary().

-type name_bin() :: binary().

-type id() :: binary().

-type get_inbox_res() :: list(inbox_res()).

-type inbox_res() :: #{remote_jid := binary(),
                       msg := exml:element(),
                       unread_count := integer(),
                       msg_id := id(),
                       timestamp := integer(),
                       archive := boolean(),
                       muted_until := integer()}.

-type entry_properties() :: #{unread_count := integer(),
                              archive := boolean(),
                              muted_until := integer()}.

-type marker() :: binary().
