-type username() :: jid:luser().

-type host() :: jid:lserver().

-type sender() :: binary().

-type content() :: binary().

-type count_bin() :: binary().

-type unread_bin() :: binary().

-type active_bin() :: binary().

-type name_bin() :: binary().

-type id() :: binary().

-type get_inbox_res() :: list(inbox_res()).

-type inbox_res() :: #{remote_jid := binary(),
                       msg := content(),
                       unread_count := count_bin(),
                       timestamp := integer(),
                       archive := binary(),
                       muted_until := binary()}.

-type entry_properties() :: #{unread_count := count_bin(),
                              archive := binary(),
                              muted_until := binary()}.

-type inbox_write_res() :: ok | {error, any()}.

-type marker() :: binary().

