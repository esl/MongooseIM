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

-type inbox_res() :: {RemoteUser :: username(),
                      MsgContent :: content(),
                      UnreadCount :: count_bin(),
                      Timestamp :: erlang:timestamp()}.

-type inbox_write_res() :: ok | {error, any()}.

-type marker() :: binary().

-type single_query_result() :: {selected, [tuple()]} |
                               {updated, non_neg_integer() | undefined} |
                               {aborted, Reason :: term()} |
                               {error, Reason :: string() | duplicate_key}.

-type query_result() :: single_query_result() | [single_query_result()].
