-type username() :: binary().
-type host() :: binary().
-type sender() :: binary().
-type content() :: binary().
-type count() :: binary().
-type id() :: binary().
-type get_inbox_res() :: list(inbox_res()).
-type inbox_res() :: {username(), content(), count()}.
-type inbox_write_res() :: ok | {error, any()}.
-type marker() :: binary().
-type single_query_result() :: {selected, [tuple()]} |
{updated, non_neg_integer() |undefined} |
{aborted, Reason :: term()} |
{error, Reason :: string() | duplicate_key}.
-type query_result() :: single_query_result() | [single_query_result()].
