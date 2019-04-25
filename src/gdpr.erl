-module(gdpr).

-export_type(
    [data_group/0,
    schema/0,
    entries/0]).

-type data_group() :: atom().
-type entry() :: [string() | binary()].
-type entries() :: [entry()].
-type schema() :: [string()].

-callback get_personal_data(jid:user(), jid:server()) ->
    [{data_group(), schema(), entries()}].
