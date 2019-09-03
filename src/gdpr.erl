-module(gdpr).

-export_type(
    [data_group/0,
    schema/0,
    entries/0,
    personal_data/0]).

-type data_group() :: atom().
-type entry() :: [string() | binary()].
-type entries() :: [entry()].
-type schema() :: [string()].
-type personal_data() :: [{gdpr:data_group(), gdpr:schema(), gdpr:entries()}].

