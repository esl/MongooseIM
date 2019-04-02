-module(gdpr).

-export_type(
    [binary_table_name/0,
    schema/0,
    entities/0,
    username/0,
    domain/0]).

-type binary_table_name() :: binary().
-type entity() :: [term()].
-type entities() :: [entity()].
-type schema() :: [binary()].
-type domain() :: binary().
-type username() :: binary().

-callback get_personal_data(username(), domain()) ->
    [{binary_table_name(), schema(), entities()}].
