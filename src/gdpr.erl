-module(gdpr).

-export_type(
    [table_name/0,
    schema/0,
    entities/0,
    username/0,
    domain/0]).


-type table_name() :: atom().
-type entity() :: [term()] | tuple() | string().
-type entities() :: [entity()].
-type schema() :: [binary() | string()].
-type domain() :: binary().
-type username() :: binary().

-callback get_personal_data(username(), domain()) ->
    [{table_name(), schema(), entities()}].
