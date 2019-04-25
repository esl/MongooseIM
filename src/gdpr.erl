-module(gdpr).

-export_type(
    [username/0,
    domain/0]).

-type domain() :: binary().
-type username() :: binary().
