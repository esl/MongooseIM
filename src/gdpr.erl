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

-callback remove_user(jid:user(), jid:server()) ->
    ok.

%% Sometimes personal data must not be removed,
%% because a user is no longer the only owner of it
-optional_callbacks([remove_user/2]).

