-module(mongoose_sanity_checks_SUITE).

-compile(export_all).

all() ->
    [is_mongooseim].


is_mongooseim(_Config) ->
    true = escalus_ejabberd:is_mongoose().