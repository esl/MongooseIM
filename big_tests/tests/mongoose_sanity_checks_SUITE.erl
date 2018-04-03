-module(mongoose_sanity_checks_SUITE).

-compile(export_all).

all() ->
    [is_mongooseim].


is_mongooseim(Config) ->
    mongooseim = escalus_server:name(Config).