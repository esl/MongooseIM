-module(mongoose_sanity_checks_SUITE).

-compile([export_all, nowarn_export_all]).
-import(distributed_helper, [mim/0, rpc/4]).
-include_lib("eunit/include/eunit.hrl").

all() ->
    [is_mongooseim,
     running_internal_databases_match_the_configured_ones].


init_per_suite(Config) ->
    Config ++ distributed_helper:require_rpc_nodes([mim]).

end_per_suite(_Config) ->
    ok.

is_mongooseim(Config) ->
    mongooseim = escalus_server:name(Config).

running_internal_databases_match_the_configured_ones(_Config) ->
    Running = rpc(mim(), mongoose_internal_databases, running_internal_databases, []),
    Config = rpc(mim(), mongoose_internal_databases, configured_internal_databases, []),
    ?assert(is_list(Config), {Running, Config}),
    ?assertMatch(Config, Running).
