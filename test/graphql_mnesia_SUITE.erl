-module(grapqql_mnesia_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile([export_all, nowarn_export_all]).

-define(OFFLINE_MSG_3_5_FIELDS, [us, timestamp, expire, from, to, packet]).

all() ->
    [{group, admin_mnesia}].

groups() ->
    [{admin_mnesia, [], admin_mnesia_handler()}].

admin_mnesia_handler() ->
    [set_master_test].

init_per_suite(C) ->
    application:ensure_all_started(jid),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    C.

end_per_suite(_C) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]).

init_per_group(admin_mnesia, Config) ->
    graphql_helper:init_admin_handler(Config).

end_per_group(admin_mnesia, _Config) ->
    escalus_fresh:clean().
