-module(graphql_mnesia_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [require_rpc_nodes/1, mim/0]).
-import(graphql_helper, [execute_command/4, execute_user_command/5, user_to_bin/1,
                         get_ok_value/2, get_err_code/1]).

all() ->
    [{group, admin_mnesia_http},
     {group, admin_mnesia_cli}].

groups() ->
    [{admin_mnesia_http, [], admin_mnesia_handler()},
     {admin_mnesia_cli, [], admin_mnesia_handler()}].

admin_mnesia_handler() ->
    [set_master_test].

init_per_suite(Config) ->
    application:ensure_all_started(jid),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    Config1 = escalus:init_per_suite(Config),
    ejabberd_node_utils:init(mim(), Config1).

end_per_suite(_C) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]).

init_per_group(admin_mnesia_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_mnesia_cli, Config) ->
    graphql_helper:init_admin_cli(Config).

end_per_group(_, _Config) ->
    graphql_helper:clean(),
    escalus_fresh:clean().

% Admin tests

set_master_test(Config) ->
    Res = set_master(mim(), Config),
    ParsedRes = get_ok_value([data, mnesia, setMaster], Res),
    ?assertEqual("ok", ParsedRes).

set_master(Node, Config) ->
    execute_command(<<"mnesia">>, <<"setMaster">>, #{node => Node}, Config).
