-module(graphql_cets_SUITE).
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, rpc/4]).
-import(domain_helper, [host_type/1]).
-import(mongooseimctl_helper, [rpc_call/3]).
-import(graphql_helper, [execute_command/4, get_unauthorized/1, get_ok_value/2]).

all() ->
    [{group, admin_cets_cli},
     {group, admin_cets_http},
     {group, domain_admin_cets}].

groups() ->
    [{admin_cets_http, [sequence], admin_cets_tests()},
     {admin_cets_cli, [sequence], admin_cets_tests()},
     {domain_admin_cets, [], domain_admin_tests()}].

admin_cets_tests() ->
    [has_sm_table_in_info,
     add_bad_node,
     unavailable_nodes_are_listed,
     unavailable_nodes_count].

domain_admin_tests() ->
    [domain_admin_get_table_info_test,
     domain_admin_get_system_info_test].

init_per_suite(Config) ->
    Config1 = escalus:init_per_suite(Config),
    ejabberd_node_utils:init(mim(), Config1).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(admin_cets_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_cets_cli, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(domain_admin_cets, Config) ->
    graphql_helper:init_domain_admin_handler(Config).

end_per_group(_, _Config) ->
    graphql_helper:clean(),
    escalus_fresh:clean().

init_per_testcase(has_sm_table_in_info, Config) ->
    case rpc_call(ejabberd_sm, sm_backend, []) of
        ejabberd_sm_cets ->
            Config;
        _ ->
            {skip, "SM backend is not CETS"}
    end;
init_per_testcase(add_bad_node, Config) ->
    case rpc_call(mongoose_config, get_opt, [[internal_databases, cets, backend]]) of
        rdbms ->
            Config;
        _ ->
            {skip, "CETS is not configured with RDBMS"}
    end;
init_per_testcase(_, Config) ->
    Config.

% Admin tests

has_sm_table_in_info(Config) ->
    Res = get_table_info(Config),
    Tables = get_ok_value([data, cets, tableInfo], Res),
    [T] = [T || T = #{<<"tableName">> := <<"cets_session">>} <- Tables],
    #{<<"memory">> := Mem, <<"nodes">> := Nodes, <<"size">> := Size} = T,
    true = is_integer(Mem),
    true = is_integer(Size),
    #{node := Node1} = mim(),
    lists:member(Node1, Nodes).

add_bad_node(Config) ->
    register_bad_node(),
    force_check(),
    wait_for_has_bad_node().

unavailable_nodes_are_listed(Config) ->
    Res = get_system_info(Config),
    Info = get_ok_value([data, cets, systemInfo], Res),
    #{<<"unavailableNodes">> := UnNodes} = Info,
    true = is_list(UnNodes),
    [_|_] = UnNodes,
    true = lists:member(<<"badnode@localhost">>, UnNodes).

unavailable_nodes_count(Config) ->
    Res = get_system_info(Config),
    Info = get_ok_value([data, cets, systemInfo], Res),
    #{<<"unavailableNodesCount">> := UnNodesCount} = Info,
    true = is_integer(UnNodesCount),
    true = UnNodesCount > 0.

% Domain admin tests

domain_admin_get_table_info_test(Config) ->
    get_unauthorized(get_table_info(Config)).

domain_admin_get_system_info_test(Config) ->
    get_unauthorized(get_system_info(Config)).

%--------------------------------------------------------------------------------------------------
%                                         Helpers
%--------------------------------------------------------------------------------------------------

get_table_info(Config) ->
    execute_command(<<"cets">>, <<"tableInfo">>, #{}, Config).

get_system_info(Config) ->
    execute_command(<<"cets">>, <<"systemInfo">>, #{}, Config).

register_bad_node() ->
    ClusterName = <<"mim">>,
    Node = <<"badnode@localhost">>,
    Num = 100,
    Timestamp = rpc(mim(), mongoose_rdbms_timestamp, select, []),
    InsertArgs = [ClusterName, Node, Num, Timestamp],
    UpdateArgs = [Timestamp, ClusterName, Node],
    Res1 = rpc(mim(), mongoose_rdbms, execute, [global, cets_disco_insert_new, InsertArgs]),
    Res2 = rpc(mim(), mongoose_rdbms, execute, [global, cets_disco_update_existing, UpdateArgs]),
    check_sql_ok(Res1),
    check_sql_ok(Res2).

check_sql_ok({updated, 1}) -> ok;
check_sql_ok({error, duplicate_key}) -> ok.

force_check() ->
    Pid = rpc(mim(), erlang, whereis, [mongoose_cets_discovery]),
    true = is_pid(Pid),
    Pid ! check.

has_bad_node() ->
    #{unavailable_nodes := UnNodes} =
        rpc(mim(), cets_discovery, system_info, [mongoose_cets_discovery]),
    lists:member('badnode@localhost', UnNodes).

wait_for_has_bad_node() ->
    mongoose_helper:wait_until(fun() -> has_bad_node() end, true).
