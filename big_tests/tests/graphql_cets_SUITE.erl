-module(graphql_cets_SUITE).
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, mim2/0, rpc/4]).
-import(domain_helper, [host_type/1]).
-import(mongooseimctl_helper, [rpc_call/3]).
-import(graphql_helper, [execute_command/4, get_unauthorized/1, get_ok_value/2]).

all() ->
    [{group, admin_cets_cli},
     {group, admin_cets_http},
     {group, domain_admin_cets}].

groups() ->
    [{admin_cets_http, [parallel], admin_cets_tests()},
     {admin_cets_cli, [parallel], admin_cets_tests()},
     {domain_admin_cets, [], domain_admin_tests()}].

admin_cets_tests() ->
    [has_sm_table_in_info,
     available_nodes,
     unavailable_nodes,
     joined_nodes,
     discovered_nodes,
     remote_nodes_without_disco,
     remote_nodes_with_unknown_tables,
     remote_unknown_tables,
     remote_nodes_with_missing_tables,
     remote_missing_tables,
     conflict_nodes,
     conflict_tables,
     discovery_works].

domain_admin_tests() ->
    [domain_admin_get_table_info_test,
     domain_admin_get_system_info_test].

init_per_suite(Config) ->
    case rpc_call(mongoose_config, get_opt, [[internal_databases, cets, backend], undefined]) of
        rdbms ->
            Config1 = escalus:init_per_suite(Config),
            Config2 = ejabberd_node_utils:init(mim(), Config1),
            add_bad_node(),
            ok = rpc_call(cets_discovery, wait_for_ready, [mongoose_cets_discovery, 5000]),
            Config2 ++ distributed_helper:require_rpc_nodes([mim, mim2]);
        _ ->
            {skip, "CETS is not configured with RDBMS"}
    end.

end_per_suite(Config) ->
    ensure_bad_node_unregistered(),
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
init_per_testcase(_, Config) ->
    Config.

% Admin tests

has_sm_table_in_info(Config) ->
    Res = get_table_info(Config),
    Tables = get_ok_value([data, cets, tableInfo], Res),
    [T] = [T || T = #{<<"tableName">> := <<"cets_session">>} <- Tables],
    #{<<"memory">> := Mem, <<"nodes">> := Nodes, <<"size">> := Size} = T,
    ?assert(is_integer(Mem), T),
    ?assert(is_integer(Size), T),
    #{node := Node1} = mim(),
    assert_member(atom_to_binary(Node1), Nodes).

available_nodes(Config) ->
    #{node := Node1} = mim(),
    #{node := Node2} = mim2(),
    Res = get_system_info(Config),
    Info = get_ok_value([data, cets, systemInfo], Res),
    #{<<"availableNodes">> := Nodes} = Info,
    assert_member(atom_to_binary(Node1), Nodes),
    assert_member(atom_to_binary(Node2), Nodes),
    assert_not_member(<<"badnode@localhost">>, Nodes).

unavailable_nodes(Config) ->
    #{node := Node1} = mim(),
    #{node := Node2} = mim2(),
    Res = get_system_info(Config),
    Info = get_ok_value([data, cets, systemInfo], Res),
    #{<<"unavailableNodes">> := Nodes} = Info,
    assert_member(<<"badnode@localhost">>, Nodes),
    assert_not_member(atom_to_binary(Node1), Nodes),
    assert_not_member(atom_to_binary(Node2), Nodes).

joined_nodes(Config) ->
    #{node := Node1} = mim(),
    #{node := Node2} = mim2(),
    Res = get_system_info(Config),
    Info = get_ok_value([data, cets, systemInfo], Res),
    #{<<"joinedNodes">> := Nodes} = Info,
    assert_member(atom_to_binary(Node1), Nodes),
    assert_member(atom_to_binary(Node2), Nodes),
    assert_not_member(<<"badnode@localhost">>, Nodes).

remote_nodes_without_disco(Config) ->
    Res = get_system_info(Config),
    Info = get_ok_value([data, cets, systemInfo], Res),
    ?assert(is_list(maps:get(<<"remoteNodesWithoutDisco">>, Info)), Info).

remote_nodes_with_unknown_tables(Config) ->
    Res = get_system_info(Config),
    Info = get_ok_value([data, cets, systemInfo], Res),
    ?assert(is_list(maps:get(<<"remoteNodesWithUnknownTables">>, Info)), Info).

remote_unknown_tables(Config) ->
    Res = get_system_info(Config),
    Info = get_ok_value([data, cets, systemInfo], Res),
    ?assert(is_list(maps:get(<<"remoteUnknownTables">>, Info)), Info).

remote_nodes_with_missing_tables(Config) ->
    Res = get_system_info(Config),
    Info = get_ok_value([data, cets, systemInfo], Res),
    ?assert(is_list(maps:get(<<"remoteNodesWithMissingTables">>, Info)), Info).

remote_missing_tables(Config) ->
    Res = get_system_info(Config),
    Info = get_ok_value([data, cets, systemInfo], Res),
    ?assert(is_list(maps:get(<<"remoteMissingTables">>, Info)), Info).

conflict_nodes(Config) ->
    Res = get_system_info(Config),
    Info = get_ok_value([data, cets, systemInfo], Res),
    ?assertMatch(#{<<"conflictNodes">> := []}, Info).

conflict_tables(Config) ->
    Res = get_system_info(Config),
    Info = get_ok_value([data, cets, systemInfo], Res),
    ?assertMatch(#{<<"conflictTables">> := []}, Info).

conflict_nodes_count(Config) ->
    Res = get_system_info(Config),
    Info = get_ok_value([data, cets, systemInfo], Res),
    ?assertMatch(#{<<"conflictNodesCount">> := 0}, Info).

discovered_nodes(Config) ->
    #{node := Node1} = mim(),
    #{node := Node2} = mim2(),
    Res = get_system_info(Config),
    Info = get_ok_value([data, cets, systemInfo], Res),
    #{<<"discoveredNodes">> := Nodes} = Info,
    assert_member(atom_to_binary(Node1), Nodes),
    assert_member(atom_to_binary(Node2), Nodes),
    assert_member(<<"badnode@localhost">>, Nodes).

discovered_nodes_count(Config) ->
    Res = get_system_info(Config),
    Info = get_ok_value([data, cets, systemInfo], Res),
    #{<<"discoveredNodesCount">> := Count} = Info,
    ?assert(is_integer(Count), Info),
    ?assert(Count > 2, Info).

discovery_works(Config) ->
    Res = get_system_info(Config),
    Info = get_ok_value([data, cets, systemInfo], Res),
    ?assertMatch(#{<<"discoveryWorks">> := true}, Info).

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

add_bad_node() ->
    ensure_bad_node_unregistered(),
    register_bad_node(),
    force_check(),
    wait_for_has_bad_node().

register_bad_node() ->
    ClusterName = <<"mim">>,
    Node = <<"badnode@localhost">>,
    Num = 100,
    Address = <<>>,
    Timestamp = rpc(mim(), mongoose_rdbms_timestamp, select, []),
    InsertArgs = [ClusterName, Node, Num, Address, Timestamp],
    {updated, 1} = rpc(mim(), mongoose_cets_discovery_rdbms, insert_new, InsertArgs).

ensure_bad_node_unregistered() ->
    ClusterName = <<"mim">>,
    Node = <<"badnode@localhost">>,
    DeleteArgs = [ClusterName, Node],
    %% Ensure the node is removed
    {updated, _} = rpc(mim(), mongoose_cets_discovery_rdbms, delete_node_from_db, DeleteArgs).

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

assert_member(Elem, List) ->
    lists:member(Elem, List)
        orelse ct:fail({assert_member_failed, Elem, List}).

assert_not_member(Elem, List) ->
    lists:member(Elem, List)
        andalso ct:fail({assert_member_failed, Elem, List}).
