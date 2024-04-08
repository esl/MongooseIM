-module(graphql_server_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [is_sm_distributed/0,
                             mim/0, mim2/0, mim3/0,
                             remove_node_from_cluster/2,
                             require_rpc_nodes/1, rpc/4]).
-import(domain_helper, [host_type/0, domain/0]).
-import(graphql_helper, [execute_user_command/5, execute_command/4, get_ok_value/2,
                         get_err_msg/1, get_err_code/1, execute_command/5]).

-include_lib("eunit/include/eunit.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, admin_http},
     {group, admin_cli}].

groups() ->
    [{admin_http, [], admin_http_groups()},
     {admin_cli, [], admin_cli_groups()},
     {server_tests, [], admin_tests()},
     {clustering_tests, [], clustering_tests()},
     {clustering_http_tests, [], clustering_http_tests()}].

admin_cli_groups() ->
    [{group, server_tests},
     {group, clustering_tests}].

admin_http_groups() ->
    [{group, server_tests},
     {group, clustering_http_tests}].

admin_tests() ->
    [get_cookie_test,
     set_and_get_loglevel_test,
     get_status_test].

clustering_tests() ->
    [join_successful,
     leave_successful,
     join_unsuccessful,
     leave_but_no_cluster,
     join_twice,
     leave_twice,
     remove_dead_from_cluster,
     remove_alive_from_cluster,
     remove_node_test,
     stop_node_test].

clustering_http_tests() ->
    [join_successful_http,
     leave_successful_http,
     remove_dead_from_cluster_http,
     remove_alive_from_cluster_http,
     remove_node_test,
     stop_node_test].

init_per_suite(Config) ->
    Config1 = dynamic_modules:save_modules(host_type(), Config),
    Config2 = lists:foldl(fun(#{node := Node} = RPCNode, ConfigAcc) ->
        ConfigAcc1 = ejabberd_node_utils:init(RPCNode, ConfigAcc),
        NodeCtlPath = distributed_helper:ctl_path(Node, ConfigAcc1),
        ConfigAcc1 ++ [{ctl_path_atom(Node), NodeCtlPath}]
    end, Config1, [mim(), mim2(), mim3()]),
    escalus:init_per_suite(Config2).

ctl_path_atom(NodeName) ->
    CtlString = atom_to_list(NodeName) ++ "_ctl",
    list_to_atom(CtlString).

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(admin_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_cli, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(Group, Config) when Group =:= clustering_tests; Group =:= clustering_http_tests ->
    case is_sm_distributed() of
        true ->
            Config;
        {false, Backend} ->
            ct:pal("Backend ~p doesn't support distributed tests", [Backend]),
            {skip, nondistributed_sm}
    end;
init_per_group(_, Config) ->
    Config.

end_per_group(Group, _Config) when Group =:= admin_http;
                                   Group =:= admin_cli ->
    graphql_helper:clean();
end_per_group(_, _Config) ->
    escalus_fresh:clean().

init_per_testcase(set_and_get_loglevel_test = CaseName, Config) ->
    Config1 = mim_loglevel:save_log_level(Config),
    escalus:init_per_testcase(CaseName, Config1);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).


end_per_testcase(set_and_get_loglevel_test = CaseName, Config) ->
    mim_loglevel:restore_log_level(Config),
    escalus:end_per_testcase(CaseName, Config);
end_per_testcase(CaseName, Config) when CaseName == join_successful
                                   orelse CaseName == join_successful_http
                                   orelse CaseName == join_twice
                                   orelse CaseName == leave_twice ->
    remove_node_from_cluster(mim2(), Config),
    escalus:end_per_testcase(CaseName, Config);
end_per_testcase(CaseName, Config) when CaseName == remove_alive_from_cluster
                                   orelse CaseName == remove_dead_from_cluster
                                   orelse CaseName == remove_alive_from_cluster_http
                                   orelse CaseName == remove_dead_from_cluster_http ->
    remove_node_from_cluster(mim2(), Config),
    remove_node_from_cluster(mim3(), Config),
    escalus:end_per_testcase(CaseName, Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

get_cookie_test(Config) ->
    Result = get_ok_value([data, server, getCookie], get_cookie(Config)),
    ?assert(is_binary(Result)).

set_and_get_loglevel_test(Config) ->
    LogLevels = all_log_levels(),
    lists:foreach(fun(LogLevel) ->
        Value = get_ok_value([data, server, setLoglevel], set_loglevel(LogLevel, Config)),
        ?assertEqual(<<"Log level successfully set.">>, Value),
        Value1 = get_ok_value([data, server, getLoglevel], get_loglevel(Config)),
        ?assertEqual(LogLevel, Value1)
    end, LogLevels),
    {_, Res} = set_loglevel(<<"AAAA">>, Config),
    [Res1] = maps:get(<<"errors">>, Res),
    ?assertEqual(<<"unknown_enum">>, graphql_helper:get_value([extensions, code], Res1)).

get_status_test(Config) ->
    Result = get_ok_value([data, server, status], get_status(Config)),
    ?assertEqual(<<"RUNNING">>, maps:get(<<"statusCode">>, Result)),
    ?assert(is_binary(maps:get(<<"message">>, Result))),
    ?assert(is_binary(maps:get(<<"version">>, Result))),
    ?assert(is_binary(maps:get(<<"commitHash">>, Result))).


join_successful(Config) ->
    #{node := Node2} = RPCSpec2 = mim2(),
    leave_cluster(Config),
    get_ok_value([], join_cluster(atom_to_binary(Node2), Config)),
    distributed_helper:verify_result(RPCSpec2, add).

leave_successful(Config) ->
    #{node := Node2} = RPCSpec2 = mim2(),
    join_cluster(atom_to_binary(Node2), Config),
    get_ok_value([], leave_cluster(Config)),
    distributed_helper:verify_result(RPCSpec2, remove).

join_unsuccessful(Config) ->
    Node2 = mim2(),
    join_cluster(<<>>, Config),
    distributed_helper:verify_result(Node2, remove).

leave_but_no_cluster(Config) ->
    Node2 = mim2(),
    get_err_code(leave_cluster(Config)),
    distributed_helper:verify_result(Node2, remove).

join_twice(Config) ->
    #{node := Node2} = RPCSpec2 = mim2(),
    get_ok_value([], join_cluster(atom_to_binary(Node2), Config)),
    ?assertEqual(<<"already_joined">>, get_err_code(join_cluster(atom_to_binary(Node2), Config))),
    distributed_helper:verify_result(RPCSpec2, add).

leave_twice(Config) ->
    #{node := Node2} = RPCSpec2 = mim2(),
    join_cluster(atom_to_binary(Node2), Config),
    get_ok_value([], leave_cluster(Config)),
    distributed_helper:verify_result(RPCSpec2, remove),
    ?assertEqual(<<"not_in_cluster">>, get_err_code(leave_cluster(Config))).

remove_dead_from_cluster(Config) ->
    % given
    Timeout = timer:seconds(60),
    #{node := Node1Nodename} = Node1 = mim(),
    #{node := _Node2Nodename} = Node2 = mim2(),
    #{node := Node3Nodename} = Node3 = mim3(),
    ok = rpc(Node2#{timeout => Timeout}, mongoose_cluster, join, [Node1Nodename]),
    ok = rpc(Node3#{timeout => Timeout}, mongoose_cluster, join, [Node1Nodename]),
    %% when
    distributed_helper:stop_node(Node3Nodename, Config),
    get_ok_value([data, server, removeFromCluster],
                  remove_from_cluster(atom_to_binary(Node3Nodename), Config)),
    %% then
    % node is down hence its not in mnesia cluster
    have_node_in_mnesia_wait(Node1, Node2, true),
    have_node_in_mnesia_wait(Node1, Node3, false),
    have_node_in_mnesia_wait(Node2, Node3, false),
    % after node awakening nodes are clustered again
    distributed_helper:start_node(Node3Nodename, Config),
    have_node_in_mnesia_wait(Node1, Node3, true),
    have_node_in_mnesia_wait(Node2, Node3, true).

remove_alive_from_cluster(Config) ->
    % given
    Timeout = timer:seconds(60),
    #{node := Node1Name} = Node1 = mim(),
    #{node := Node2Name} = Node2 = mim2(),
    Node3 = mim3(),
    ok = rpc(Node2#{timeout => Timeout}, mongoose_cluster, join, [Node1Name]),
    ok = rpc(Node3#{timeout => Timeout}, mongoose_cluster, join, [Node1Name]),
    %% when
    %% Node2 is still running
    %% then
    get_ok_value([], remove_from_cluster(atom_to_binary(Node2Name), Config)),
    have_node_in_mnesia(Node1, Node3, true),
    have_node_in_mnesia(Node1, Node2, false),
    have_node_in_mnesia(Node3, Node2, false).

remove_node_test(Config) ->
    #{node := NodeName} = mim3(),
    Value = get_ok_value([data, server, removeNode], remove_node(NodeName, Config)),
    ?assertEqual(<<"MongooseIM node removed from the Mnesia schema">>, Value).

stop_node_test(Config) ->
    #{node := Node3Nodename} = mim3(),
    get_ok_value([data, server, stop], stop_node(Node3Nodename, Config)),
    Timeout = timer:seconds(3),
    F = fun() -> rpc:call(Node3Nodename, application, which_applications, [], Timeout) end,
    mongoose_helper:wait_until(F, {badrpc, nodedown}, #{sleep_time => 1000, name => stop_node}),
    distributed_helper:start_node(Node3Nodename, Config).

join_successful_http(Config) ->
    #{node := Node2} = RPCSpec2 = mim2(),
    leave_cluster(Config),
    distributed_helper:verify_result(RPCSpec2, remove),
    get_ok_value([], join_cluster(atom_to_binary(Node2), Config)),
    distributed_helper:verify_result(RPCSpec2, add).

leave_successful_http(Config) ->
    #{node := Node2} = RPCSpec2 = mim2(),
    join_cluster(atom_to_binary(Node2), Config),
    distributed_helper:verify_result(RPCSpec2, add),
    get_ok_value([], leave_cluster(Config)),
    distributed_helper:verify_result(RPCSpec2, remove).

remove_dead_from_cluster_http(Config) ->
    % given
    Timeout = timer:seconds(60),
    #{node := Node1Nodename} = Node1 = mim(),
    #{node := _Node2Nodename} = Node2 = mim2(),
    #{node := Node3Nodename} = Node3 = mim3(),
    ok = rpc(Node2#{timeout => Timeout}, mongoose_cluster, join, [Node1Nodename]),
    ok = rpc(Node3#{timeout => Timeout}, mongoose_cluster, join, [Node1Nodename]),
    %% when
    distributed_helper:stop_node(Node3Nodename, Config),
    F2 = fun() ->
        test == rpc(Node1#{timeout => Timeout}, mongoose_config, get_opt, [listen, test])
    end,
    mongoose_helper:wait_until(F2, false, #{sleep_time => 200, name => wait_for_mim1,
                                            time_left => timer:seconds(20)}),
    get_ok_value([data, server, removeFromCluster],
                  remove_from_cluster(atom_to_binary(Node3Nodename), Config)),
    have_node_in_mnesia_wait(Node1, Node2, true),
    have_node_in_mnesia_wait(Node1, Node3, false),
    have_node_in_mnesia_wait(Node2, Node3, false),
    % after node awakening nodes are clustered again
    distributed_helper:start_node(Node3Nodename, Config),
    ensure_node_started(Node3),
    have_node_in_mnesia_wait(Node1, Node3, true),
    have_node_in_mnesia_wait(Node2, Node3, true).

remove_alive_from_cluster_http(Config) ->
    % given
    Timeout = timer:seconds(60),
    #{node := Node1Name} = Node1 = mim(),
    #{node := Node2Name} = Node2 = mim2(),
    Node3 = mim3(),
    ok = rpc(Node2#{timeout => Timeout}, mongoose_cluster, join, [Node1Name]),
    ok = rpc(Node3#{timeout => Timeout}, mongoose_cluster, join, [Node1Name]),
    %% when
    %% Node2 is still running
    %% then
    get_ok_value([], remove_from_cluster(atom_to_binary(Node2Name), Config)),
    have_node_in_mnesia_wait(Node1, Node3, true),
    have_node_in_mnesia_wait(Node1, Node2, false),
    have_node_in_mnesia_wait(Node3, Node2, false).

ensure_node_started(Node) ->
    Timeout = timer:seconds(60),
    F = fun() ->
        case rpc(Node#{timeout => Timeout}, mongoose_server_api, status, []) of
            {ok, {true, _, _, _}} -> true;
            _Other -> false
        end
    end,
    mongoose_helper:wait_until(F, true, #{sleep_time => 200, name => wait_for_start_mim3,
                                          time_left => timer:seconds(20)}).

%-----------------------------------------------------------------------
%                                Helpers
%-----------------------------------------------------------------------

have_node_in_mnesia_wait(Node1, #{node := Node2}, Value) ->
    mongoose_helper:wait_until(fun() ->
                                   DbNodes1 = distributed_helper:rpc(Node1, mnesia,
                                                                     system_info, [db_nodes]),
                                   lists:member(Node2, DbNodes1)
                               end,
                               Value,
                               #{
                                 time_left => timer:seconds(12),
                                 sleep_time => 200,
                                 name => have_node_in_mnesia
                                }).

all_log_levels() ->
    [<<"NONE">>,
     <<"EMERGENCY">>,
     <<"ALERT">>,
     <<"CRITICAL">>,
     <<"ERROR">>,
     <<"WARNING">>,
     <<"NOTICE">>,
     <<"INFO">>,
     <<"DEBUG">>,
     <<"ALL">>].

have_node_in_mnesia(Node1, #{node := Node2}, ShouldBe) ->
    DbNodes1 = distributed_helper:rpc(Node1, mnesia, system_info, [db_nodes]),
    ?assertEqual(ShouldBe, lists:member(Node2, DbNodes1)).

get_cookie(Config) ->
    execute_command(<<"server">>, <<"getCookie">>, #{}, Config).

get_loglevel(Config) ->
    execute_command(<<"server">>, <<"getLoglevel">>, #{}, Config).

set_loglevel(LogLevel, Config) ->
    execute_command(<<"server">>, <<"setLoglevel">>, #{<<"level">> => LogLevel}, Config).

get_status(Config) ->
    execute_command(<<"server">>, <<"status">>, #{}, Config).

get_status(Node, Config) ->
    execute_command(Node, <<"server">>, <<"status">>, #{}, Config).

join_cluster(Node, Config) ->
    execute_command(<<"server">>, <<"joinCluster">>, #{<<"node">> => Node}, Config).

leave_cluster(Config) ->
    execute_command(<<"server">>, <<"leaveCluster">>, #{}, Config).

remove_from_cluster(Node, Config) ->
    execute_command(<<"server">>, <<"removeFromCluster">>, #{<<"node">> => Node}, Config).

stop_node(Node, Config) ->
    execute_command(Node, <<"server">>, <<"stop">>, #{}, Config).

remove_node(Node, Config) ->
    execute_command(Node, <<"server">>, <<"removeNode">>, #{<<"node">> => Node}, Config).
