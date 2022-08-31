-module(graphql_server_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [is_sm_distributed/0,
                             mim/0, mim2/0, mim3/0,
                             require_rpc_nodes/1]).
-import(domain_helper, [host_type/0, domain/0]).
-import(graphql_helper, [execute_user_command/5, execute_command/4, get_ok_value/2,
                         get_err_msg/1, get_err_code/1]).

-include_lib("eunit/include/eunit.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, admin_http},
     {group, admin_cli}].

groups() ->
    [{admin_http, [], admin_groups()},
     {admin_cli, [], admin_groups()},
     {server_tests, [], admin_tests()},
     {clustering_two_tests, [], clustering_tests()}].

admin_groups() ->
    [{group, server_tests},
     {group, clustering_two_tests}].

admin_tests() ->
    [get_cookie_test,
     get_loglevel_test,
     get_status_test,
     set_loglevel_test].

clustering_tests() ->
    [get_status_test, join_successful_prompt].

init_per_suite(Config) ->
    Config1 = dynamic_modules:save_modules(host_type(), Config),
    #{node := Node1} = RPCNode1 = mim(),
    #{node := Node2} = RPCNode2 = mim2(),
    #{node := Node3} = RPCNode3 = mim3(),
    Config2 = ejabberd_node_utils:init(RPCNode1, Config1),
    Config3 = ejabberd_node_utils:init(RPCNode2, Config2),
    Config4 = ejabberd_node_utils:init(RPCNode3, Config3),
    NodeCtlPath = distributed_helper:ctl_path(Node1, Config4),
    Node2CtlPath = distributed_helper:ctl_path(Node2, Config4),
    Node3CtlPath = distributed_helper:ctl_path(Node3, Config4),
    escalus:init_per_suite([{ctl_path_atom(Node1), NodeCtlPath},
                            {ctl_path_atom(Node2), Node2CtlPath},
                            {ctl_path_atom(Node3), Node3CtlPath}]
                           ++ Config3).

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
init_per_group(clustering_two_tests, Config) ->
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

get_cookie_test(Config) ->
    Result = get_ok_value([data, server, getCookie], get_cookie(Config)),
    ?assert(is_binary(Result)).

get_loglevel_test(Config) ->
    Result = get_ok_value([data, server, getLoglevel], get_loglevel(Config)),
    ?assert(is_binary(Result)).

get_status_test(Config) ->
    Result = get_ok_value([data, server, status], get_status(Config)),
    ?assertEqual(<<"RUNNING">>, maps:get(<<"statusCode">>, Result)),
    ?assert(is_binary(maps:get(<<"message">>, Result))).

join_successful_prompt(Config) ->
    #{node := Node2} = RPCSpec2 = mim2(),
    join_cluster(atom_to_binary(Node2), Config),
    distributed_helper:verify_result(RPCSpec2, add).

%leave_successful_prompt(Config) ->
%    Node2 = mim2(),
%    add_node_to_cluster(Node2, Config),
%    {_, OpCode} = mongooseimctl_interactive("leave_cluster", [], "yes\n", Config),
%    distributed_helper:verify_result(Node2, remove),
%    ?eq(0, OpCode).
%
%join_unsuccessful(Config) ->
%    Node2 = mim2(),
%    {_, OpCode} = mongooseimctl_interactive("join_cluster", [], "no\n", Config),
%    distributed_helper:verify_result(Node2, remove),
%    ?ne(0, OpCode).
%
%leave_unsuccessful(Config) ->
%    Node2 = mim(),
%    add_node_to_cluster(Node2, Config),
%    {_, OpCode} = mongooseimctl_interactive("leave_cluster", [], "no\n", Config),
%    distributed_helper:verify_result(Node2, add),
%    ?ne(0, OpCode).
%
%leave_but_no_cluster(Config) ->
%    Node2 = mim2(),
%    {_, OpCode} = mongooseimctl_interactive("leave_cluster", [], "yes\n", Config),
%    distributed_helper:verify_result(Node2, remove),
%    ?ne(0, OpCode).
%
%join_twice(Config) ->
%    #{node := Node2} = RPCSpec2 = mim2(),
%    {_, OpCode1} = mongooseimctl_interactive("join_cluster",
%                                             [atom_to_list(Node2)], "yes\n", Config),
%    {_, OpCode2} = mongooseimctl_interactive("join_cluster",
%                                             [atom_to_list(Node2)], "yes\n", Config),
%    distributed_helper:verify_result(RPCSpec2, add),
%    ?eq(0, OpCode1),
%    ?ne(0, OpCode2).

get_cookie(Config) ->
    execute_command(<<"server">>, <<"getCookie">>, #{}, Config).

get_loglevel(Config) ->
    execute_command(<<"server">>, <<"getLoglevel">>, #{}, Config).

get_status(Config) ->
    execute_command(<<"server">>, <<"status">>, #{}, Config).

join_cluster(Cluster, Config) ->
    execute_command(<<"server">>, <<"joinCluster">>, #{<<"cluster">> => Cluster}, Config).
