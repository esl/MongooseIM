%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(cluster_commands_SUITE).
-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [add_node_to_cluster/2,
                             is_sm_distributed/0,
                             mim/0, mim2/0, mim3/0,
                             remove_node_from_cluster/2,
                             require_rpc_nodes/1,
                             rpc/4]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).
-define(ne(A, B), ?assertNot(A == B)).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, clustered},
     {group, clustering_two}].

groups() ->
    [{clustered, [], [one_to_one_message]},
     {clustering_two, [], clustering_two_tests()}].

suite() ->
    require_rpc_nodes([mim, mim2, mim3]) ++ escalus:suite().

clustering_two_tests() ->
    [leave_using_rpc,
     join_twice_using_rpc,
     join_twice_in_parallel_using_rpc].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    #{node := Node1} = RPCNode1 = mim(),
    #{node := Node2} = RPCNode2 = mim2(),
    #{node := Node3} = RPCNode3 = mim3(),
    Config1 = ejabberd_node_utils:init(RPCNode1, Config),
    Config2 = ejabberd_node_utils:init(RPCNode2, Config1),
    Config3 = ejabberd_node_utils:init(RPCNode3, Config2),
    NodeCtlPath = distributed_helper:ctl_path(Node1, Config3),
    Node2CtlPath = distributed_helper:ctl_path(Node2, Config3),
    Node3CtlPath = distributed_helper:ctl_path(Node3, Config3),
    escalus:init_per_suite([{ctl_path_atom(Node1), NodeCtlPath},
                            {ctl_path_atom(Node2), Node2CtlPath},
                            {ctl_path_atom(Node3), Node3CtlPath}]
                           ++ Config3).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(Group, Config) when Group == clustered ->
    Node2 = mim2(),
    Config1 = add_node_to_cluster(Node2, Config),
    case is_sm_distributed() of
        true ->
            escalus:create_users(Config1, escalus:get_users([alice, clusterguy]));
        {false, Backend} ->
            ct:pal("Backend ~p doesn't support distributed tests", [Backend]),
            Node2 = mim2(),
            remove_node_from_cluster(Node2, Config1),
            {skip, nondistributed_sm}
    end;

init_per_group(Group, _Config) when Group == clustering_two orelse Group == clustering_three ->
    case is_sm_distributed() of
        true ->
            ok;
        {false, Backend} ->
            ct:pal("Backend ~p doesn't support distributed tests", [Backend]),
            {skip, nondistributed_sm}
    end;

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(Group, Config) when Group == clustered ->
    escalus:delete_users(Config, escalus:get_users([alice, clusterguy])),
    Node2 = mim2(),
    remove_node_from_cluster(Node2, Config);

%% Users are gone after mnesia cleaning
%% hence there is no need to delete them manually
end_per_group(Group, _Config) when Group == clustering_two orelse Group == clustering_three ->
    ok;
end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) when CaseName == join_twice_using_rpc
                                   orelse CaseName == join_twice_in_parallel_using_rpc ->
    Node2 = mim2(),
    remove_node_from_cluster(Node2, Config),
    escalus:end_per_testcase(CaseName, Config);

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------

one_to_one_message(ConfigIn) ->
    %% Given Alice connected to node one and ClusterGuy connected to node two
    Metrics = [{[global, data, dist], [{recv_oct, '>'}, {send_oct, '>'}]}],
    Config = [{mongoose_metrics, Metrics} | ConfigIn],
    escalus:story(Config, [{alice, 1}, {clusterguy, 1}], fun(Alice, ClusterGuy) ->
        %% When Alice sends a message to ClusterGuy
        Msg1 = escalus_stanza:chat_to(ClusterGuy, <<"Hi!">>),
        escalus:send(Alice, Msg1),
        %% Then he receives it
        Stanza1 = escalus:wait_for_stanza(ClusterGuy, 5000),
        escalus:assert(is_chat_message, [<<"Hi!">>], Stanza1),

        %% When ClusterGuy sends a response
        Msg2 = escalus_stanza:chat_to(Alice, <<"Oh hi!">>),
        escalus:send(ClusterGuy, Msg2),
        %% Then Alice also receives it
        Stanza2 = escalus:wait_for_stanza(Alice, 5000),
        escalus:assert(is_chat_message, [<<"Oh hi!">>], Stanza2)
    end).

%%--------------------------------------------------------------------
%% Manage cluster commands tests
%%--------------------------------------------------------------------

%% This function checks that it's ok to call mongoose_cluster:join/1 twice
join_twice_using_rpc(_Config) ->
    %% given
    #{node := Node1} = mim(),
    RPCSpec2 = mim2(),
    Timeout = timer:seconds(60),
    %% when
    ok = rpc(RPCSpec2#{timeout => Timeout}, mongoose_cluster, join, [Node1]),
    ok = rpc(RPCSpec2#{timeout => Timeout}, mongoose_cluster, join, [Node1]),
    %% then
    distributed_helper:verify_result(RPCSpec2, add),
    ok.

%% Check, that global transaction allows to run only one cluster operation at the time.
%% It should technically behave the same way as join_twice_using_rpc test (i.e. not fail).
join_twice_in_parallel_using_rpc(_Config) ->
    %% given
    #{node := Node1} = mim(),
    RPCSpec2 = mim2(),
    Timeout = timer:seconds(60),
    %% when
    Pid1 = proc_lib:spawn_link(fun() ->
        ok = rpc(RPCSpec2#{timeout => Timeout}, mongoose_cluster, join, [Node1])
        end),
    Pid2 = proc_lib:spawn_link(fun() ->
        ok = rpc(RPCSpec2#{timeout => Timeout}, mongoose_cluster, join, [Node1])
        end),
    %% then
    distributed_helper:verify_result(RPCSpec2, add),
    wait_for_process_to_stop(Pid1, Timeout),
    wait_for_process_to_stop(Pid2, Timeout),
    ok.

leave_using_rpc(Config) ->
    %% given
    Node1 = mim(),
    Node2 = mim2(),
    add_node_to_cluster(Node2, Config),
    %% when
    Result = distributed_helper:rpc(Node1#{timeout => timer:seconds(30)},
                                    mongoose_server_api, leave_cluster, []),
    ct:pal("leave_using_rpc result ~p~n", [Result]),
    %% then
    distributed_helper:verify_result(Node2, remove),
    ok.

%% Helpers
mongooseimctl_interactive(C, A, R, Config) ->
    #{node := DefaultNode} = mim(),
    mongooseimctl_interactive(DefaultNode, C, A, R, Config).
mongooseimctl_interactive(Node, Cmd, Args, Response, Config) ->
    CtlCmd = escalus_config:get_config(ctl_path_atom(Node), Config),
    run_interactive(string:join([CtlCmd, Cmd | normalize_args(Args)], " "), Response).

normalize_args(Args) ->
    lists:map(fun
                  (Arg) when is_binary(Arg) ->
                      binary_to_list(Arg);
                  (Arg) when is_list(Arg) ->
                      Arg
              end, Args).

ctl_path_atom(NodeName) ->
    CtlString = atom_to_list(NodeName) ++ "_ctl",
    list_to_atom(CtlString).

%% Long timeout for mnesia and ejabberd app restart
run_interactive(Cmd, Response) ->
    run_interactive(Cmd, Response, timer:seconds(60)).

run_interactive(Cmd, Response, Timeout) ->
    Port = erlang:open_port({spawn, Cmd}, [exit_status]),
    %% respond to interactive question (yes/no)
    Port ! {self(), {command, Response}},
    mongooseimctl_helper:loop(Cmd, [], Port, [], Timeout).

nodes_clustered(#{node := Node1Name} = Node1, #{node := Node2Name} = Node2, ShouldBe) ->
    DbNodes1 = distributed_helper:rpc(Node1, mnesia, system_info, [db_nodes]),
    DbNodes2 = distributed_helper:rpc(Node2, mnesia, system_info, [db_nodes]),
    Pairs = [{Node1Name, DbNodes2, ShouldBe},
        {Node2Name, DbNodes1, ShouldBe},
        {Node1Name, DbNodes1, true},
        {Node2Name, DbNodes2, true}],
    [?assertEqual(ShouldBelong, lists:member(Element, List))
        || {Element, List, ShouldBelong} <- Pairs].

have_node_in_mnesia(Node1, #{node := Node2}, ShouldBe) ->
    DbNodes1 = distributed_helper:rpc(Node1, mnesia, system_info, [db_nodes]),
    ?assertEqual(ShouldBe, lists:member(Node2, DbNodes1)).

wait_for_process_to_stop(Pid, Timeout) ->
    erlang:monitor(process, Pid),
    receive
        {'DOWN', _, process, Pid, _} -> ok
    after Timeout ->
            ct:fail(wait_for_process_to_stop_timeout)
    end.
