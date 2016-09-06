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
-compile(export_all).

-import(distributed_helper, [add_node_to_cluster/2, rpc/5,
        remove_node_from_cluster/2, is_sm_distributed/0]).
-import(ejabberdctl_helper, [ejabberdctl/3, rpc_call/3]).
-import(ejabberd_node_utils, [mim/0, mim2/0, mim3/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-define(LOCAL_NODE, mim()).
-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).
-define(ne(A, B), ?assertNot(A == B)).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, clustered},
        {group, mnesia},
        {group, clustering_two},
        {group, clustering_three}].
groups() ->
    [{clustered, [], [one_to_one_message]},
        {clustering_two, [], clustering_two_tests()},
        {clustering_three, [], clustering_three_tests()},
        {mnesia, [], [set_master_test]}].
suite() ->
    require_all_nodes() ++
    escalus:suite().

clustering_two_tests() ->
    [join_successful_prompt,
        join_successful_force,
        leave_successful_prompt,
        leave_successful_force,
        join_unsuccessful,
        leave_unsuccessful,
        leave_but_no_cluster,
        join_twice,
        leave_twice].

clustering_three_tests() ->
    [cluster_of_three,
        leave_the_three,
        remove_dead_from_cluster,
        remove_alive_from_cluster].

require_all_nodes() ->
    [{require, mim_node, {hosts, mim, node}},
     {require, mim_node2, {hosts, mim2, node}},
     {require, mim_node3, {hosts, mim3, node}}].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Node1 = mim(),
    Node2 = mim2(),
    Node3 = mim3(),
    Config1 = ejabberd_node_utils:init(Node1, Config),
    Config2 = ejabberd_node_utils:init(Node2, Config1),
    Config3 = ejabberd_node_utils:init(Node3, Config2),
    NodeCtlPath = distributed_helper:ctl_path(Node1, Config3),
    Node2CtlPath = distributed_helper:ctl_path(Node2, Config3),
    Node3CtlPath = distributed_helper:ctl_path(Node3, Config3),
    escalus:init_per_suite([{ctl_path_atom(Node1), NodeCtlPath},
        {ctl_path_atom(Node2), Node2CtlPath},
        {ctl_path_atom(Node3), Node3CtlPath}]
    ++ Config3).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(Group, Config) when Group == clustered orelse Group == mnesia ->
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

end_per_group(Group, Config) when Group == clustered orelse Group == mnesia ->
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

end_per_testcase(cluster_of_three, Config) ->
    Node2 = mim2(),
    Node3 = mim3(),
    remove_node_from_cluster(Node2, Config),
    remove_node_from_cluster(Node3, Config),
    escalus:end_per_testcase(cluster_of_three, Config);

end_per_testcase(CaseName, Config) when CaseName == remove_alive_from_cluster
                                   orelse CaseName == remove_dead_from_cluster->
    Node3 = mim3(),
    Node2 = mim2(),
    remove_node_from_cluster(Node3, Config),
    remove_node_from_cluster(Node2, Config),
    escalus:end_per_testcase(CaseName, Config);

end_per_testcase(CaseName, Config) when CaseName == join_successful_prompt
                                   orelse CaseName == join_successful_force
                                   orelse CaseName == leave_unsuccessful_prompt
                                   orelse CaseName == leave_unsuccessful_force
                                   orelse CaseName == join_twice ->
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
%% mnesia tests
%%--------------------------------------------------------------------

set_master_test(ConfigIn) ->
    TableName = passwd,
    NodeList =  rpc_call(mnesia, system_info, [running_db_nodes]),
    ejabberdctl("set_master", ["self"], ConfigIn),
    [MasterNode] = rpc_call(mnesia, table_info, [TableName, master_nodes]),
    true = lists:member(MasterNode, NodeList),
    RestNodesList = lists:delete(MasterNode, NodeList),
    OtherNode = hd(RestNodesList),
    ejabberdctl("set_master", [atom_to_list(OtherNode)], ConfigIn),
    [OtherNode] = rpc_call(mnesia, table_info, [TableName, master_nodes]),
    ejabberdctl("set_master", ["self"], ConfigIn),
    [MasterNode] = rpc_call(mnesia, table_info, [TableName, master_nodes]).


%%--------------------------------------------------------------------
%% Manage cluster commands tests
%%--------------------------------------------------------------------


join_successful_prompt(Config) ->
    %% given
    Node2 = mim2(),
    %% when
    {_, OpCode} = ejabberdctl_interactive("join_cluster", [atom_to_list(Node2)], "yes\n", Config),
    %% then
    distributed_helper:verify_result(Node2, add),
    ?eq(0, OpCode).

join_successful_force(Config) ->
    %% given
    Node2 = mim2(),
    %% when
    {_, OpCode} = ejabberdctl_force("join_cluster", [atom_to_list(Node2)], "--force", Config),
    %% then
    distributed_helper:verify_result(Node2, add),
    ?eq(0, OpCode).

leave_successful_prompt(Config) ->
    %% given
    Node2 = mim2(),
    add_node_to_cluster(Node2, Config),
    %% when
    {_, OpCode} = ejabberdctl_interactive("leave_cluster", [], "yes\n", Config),
    %% then
    distributed_helper:verify_result(Node2, remove),
    ?eq(0, OpCode).

leave_successful_force(Config) ->
    %% given
    Node2 = mim2(),
    add_node_to_cluster(Node2, Config),
    %% when
    {_, OpCode} = ejabberdctl_force("leave_cluster", [], "-f", Config),
    %% then
    distributed_helper:verify_result(Node2, remove),
    ?eq(0, OpCode).

join_unsuccessful(Config) ->
    %% given
    Node2 = mim2(),
    %% when
    {_, OpCode} = ejabberdctl_interactive("join_cluster", [], "no\n", Config),
    %% then
    distributed_helper:verify_result(Node2, remove),
    ?ne(0, OpCode).

leave_unsuccessful(Config) ->
    %% given
    Node2 = mim(),
    add_node_to_cluster(Node2, Config),
    %% when
    {_, OpCode} = ejabberdctl_interactive("leave_cluster", [], "no\n", Config),
    %% then
    distributed_helper:verify_result(Node2, add),
    ?ne(0, OpCode).

leave_but_no_cluster(Config) ->
    %% given
    Node2 = mim2(),
    %% when
    {_, OpCode} = ejabberdctl_interactive("leave_cluster", [], "yes\n", Config),
    %% then
    distributed_helper:verify_result(Node2, remove),
    ?ne(0, OpCode).

join_twice(Config) ->
    %% given
    Node2 = mim2(),
    %% when
    {_, OpCode1} = ejabberdctl_interactive("join_cluster", [atom_to_list(Node2)], "yes\n", Config),
    {_, OpCode2} = ejabberdctl_interactive("join_cluster", [atom_to_list(Node2)], "yes\n", Config),
    %% then
    distributed_helper:verify_result(Node2, add),
    ?eq(0, OpCode1),
    ?ne(0, OpCode2).

leave_twice(Config) ->
    %% given
    Node2 = mim2(),
    add_node_to_cluster(Node2, Config),
    %% when
    {_, OpCode1} = ejabberdctl_force("leave_cluster", [], "--force", Config),
    {_, OpCode2} = ejabberdctl_force("leave_cluster", [], "-f", Config),
    %% then
    distributed_helper:verify_result(Node2, remove),
    ?eq(0, OpCode1),
    ?ne(0, OpCode2).

cluster_of_three(Config) ->
    %% given
    ClusterMember = mim(),
    Node2 = mim2(),
    Node3 = mim3(),
    %% when
    {_, OpCode1} = ejabberdctl_force(Node2, "join_cluster", [atom_to_list(ClusterMember)], "-f", Config),
    {_, OpCode2} = ejabberdctl_force(Node3, "join_cluster", [atom_to_list(ClusterMember)], "-f", Config),
    %% then
    ?eq(0, OpCode1),
    ?eq(0, OpCode2),
    nodes_clustered(Node2, ClusterMember, true),
    nodes_clustered(Node3, ClusterMember, true),
    nodes_clustered(Node2, Node3, true).

leave_the_three(Config) ->
    %% given
    Timeout = timer:seconds(60),
    ClusterMember = mim(),
    Node2 = mim2(),
    Node3 = mim3(),
    ok = rpc(Node2, mongoose_cluster, join, [ClusterMember], Timeout),
    ok = rpc(Node3, mongoose_cluster, join, [ClusterMember], Timeout),
    %% when
    {_, OpCode1} = ejabberdctl_interactive(Node2, "leave_cluster", [], "yes\n", Config),
    nodes_clustered(Node2, ClusterMember, false),
    nodes_clustered(Node3, ClusterMember, true),
    {_, OpCode2} = ejabberdctl_interactive(Node3, "leave_cluster", [], "yes\n", Config),
    %% then
    nodes_clustered(Node3, ClusterMember, false),
    nodes_clustered(Node2, Node3, false),
    ?eq(0, OpCode1),
    ?eq(0, OpCode2).

remove_dead_from_cluster(Config) ->
    % given
    Timeout = timer:seconds(60),
    Node1 = mim(),
    Node2 = mim2(),
    Node3 = mim3(),
    ok = rpc(Node2, mongoose_cluster, join, [Node1], Timeout),
    ok = rpc(Node3, mongoose_cluster, join, [Node1], Timeout),
    %% when
    stop_node(Node3, Config),
    {_, OpCode1} = ejabberdctl_interactive(Node1, "remove_from_cluster", [atom_to_list(Node3)], "yes\n", Config),
    %% then
    ?eq(0, OpCode1),
    % node is down hence its not in mnesia cluster
    have_node_in_mnesia(Node1, Node2, true),
    have_node_in_mnesia(Node1, Node3, false),
    have_node_in_mnesia(Node2, Node3, false),
    % after node awakening nodes are clustered again
    start_node(Node3, Config),
    have_node_in_mnesia(Node1, Node3, true),
    have_node_in_mnesia(Node2, Node3, true).

remove_alive_from_cluster(Config) ->
    % given
    Timeout = timer:seconds(60),
    Node1 = mim(),
    Node2 = mim2(),
    Node3 = mim3(),
    ok = rpc(Node2, mongoose_cluster, join, [Node1], Timeout),
    ok = rpc(Node3, mongoose_cluster, join, [Node1], Timeout),
    %% when
    %% Node2 is still running
    {_, OpCode1} = ejabberdctl_force(Node1, "remove_from_cluster", [atom_to_list(Node2)], "-f", Config),
    %% then
    ?eq(0, OpCode1),
    % node is down hence its not in mnesia cluster
    have_node_in_mnesia(Node1, Node3, true),
    have_node_in_mnesia(Node1, Node2, false),
    have_node_in_mnesia(Node3, Node2, false).



%% Helpers
ejabberdctl_interactive(C, A, R, Config) ->
    DefaultNode = mim(),
    ejabberdctl_interactive(DefaultNode, C, A, R, Config).
ejabberdctl_interactive(Node, Cmd, Args, Response, Config) ->
    CtlCmd = escalus_config:get_config(ctl_path_atom(Node), Config),
    run_interactive(string:join([CtlCmd, Cmd | ejabberdctl_helper:normalize_args(Args)], " "), Response).

ejabberdctl_force(Command, Args, ForceFlag, Config) ->
    DefaultNode = mim(),
    ejabberdctl_force(DefaultNode, Command, Args, ForceFlag, Config).
ejabberdctl_force(Node, Cmd, Args, ForceFlag, Config) ->
    ejabberdctl_helper:ejabberdctl(Node, Cmd, [ForceFlag | Args], Config).

mongooseim_script_path(Node, Config) ->
    distributed_helper:script_path(Node, Config, "mongooseim").

mongooseim_script(Node, Cmd, Args, Config) ->
    CtlCmd = mongooseim_script_path(Node, Config),
    ejabberdctl_helper:run(string:join([CtlCmd, Cmd | ejabberdctl_helper:normalize_args(Args)], " ")).

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
    ejabberdctl_helper:loop(Port, [], Timeout).

nodes_clustered(Node1, Node2, ShouldBe) ->
    DbNodes1 = distributed_helper:rpc(Node1, mnesia, system_info, [db_nodes]),
    DbNodes2 = distributed_helper:rpc(Node2, mnesia, system_info, [db_nodes]),
    Pairs = [{Node1, DbNodes2, ShouldBe},
        {Node2, DbNodes1, ShouldBe},
        {Node1, DbNodes1, true},
        {Node2, DbNodes2, true}],
    [?assertEqual(ShouldBelong, lists:member(Element, List))
        || {Element, List, ShouldBelong} <- Pairs].

have_node_in_mnesia(Node1, Node2, ShouldBe) ->
    DbNodes1 = distributed_helper:rpc(Node1, mnesia, system_info, [db_nodes]),
    ?assertEqual(ShouldBe, lists:member(Node2, DbNodes1)).

start_node(Node, Config) ->
    {_, 0} = ejabberdctl_helper:ejabberdctl(Node, "start", [], Config),
    {_, 0} = ejabberdctl_helper:ejabberdctl(Node, "started", [], Config),
    %% TODO Looks like "started" run by ejabberdctl fun is not really synchronous
    timer:sleep(3000).

stop_node(Node, Config) ->
    {_, 0} = mongooseim_script(Node, "stop", [], Config).
