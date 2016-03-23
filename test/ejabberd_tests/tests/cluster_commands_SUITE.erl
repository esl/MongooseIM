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

-import(distributed_helper, [add_node_to_cluster/1,
                             remove_node_from_cluster/1,
                             is_sm_distributed/0]).
-import(ejabberdctl_helper, [ejabberdctl/3, rpc_call/3]).


-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, clustered},
     {group, ejabberdctl},
      {group, clustering}].

groups() ->
    [{clustered, [], [one_to_one_message]},
      {clustering, [], [join_successful, leave_successful]},
     {ejabberdctl, [], [set_master_test]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config1 = escalus:init_per_suite(Config),
    Node = ct:get_config(ejabberd2_node),
    Config2 = ejabberd_node_utils:init(Node, Config1),
    ejabberd_node_utils:backup_config_file(Node, Config2),
    MainDomain = ct:get_config(ejabberd_domain),
    Ch = [{hosts, "[\"" ++ binary_to_list(MainDomain) ++ "\"]"}],
    ejabberd_node_utils:modify_config_file(Node, "reltool_vars/node2_vars.config", Ch, Config2),
    ejabberd_node_utils:call_ctl(Node, reload_local, Config2),
    {ok, EjdWD} = escalus_ejabberd:rpc(file, get_cwd, []),
    CtlPath = case filelib:is_file(EjdWD ++ "/bin/ejabberdctl") of
                  true -> EjdWD ++ "/bin/ejabberdctl";
                  false -> EjdWD ++ "/bin/mongooseimctl"
              end,
    escalus:init_per_suite([{ctl_path, CtlPath} | Config2]).

end_per_suite(Config) ->
    Node = ct:get_config(ejabberd2_node),
    ejabberd_node_utils:restore_config_file(Node, Config),
    ejabberd_node_utils:restart_application(Node, ejabberd),
    escalus:end_per_suite(Config).

init_per_group(Group, Config) when Group == clustered orelse Group == ejabberdctl ->
  Config1 = add_node_to_cluster(Config),
  case is_sm_distributed() of
        true ->
            escalus:create_users(Config1, escalus:get_users([alice, clusterguy]));
        {false, Backend} ->
            ct:pal("Backend ~p doesn't support distributed tests", [Backend]),
            remove_node_from_cluster(Config1),
            {skip, nondistributed_sm}
    end;
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(Group, Config) when Group == clustered orelse Group == ejabberdctl ->
    escalus:delete_users(Config, escalus:get_users([alice, clusterguy])),
    remove_node_from_cluster(Config);
end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------

one_to_one_message(ConfigIn) ->
    %% Given Alice connected to node one and ClusterGuy connected to node two
    Metrics = [{[data, dist], [{recv_oct, '>'}, {send_oct, '>'}]}],
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
%% Ejabberdctl tests
%%--------------------------------------------------------------------

set_master_test(ConfigIn) ->
    TableName = passwd,
    NodeList = nodes(),
    ejabberdctl("set_master", ["self"], ConfigIn),
    [MasterNode] = rpc_call(mnesia, table_info, [TableName, master_nodes]),
    true = lists:member(MasterNode, NodeList),
    RestNodesList = lists:delete(MasterNode, NodeList),
    OtherNode = hd(RestNodesList),
    ejabberdctl("set_master", [atom_to_list(OtherNode)], ConfigIn),
    [OtherNode] = rpc_call(mnesia, table_info, [TableName, master_nodes]),
    ejabberdctl("set_master", ["self"], ConfigIn),
    [MasterNode] = rpc_call(mnesia, table_info, [TableName, master_nodes]).

join_successful(Config) ->
  Node2 = ct:get_config(ejabberd2_node),
  ejabberdctl("join_cluster", [atom_to_list(Node2)], Config),
  verify_result(add).


leave_successful(Config) ->
  ejabberdctl("leave_cluster", [], Config),
  verify_result(remove).


