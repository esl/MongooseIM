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


-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, clustered}].

groups() ->
    [{clustered, [], [one_to_one_message]}].

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

    Config2.

end_per_suite(Config) ->
    Node = ct:get_config(ejabberd2_node),
    ejabberd_node_utils:restore_config_file(Node, Config),
    ejabberd_node_utils:restart_application(Node, ejabberd),
    escalus:end_per_suite(Config).

init_per_group(clustered, Config) ->

    Config1 = add_node_to_cluster(Config),

    case is_sm_distributed() of
        true ->
            escalus:create_users(Config1, {by_name, [alice, clusterguy]});
        {false, Backend} ->
            ct:pal("Backend ~p doesn't support distributed tests", [Backend]),
            remove_node_from_cluster(Config1),
            {skip, nondistributed_sm}
    end;
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(clustered, Config) ->
    escalus:delete_users(Config, {by_name, [alice, clusterguy]}),
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
%% Helpers
%%--------------------------------------------------------------------
