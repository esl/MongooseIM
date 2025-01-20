-module(persistent_cluster_id_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test cases
-export([
         can_start_with_cluster_id_in_cets_only/1,
         all_nodes_in_the_cluster_have_the_same_cluster_id/1,
         id_persists_after_restart/1,
         same_cluster_id_in_backend_and_mnesia/1,
         backed_up_id_if_rdbms_is_added/1,
         cluster_id_is_restored_to_mnesia_from_rdbms_if_mnesia_lost/1,
         clean_start_and_two_nodes/1
        ]).

-import(distributed_helper, [mim/0, mim2/0]).

-import(domain_helper, [host_type/0]).

all() ->
    [
     {group, cets},
     {group, mnesia},
     {group, rdbms}
    ].

tests() ->
    [
     all_nodes_in_the_cluster_have_the_same_cluster_id,
     id_persists_after_restart,
     same_cluster_id_in_backend_and_mnesia,
     backed_up_id_if_rdbms_is_added,
     cluster_id_is_restored_to_mnesia_from_rdbms_if_mnesia_lost,
     clean_start_and_two_nodes
    ].

groups() ->
    [
     {cets, [], [can_start_with_cluster_id_in_cets_only]},
     {mnesia, [], [all_nodes_in_the_cluster_have_the_same_cluster_id]},
     {rdbms, [], tests()}
    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    distributed_helper:require_rpc_nodes([mim]) ++ Config.

end_per_suite(_Config) ->
    ok.

%%%===================================================================
%%% Group specific setup/teardown
%%%===================================================================
group(_Groupname) ->
    [].

init_per_group(rdbms, Config) ->
    case mongoose_helper:is_rdbms_enabled(host_type()) of
        true -> Config;
        false -> {skip, require_rdbms}
    end;
init_per_group(_, Config) ->
    case not mongoose_helper:is_rdbms_enabled(host_type()) of
        true -> Config;
        false -> {skip, require_no_rdbms}
    end.

end_per_group(_Groupname, _Config) ->
    ok.

%%%===================================================================
%%% Testcase specific setup/teardown
%%%===================================================================
init_per_testcase(can_start_with_cluster_id_in_cets_only, Config) ->
    Config1 = ejabberd_node_utils:init(Config),
    ejabberd_node_utils:backup_config_file(Config1),
    Config1;
init_per_testcase(all_nodes_in_the_cluster_have_the_same_cluster_id, Config) ->
    distributed_helper:add_node_to_cluster(mim2(), Config),
    Config;
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(can_start_with_cluster_id_in_cets_only, Config) ->
    ejabberd_node_utils:restore_config_file(Config),
    ejabberd_node_utils:ensure_started_application(mongooseim);
end_per_testcase(all_nodes_in_the_cluster_have_the_same_cluster_id, Config) ->
    distributed_helper:remove_node_from_cluster(mim2(), Config),
    Config;
end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================

can_start_with_cluster_id_in_cets_only(_Config) ->
    Toml = "[general]
    hosts = [\"example.com\"]
    default_server_domain = \"example.com\"
    sm_backend = \"cets\"
    s2s_backend = \"cets\"
    component_backend = \"cets\"
    [internal_databases.cets]
    backend = \"file\"
    node_list_file = \"etc/cets_disco.txt\"",
    ejabberd_node_utils:replace_config_file(Toml),
    ejabberd_node_utils:restart_application(mongooseim).

all_nodes_in_the_cluster_have_the_same_cluster_id(_Config) ->
    {ok, ID_mim1} = mongoose_helper:successful_rpc(
               mim(), mongoose_cluster_id, get_cached_cluster_id, []),
    {ok, ID_mim2} = mongoose_helper:successful_rpc(
               mim2(), mongoose_cluster_id, get_cached_cluster_id, []),
    ?assertEqual(ID_mim1, ID_mim2).

id_persists_after_restart(_Config) ->
    {ok, FirstID} = mongoose_helper:successful_rpc(
               mim(), mongoose_cluster_id, get_cached_cluster_id, []),
    ejabberd_node_utils:restart_application(mongooseim),
    {ok, SecondID} = mongoose_helper:successful_rpc(
               mim(), mongoose_cluster_id, get_cached_cluster_id, []),
    ?assertEqual(FirstID, SecondID).

same_cluster_id_in_backend_and_mnesia(_Config) ->
    {ok, MnesiaID} = mongoose_helper:successful_rpc(
               mim(), mongoose_cluster_id, get_cached_cluster_id, []),
    {ok, BackendID} = mongoose_helper:successful_rpc(
               mim(), mongoose_cluster_id, get_backend_cluster_id, []),
    ?assertEqual(MnesiaID, BackendID).

backed_up_id_if_rdbms_is_added(_Config) ->
    ok = mongoose_helper:successful_rpc(
           mim(), mongoose_cluster_id, clean_table, []),
    {ok, MnesiaID} = mongoose_helper:successful_rpc(
           mim(), mongoose_cluster_id, get_cached_cluster_id, []),
    {error, no_value_in_backend} = mongoose_helper:successful_rpc(
           mim(), mongoose_cluster_id, get_backend_cluster_id, []),
    {ok, AfterRestartID} = mongoose_helper:successful_rpc(
           mim(), mongoose_cluster_id, start, []),
    {ok, BackendID} = mongoose_helper:successful_rpc(
           mim(), mongoose_cluster_id, get_backend_cluster_id, []),
    ?assertEqual(AfterRestartID, MnesiaID),
    ?assertEqual(AfterRestartID, BackendID).

cluster_id_is_restored_to_mnesia_from_rdbms_if_mnesia_lost(_Config) ->
    {ok, FirstID} = mongoose_helper:successful_rpc(
               mim(), mongoose_cluster_id, get_cached_cluster_id, []),
    %% mongoose_cluster:leave/0 does everything we need here:
    %%  it stops the node, deletes everything related to mnesia from the system,
    %%  the hardcore way, removing folders from the OS and so on, and restarts the node.
    %%  Assuming this works correctly, then we can be sure that "mnesia files were lost".
    Node = mim(),
    ok = distributed_helper:rpc(
           Node#{timeout => timer:seconds(30)}, mongoose_cluster, leave, []),
    {ok, SecondID} = mongoose_helper:successful_rpc(
               Node, mongoose_cluster_id, get_cached_cluster_id, []),
    ?assertEqual(FirstID, SecondID).

clean_start_and_two_nodes(_Config) ->
    {ok, MnesiaID} = mongoose_helper:successful_rpc(
           mim(), mongoose_cluster_id, get_cached_cluster_id, []),
    {ok, MnesiaID2} = mongoose_helper:successful_rpc(
           mim2(), mongoose_cluster_id, get_cached_cluster_id, []),
    %% Sanity check: IDs are in sync
    ?assertEqual(MnesiaID, MnesiaID2),
    %% Remove an old ID from anywhere
    ok = mongoose_helper:successful_rpc(
           mim(), mongoose_cluster_id, clean_table, []),
    ok = mongoose_helper:successful_rpc(
           mim(), mongoose_cluster_id, clean_cache, []),
    ok = mongoose_helper:successful_rpc(
           mim2(), mongoose_cluster_id, clean_cache, []),
    {ok, AfterRestartID} = mongoose_helper:successful_rpc(
           mim(), mongoose_cluster_id, start, []),
    {ok, AfterRestartID2} = mongoose_helper:successful_rpc(
           mim2(), mongoose_cluster_id, start, []),
    %% We've created a new ID
    ?assertNotEqual(AfterRestartID, MnesiaID),
    %% Both nodes have the same ID
    ?assertEqual(AfterRestartID, AfterRestartID2).
