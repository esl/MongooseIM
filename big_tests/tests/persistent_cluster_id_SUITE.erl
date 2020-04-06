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
         all_nodes_in_the_cluster_have_the_same_cluster_id/1,
         id_persists_after_restart/1,
         same_cluster_id_in_backend_and_mnesia/1,
         backed_up_id_if_rdbms_is_added/1,
         cluster_id_is_restored_to_mnesia_from_rdbms_if_mnesia_lost/1
        ]).

-import(distributed_helper, [mim/0, mim2/0]).

all() ->
    [
     {group, mnesia},
     {group, rdbms}
    ].

tests() ->
    [
     all_nodes_in_the_cluster_have_the_same_cluster_id,
     id_persists_after_restart,
     same_cluster_id_in_backend_and_mnesia,
     backed_up_id_if_rdbms_is_added,
     cluster_id_is_restored_to_mnesia_from_rdbms_if_mnesia_lost
    ].

groups() ->
    [
     {mnesia, [], [all_nodes_in_the_cluster_have_the_same_cluster_id]},
     {rdbms, [], tests()}
    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%%===================================================================
%%% Group specific setup/teardown
%%%===================================================================
group(_Groupname) ->
    [].

init_per_group(mnesia, Config) ->
    case not mongoose_helper:is_rdbms_enabled(domain()) of
        true -> Config;
        false -> {skip, require_no_rdbms}
    end;
init_per_group(_Groupname, Config) ->
    case mongoose_helper:is_rdbms_enabled(domain()) of
        true -> Config;
        false -> {skip, require_rdbms}
    end.

end_per_group(_Groupname, _Config) ->
    ok.

%%%===================================================================
%%% Testcase specific setup/teardown
%%%===================================================================
init_per_testcase(all_nodes_in_the_cluster_have_the_same_cluster_id, Config) ->
    distributed_helper:add_node_to_cluster(mim2(), Config),
    Config;
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(all_nodes_in_the_cluster_have_the_same_cluster_id, Config) ->
    distributed_helper:remove_node_from_cluster(mim2(), Config),
    Config;
end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================
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


domain() ->
    ct:get_config({hosts, mim, domain}).
