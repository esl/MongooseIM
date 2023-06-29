-module(cets_disco_SUITE).
-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, mim2/0, rpc/4]).
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, file}, {group, rdbms}].

groups() ->
    [{file, [], file_cases()},
     {rdbms, [], rdbms_cases()}].

file_cases() ->
    [file_backend].

rdbms_cases() ->
    [rdbms_backend,
     rdbms_backend_supports_auto_cleaning].

suite() ->
    distributed_helper:require_rpc_nodes([mim, mim2]) ++ escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(rdbms, Config) ->
    case not ct_helper:is_ct_running()
         orelse mongoose_helper:is_rdbms_enabled(domain_helper:host_type()) of
        false -> {skip, rdbms_or_ct_not_running};
        true -> Config
    end;
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

file_backend(Config) ->
    Path = filename:join(?config(mim_data_dir, Config), "nodes.txt"),
    Opts = #{disco_file => Path},
    State = rpc(mim(), cets_discovery_file, init, [Opts]),
    {{ok, Nodes}, _} = rpc(mim(), cets_discovery_file, get_nodes, [State]),
    ['node1@localhost', 'node2@otherhost'] = lists:sort(Nodes).

rdbms_backend(_Config) ->
    CN = <<"big_test">>,
    Opts1 = #{cluster_name => CN, node_name_to_insert => <<"test1">>},
    Opts2 = #{cluster_name => CN, node_name_to_insert => <<"test2">>},
    State1 = disco_init(Opts1),
    disco_get_nodes(State1),
    State2 = disco_init(Opts2),
    {{ok, Nodes}, State2_2} = disco_get_nodes(State2),
    %% "test2" node can see "test1"
    true = lists:member(test1, Nodes),
    {{ok, _}, State2_3} = disco_get_nodes(State2_2),
    %% Check that we follow the right code branch
    #{last_query_info := #{already_registered := true}} = State2_3.

rdbms_backend_supports_auto_cleaning(_Config) ->
    CN = <<"big_test2">>,
    Opts1 = #{cluster_name => CN, node_name_to_insert => <<"test1">>, override_timestamp => month_ago()},
    Opts2 = #{cluster_name => CN, node_name_to_insert => <<"test2">>, min_node_count_to_expire => 1},
    %% test1 row is written
    State1 = disco_init(Opts1),
    {_, State1_2} = disco_get_nodes(State1),
    {{ok, Nodes1}, _} = disco_get_nodes(State1_2),
    %% It is in DB
    true = lists:member(test1, Nodes1),
    %% test2 would clean test1 registration
    State2 = disco_init(Opts2),
    {{ok, Nodes2}, State2_2} = disco_get_nodes(State2),
    false = lists:member(test1, Nodes2),
    #{last_query_info := #{run_cleaning_result := {removed, [test1]}}} = State2_2.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

disco_init(Opts) ->
    rpc(mim(), mongoose_cets_discovery_rdbms, init, [Opts]).

disco_get_nodes(State) ->
    rpc(mim(), mongoose_cets_discovery_rdbms, get_nodes, [State]).

timestamp() ->
    os:system_time(second).

month_ago() ->
    timestamp() - timer:hours(24 * 30) div 1000.
