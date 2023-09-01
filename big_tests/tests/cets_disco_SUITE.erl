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

init_per_testcase(rdbms_backend_supports_auto_cleaning = CaseName, Config) ->
    mock_timestamp(mim(), month_ago()) ++
        escalus:init_per_testcase(CaseName, Config);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(rdbms_backend_supports_auto_cleaning = CaseName, Config) ->
    unmock_timestamp(mim()),
    escalus:end_per_testcase(CaseName, Config);
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
    State1 = disco_init(mim(), Opts1),
    disco_get_nodes(mim(), State1),
    State2 = disco_init(mim2(), Opts2),
    {{ok, Nodes}, State2_2} = disco_get_nodes(mim2(), State2),
    %% "test2" node can see "test1"
    true = lists:member(test1, Nodes),
    {{ok, _}, State2_3} = disco_get_nodes(mim2(), State2_2),
    %% Check that we follow the right code branch
    #{last_query_info := #{already_registered := true}} = State2_3.

rdbms_backend_supports_auto_cleaning(Config) ->
    ensure_mocked(Config),
    CN = <<"big_test2">>,
    Opts1 = #{cluster_name => CN, node_name_to_insert => <<"test1">>},
    Opts2 = #{cluster_name => CN, node_name_to_insert => <<"test2">>},
    %% test1 row is written with an old (mocked) timestamp
    State1 = disco_init(mim(), Opts1),
    {_, State1_2} = disco_get_nodes(mim(), State1),
    {{ok, Nodes1}, State1_3} = disco_get_nodes(mim(), State1_2),
    Timestamp = proplists:get_value(mocked_timestamp, Config),
    #{last_query_info := #{timestamp := Timestamp}} = State1_3,
    %% It is in DB
    true = lists:member(test1, Nodes1),
    %% test2 would clean test1 registration
    %% We don't mock on mim2 node, so timestamps would differ
    State2 = disco_init(mim2(), Opts2),
    {{ok, Nodes2}, State2_2} = disco_get_nodes(mim2(), State2),
    false = lists:member(test1, Nodes2),
    #{last_query_info := #{run_cleaning_result := {removed, [test1]}}} = State2_2.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

disco_init(Node, Opts) ->
    rpc(Node, mongoose_cets_discovery_rdbms, init, [Opts]).

disco_get_nodes(Node, State) ->
    rpc(Node, mongoose_cets_discovery_rdbms, get_nodes, [State]).

timestamp() ->
    os:system_time(second).

month_ago() ->
    timestamp() - timer:hours(24 * 30) div 1000.

mock_timestamp(Node, Timestamp) ->
    ok = rpc(Node, meck, new, [mongoose_rdbms_timestamp, [passthrough, no_link]]),
    ok = rpc(Node, meck, expect, [mongoose_rdbms_timestamp, select, 0, Timestamp]),
    %% Ensure that we mock
    EnsureMocked = fun() ->
        Timestamp = rpc(Node, mongoose_rdbms_timestamp, select, [])
        end,
    EnsureMocked(),
    [{ensure_mocked, EnsureMocked}, {mocked_timestamp, Timestamp}].

ensure_mocked(Config) ->
    EnsureMocked = proplists:get_value(ensure_mocked, Config),
    EnsureMocked().

unmock_timestamp(Node) ->
    ok = rpc(Node, meck, unload, [mongoose_rdbms_timestamp]).
