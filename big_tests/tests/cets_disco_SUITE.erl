-module(cets_disco_SUITE).
-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, mim2/0, rpc/4]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


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
     rdbms_backend_supports_auto_cleaning,
     rdbms_backend_node_doesnt_remove_itself,
     rdbms_backend_db_queries].

suite() ->
    distributed_helper:require_rpc_nodes([mim, mim2]).

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

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

init_per_testcase(_CaseName, Config) -> Config.

end_per_testcase(_CaseName, Config) ->
    unmock(mim()),
    unmock(mim2()).

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

file_backend(Config) ->
    Path = filename:join(?config(mim_data_dir, Config), "nodes.txt"),
    Opts = #{disco_file => Path},
    State = rpc(mim(), cets_discovery_file, init, [Opts]),
    {{ok, Nodes}, _} = rpc(mim(), cets_discovery_file, get_nodes, [State]),
    ?assertEqual(lists:sort(['node1@localhost', 'node2@otherhost']), lists:sort(Nodes)).

rdbms_backend(_Config) ->
    CN = random_cluster_name(?FUNCTION_NAME),
    Opts1 = #{cluster_name => CN, node_name_to_insert => <<"test1">>},
    Opts2 = #{cluster_name => CN, node_name_to_insert => <<"test2">>},

    State1 = disco_init(mim(), Opts1),
    {{ok, Nodes1_2}, State1_2} = disco_get_nodes(mim(), State1),
    ?assertMatch(#{last_query_info := #{already_registered := false}}, State1_2),
    ?assertEqual([], Nodes1_2),

    %% "test2" node can see "test1" on initial registration
    State2 = disco_init(mim2(), Opts2),
    {{ok, Nodes2_2}, State2_2} = disco_get_nodes(mim2(), State2),
    ?assertMatch(#{last_query_info := #{already_registered := false}}, State2_2),
    ?assertEqual([test1], Nodes2_2),

    %% "test2" node can see "test1" on update
    {{ok, Nodes2_3}, State2_3} = disco_get_nodes(mim2(), State2_2),
    ?assertEqual(lists:sort([test1, test2]), lists:sort(Nodes2_3)),
    ?assertMatch(#{last_query_info := #{already_registered := true}}, State2_3).

rdbms_backend_supports_auto_cleaning(_Config) ->
    Timestamp = month_ago(),
    mock_timestamp(mim(), Timestamp),
    CN = random_cluster_name(?FUNCTION_NAME),
    Opts1 = #{cluster_name => CN, node_name_to_insert => <<"test1">>},
    Opts2 = #{cluster_name => CN, node_name_to_insert => <<"test2">>},

    %% test1 row is written with an old (mocked) timestamp
    State1 = disco_init(mim(), Opts1),
    {{ok, Nodes1_2}, State1_2} = disco_get_nodes(mim(), State1),
    {{ok, Nodes1_3}, State1_3} = disco_get_nodes(mim(), State1_2),
    ?assertEqual([], Nodes1_2),
    ?assertEqual([test1], Nodes1_3),
    ?assertMatch(#{last_query_info := #{timestamp := Timestamp}}, State1_2),
    ?assertMatch(#{last_query_info := #{timestamp := Timestamp}}, State1_3),

    %% test2 would clean test1 registration
    %% We don't mock on mim2 node, so timestamps would differ
    State2 = disco_init(mim2(), Opts2),
    {{ok, Nodes2_2}, State2_2} = disco_get_nodes(mim2(), State2),
    ?assertEqual([], Nodes2_2),
    ?assertMatch(#{last_query_info := #{run_cleaning_result := {removed, [<<"test1">>]}}},
                 State2_2),
    {{ok, Nodes2_3}, State2_3} = disco_get_nodes(mim2(), State2),
    ?assertEqual([test2], Nodes2_3),
    #{last_query_info := #{last_rows := SelectedRows}} = State2_3,
    ?assertMatch(1, length(SelectedRows)).

rdbms_backend_node_doesnt_remove_itself(_Config) ->
    Timestamp = month_ago(),
    mock_timestamp(mim(), Timestamp),
    CN = random_cluster_name(?FUNCTION_NAME),
    Opts1 = #{cluster_name => CN, node_name_to_insert => <<"test1">>},
    Opts2 = #{cluster_name => CN, node_name_to_insert => <<"test2">>},

    %% test1 row is written with an old (mocked) timestamp
    State1 = disco_init(mim(), Opts1),
    {{ok, Nodes1_2}, State1_2} = disco_get_nodes(mim(), State1),
    ?assertEqual([], Nodes1_2),
    ?assertMatch(#{last_query_info := #{timestamp := Timestamp}}, State1_2),

    unmock_timestamp(mim()),
    %% test1 row is not removed and timestamp is updated
    {{ok, Nodes1_3}, State1_3} = disco_get_nodes(mim(), State1_2),
    ?assertNotMatch(#{last_query_info := #{timestamp := Timestamp}}, State1_3),
    ?assertMatch(#{last_query_info := #{run_cleaning_result := {removed, []}}},
                 State1_3),
    ?assertEqual([test1], Nodes1_3),

    State2 = disco_init(mim2(), Opts2),
    {{ok, Nodes2_2}, State2_2} = disco_get_nodes(mim2(), State2),
    ?assertEqual([test1], Nodes2_2),
    ?assertMatch(#{last_query_info := #{run_cleaning_result := {removed, []}}},
                 State2_2).

rdbms_backend_db_queries(_Config) ->
    CN = random_cluster_name(?FUNCTION_NAME),
    TS = rpc(mim(), mongoose_rdbms_timestamp, select, []),
    TS2 = TS + 100,

    %% insertion fails if node name or node num is already added for the cluster
    ?assertEqual({updated, 1}, insert_new(CN, <<"test1">>, TS, 1)),
    ?assertMatch({error, _}, insert_new(CN, <<"test1">>, TS, 1)),
    ?assertMatch({error, _}, insert_new(CN, <<"test1">>, TS, 2)),
    ?assertMatch({error, _}, insert_new(CN, <<"test2">>, TS, 1)),
    ?assertEqual({updated, 1}, insert_new(CN, <<"test2">>, TS, 2)),

    %% update of the timestamp works correctly
    {selected, SelectedNodes1} = select(CN),
    ?assertEqual(lists:sort([{<<"test1">>, 1, TS}, {<<"test2">>, 2, TS}]),
                 lists:sort(SelectedNodes1)),
    ?assertEqual({updated, 1}, update_existing(CN, <<"test1">>, TS2)),
    {selected, SelectedNodes2} = select(CN),
    ?assertEqual(lists:sort([{<<"test1">>, 1, TS2}, {<<"test2">>, 2, TS}]),
                 lists:sort(SelectedNodes2)),

    %% node removal work correctly
    ?assertEqual({updated, 1}, delete_node_from_db(CN, <<"test1">>)),
    ?assertEqual({selected, [{<<"test2">>, 2, TS}]}, select(CN)).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

disco_init(Node, Opts) ->
    State = rpc(Node, mongoose_cets_discovery_rdbms, init, [Opts]),
    log_disco_request(?FUNCTION_NAME, Node, Opts, State),
    State.

disco_get_nodes(Node, State) ->
    NewState = rpc(Node, mongoose_cets_discovery_rdbms, get_nodes, [State]),
    log_disco_request(?FUNCTION_NAME, Node, State, NewState),
    NewState.

log_disco_request(disco_init, Node, #{cluster_name := CN} = Opts, State) ->
    ct:pal("[0] disco_init(~p,~n" ++
           "               ~p) =~n" ++
           "        ~p",
           [Node, Opts, State]),
    erlang:put({disco, Node, CN}, 1);
log_disco_request(disco_get_nodes, Node, #{cluster_name := CN} = OldState, NewState) ->
    N = case erlang:get({disco, Node, CN}) of
            undefined -> 1;
            Int when is_integer(Int) -> Int
        end,
    ct:pal("[~p] disco_get_nodes(~p,~n" ++
           "                    ~p) =~n" ++
           "        ~p",
           [N, Node, OldState, NewState]),
    erlang:put({disco, Node, CN}, N+1).

timestamp() ->
    os:system_time(second).

month_ago() ->
    timestamp() - timer:hours(24 * 30) div 1000.

mock_timestamp(Node, Timestamp) ->
    ok = rpc(Node, meck, new, [mongoose_rdbms_timestamp, [passthrough, no_link]]),
    ok = rpc(Node, meck, expect, [mongoose_rdbms_timestamp, select, 0, Timestamp]),
    %% Ensure that we mock
    Timestamp = rpc(Node, mongoose_rdbms_timestamp, select, []).

unmock_timestamp(Node) ->
    ok = rpc(Node, meck, unload, [mongoose_rdbms_timestamp]).

unmock(Node) ->
    rpc(Node, meck, unload, []).

random_cluster_name(CaseName) ->
    Rand = rpc(mim(), mongoose_bin, gen_from_crypto, []),
    <<"big_test_", (atom_to_binary(CaseName))/binary, "_", Rand/binary>>.

insert_new(CN, BinNode, TS, NodeNum) ->
    Ret = rpc(mim(), mongoose_cets_discovery_rdbms, insert_new, [CN, BinNode, TS, NodeNum]),
    ct:pal("insert_new(~p, ~p, ~p, ~p) = ~p", [CN, BinNode, TS, NodeNum, Ret]),
    Ret.

select(CN) ->
    Ret = rpc(mim(), mongoose_cets_discovery_rdbms, select, [CN]),
    ct:pal("select(~p) = ~p", [CN, Ret]),
    Ret.

update_existing(CN, BinNode, TS) ->
    Ret = rpc(mim(), mongoose_cets_discovery_rdbms, update_existing, [CN, BinNode, TS]),
    ct:pal("select(~p, ~p, ~p) = ~p", [CN, BinNode, TS, Ret]),
    Ret.

delete_node_from_db(CN, BinNode) ->
    Ret = rpc(mim(), mongoose_cets_discovery_rdbms, delete_node_from_db, [CN, BinNode]),
    ct:pal("delete_node_from_db(~p, ~p) = ~p", [CN, BinNode, Ret]),
    Ret.
