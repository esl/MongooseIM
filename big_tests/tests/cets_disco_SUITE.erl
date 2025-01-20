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
     rdbms_backend_supports_cluster_change,
     rdbms_backend_cluster_name_contains_cets_version,
     rdbms_backend_supports_auto_cleaning,
     rdbms_backend_node_doesnt_remove_itself,
     rdbms_backend_db_queries,
     rdbms_backend_publishes_node_ip,
     no_record_for_node,
     no_ip_in_db,
     epmd_just_returns_ip_from_db,
     address_please,
     address_please_returns_ip,
     address_please_returns_ip_fallbacks_to_resolve_with_file_backend,
     address_please_returns_ip_127_0_0_1_from_db].

suite() ->
    distributed_helper:require_rpc_nodes([mim, mim2]).

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_group(rdbms, Config) ->
    case not ct_helper:is_ct_running()
         orelse mongoose_helper:is_rdbms_enabled(domain_helper:host_type()) of
        false -> {skip, rdbms_or_ct_not_running};
        true ->
            stop_and_delete_cets_discovery_if_running(),
            Config
    end;
init_per_group(_, Config) ->
    Config.

end_per_group(rdbms, Config) ->
    restore_default_cets_discovery(),
    Config;
end_per_group(_, Config) ->
    Config.

init_per_testcase(address_please_returns_ip, Config) ->
    start_cets_discovery(Config);
init_per_testcase(address_please_returns_ip_fallbacks_to_resolve_with_file_backend, Config) ->
    start_cets_discovery_with_file_backnend(Config);
init_per_testcase(address_please_returns_ip_127_0_0_1_from_db, Config) ->
    start_cets_discovery_with_real_ips(Config);
init_per_testcase(_CaseName, Config) -> Config.

end_per_testcase(Name, Config) when Name == address_please_returns_ip;
                                    Name == address_please_returns_ip_fallbacks_to_resolve_with_file_backend;
                                    Name == address_please_returns_ip_127_0_0_1_from_db ->
    stop_cets_discovery(),
    Config;
end_per_testcase(_CaseName, _Config) ->
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

    init_and_get_nodes(mim(), Opts1, []),

    %% "test2" node can see "test1" on initial registration
    State2 = init_and_get_nodes(mim2(), Opts2, [test1]),

    %% "test2" node can see "test1" on update
    get_nodes(mim2(), State2, [test1, test2]).

rdbms_backend_supports_cluster_change(_Config) ->
    CN1 = random_cluster_name(?FUNCTION_NAME),
    CN2 = <<CN1/binary, "_new">>,
    Opts1 = #{cluster_name => CN1, node_name_to_insert => <<"test1">>},
    Opts2 = #{cluster_name => CN1, node_name_to_insert => <<"test2">>},

    %% Nodes test1 and test2 are in CN1, and they become connected
    State1 = init_and_get_nodes(mim(), Opts1, []),
    State2 = init_and_get_nodes(mim2(), Opts2, [test1]),
    get_nodes(mim(), State1, [test1, test2]),

    %% Node test1 moves to CN2, and the nodes are disconnected
    NewState1 = init_and_get_nodes(mim(), Opts1#{cluster_name := CN2}, []),
    get_nodes(mim2(), State2, [test2]),
    NewState1A = get_nodes(mim(), NewState1, [test1]),

    %% Node test2 moves to CN2, and the nodes are connected again
    init_and_get_nodes(mim2(), Opts2#{cluster_name := CN2}, [test1]),
    get_nodes(mim(), NewState1A, [test1, test2]).

rdbms_backend_cluster_name_contains_cets_version(_Config) ->
    CN = random_cluster_name(?FUNCTION_NAME),
    Opts = #{cluster_name => CN, node_name_to_insert => <<"test1">>},
    #{cluster_name := CNWithVsn} = init_and_get_nodes(mim(), Opts, []),
    [<<>>, Vsn] = binary:split(CNWithVsn, CN),
    ?assertMatch({match, _}, re:run(Vsn, "-[0-9]+\\.[0-9]+")).

rdbms_backend_supports_auto_cleaning(_Config) ->
    Timestamp = month_ago(),
    mock_timestamp(mim(), Timestamp),
    CN = random_cluster_name(?FUNCTION_NAME),
    Opts1 = #{cluster_name => CN, node_name_to_insert => <<"test1">>},
    Opts2 = #{cluster_name => CN, node_name_to_insert => <<"test2">>},

    %% test1 row is written with an old (mocked) timestamp
    State1 = init_and_get_nodes(mim(), Opts1, []),
    ?assertMatch(#{last_query_info := #{timestamp := Timestamp}}, State1),
    State1A = get_nodes(mim(), State1, [test1]),
    ?assertMatch(#{last_query_info := #{timestamp := Timestamp}}, State1A),

    %% test2 would clean test1 registration
    %% We don't mock on mim2 node, so timestamps would differ
    State2 = init_and_get_nodes(mim2(), Opts2, []),
    ?assertMatch(#{last_query_info := #{run_cleaning_result := {removed, [<<"test1">>]}}}, State2),
    State2A = get_nodes(mim2(), State2, [test2]),
    #{last_query_info := #{last_rows := SelectedRows}} = State2A,
    ?assertMatch(1, length(SelectedRows)).

rdbms_backend_node_doesnt_remove_itself(_Config) ->
    Timestamp = month_ago(),
    mock_timestamp(mim(), Timestamp),
    CN = random_cluster_name(?FUNCTION_NAME),
    Opts1 = #{cluster_name => CN, node_name_to_insert => <<"test1">>},
    Opts2 = #{cluster_name => CN, node_name_to_insert => <<"test2">>},

    %% test1 row is written with an old (mocked) timestamp
    State1 = init_and_get_nodes(mim(), Opts1, []),
    ?assertMatch(#{last_query_info := #{timestamp := Timestamp}}, State1),

    unmock_timestamp(mim()),
    %% test1 row is not removed and timestamp is updated
    State1A = get_nodes(mim(), State1, [test1]),
    ?assertNotMatch(#{last_query_info := #{timestamp := Timestamp}}, State1A),
    ?assertMatch(#{last_query_info := #{run_cleaning_result := {removed, []}}}, State1A),

    State2 = init_and_get_nodes(mim2(), Opts2, [test1]),
    ?assertMatch(#{last_query_info := #{run_cleaning_result := {removed, []}}}, State2).

rdbms_backend_db_queries(_Config) ->
    CN = random_cluster_name(?FUNCTION_NAME),
    TS = rpc(mim(), mongoose_rdbms_timestamp, select, []),
    TS2 = TS + 100,

    %% insertion fails if node name or node num is already added for the cluster
    ?assertEqual({updated, 1}, insert_new(CN, <<"testA">>, 1, <<>>, TS)),
    ?assertMatch({error, _}, insert_new(CN, <<"testA">>, 1, <<>>, TS)),
    ?assertMatch({error, _}, insert_new(CN, <<"testA">>, 2, <<>>, TS)),
    ?assertMatch({error, _}, insert_new(CN, <<"testB">>, 1, <<>>, TS)),
    ?assertEqual({updated, 1}, insert_new(CN, <<"testB">>, 2, <<>>, TS)),

    %% insertion fails if node is a member of another cluster
    ?assertMatch({error, _}, insert_new(<<"my-cluster">>, <<"testA">>, 1, <<>>, TS)),

    %% update of the timestamp works correctly
    {selected, SelectedNodes1} = select(CN),
    ?assertEqual(lists:sort([{<<"testA">>, 1, <<>>, TS}, {<<"testB">>, 2, <<>>, TS}]),
                 lists:sort(SelectedNodes1)),
    ?assertEqual({updated, 1}, update_existing(<<"testA">>, <<>>, TS2)),
    {selected, SelectedNodes2} = select(CN),
    ?assertEqual(lists:sort([{<<"testA">>, 1, <<>>, TS2}, {<<"testB">>, 2, <<>>, TS}]),
                 lists:sort(SelectedNodes2)),

    %% node removal works correctly
    ?assertEqual({updated, 1}, delete_node_from_db(<<"testA">>)),
    ?assertEqual({selected, [{<<"testB">>, 2, <<>>, TS}]}, select(CN)).

rdbms_backend_publishes_node_ip(_Config) ->
    %% get_pairs would return only real available nodes, so use the real node names
    Node1b = atom_to_binary(maps:get(node, mim())),
    Node2b = atom_to_binary(maps:get(node, mim2())),
    CN = random_cluster_name(?FUNCTION_NAME),
    Opts1 = #{cluster_name => CN, node_name_to_insert => Node1b,
              node_ip_binary => <<"127.0.0.1">>},
    Opts2 = #{cluster_name => CN, node_name_to_insert => Node2b,
              node_ip_binary => <<"127.0.0.1">>},
    State1 = disco_init(mim(), Opts1),
    State2 = disco_init(mim2(), Opts2),
    {{ok, _Nodes1_2}, State1_2} = disco_get_nodes(mim(), State1),
    {{ok, _Nodes2_2}, State2_2} = disco_get_nodes(mim2(), State2),
    {{ok, _Nodes1_3}, State1_3} = disco_get_nodes(mim(), State1_2),
    {{ok, _Nodes2_3}, State2_3} = disco_get_nodes(mim2(), State2_2),
    {ok, {127, 0, 0, 1}} = match_node_name(mim2(), State2_3, Node1b),
    {ok, {127, 0, 0, 1}} = match_node_name(mim(), State1_3, Node2b).

no_record_for_node(_Config) ->
    Node = <<"mongoose@badhost">>,
    BackState = #{address_pairs => #{}},
    {error, {no_record_for_node, Node}} = match_node_name(mim(), BackState, Node),
    ok.

no_ip_in_db(_Config) ->
    Node = <<"mongoose@noiphost">>,
    BackState = #{address_pairs => #{Node => <<>>}},
    {error, {no_ip_in_db, Node}} = match_node_name(mim(), BackState, Node),
    ok.

epmd_just_returns_ip_from_db(_Config) ->
    Node = <<"mongoose@noepmdhost">>,
    %% IP from a test range
    BackState = #{address_pairs => #{Node => <<"192.0.2.1">>}},
    {ok, {192, 0, 2, 1}} = match_node_name(mim(), BackState, Node).

address_please(_Config) ->
    {error, nxdomain} =
        rpc(mim(), mongoose_epmd, address_please, ["mongooseim", "badbadhost", inet]).

address_please_returns_ip(_Config) ->
    Res = rpc(mim(), mongoose_epmd, address_please, ["testmim2", "localhost", inet]),
    Info = rpc(mim(), cets_discovery, system_info, [mongoose_cets_discovery]),
    ct:log("system_info ~p", [Info]),
    {ok, {192, 168, 115, 112}} = Res.

address_please_returns_ip_fallbacks_to_resolve_with_file_backend(_Config) ->
    Res = rpc(mim2(), mongoose_epmd, address_please, ["testmim1", "localhost", inet]),
    Info = rpc(mim2(), cets_discovery, system_info, [mongoose_cets_discovery]),
    ct:log("system_info ~p", [Info]),
    {ok, {127, 0, 0, 1}} = Res.

address_please_returns_ip_127_0_0_1_from_db(_Config) ->
    Res = rpc(mim2(), mongoose_epmd, address_please, ["node1", "localhost", inet]),
    Info = rpc(mim2(), cets_discovery, system_info, [mongoose_cets_discovery]),
    ct:log("system_info ~p", [Info]),
    {ok, {127, 0, 0, 1}} = Res.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

init_and_get_nodes(RPCNode, Opts, ExpectedNodes) ->
    StateIn = disco_init(RPCNode, Opts),
    get_nodes(RPCNode, StateIn, ExpectedNodes, false).

get_nodes(RPCNode, StateIn, ExpectedNodes) ->
    get_nodes(RPCNode, StateIn, ExpectedNodes, true).

get_nodes(RPCNode, StateIn, ExpectedNodes, AlreadyRegistered) ->
    {{ok, Nodes}, State} = disco_get_nodes(RPCNode, StateIn),
    ?assertEqual(lists:sort(ExpectedNodes), lists:sort(Nodes)),
    ?assertMatch(#{last_query_info := #{already_registered := AlreadyRegistered}}, State),
    State.

disco_init(Node, Opts) ->
    State = rpc(Node, mongoose_cets_discovery_rdbms, init, [Opts]),
    log_disco_request(?FUNCTION_NAME, Node, Opts, State),
    State.

disco_get_nodes(Node, State) ->
    NewState = rpc(Node, mongoose_cets_discovery_rdbms, get_nodes, [State]),
    log_disco_request(?FUNCTION_NAME, Node, State, NewState),
    NewState.

match_node_name(Node, SysInfo, NodeToLookup) ->
    rpc(Node, mongoose_epmd, match_node_name, [SysInfo, NodeToLookup]).

log_disco_request(disco_init, Node, #{cluster_name := CN} = Opts, State) ->
    ct:log("[0] disco_init(~p,~n" ++
           "               ~p) =~n" ++
           "        ~p",
           [Node, Opts, State]),
    erlang:put({disco, Node, CN}, 1);
log_disco_request(disco_get_nodes, Node, #{cluster_name := CN} = OldState, NewState) ->
    N = case erlang:get({disco, Node, CN}) of
            undefined -> 1;
            Int when is_integer(Int) -> Int
        end,
    ct:log("[~p] disco_get_nodes(~p,~n" ++
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

insert_new(CN, BinNode, NodeNum, Address, TS) ->
    Ret = rpc(mim(), mongoose_cets_discovery_rdbms, insert_new, [CN, BinNode, NodeNum, Address, TS]),
    ct:log("insert_new(~p, ~p, ~p, ~p, ~p) = ~p", [CN, BinNode, NodeNum, Address, TS, Ret]),
    Ret.

select(CN) ->
    Ret = rpc(mim(), mongoose_cets_discovery_rdbms, select, [CN]),
    ct:log("select(~p) = ~p", [CN, Ret]),
    Ret.

update_existing(BinNode, Address, TS) ->
    Ret = rpc(mim(), mongoose_cets_discovery_rdbms, update_existing, [BinNode, Address, TS]),
    ct:log("select(~p, ~p, ~p) = ~p", [BinNode, Address, TS, Ret]),
    Ret.

delete_node_from_db(BinNode) ->
    Ret = rpc(mim(), mongoose_cets_discovery_rdbms, delete_node_from_db, [BinNode]),
    ct:log("delete_node_from_db(~p) = ~p", [BinNode, Ret]),
    Ret.

start_cets_discovery(Config) ->
    start_disco(mim(), cets_disco_spec(<<"testmim1@localhost">>, <<"192.168.115.111">>)),
    start_disco(mim2(), cets_disco_spec(<<"testmim2@localhost">>, <<"192.168.115.112">>)),
    force_nodes_to_see_each_other(mim(), mim2()),
    Config.

start_cets_discovery_with_real_ips(Config) ->
    start_disco(mim(), cets_disco_spec(<<"node1@localhost">>, <<"127.0.0.1">>)),
    start_disco(mim2(), cets_disco_spec(<<"node2@localhost">>, <<"127.0.0.1">>)),
    force_nodes_to_see_each_other(mim(), mim2()),
    Config.

start_cets_discovery_with_file_backnend(Config) ->
    start_disco(mim(), cets_disco_spec_for_file_backend()),
    start_disco(mim2(), cets_disco_spec_for_file_backend()),
    Config.

stop_cets_discovery() ->
    ok = rpc(mim(), supervisor, terminate_child, [ejabberd_sup, cets_discovery]),
    ok = rpc(mim2(), supervisor, terminate_child, [ejabberd_sup, cets_discovery]).

stop_and_delete_cets_discovery() ->
    stop_cets_discovery(),
    ok = rpc(mim(), supervisor, delete_child, [ejabberd_sup, cets_discovery]),
    ok = rpc(mim2(), supervisor, delete_child, [ejabberd_sup, cets_discovery]).

stop_and_delete_cets_discovery_if_running() ->
    case rpc(mim(), erlang, whereis, [mongoose_cets_discovery]) of
        undefined ->
            ok;
        _ ->
            stop_and_delete_cets_discovery()
    end.

restore_default_cets_discovery() ->
    restore_default_cets_discovery(mim()),
    restore_default_cets_discovery(mim2()).

restore_default_cets_discovery(Node) ->
    case rpc(Node, mongoose_cets_discovery, supervisor_specs, []) of
        [] ->
            ok;
        [Spec] ->
            start_disco(Node, Spec)
    end.

cets_disco_spec(Node, IP) ->
    DiscoOpts = #{
        backend_module => mongoose_cets_discovery_rdbms,
        cluster_name => <<"mim">>,
        node_name_to_insert => Node,
        node_ip_binary => IP,
        name => mongoose_cets_discovery},
     cets_disco_spec(DiscoOpts).

cets_disco_spec_for_file_backend() ->
    DiscoOpts = #{
        backend_module => cets_discovery_file,
        disco_file => "/tmp/does_not_exist",
        name => mongoose_cets_discovery},
     cets_disco_spec(DiscoOpts).

cets_disco_spec(DiscoOpts) ->
     #{
        id => cets_discovery,
        start => {mongoose_cets_discovery, start_link, [DiscoOpts]},
        restart => temporary,
        type => worker,
        shutdown => infinity,
        modules => [cets_discovery]}.

send_check(Node) ->
    rpc(Node, erlang, send, [mongoose_cets_discovery, check]).

wait_for_get_nodes(Node) ->
    ok = rpc(Node, cets_discovery, wait_for_get_nodes, [mongoose_cets_discovery, 5000]).

start_disco(Node, Spec) ->
    {ok, _} = rpc(Node, supervisor, start_child, [ejabberd_sup, Spec]).

force_nodes_to_see_each_other(Node1, Node2) ->
    send_check(Node2),
    wait_for_get_nodes(Node2),
    send_check(Node1),
    wait_for_get_nodes(Node1),
    send_check(Node2),
    wait_for_get_nodes(Node2).
