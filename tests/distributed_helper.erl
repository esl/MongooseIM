-module(distributed_helper).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

cluster_users() ->
    AllUsers = ct:get_config(escalus_server2_users) ++
               ct:get_config(escalus_users),
    [proplists:lookup(alice, AllUsers), proplists:lookup(clusterguy, AllUsers)].

add_node_to_cluster(Config) ->
    Config1 = set_ejabberd_cwds(Config),
    StartCmd = ctl1_path(Config1) ++ " start",
    StopCmd = ctl1_path(Config1) ++ " stop",
    StatusCmd = ctl1_path(Config1) ++ " status",

    Node2 = atom_to_list(ct:get_config(ejabberd2_node)),
    AddToClusterCmd = ctl1_path(Config1) ++ " add_to_cluster " ++ Node2,

    MnesiaDir = filename:join([?config(ejabberd_cwd, Config1), "Mnesia*"]),
    MnesiaCmd = "rm -rf " ++ MnesiaDir,

    Res1 = call_ejabberd2(os, cmd, [MnesiaCmd]),
    Res2 = call_ejabberd2(os, cmd, [StopCmd]),
    wait_until_stopped(StatusCmd, 120),

    Res3 = call_ejabberd2(os, cmd, [AddToClusterCmd]),
    Res4 = call_ejabberd2(os, cmd, [StartCmd]),
    wait_until_started(StatusCmd, 120),

    ?assertEqual(Res1, ""),
    ?assertEqual(Res2, ""),
    "Node added to cluster" ++ _ = Res3,
    ?assertEqual(Res4, ""),

    verify_result(add),

    Config1.

remove_node_from_cluster(Config) ->
    StartCmd = ctl2_path(Config) ++ " start",
    StopCmd = ctl2_path(Config) ++ " stop",
    StatusCmd = ctl2_path(Config) ++ " status",
    Node2 = atom_to_list(ct:get_config(ejabberd2_node)),
    RemoveCmd = ctl1_path(Config) ++ " remove_from_cluster " ++ Node2,

    MnesiaDir = filename:join([?config(ejabberd2_cwd, Config), "Mnesia*"]),
    MnesiaCmd = "rm -rf " ++ MnesiaDir,

    Res1 = call_ejabberd(os, cmd, [StopCmd]),
    wait_until_stopped(StatusCmd, 120),

    Res2 = call_ejabberd(os, cmd, [RemoveCmd]),
    Res3 = call_ejabberd(os, cmd, [MnesiaCmd]),
    Res4 = call_ejabberd(os, cmd, [StartCmd]),
    wait_until_started(StatusCmd, 120),

    ?assertEqual(Res1, ""),
    ?assertEqual(Res2, "{atomic,ok}\n"),
    ?assertEqual(Res3, ""),
    ?assertEqual(Res4, ""),

    verify_result(remove),

    ok.

set_ejabberd_cwds(Config) ->
    {ok, Cwd1} = call_ejabberd(file, get_cwd, []),
    {ok, Cwd2} = call_ejabberd2(file, get_cwd, []),
    [{ejabberd_cwd, Cwd1}, {ejabberd2_cwd, Cwd2} | Config].

ctl1_path(Config) ->
    filename:join([?config(ejabberd_cwd, Config), "bin", "mongooseimctl"]).

ctl2_path(Config) ->
    filename:join([?config(ejabberd2_cwd, Config), "bin", "mongooseimctl"]).

call_ejabberd(M, F, A) ->
    Node = ct:get_config(ejabberd_node),
    rpc:call(Node, M, F, A).

call_ejabberd2(M, F, A) ->
    Node = ct:get_config(ejabberd2_node),
    rpc:call(Node, M, F, A).

wait_until_started(_, 0) ->
    erlang:error({timeout, starting_node});
wait_until_started(Cmd, Retries) ->
    Result = os:cmd(Cmd),
    case re:run(Result, "The node .* is started") of
        {match, _} ->
            ok;
        nomatch ->
            timer:sleep(1000),
            wait_until_started(Cmd, Retries-1)
    end.

wait_until_stopped(_, 0) ->
    erlang:error({timeout, stopping_node});
wait_until_stopped(Cmd, Retries) ->
    case os:cmd(Cmd) of
        "Failed RPC connection" ++ _ ->
            ok;
        "The node" ++ _ ->
            timer:sleep(1000),
            wait_until_stopped(Cmd, Retries-1)
    end.

verify_result(Op) ->
    Node1 = ct:get_config(ejabberd_node),
    Node2 = ct:get_config(ejabberd2_node),
    Nodes1 = call_ejabberd(erlang, nodes, []),
    Nodes2 = call_ejabberd2(erlang, nodes, []),
    DbNodes1 = call_ejabberd(mnesia, system_info, [running_db_nodes]),
    DbNodes2 = call_ejabberd2(mnesia, system_info, [running_db_nodes]),

    Pairs = [{Node2, Nodes1,   should_belong(Op)},
             {Node1, Nodes2,   should_belong(Op)},
             {Node1, DbNodes2, should_belong(Op)},
             {Node2, DbNodes1, should_belong(Op)},
             {Node1, DbNodes1, true},
             {Node2, DbNodes2, true}],

    [?assertEqual(ShouldBelong, lists:member(Element, List))
     || {Element, List, ShouldBelong} <- Pairs].

should_belong(add)    -> true;
should_belong(remove) -> false.
