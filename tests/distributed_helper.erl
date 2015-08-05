-module(distributed_helper).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(ejabberd_node_utils, [get_cwd/2,
                              call_fun/4]).

-compile(export_all).

is_sm_distributed() ->
    Backend = escalus_ejabberd:rpc(ejabberd_sm_backend, backend, []),
    is_sm_backend_distributed(Backend).

is_sm_backend_distributed(ejabberd_sm_mnesia) -> true;
is_sm_backend_distributed(Other)              -> {false, Other}.

add_node_to_cluster(ConfigIn) ->
    Node = ct:get_config(ejabberd_node),
    Node2 = ct:get_config(ejabberd2_node),
    Config = ejabberd_node_utils:init(Node2,
                                      ejabberd_node_utils:init(Node, ConfigIn)),

    Node2Ctl = ctl_path(Node2, Config),

    StartCmd = Node2Ctl ++ " start",
    StopCmd = Node2Ctl  ++ " stop",
    StatusCmd = Node2Ctl ++ " status",


    AddToClusterCmd = ctl_path(Node2, Config) ++ " add_to_cluster " ++ atom_to_list(Node),

    MnesiaDir = filename:join([get_cwd(Node2, Config), "Mnesia*"]),
    MnesiaCmd = "rm -rf " ++ MnesiaDir,

    Res1 = call_fun(Node, os, cmd, [MnesiaCmd]),
    Res2 = call_fun(Node, os, cmd, [StopCmd]),
    wait_until_stopped(StatusCmd, 120),

    Res3 = call_fun(Node, os, cmd, [AddToClusterCmd]),
    Res4 = call_fun(Node, os, cmd, [StartCmd]),
    wait_until_started(StatusCmd, 120),

    ?assertEqual(Res1, ""),
    ?assertEqual(Res2, ""),
    "Node added to cluster" ++ _ = Res3,
    ?assertEqual(Res4, ""),

    verify_result(add),

    Config.

remove_node_from_cluster(Config) ->
    Node = ct:get_config(ejabberd_node),
    Node2 = ct:get_config(ejabberd2_node),
    Node2Ctl = ctl_path(Node2, Config),
    StartCmd = Node2Ctl ++ " start",
    StopCmd = Node2Ctl ++ " stop",
    StatusCmd = Node2Ctl ++ " status",
    RemoveCmd = ctl_path(Node, Config) ++ " remove_from_cluster " ++ atom_to_list(Node2),

    MnesiaDir = filename:join([get_cwd(Node2, Config), "Mnesia*"]),
    MnesiaCmd = "rm -rf " ++ MnesiaDir,

    Res1 = call_fun(Node, os, cmd, [StopCmd]),
    wait_until_stopped(StatusCmd, 120),

    Res2 = call_fun(Node, os, cmd, [RemoveCmd]),
    Res3 = call_fun(Node, os, cmd, [MnesiaCmd]),
    Res4 = call_fun(Node, os, cmd, [StartCmd]),
    wait_until_started(StatusCmd, 120),

    ?assertEqual(Res1, ""),
    ?assertEqual(Res2, "{atomic,ok}\n"),
    ?assertEqual(Res3, ""),
    ?assertEqual(Res4, ""),

    verify_result(remove),

    ok.


ctl_path(Node, Config) ->
    filename:join([get_cwd(Node, Config), "bin", "mongooseimctl"]).

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
    Nodes1 = call_fun(Node1, erlang, nodes, []),
    Nodes2 = call_fun(Node2, erlang, nodes, []),
    DbNodes1 = call_fun(Node1, mnesia, system_info, [running_db_nodes]),
    DbNodes2 = call_fun(Node2, mnesia, system_info, [running_db_nodes]),

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
