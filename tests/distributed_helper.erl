-module(distributed_helper).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

cluster_users() ->
    AllUsers = ct:get_config(escalus_server2_users) ++
               ct:get_config(escalus_users),
    [proplists:lookup(alice, AllUsers), proplists:lookup(clusterguy, AllUsers)].

add_node_to_cluster(Config) ->
    Config1 = set_ejabberd_cwds(Config),
    StartCmd = ctl1_path(Config1) ++ " start",
    StopCmd = ctl1_path(Config1) ++ " stop",
    Node2 = atom_to_list(ct:get_config(ejabberd2_node)),
    AddToClusterCmd = ctl1_path(Config1) ++ " add_to_cluster " ++ Node2,

    MnesiaDir = filename:join([?config(ejabberd_cwd, Config1), "Mnesia*"]),
    MnesiaCmd = "rm -rf " ++ MnesiaDir,

    %% @todo make synchronous versions of start/stop commands
    %%       in order not to use sleeps here
    Res1 = call_ejabberd2(os, cmd, [MnesiaCmd]),
    Res2 = call_ejabberd2(os, cmd, [StopCmd]),
    timer:sleep(10000),

    Res3 = call_ejabberd2(os, cmd, [AddToClusterCmd]),
    Res4 = call_ejabberd2(os, cmd, [StartCmd]),
    timer:sleep(10000),

    "" = Res1,
    "" = Res2,
    "Node added to cluster" ++ _ = Res3,
    "" = Res4,

    Config1.

remove_node_from_cluster(Config) ->
    StartCmd = ctl2_path(Config) ++ " start",
    StopCmd = ctl2_path(Config) ++ " stop",
    Node2 = atom_to_list(ct:get_config(ejabberd2_node)),
    RemoveCmd = ctl1_path(Config) ++ " remove_from_cluster " ++ Node2,

    MnesiaDir = filename:join([?config(ejabberd2_cwd, Config), "Mnesia*"]),
    MnesiaCmd = "rm -rf " ++ MnesiaDir,

    Res1 = call_ejabberd(os, cmd, [StopCmd]),
    timer:sleep(10000),

    Res2 = call_ejabberd(os, cmd, [RemoveCmd]),
    Res3 = call_ejabberd(os, cmd, [MnesiaCmd]),
    Res4 = call_ejabberd(os, cmd, [StartCmd]),
    timer:sleep(10000),

    "" = Res1,
    "{atomic,ok}\n" = Res2,
    "" = Res3,
    "" = Res4,
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

