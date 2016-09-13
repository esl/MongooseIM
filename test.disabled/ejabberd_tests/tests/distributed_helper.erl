-module(distributed_helper).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(ejabberd_node_utils, [get_cwd/2, mim/0, mim2/0, fed/0]).

-compile(export_all).

is_sm_distributed() ->
    Backend = escalus_ejabberd:rpc(ejabberd_sm_backend, backend, []),
    is_sm_backend_distributed(Backend).

is_sm_backend_distributed(ejabberd_sm_mnesia) -> true;
is_sm_backend_distributed(Other) -> {false, Other}.

add_node_to_cluster(Config) ->
    Node2 = mim2(),
    add_node_to_cluster(Node2, Config).

add_node_to_cluster(Node, Config) ->
    ClusterMember = mim(),
    ok = rpc(Node, mongoose_cluster, join, [ClusterMember], cluster_op_timeout()),
    verify_result(Node, add),
    Config.

remove_node_from_cluster(_Config) ->
    Node = mim2(),
    remove_node_from_cluster(Node, _Config).

remove_node_from_cluster(Node, _Config) ->
    ok = rpc(Node, mongoose_cluster, leave, [], cluster_op_timeout()),
    verify_result(Node, remove),
    ok.

ctl_path(Node, Config) ->
    script_path(Node, Config, "mongooseimctl").

script_path(Node, Config, Script) ->
    filename:join([get_cwd(Node, Config), "bin", Script]).

wait_until_started(_, 0) ->
    erlang:error({timeout, starting_node});
wait_until_started(Cmd, Retries) ->
    Result = os:cmd(Cmd),
    case Result of
        "pong" ++ _ ->
            ok;
        _ ->
            timer:sleep(1000),
            wait_until_started(Cmd, Retries - 1)
    end.

wait_until_stopped(_, 0) ->
    erlang:error({timeout, stopping_node});
wait_until_stopped(Cmd, Retries) ->
    case os:cmd(Cmd) of
        "pong" ++ _ ->
            timer:sleep(1000),
            wait_until_stopped(Cmd, Retries - 1);
        _ ->
            ok
    end.

verify_result(Node, Op) ->
    VerifyNode = mim(),
    DbNodes1 = rpc(Node, mnesia, system_info, [running_db_nodes]),
    DbNodes2 = rpc(VerifyNode, mnesia, system_info, [running_db_nodes]),
    Pairs = [{Node, DbNodes2, should_belong(Op)},
        {VerifyNode, DbNodes1, should_belong(Op)},
        {Node, DbNodes1, true},
        {VerifyNode, DbNodes2, true}],
    [?assertEqual(ShouldBelong, lists:member(Element, List))
        || {Element, List, ShouldBelong} <- Pairs].

should_belong(add) -> true;
should_belong(remove) -> false.

cluster_op_timeout() ->
    %% This timeout is deliberately a long one.
    timer:seconds(30).

rpc(Node, M, F, A) ->
    rpc(Node, M, F, A, timer:seconds(5)).

rpc(Node, M, F, A, TimeOut) ->
    Cookie = ct:get_config(ejabberd_cookie),
    escalus_ct:rpc_call(Node, M, F, A, TimeOut, Cookie).
