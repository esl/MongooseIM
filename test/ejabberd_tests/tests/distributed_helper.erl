-module(distributed_helper).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(ejabberd_node_utils, [get_cwd/2]).

-compile(export_all).

is_sm_distributed() ->
    Backend = escalus_ejabberd:rpc(ejabberd_sm_backend, backend, []),
    is_sm_backend_distributed(Backend).

is_sm_backend_distributed(ejabberd_sm_mnesia) -> true;
is_sm_backend_distributed(Other)              -> {false, Other}.

add_node_to_cluster(Config) ->
    %% TODO: nodes should be described in a uniform fashion, not with adhoc names
    Node = ct:get_config(ejabberd_node),
    Node2 = ct:get_config(ejabberd2_node),
    ok = rpc(Node2, mongoose_cluster, join, [Node], cluster_op_timeout()),
    verify_result(add),
    Config.

remove_node_from_cluster(Config) ->
    Node2 = ct:get_config(ejabberd2_node),
    ok = rpc(Node2, mongoose_cluster, leave, [], cluster_op_timeout()),
    verify_result(remove),
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
            wait_until_started(Cmd, Retries-1)
    end.

wait_until_stopped(_, 0) ->
    erlang:error({timeout, stopping_node});
wait_until_stopped(Cmd, Retries) ->
    case os:cmd(Cmd) of
        "pong" ++ _->
            timer:sleep(1000),
            wait_until_stopped(Cmd, Retries-1);
        _ ->
            ok
    end.

verify_result(Op) ->
    Node1 = ct:get_config(ejabberd_node),
    Node2 = ct:get_config(ejabberd2_node),
    DbNodes1 = rpc(Node1, mnesia, system_info, [running_db_nodes]),
    DbNodes2 = rpc(Node2, mnesia, system_info, [running_db_nodes]),
    Pairs = [{Node1, DbNodes2, should_belong(Op)},
             {Node2, DbNodes1, should_belong(Op)},
             {Node1, DbNodes1, true},
             {Node2, DbNodes2, true}],
    [?assertEqual(ShouldBelong, lists:member(Element, List))
     || {Element, List, ShouldBelong} <- Pairs].

should_belong(add)    -> true;
should_belong(remove) -> false.

cluster_op_timeout() ->
    %% This timeout is deliberately a long one.
    timer:seconds(25).

rpc(Node, M, F, A) ->
    rpc(Node, M, F, A, timer:seconds(5)).

rpc(Node, M, F, A, TimeOut) ->
    Cookie = ct:get_config(ejabberd_cookie),
    escalus_ct:rpc_call(Node, M, F, A, TimeOut, Cookie).
