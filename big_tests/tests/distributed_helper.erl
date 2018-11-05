-module(distributed_helper).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(ejabberd_node_utils, [get_cwd/2]).

-compile(export_all).

is_sm_distributed() ->
    Backend = rpc(mim(), ejabberd_sm_backend, backend, []),
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
    escalus_rpc:call(Node, M, F, A, TimeOut, Cookie).

%% @doc Require nodes defined in `test.config' for later convenient RPCing into.
%%
%% The use case would be to require and import the same names in your suite like:
%%
%%  -import(distributed_helper, [mim/0, fed/0,
%%                               require_rpc_nodes/1,
%%                               rpc/4]).
%%
%%  ...
%%
%%  suite() ->
%%      require_rpc_nodes([mim, fed]) ++ escalus:suite().
%%
%%  ...
%%
%%  example_test(_Config) ->
%%      RPCResult = rpc(mim(), remote_mod, remote_fun, [arg1, arg2]),
%%      ...
%%
require_rpc_nodes(Nodes) ->
    [ {require, {hosts, Node, node}} || Node <- Nodes ].

%% @doc Shorthand for hosts->mim->node from `test.config'.
mim() ->
    get_or_fail({hosts, mim, node}).

mim2() ->
    get_or_fail({hosts, mim2, node}).

mim3() ->
    get_or_fail({hosts, mim3, node}).

%% @doc Shorthand for hosts->fed->node from `test.config'.
fed() ->
    get_or_fail({hosts, fed, node}).

get_or_fail(Key) ->
    Val = ct:get_config(Key),
    Val == undefined andalso error({undefined, Key}),
    Val.

start_node(Node, Config) ->
    {_, 0} = ejabberdctl_helper:ejabberdctl(Node, "start", [], Config),
    {_, 0} = ejabberdctl_helper:ejabberdctl(Node, "started", [], Config),
    %% TODO Looks like "started" run by ejabberdctl fun is not really synchronous
    timer:sleep(3000).

stop_node(Node, Config) ->
    {_, 0} = mongooseim_script(Node, "stop", [], Config).

mongooseim_script(Node, Cmd, Args, Config) ->
    CtlCmd = script_path(Node, Config, "mongooseim"),
    ejabberdctl_helper:run(string:join([CtlCmd, Cmd | ejabberdctl_helper:normalize_args(Args)], " ")).

