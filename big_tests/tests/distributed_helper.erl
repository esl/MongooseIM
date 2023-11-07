-module(distributed_helper).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(ejabberd_node_utils, [get_cwd/2]).

-compile([export_all, nowarn_export_all]).

-type rpc_spec() :: #{node := node(),
                      cookie => atom(),
                      timeout => non_neg_integer()}.

-export_type([rpc_spec/0]).

is_sm_distributed() ->
    Backend = rpc(mim(), ejabberd_sm, sm_backend, []),
    is_sm_backend_distributed(Backend).

is_sm_backend_distributed(ejabberd_sm_mnesia) -> true;
is_sm_backend_distributed(Other) -> {false, Other}.

add_node_to_cluster(Config) ->
    Node2 = mim2(),
    add_node_to_cluster(Node2, Config).

has_mnesia(Node) ->
    %% TODO We should check that Mnesia is configured here instead of is_running.
    %% But it would require the issue fixed first:
    %% "MIM-2067 Actually disable mnesia from starting in tests in pgsql_cets"
    rpc(Node, mnesia, system_info, [is_running]) =:= yes.

add_node_to_cluster(Node, Config) ->
    case has_mnesia(Node) of
        true ->
            add_node_to_mnesia_cluster(Node, Config);
        false ->
            ok
    end,
    Config.

add_node_to_mnesia_cluster(Node, Config) ->
    ClusterMemberNode = maps:get(node, mim()),
    ok = rpc(Node#{timeout => cluster_op_timeout()},
             mongoose_cluster, join, [ClusterMemberNode]),
    verify_result(Node, add).

remove_node_from_cluster(_Config) ->
    Node = mim2(),
    remove_node_from_cluster(Node, _Config).

remove_node_from_cluster(Node, _Config) ->
    case has_mnesia(Node) of
        true ->
            ok = rpc(Node#{timeout => cluster_op_timeout()}, mongoose_cluster, leave, []),
            verify_result(Node, remove);
        false ->
            ok
    end,
    ok.

ctl_path(Node, Config) ->
    script_path(Node, Config, "mongooseimctl").

bin_path(Node, Config) ->
    filename:join([get_cwd(Node, Config), "bin"]).

script_path(Node, Config, Script) ->
    filename:join([get_cwd(Node, Config), "bin", Script]).

verify_result(Node, Op) ->
    mongoose_helper:wait_until(fun() -> catch do_verify_result(Node, Op) end, [],
                               #{time_left => timer:seconds(20),
                                 sleep_time => 1000,
                                 name => verify_result}),
    mongoose_helper:wait_until(fun() -> check_mongooseim_on_node_started(mim()) end, true,
                               #{time_left => timer:seconds(20),
                                 sleep_time => 1000,
                                 name => verify_mongooseim_started}).

check_mongooseim_on_node_started(Node) ->
    lists:keymember(mongooseim, 1, rpc(Node, application, which_applications, [])).

do_verify_result(Node, Op) ->
    VerifyNode = mim(),
    DbNodes1 = rpc(Node, mnesia, system_info, [running_db_nodes]),
    DbNodes2 = rpc(VerifyNode, mnesia, system_info, [running_db_nodes]),
    Checks = [{Node, DbNodes2, should_belong(Op)},
              {VerifyNode, DbNodes1, should_belong(Op)},
              {Node, DbNodes1, true},
              {VerifyNode, DbNodes2, true}],
    Results = [case lists:member(maps:get(node, CurrentNode), RunningNodes) of
                   ShouldBelong ->
                       [];
                   _ ->
                       ct:log("~p has ~p~n~p has ~p~n", [Node, DbNodes1, VerifyNode, DbNodes2]),
                       [Check]
               end || Check = {CurrentNode, RunningNodes, ShouldBelong} <- Checks],
    lists:append(Results).

should_belong(add) -> true;
should_belong(remove) -> false.

cluster_op_timeout() ->
    %% This timeout is deliberately a long one.
    timer:seconds(30).

%% @doc Perform a remote call on a target node described by `RPCSpec'.
%%
%% We can define the spec once for multiple calls:
%%
%% ```
%% -define(dh, distributed_helper).
%%
%% my_test(Config) ->
%%    Spec = #{node => ?dh:mim()},
%%    ...
%%    ?dh:rpc(Spec, ejabberd_sm, get_full_session_list, []),
%%    ?dh:rpc(Spec#{timeout => timer:seconds(30),
%%            mongoose_cluster, join, [Node1])
%%    ...
%% '''
%%
%% Or inline for a quick-and-dirty hack (but beware of code review):
%%
%% ```
%% my_test(Config) ->
%%    ...
%%    ?dh:rpc(#{node => mongooseim@localhost}, ejabberd_sm, get_full_session_list, []),
%%    ...
%% '''
%% @end
-spec rpc(Spec, _, _, _) -> any() when
      Spec :: rpc_spec().
rpc(#{} = RPCSpec, M, F, A) ->
    Node = maps:get(node, RPCSpec),
    Cookie = maps:get(cookie, RPCSpec, erlang:get_cookie()),
    TimeOut = maps:get(timeout, RPCSpec, timer:seconds(5)),
    case ct_rpc:call(Node, M, F, A, TimeOut, Cookie) of
        {badrpc, Reason} -> error({badrpc, Reason}, [RPCSpec, M, F, A]);
        Result -> Result
    end.

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
-spec mim() -> rpc_spec().
mim() ->
    rpc_spec(mim).

-spec mim2() -> rpc_spec().
mim2() ->
    rpc_spec(mim2).

-spec mim3() -> rpc_spec().
mim3() ->
    rpc_spec(mim3).

%% @doc Shorthand for hosts->fed->node from `test.config'.
-spec fed() -> rpc_spec().
fed() ->
    rpc_spec(fed).

rpc_spec(NodeKey) ->
    #{node => get_or_fail({hosts, NodeKey, node})}.

get_or_fail(Key) ->
    Val = ct:get_config(Key),
    Val == undefined andalso error({undefined, Key}),
    Val.

start_node(Node, Config) ->
    {_, 0} = mongooseimctl_helper:mongooseimctl(Node, "start", [], Config),
    timer:sleep(3000).

stop_node(Node, Config) ->
    {_, 0} = mongooseim_script(Node, "stop", [], Config).

mongooseim_script(Node, Cmd, Args, Config) ->
    CtlCmd = script_path(Node, Config, "mongooseim"),
    mongooseimctl_helper:run(CtlCmd, [Cmd | Args]).

subhost_pattern(SubhostTemplate) ->
    rpc(mim(), mongoose_subdomain_utils, make_subdomain_pattern, [SubhostTemplate]).

lookup_config_opt(Key) ->
    rpc(mim(), mongoose_config, lookup_opt, [Key]).
