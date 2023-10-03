%% @doc Checks that internal databases are not started accidentally.
-module(ct_sanity_hook).

%% Callbacks
-export([id/1]).
-export([init/2]).
-export([post_init_per_suite/4,
         post_init_per_group/4,
         post_init_per_testcase/4]).
-export([post_end_per_suite/4,
         post_end_per_group/4,
         post_end_per_testcase/4]).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    "ct_sanity_hook_001".

init(_Id, _Opts) ->
    Nodes = alive_mim_nodes(),
    {ok, #{
        alive_mim_nodes => Nodes,
        running_internal_databases => running_internal_databases(Nodes)
    }}.

post_init_per_suite(_SuiteName, _Config, Return, State) ->
    {Return, State}.

post_init_per_group(_GroupName, _Config, Return, State) ->
    {Return, State}.

post_init_per_testcase(_TC, _Config, Return, State) ->
    {Return, State}.

post_end_per_suite(_SuiteName, _Config, Return, State) ->
    Return2 = assert_same_databases(State, Return),
    {Return2, State}.

post_end_per_group(_GroupName, _Config, Return, State) ->
    {Return, State}.

post_end_per_testcase(_TC, _Config, Return, State) ->
    {Return, State}.

running_internal_databases(Nodes) ->
    [{Node, get_databases(Node)} || Node <- Nodes].

get_databases(Node) ->
    case rpc:call(Node, mongoose_internal_databases, running_internal_databases, []) of
        List when is_list(List) ->
            List;
        Other ->
            error({get_databases_failed, Node, Other})
    end.

alive_mim_nodes() ->
    [Node || Node <- mim_nodes(), net_adm:ping(Node) =:= pong].

mim_nodes() ->
    [proplists:get_value(node, Opts) || {_, Opts} <- ct:get_config(hosts)].

assert_same_databases(#{alive_mim_nodes := Nodes, running_internal_databases := Old}, Return) ->
    New = running_internal_databases(Nodes),
    case Old of
        New ->
            Return;
        _ ->
            {fail, #{reason => assert_same_databases_failed,
                     expected_internal_databases => Old,
                     running_internal_databases => New}}
    end.
