-module(metrics_helper).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

get_counter_value(CounterName) ->
    get_counter_value(CounterName, []).

get_counter_value(CounterName, Config) ->
    get_counter_value(ct:get_config(ejabberd_domain), CounterName, Config).

get_counter_value(Host, Metric, Config) ->
    RPCFun = case all_metrics_are_global(Config) of
               true ->
                     fun() ->
                             with_rpc_to_node_2(
                               escalus_ejabberd, rpc,
                               [mongoose_metrics, get_metric_value, [Host, Metric]])
                     end;
               false ->
                     fun() ->
                             escalus_ejabberd:rpc(
                               mongoose_metrics, get_metric_value, [Host, Metric])
                     end
           end,
    case RPCFun() of
        {ok, [{count, Total}, {one, _}]} ->
            {value, Total};
        {ok, [{value, Value} | _]} when is_integer(Value) ->
            {value, Value};
        {ok, Value} ->
            {value, Value};
        _ ->
            {error, unknown_counter}
    end.

assert_counter(Value, CounterName) ->
    assert_counter(Value, CounterName, []).

assert_counter(Value, CounterName, Config) ->
    {value, Value} = get_counter_value(CounterName, Config).

-spec prepare_by_all_metrics_are_global(Config :: list(), UseAllMetricsAreGlobal :: boolean()) ->
    list().
prepare_by_all_metrics_are_global(Config, false) ->
    escalus:create_users(Config, escalus:get_users([alice, bob]));
prepare_by_all_metrics_are_global(Config, true) ->
    Config1 = [{all_metrics_are_global, true} | Config],
    escalus:create_users(Config1, escalus:get_users([clusterguy, clusterbuddy])).

-spec finalise_by_all_metrics_are_global(Config :: list(), UseAllMetricsAreGlobal :: boolean()) ->
    list().
finalise_by_all_metrics_are_global(Config, false) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob]));
finalise_by_all_metrics_are_global(Config, true) ->
    Config1 = lists:keydelete(all_metrics_are_global, 1, Config),
    escalus:delete_users(Config1, escalus:get_users([clusterguy, clusterbuddy])).

all_metrics_are_global(Config) ->
    case lists:keyfind(all_metrics_are_global, 1, Config) of
        {_, Value} -> Value;
        _ -> escalus_ejabberd:rpc(ejabberd_config, get_local_option, [all_metrics_are_global])
    end.

make_global_group_name(GN) ->
    list_to_atom(atom_to_list(GN) ++ "_all_metrics_are_global").

make_global_groups_names(GroupsNames) ->
    [{group, make_global_group_name(GN)} || GN <- GroupsNames].

make_global_groups(Groups) ->
    [{make_global_group_name(GN), Opts, Cases} || {GN, Opts, Cases} <- Groups].

userspec(User1Count, Config) ->
    [User1ID | _] = user_ids(Config),
    [{User1ID, User1Count}].

userspec(User1Count, User2Count, Config) ->
    [User1ID, User2ID | _] = user_ids(Config),
    [{User1ID, User1Count}, {User2ID, User2Count}].

user_ids(Config) ->
    [ UserID || {UserID, _Otps} <- proplists:get_value(escalus_users, Config, []) ].

%% TODO: Remove this and refactor functions using it to use hosts option instead
%%       as soon as escalus_ejabberd supports it
with_rpc_to_node_2(M, F, A) ->
    Node = ct:get_config(ejabberd_node),
    Node2 = ct:get_config(ejabberd2_node),
    ct_config:update_config(ejabberd_node, Node2),
    Result = apply(M, F, A),
    ct_config:update_config(ejabberd_node, Node),
    Result.

