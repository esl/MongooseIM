-module(metrics_helper).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, mim2/0,
                             rpc/4]).

%% We introduce a convention, where metrics-related suites use only 2 accounts
%% but it depends on `all_metrics_are_global` flag, which duet it will be.
-define(WAIT_TIME, 500).
-define(METRICS_GROUP_USERS, [alice, bob]).
-define(ONLY_GLOBAL_METRICS_GROUP_USERS, [clusterguy, clusterbuddy]).

get_counter_value(CounterName) ->
    get_counter_value(domain_helper:host_type(mim), CounterName).

get_counter_value(HostType, Metric) ->
    case rpc(mim(), mongoose_metrics, get_metric_value, [HostType, Metric]) of
        {ok, [{count, Total}, {one, _}]} ->
            {value, Total};
        {ok, [{value, Value} | _]} when is_integer(Value) ->
            {value, Value};
        {ok, Value} ->
            {value, Value};
        _ ->
            {error, unknown_counter}
    end.

sample(Name) ->
    rpc(mim(), mongoose_metrics, sample_metric, [[global, Name]]).

assert_counter(Value, CounterName) ->
    assert_counter(domain_helper:host_type(mim), Value, CounterName).

assert_counter(HostType, Value, CounterName) ->
    {value, Value} = get_counter_value(HostType, CounterName).

-spec prepare_by_all_metrics_are_global(Config :: list(), UseAllMetricsAreGlobal :: boolean()) ->
    list().
prepare_by_all_metrics_are_global(Config, false) ->
    escalus:create_users(Config, escalus:get_users(?METRICS_GROUP_USERS));
prepare_by_all_metrics_are_global(Config, true) ->
    Config1 = [{all_metrics_are_global, true} | Config],
    %% TODO: Refactor once escalus becomes compatible with multiple nodes RPC
    Config2 = distributed_helper:add_node_to_cluster(mim2(), Config1),
    escalus:create_users(Config2, escalus:get_users(?ONLY_GLOBAL_METRICS_GROUP_USERS)).

-spec finalise_by_all_metrics_are_global(Config :: list(), UseAllMetricsAreGlobal :: boolean()) ->
    list().
finalise_by_all_metrics_are_global(Config, false) ->
    escalus:delete_users(Config, escalus:get_users(?METRICS_GROUP_USERS));
finalise_by_all_metrics_are_global(Config, true) ->
    Config1 = lists:keydelete(all_metrics_are_global, 1, Config),
    %% TODO: Refactor once escalus becomes compatible with multiple nodes RPC
    distributed_helper:remove_node_from_cluster(mim2(), Config1),
    escalus:delete_users(Config1,
                         escalus:get_users(?ONLY_GLOBAL_METRICS_GROUP_USERS)).

all_metrics_are_global(Config) ->
    case lists:keyfind(all_metrics_are_global, 1, Config) of
        {_, Value} -> Value;
        _ -> false
    end.

make_global_group_name(GN) ->
    list_to_atom(atom_to_list(GN) ++ "_all_metrics_are_global").

make_global_groups_names(GroupsNames) ->
    [{group, make_global_group_name(GN)} || GN <- GroupsNames].

make_global_groups(Groups) ->
    [{make_global_group_name(GN), Opts, Cases} || {GN, Opts, Cases} <- Groups].

%% Converts legacy userspec format to the new one. In order for this function to work 100%
%% correctly, the suite has to use (prepare|finalise)_by_all_metrics_are_global.
%% This function is an abstraction over `all_metrics_are_global` true/false distinction.
%% It automatically picks proper users and the suite provides only a number of resources
%% for user 1 and user 2.
userspec(User1Count, Config) ->
    [User1ID, _] = user_ids(Config),
    [{User1ID, User1Count}].

userspec(User1Count, User2Count, Config) ->
    [User1ID, User2ID] = user_ids(Config),
    [{User1ID, User1Count}, {User2ID, User2Count}].

user_ids(Config) ->
    case all_metrics_are_global(Config) of
        true -> ?ONLY_GLOBAL_METRICS_GROUP_USERS;
        _ -> ?METRICS_GROUP_USERS
    end.

wait_for_counter(ExpectedValue, Counter) ->
        wait_for_counter(domain_helper:host_type(mim), ExpectedValue, Counter).

wait_for_counter(HostType, ExpectedValue, Counter) ->
    mongoose_helper:wait_until(fun() ->
                                       assert_counter(HostType, ExpectedValue, Counter)
                                   end,
                                   {value, ExpectedValue},
                                   #{name => Counter, time_left => ?WAIT_TIME, sleep_time => 20}).

make_host_type_name(HT) when is_atom(HT) ->
    HT;
make_host_type_name(HT) when is_binary(HT) ->
    binary:replace(HT, <<" ">>, <<"_">>, [global]).
