%% @doc MongooseIM RDBMS backend for cets_discovery.
-module(mongoose_cets_discovery_rdbms).
-behaviour(cets_discovery).
-export([init/1, get_nodes/1]).

-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type opts() :: #{cluster_name => binary(), node_name_to_insert => binary(), last_query_info => map(),
                  min_node_count_to_expire := non_neg_integer(), expire_time := non_neg_integer(),
                  override_timestamp := non_neg_integer()}.
-type state() :: opts().

-spec init(opts()) -> state().
init(Opts = #{cluster_name := _, node_name_to_insert := _}) ->
    maps:merge(defaults(), Opts).

defaults() ->
    #{expire_time => 60 * 60 * 24, %% one day in seconds
      min_node_count_to_expire => 3,
      last_query_info => #{}}.

-spec get_nodes(state()) -> {cets_discovery:get_nodes_result(), state()}.
get_nodes(State = #{cluster_name := ClusterName, node_name_to_insert := Node}) ->
    %% We could pass the timestamp as an argument in tests
    Timestamp = maps:get(override_timestamp, State, timestamp()),
    try
        try_register(ClusterName, Node, Timestamp, State)
    of
        {Num, Nodes, Info} ->
            mongoose_node_num:set_node_num(Num),
            {{ok, Nodes}, State#{last_query_info => Info}}
    catch Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => discovery_failed_select, class => Class,
                         reason => Reason, stacktrace => Stacktrace}),
            {{error, Reason}, State}
    end.

try_register(ClusterName, NodeBin, Timestamp, State) when is_binary(NodeBin), is_binary(ClusterName) ->
    Node = binary_to_atom(NodeBin),
    prepare(),
    {selected, Rows} = select(ClusterName),
    Zipped = [{binary_to_atom(DbNodeBin), Num, TS} || {DbNodeBin, Num, TS} <- Rows],
    {Nodes, Nums, _Timestamps} = lists:unzip3(Zipped),
    AlreadyRegistered = lists:member(Node, Nodes),
    NodeNum =
        case AlreadyRegistered of
            true ->
                 update_existing(ClusterName, NodeBin, Timestamp),
                 {value, {_, Num, _TS}} = lists:keysearch(Node, 1, Zipped),
                 Num;
            false ->
                 Num = next_free_num(lists:usort(Nums)),
                 %% Could fail with duplicate node_num reason.
                 %% In this case just wait for the next get_nodes call.
                 insert_new(ClusterName, NodeBin, Timestamp, Num),
                 Num
        end,
    RunCleaningResult = run_cleaning(ClusterName, Timestamp, Rows, State),
    %% This could be used for debugging
    Info = #{already_registered => AlreadyRegistered, timestamp => Timestamp,
             node_num => Num, last_rows => Rows, run_cleaning_result => RunCleaningResult},
    Nodes2 = skip_expired_nodes(Nodes, RunCleaningResult),
    {NodeNum, Nodes2, Info}.

skip_expired_nodes(Nodes, {removed, ExpiredNodes}) ->
    Nodes -- ExpiredNodes;
skip_expired_nodes(Nodes, {skip, _}) ->
    Nodes.

run_cleaning(ClusterName, Timestamp, Rows, State) ->
    Expired = [{DbNodeBin, Num, DbTS} || {DbNodeBin, Num, DbTS} <- Rows,
               is_expired(DbTS, Timestamp, State)],
    ExpiredNodes = [binary_to_atom(DbNodeBin) || {DbNodeBin, _Num, _TS} <- Expired],
    case Expired of
        [] ->
            {skip, nothing_expired};
        _ ->
            %% We ensure that the node has the correct clocks.
            %% If clocks are too far in the future, such node would delete registrations
            %% from other nodes.
            %% If clocks are too far in the past, such node would deleted by other nodes.
            Alarm = mongoose_time_drift:has_alarm(),
            Nodes = nodes(),
            Enough = enough_visible_nodes(State, Nodes),
            Res =
                case {Alarm, Enough} of
                    {true, _} ->
                        {skip, time_drift};
                    {_, false} ->
                        {skip, not_enough_nodes};
                    _ ->
                        [delete_node_from_db(ClusterName, DbNodeBin)
                         || {DbNodeBin, _Num, _TS} <- Expired],
                        {removed, ExpiredNodes}
                end,
            ?LOG_WARNING(#{what => cets_expired_nodes,
                           text => <<"Expired nodes are detected in discovery_nodes table">>,
                           expired_list => Expired,
                           time_drift_alarm => Alarm, enough_visible_nodes => Enough,
                           visible_nodes => Nodes, run_cleaning_result => Res}),
            Res
    end.

enough_visible_nodes(#{min_node_count_to_expire := MinNodesNum}, Nodes) ->
    (length(Nodes) + 1) >= MinNodesNum.

is_expired(DbTS, Timestamp, #{expire_time := ExpireTime}) when is_integer(DbTS) ->
    DiffSeconds = (Timestamp - DbTS) div 1000000,
    DiffSeconds > ExpireTime.

delete_node_from_db(ClusterName, Node) ->
    mongoose_rdbms:execute_successfully(global, cets_delete_node_from_db, [ClusterName, Node]).

prepare() ->
    T = discovery_nodes,
    mongoose_rdbms:prepare(cets_disco_select, T, [cluster_name], select()),
    mongoose_rdbms:prepare(cets_disco_insert_new, T,
                           [cluster_name, node_name, node_num, updated_timestamp], insert_new()),
    mongoose_rdbms:prepare(cets_disco_update_existing, T,
                           [updated_timestamp, cluster_name, node_name], update_existing()),
    mongoose_rdbms:prepare(cets_delete_node_from_db, T,
                           [cluster_name, node_name], delete_node_from_db()).

select() ->
    <<"SELECT node_name, node_num, updated_timestamp FROM discovery_nodes WHERE cluster_name = ?">>.

select(ClusterName) ->
    mongoose_rdbms:execute_successfully(global, cets_disco_select, [ClusterName]).

insert_new() ->
    <<"INSERT INTO discovery_nodes (cluster_name, node_name, node_num, updated_timestamp)"
      " VALUES (?, ?, ?, ?)">>.

insert_new(ClusterName, Node, Timestamp, Num) ->
    mongoose_rdbms:execute(global, cets_disco_insert_new, [ClusterName, Node, Num, Timestamp]).

update_existing() ->
    <<"UPDATE discovery_nodes SET updated_timestamp = ? WHERE cluster_name = ? AND node_name = ?">>.

delete_node_from_db() ->
    <<"DELETE FROM discovery_nodes WHERE cluster_name = ? AND node_name = ?">>.

update_existing(ClusterName, Node, Timestamp) ->
    mongoose_rdbms:execute(global, cets_disco_update_existing, [Timestamp, ClusterName, Node]).

timestamp() ->
    os:system_time(microsecond).

%% Returns a next free node id based on the currently registered ids
next_free_num([]) ->
    0;
next_free_num([H | T = [E | _]]) when ((H + 1) =:= E) ->
    %% Sequential, ignore H
    next_free_num(T);
next_free_num([H | _]) ->
    H + 1.

-ifdef(TEST).

jid_to_opt_binary_test_() ->
    [?_assertEqual(0, next_free_num([])),
     ?_assertEqual(3, next_free_num([1, 2, 5])),
     ?_assertEqual(3, next_free_num([1, 2]))].

-endif.
