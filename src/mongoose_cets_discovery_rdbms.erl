%% @doc MongooseIM RDBMS backend for cets_discovery.
-module(mongoose_cets_discovery_rdbms).
-behaviour(cets_discovery).
-export([init/1, get_nodes/1]).

-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type opts() :: #{cluster_name => binary(), node_name_to_insert => binary(), last_query_info => map()}.
-type state() :: opts().

-spec init(opts()) -> state().
init(Opts = #{cluster_name := _, node_name_to_insert := _}) ->
    Opts#{last_query_info => #{}}.

-spec get_nodes(state()) -> {cets_discovery:get_nodes_result(), state()}.
get_nodes(State = #{cluster_name := ClusterName, node_name_to_insert := Node}) ->
    try
        case is_rdbms_running() of
            true ->
                try_register(ClusterName, Node);
            false ->
                skip
        end
    of
        {Num, Nodes, Info} ->
            mongoose_node_num:set_node_num(Num),
            {{ok, Nodes}, State#{last_query_info => Info}};
        skip ->
            {{error, rdbms_not_running}, State}
    catch Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => discovery_failed_select, class => Class,
                         reason => Reason, stacktrace => Stacktrace}),
            {{error, Reason}, State}
    end.

is_rdbms_running() ->
    try mongoose_wpool:get_worker(rdbms, global) of
         {ok, _} -> true;
         _ -> false
    catch _:_ ->
         false
    end.

try_register(ClusterName, NodeBin) when is_binary(NodeBin), is_binary(ClusterName) ->
    Node = binary_to_atom(NodeBin),
    prepare(),
    {selected, Rows} = select(ClusterName),
    Pairs = [{binary_to_atom(DbNodeBin), Num} || {DbNodeBin, Num} <- Rows],
    {Nodes, Nums} = lists:unzip(Pairs),
    AlreadyRegistered = lists:member(Node, Nodes),
    Timestamp = timestamp(),
    NodeNum =
        case AlreadyRegistered of
            true ->
                 update_existing(ClusterName, NodeBin, Timestamp),
                 {value, {_, Num}} = lists:keysearch(Node, 1, Pairs),
                 Num;
            false ->
                 Num = next_free_num(lists:usort(Nums)),
                 %% Could fail with duplicate node_num reason.
                 %% In this case just wait for the next get_nodes call.
                 insert_new(ClusterName, NodeBin, Timestamp, Num),
                 Num
        end,
    %% This could be used for debugging
    Info = #{already_registered => AlreadyRegistered, timestamp => Timestamp,
             node_num => Num, last_rows => Rows},
    {NodeNum, Nodes, Info}.

prepare() ->
    T = discovery_nodes,
    mongoose_rdbms:prepare(cets_disco_select, T, [cluster_name], select()),
    mongoose_rdbms:prepare(cets_disco_insert_new, T,
                           [cluster_name, node_name, node_num, updated_timestamp], insert_new()),
    mongoose_rdbms:prepare(cets_disco_update_existing, T,
                           [updated_timestamp, cluster_name, node_name], update_existing()).

select() ->
    <<"SELECT node_name, node_num FROM discovery_nodes WHERE cluster_name = ?">>.

select(ClusterName) ->
    mongoose_rdbms:execute_successfully(global, cets_disco_select, [ClusterName]).

insert_new() ->
    <<"INSERT INTO discovery_nodes (cluster_name, node_name, node_num, updated_timestamp)"
      " VALUES (?, ?, ?, ?)">>.

insert_new(ClusterName, Node, Timestamp, Num) ->
    mongoose_rdbms:execute(global, cets_disco_insert_new, [ClusterName, Node, Num, Timestamp]).

update_existing() ->
    <<"UPDATE discovery_nodes SET updated_timestamp = ? WHERE cluster_name = ? AND node_name = ?">>.

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
