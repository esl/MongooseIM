%% @doc MongooseIM RDBMS backend for cets_discovery.
-module(mongoose_cets_discovery_rdbms).
-behaviour(cets_discovery).
-export([init/1, get_nodes/1]).

-include_lib("kernel/include/logger.hrl").

-type opts() :: #{cluster_name => binary(), node_name_to_insert => binary()}.
-type state() :: opts().

-spec init(opts()) -> state().
init(Opts = #{cluster_name := _, node_name_to_insert := _}) ->
    Opts.

-spec get_nodes(state()) -> {cets_discovery:get_nodes_result(), state()}.
get_nodes(State = #{cluster_name := ClusterName, node_name_to_insert := Node}) ->
    try
        {Num, Nodes} = try_register(ClusterName, Node),
        mongoose_node_num:set_node_num(Num),
        {{ok, Nodes}, State}
    catch Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => discovery_failed_select, class => Class,
                         reason => Reason, stacktrace => Stacktrace}),
            {{error, Reason}, State}
    end.

try_register(ClusterName, Node) ->
    prepare(),
    {selected, Rows} = select(ClusterName),
    Pairs = [{binary_to_atom(NodeBin), Num} || {NodeBin, Num} <- Rows],
    {Nodes, Nums} = lists:unzip(Pairs),
    Inserted = lists:member(Node, Nodes),
    Timestamp = timestamp(),
    NodeNum =
        case Inserted of
            true ->
                 update_existing(ClusterName, Node, Timestamp),
                 {value, {_, Num}} = lists:keysearch(Node, 1, Pairs),
                 Num;
            false ->
                 Num = next_free_num(lists:usort(Nums)),
                 insert_new(ClusterName, Node, Timestamp, Num),
                 Num
        end,
    {NodeNum, Nodes}.

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
