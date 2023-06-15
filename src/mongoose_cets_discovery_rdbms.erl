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
        {Id, Nodes} = try_register(ClusterName, Node),
        mongoose_short_number_node_id:set_node_id(Id),
        {{ok, Nodes}, State}
    catch Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => discovery_failed_select, class => Class,
                         reason => Reason, stacktrace => Stacktrace}),
            {{error, Reason}, State}
    end.

try_register(ClusterName, Node) ->
    prepare(),
    {selected, Rows} = select(ClusterName),
    Pairs = [{binary_to_atom(Node), Id} || {Node, Id} <- Rows],
    {Nodes, Ids} = lists:unzip(Pairs),
    Inserted = lists:member(Node, Nodes),
    Timestamp = timestamp(),
    NodeId =
        case Inserted of
            true ->
                 update_existing(ClusterName, Node, Timestamp),
                 {value, {_, Id}} = lists:keysearch(Node, 1, Pairs),
                 Id;
            false ->
                 Id = next_id(lists:usort(Ids)),
                 insert_new(ClusterName, Node, Timestamp, Id),
                 Id
        end,
    {NodeId, Nodes}.

prepare() ->
    T = discovery_nodes,
    mongoose_rdbms:prepare(cets_disco_select, T, [cluster_name], select()),
    mongoose_rdbms:prepare(cets_disco_insert_new, T,
                           [cluster_name, node_name, node_id, timestamp], insert_new()),
    mongoose_rdbms:prepare(cets_disco_update_existing, T,
                           [timestamp, cluster_name, node_name], update_existing()).

select() ->
    <<"SELECT node_name, node_id FROM discovery_nodes WHERE cluster_name = ?">>.
    
select(ClusterName) ->
    mongoose_rdbms:execute_successfully(global, cets_disco_select, [ClusterName]).

insert_new() ->
    <<"INSERT INTO discovery_nodes (cluster_name, node_name, node_id, timestamp)"
      " VALUES (?, ?, ?, ?)">>.

insert_new(ClusterName, Node, Timestamp, Id) ->
    mongoose_rdbms:execute(global, cets_disco_insert_new, [ClusterName, Node, Id, Timestamp]).

update_existing() ->
    <<"UPDATE discovery_nodes SET timestamp = ? WHERE cluster_name = ? AND node_name = ?">>.

update_existing(ClusterName, Node, Timestamp) ->
    mongoose_rdbms:execute(global, cets_disco_update_existing, [Timestamp, ClusterName, Node]).

timestamp() ->
    os:system_time(microsecond).

%% Returns a next free node id based on the currently registered ids
next_id([]) ->
    0;
next_id([H | T = [E | _]]) when ((H + 1) =:= E) ->
    %% Sequential, ignore H
    next_id(T);
next_id([H | _]) ->
    H + 1.
