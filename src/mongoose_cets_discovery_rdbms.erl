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
    prepare(),
    insert(ClusterName, Node),
    try mongoose_rdbms:execute_successfully(global, cets_disco_select, [ClusterName]) of
        {selected, Rows} ->
            Nodes = [binary_to_atom(X) || {X} <- Rows, X =/= <<>>],
            {{ok, Nodes}, State}
        catch Class:Reason:Stacktrace ->
                ?LOG_ERROR(#{
                    what => discovery_failed_select,
                    class => Class,
                    reason => Reason,
                    stacktrace => Stacktrace
                }),
                {{error, Reason}, State}
    end.

prepare() ->
    Filter = [<<"node_name">>, <<"cluster_name">>],
    Fields = [<<"updated_timestamp">>],
    rdbms_queries:prepare_upsert(global, cets_disco_insert, discovery_nodes,
                                 Filter ++ Fields, Fields, Filter),
    mongoose_rdbms:prepare(cets_disco_select, discovery_nodes, [cluster_name],
            <<"SELECT node_name FROM discovery_nodes WHERE cluster_name = ?">>).

insert(ClusterName, Node) ->
    Timestamp = os:system_time(microsecond),
    Filter = [Node, ClusterName],
    Fields = [Timestamp],
    try 
        {updated, _} = rdbms_queries:execute_upsert(global, cets_disco_insert,
                                                    Filter ++ Fields, Fields,
                                                    Filter)
        catch Class:Reason:Stacktrace ->
                ?LOG_ERROR(#{
                    what => discovery_failed_insert,
                    class => Class,
                    reason => Reason,
                    stacktrace => Stacktrace
                })
    end.
