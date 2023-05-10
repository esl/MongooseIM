%% @doc MongooseIM RDBMS backend for cets_discovery.
-module(mongoose_cets_discovery_rdbms).
-behaviour(cets_discovery).
-export([init/1, get_nodes/1]).

-include_lib("kernel/include/logger.hrl").

-type opts() :: #{}.
-type state() :: opts().

-spec init(opts()) -> state().
init(Opts) ->
    Opts.

-spec get_nodes(state()) -> {cets_discovery:get_nodes_result(), state()}.
get_nodes(State = #{}) ->
    prepare(),
    insert(),
    try mongoose_rdbms:execute_successfully(global, cets_disco_select, []) of
        {selected, Rows} ->
            Nodes = [binary_to_atom(X, latin1) || {X} <- Rows, X =/= <<>>],
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
    Filter = [<<"node_name">>],
    Fields = [<<"updated_timestamp">>],
    rdbms_queries:prepare_upsert(global, cets_disco_insert, discovery_nodes,
                                 Filter ++ Fields, Fields, Filter),
    mongoose_rdbms:prepare(cets_disco_select, discovery_nodes, [node_name],
            <<"SELECT node_name FROM discovery_nodes">>).

insert() ->
    Node = atom_to_binary(node(), latin1),
    Timestamp = os:system_time(microsecond),
    try 
        {updated, _} = rdbms_queries:execute_upsert(global, cets_disco_insert,
                                                    [Node, Timestamp], [Timestamp], [Node])
        catch Class:Reason:Stacktrace ->
                ?LOG_ERROR(#{
                    what => discovery_failed_insert,
                    class => Class,
                    reason => Reason,
                    stacktrace => Stacktrace
                })
    end.
