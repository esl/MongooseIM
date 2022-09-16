-module(mongoose_server_api).

-export([get_loglevel/0, status/0, get_cookie/0, join_cluster/1, leave_cluster/0,
         remove_from_cluster/1, stop/0, restart/0, remove_node/1, set_loglevel/1,
         graphql_get_loglevel/0]).

-ignore_xref([get_loglevel/0]).

-spec get_loglevel() -> {ok, string()}.
get_loglevel() ->
    Level = mongoose_logs:get_global_loglevel(),
    Number = mongoose_logs:loglevel_keyword_to_number(Level),
    String = io_lib:format("global loglevel is ~p, which means '~p'", [Number, Level]),
    {ok, String}.

-spec graphql_get_loglevel() -> {ok, mongoose_logs:atom_log_level()}.
graphql_get_loglevel() ->
    {ok, mongoose_logs:get_global_loglevel()}.

-spec set_loglevel(mongoose_logs:atom_log_level()) ->
    {ok, string()} | {invalid_level, string()}.
set_loglevel(Level) ->
    case mongoose_logs:set_global_loglevel(Level) of
        ok ->
            {ok, "Log level successfully set."};
        {error, _} ->
            {invalid_level, io_lib:format("Log level ~p does not exist.", [Level])}
    end.

-spec status() -> {'mongooseim_not_running', string()} | {'ok', string()}.
status() ->
    {InternalStatus, ProvidedStatus} = init:get_status(),
    String1 = io_lib:format("The node ~p is ~p. Status: ~p.",
                            [node(), InternalStatus, ProvidedStatus]),
    case lists:keysearch(mongooseim, 1, application:which_applications()) of
        false ->
            {mongooseim_not_running, String1 ++ " MongooseIM is not running in that node."};
        {value, {_, _, Version}} ->
            {ok, String1 ++ io_lib:format(" MongooseIM ~s is running in that node.", [Version])}
    end.

-spec get_cookie() -> string().
get_cookie() ->
    atom_to_list(erlang:get_cookie()).

-spec join_cluster(string()) -> {ok, string()} | {pang, string()} | {already_joined, string()} |
                                {mnesia_error, string()} | {error, any()}.
join_cluster(NodeString) ->
    NodeAtom = list_to_atom(NodeString),
    NodeList = mnesia:system_info(db_nodes),
    case lists:member(NodeAtom, NodeList) of
        true ->
            String = io_lib:format(
                "The MongooseIM node ~s has already joined the cluster~n.", [NodeString]),
            {already_joined, String};
        _ ->
            do_join_cluster(NodeAtom)
    end.

do_join_cluster(Node) ->
    try mongoose_cluster:join(Node) of
        ok ->
            String = io_lib:format("You have successfully added the MongooseIM node"
                ++ " ~p to the cluster with node member ~p~n.", [node(), Node]),
            {ok, String}
    catch
        error:pang ->
            String = io_lib:format(
                "Timeout while attempting to connect to a MongooseIM node ~s~n", [Node]),
            {pang, String};
        error:{cant_get_storage_type, {T, E, R}} ->
            String =
                io_lib:format("Cannot get storage type for table ~p~n. Reason: ~p:~p", [T, E, R]),
            {mnesia_error, String};
        E:R:S ->
            {error, {E, R, S}}
    end.

-spec leave_cluster() -> {ok, string()} | {error, term()} | {not_in_cluster, string()}.
leave_cluster() ->
    NodeList = mnesia:system_info(running_db_nodes),
    ThisNode = node(),
    case NodeList of
        [ThisNode] ->
            String = io_lib:format("The MongooseIM node ~p is not in the cluster~n", [node()]),
            {not_in_cluster, String};
        _ ->
            do_leave_cluster()
    end.

do_leave_cluster() ->
    try mongoose_cluster:leave() of
        ok ->
            String = io_lib:format(
                "The MongooseIM node ~p has successfully left the cluster~n", [node()]),
            {ok, String}
    catch
        E:R ->
            {error, {E, R}}
    end.

-spec remove_from_cluster(string()) -> {ok, string()} |
                                       {node_is_alive, string()} |
                                       {mnesia_error, string()} |
                                       {rpc_error, string()}.
remove_from_cluster(NodeString) ->
    Node = list_to_atom(NodeString),
    IsNodeAlive = mongoose_cluster:is_node_alive(Node),
    case IsNodeAlive of
        true ->
            remove_rpc_alive_node(Node);
        false ->
            remove_dead_node(Node)
    end.

remove_dead_node(DeadNode) ->
    try mongoose_cluster:remove_from_cluster(DeadNode) of
        ok ->
            String =
                io_lib:format(
                    "The dead MongooseIM node ~p has been removed from the cluster~n", [DeadNode]),
            {ok, String}
    catch
        error:{node_is_alive, DeadNode} ->
            String = io_lib:format(
                "The MongooseIM node ~p is alive but shoud not be.~n", [DeadNode]),
            {node_is_alive, String};
        error:{del_table_copy_schema, R} ->
            String = io_lib:format("Cannot delete table schema~n. Reason: ~p", [R]),
            {mnesia_error, String}
    end.

remove_rpc_alive_node(AliveNode) ->
    case rpc:call(AliveNode, mongoose_cluster, leave, []) of
        {badrpc, Reason} ->
            String =
                io_lib:format(
                    "Cannot remove the MongooseIM node ~p~n. RPC Reason: ~p", [AliveNode, Reason]),
            {rpc_error, String};
        ok ->
            String = io_lib:format(
                "The MongooseIM node ~p has been removed from the cluster~n", [AliveNode]),
            {ok, String};
        Unknown ->
            String = io_lib:format("Unknown error: ~p~n", [Unknown]),
            {rpc_error, String}
    end.

-spec stop() -> ok.
stop() ->
    timer:sleep(500),
    init:stop().

-spec restart() -> ok.
restart() ->
    timer:sleep(500),
    init:restart().

-spec remove_node(string()) -> {ok, string()}.
remove_node(Node) ->
    mnesia:del_table_copy(schema, list_to_atom(Node)),
    {ok, "MongooseIM node removed from the Mnesia schema"}.
