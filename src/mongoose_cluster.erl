-module(mongoose_cluster).

%% This is a library module for cluster management: joining / leaving a cluster.

%% TODO: it might make sense to expose this stuff as mod_admin_extra_cluster

-export([join/1, leave/0, remove_from_cluster/1, is_node_alive/1]).

-include("mongoose.hrl").

-dialyzer({[no_match, no_return], set_extra_db_nodes/1}).

%%
%% API
%%

%% @doc Join a cluster designated by ClusterMember.
%% This drops all current connections and discards all persistent
%% data from Mnesia. Use with caution!
%% Next time the node starts, it will connect to other members automatically.
-spec join(node()) -> ok.
join(ClusterMember) ->
    node_trans(fun() -> do_join(ClusterMember) end).

do_join(ClusterMember) ->
    ?INFO_MSG("join ~p", [ClusterMember]),
    with_app_stopped(mongooseim,
                     fun () ->
                             check_networking(ClusterMember),
                             unsafe_join(node(), ClusterMember)
                     end).

%% @doc Leave cluster.
%% This drops all current connections and discards all persistent
%% data from Mnesia. Use with caution!
%% Next time the node starts, it will NOT connect to previous members.
%% Remaining members will remove this node from the cluster Mnesia schema.
-spec leave() -> ok.
leave() ->
    node_trans(fun() -> do_leave() end).

do_leave() ->
    ?INFO_MSG("leave", []),
    with_app_stopped(mongooseim,
                     fun () ->
                             catch mnesia:stop(),
                             detach_nodes(mnesia_nodes()),
                             delete_mnesia(),
                             ok = mnesia:start()
                     end).

%% @doc Remove dead node from the cluster.
%% The removing node must be down
-spec remove_from_cluster(node()) -> ok.
remove_from_cluster(Node) ->
    node_trans(fun() -> do_remove_from_cluster(Node) end).

do_remove_from_cluster(Node) ->
    NodeAlive = is_node_alive(Node),
    NodeAlive andalso error({node_is_alive, Node}),
    remove_dead_from_cluster(Node).

%%
%% Helpers
%%

remove_dead_from_cluster(DeadNode) ->
    ?INFO_MSG("removing dead node ~p from the cluster", [DeadNode]),
    case mnesia:del_table_copy(schema, DeadNode) of
        {atomic, ok} ->
            ok;
        {aborted, R} ->
            error({del_table_copy_schema, R})
    end.

is_node_alive(Node) ->
    try check_networking(Node) of
        true ->
            true
    catch
        error:_ ->
            false
    end.

is_app_running(App) ->
    lists:keymember(App, 1, application:which_applications()).

check_networking(ClusterMember) ->
    ok == wait_for_pong(ClusterMember) orelse error(pang, [ClusterMember]).

unsafe_join(Node, ClusterMember) ->
    delete_mnesia(),
    ok = mnesia:start(),
    set_extra_db_nodes(ClusterMember),
    true = lists:member(ClusterMember, mnesia:system_info(running_db_nodes)),
    ok = change_schema_type(Node),
    Tables = [ {T, table_type(ClusterMember, T)}
               || T <- mnesia:system_info(tables),
                  T /= schema ],
    Copied = [ {Table, mnesia:add_table_copy(T, Node, Type)}
               || {T, Type} = Table <- Tables ],
    lists:foreach(fun check_if_successful_copied/1, Copied),
    ok.

set_extra_db_nodes(ClusterMember) ->
    case mnesia:change_config(extra_db_nodes, [ClusterMember]) of
        {ok, [ClusterMember]} ->
            ok;
        Other ->
            error(#{reason => set_extra_db_nodes_failed,
                    result => Other,
                    cluster_member => ClusterMember})
    end.

check_if_successful_copied(TableEl) ->
    case TableEl of
        {_, {atomic, ok}} ->
            ok;
        {_, {aborted, {already_exists, _, _}}} ->
            ok;
        Other ->
            error({add_table_copy_error, TableEl, Other})
    end.

change_schema_type(Node) ->
    case mnesia:change_table_copy_type(schema, Node, disc_copies) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, _, _, _}} ->
            ok;
        {aborted, R} ->
            {error, R}
    end.

table_type(ClusterMember, T) ->
    try rpc:call(ClusterMember, mnesia, table_info, [T, storage_type]) of
        Type when Type =:= disc_copies;
                  Type =:= ram_copies;
                  Type =:= disc_only_copies -> Type
    catch
        E:R -> error({cant_get_storage_type, {T, E, R}}, [T])
    end.

%% This will remove all your Mnesia data!
%% You've been warned.
delete_mnesia() ->
    catch mnesia:stop(),
    Dir = mnesia:system_info(directory),
    case application:get_env(mnesia, dir, undefined) of
        undefined -> ok;
        Dir ->
            %% Both settings match, OK!
            ok;
        AppEnvDir ->
            ?WARNING_MSG("mnesia:system_info(directory) returned ~p, but application:get_env(mnesia, dir) "
                         "returned ~p: the values are different", [Dir, AppEnvDir]),
            ok
    end,
    ok = rmrf(Dir),
    ?WARNING_MSG("Mnesia schema and files deleted", []),
    ok.

wait_for_pong(Node) ->
    wait_for_pong(net_adm:ping(Node), Node, 5, 100).

wait_for_pong(pong, _Node, _Retries, _Interval) ->
    ok;
wait_for_pong(pang, _Node, 0, _Interval) ->
    timeout;
wait_for_pong(pang, Node, Retries, Interval) ->
    timer:sleep(Interval),
    wait_for_pong(net_adm:ping(Node), Node, Retries - 1, Interval).

rmrf(Dir) ->
    case file:list_dir(Dir) of
        {error, enoent} -> ok;
        {error, enotdir} ->
            ok = file:delete(Dir);
        {ok, Dirs} ->
            [ ok = rmrf(filename:join(Dir, Sub)) || Sub <- Dirs],
            ok
    end.

detach_nodes(Nodes) ->
    Node = node(),
    {_, []} = rpc:multicall(Nodes, mnesia, del_table_copy, [schema, Node]).

mnesia_nodes() ->
    mnesia:system_info(db_nodes) -- [node()].

with_app_stopped(App, F) ->
    Running = is_app_running(App),
    Running andalso application:stop(App),
    try
        F()
    after
        Running andalso application:start(App)
    end.

node_trans(F) ->
    global:trans({{mongoose_cluster_op, node()}, self()}, F).
