-module(mongoose_cluster).

%% This is a library module for cluster management: joining / leaving a cluster.

%% TODO: it might make sense to expose this stuff as mod_admin_extra_cluster

-export([join/1,
         leave/0]).

-include("ejabberd.hrl").

%%
%% API
%%

%% @doc Join a cluster designated by ClusterMember.
%% This drops all current connections and discards all persistent
%% data from Mnesia. Use with caution!
%% Next time the node starts, it will connect to other members automatically.
%% TODO: when/if exposing through ejabberd_admin make sure it's guarded
%%       by an interactive yes/no question or some flag
-spec join(node()) -> ok.
join(ClusterMember) ->
    ?INFO_MSG("join ~p", [ClusterMember]),
    with_app_stopped(ejabberd,
                     fun () ->
                             check_networking(ClusterMember),
                             unsafe_join(node(), ClusterMember)
                     end).

%% @doc Leave cluster.
%% This drops all current connections and discards all persistent
%% data from Mnesia. Use with caution!
%% Next time the node starts, it will NOT connect to previous members.
%% Remaining members will remove this node from the cluster Mnesia schema.
%% TODO: when/if exposing through ejabberd_admin make sure it's guarded
%%       by an interactive yes/no question or some flag
-spec leave() -> ok.
leave() ->
    ?INFO_MSG("leave", []),
    with_app_stopped(ejabberd,
                     fun () ->
                             catch mnesia:stop(),
                             detach_nodes(mnesia_nodes()),
                             delete_mnesia(),
                             ok = mnesia:start()
                     end).

%%
%% Helpers
%%

is_app_running(App) ->
    lists:keymember(App, 1, application:which_applications()).

check_networking(ClusterMember) ->
    ok == wait_for_pong(ClusterMember) orelse error(pang, [ClusterMember]).

unsafe_join(Node, ClusterMember) ->
    delete_mnesia(),
    ok = mnesia:start(),
    {ok, [ClusterMember]} = mnesia:change_config(extra_db_nodes, [ClusterMember]),
    true = lists:member(ClusterMember, mnesia:system_info(running_db_nodes)),
    {atomic, ok} = mnesia:change_table_copy_type(schema, Node, disc_copies),
    Tables = [ {T, table_type(ClusterMember, T)}
               || T <- mnesia:system_info(tables),
                  T /= schema ],
    Copied = [ {Table, mnesia:add_table_copy(T, Node, Type)}
               || {T, Type} = Table <- Tables ],
    Expected = lists:zip(Tables, repeat(length(Tables), {atomic, ok})),
    Expected = Copied,
    ok.

table_type(ClusterMember, T) ->
    try rpc:call(ClusterMember, mnesia, table_info, [T, storage_type]) of
        Type when Type =:= disc_copies;
                  Type =:= ram_copies;
                  Type =:= disc_only_copies -> Type
    catch
        E:R -> error({cant_get_storage_type, {E,R}}, [T])
    end.

%% This will remove all your Mnesia data!
%% You've been warned.
delete_mnesia() ->
    catch mnesia:stop(),
    catch mnesia:delete_schema(),
    Dir = mnesia:system_info(directory),
    case application:get_env(mnesia, dir, undefined) of
        undefined -> ok;
        Dir ->
            %% Both settings match, OK!
            ok;
        _NotDir ->
            error(mnesia_dir_inconsistent)
    end,
    ok = rmrf(Dir),
    ?WARNING_MSG("Mnesia schema and files deleted", []),
    ok.

repeat(N, El) ->
    [ El || _ <- lists:seq(1, N) ].

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
            ok = file:del_dir(Dir),
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
