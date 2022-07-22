%%%-------------------------------------------------------------------
%%% File    : ejabberd_admin.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : Administrative functions and commands
%%% Created :  7 May 2006 by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%-------------------------------------------------------------------

-module(ejabberd_admin).
-author('mickael.remond@process-one.net').

-define(REGISTER_WORKERS_NUM, 10).

-export([start/0, stop/0,
         %% Server
         status/0,
         %% Accounts
         register/3, register/2, unregister/2,
         registered_users/1,
         import_users/1,
         %% Purge DB
         delete_expired_messages/1, delete_old_messages/2,
         get_loglevel/0,
         join_cluster/1, leave_cluster/0,
         remove_from_cluster/1]).

-export([registrator_proc/1]).

-ignore_xref([
    backup_mnesia/1, delete_expired_messages/1, delete_old_messages/2,
    dump_mnesia/1, dump_table/2, dump_to_textfile/1, dump_to_textfile/2,
    get_loglevel/0, import_users/1, install_fallback_mnesia/1,
    join_cluster/1, leave_cluster/0, load_mnesia/1, mnesia_change_nodename/4,
    register/2, register/3, registered_users/1, remove_from_cluster/1,
    restore_mnesia/1, set_master/1, status/0,
    stop/0, unregister/2]).

-include("mongoose.hrl").
-include("ejabberd_commands.hrl").

start() ->
    ejabberd_commands:register_commands(commands()).

stop() ->
    ejabberd_commands:unregister_commands(commands()).

%%%
%%% ejabberd commands
%%%

-spec commands() -> [ejabberd_commands:cmd()].
commands() ->
    [
     %% The commands status, stop and restart are implemented also in ejabberd_ctl
     %% They are defined here so that other interfaces can use them too
     #ejabberd_commands{name = status, tags = [server],
                        desc = "Get status of the ejabberd server",
                        module = ?MODULE, function = status,
                        args = [], result = {res, restuple}},
     #ejabberd_commands{name = restart, tags = [server],
                        desc = "Restart ejabberd gracefully",
                        module = init, function = restart,
                        args = [], result = {res, rescode}},
     #ejabberd_commands{name = get_loglevel, tags = [logs, server],
                        desc = "Get the current loglevel",
                        module = ?MODULE, function = get_loglevel,
                        args = [],
                        result = {res, restuple}},
     #ejabberd_commands{name = register, tags = [accounts],
                        desc = "Register a user",
                        module = ?MODULE, function = register,
                        args = [{host, binary}, {password, binary}],
                        result = {res, restuple}},
     #ejabberd_commands{name = register_identified, tags = [accounts],
                        desc = "Register a user with a specific jid",
                        module = ?MODULE, function = register,
                        args = [{user, binary}, {host, binary}, {password, binary}],
                        result = {res, restuple}},
     #ejabberd_commands{name = unregister, tags = [accounts],
                        desc = "Unregister a user",
                        module = ?MODULE, function = unregister,
                        args = [{user, binary}, {host, binary}],
                        result = {res, restuple}},
     #ejabberd_commands{name = registered_users, tags = [accounts],
                        desc = "List all registered users in HOST",
                        module = ?MODULE, function = registered_users,
                        args = [{host, binary}],
                        result = {users, {list, {user_jid, binary}}}},
     #ejabberd_commands{name = import_users, tags = [accounts],
                        desc = "Import users from CSV file",
                        module = ?MODULE, function = import_users,
                        args = [{file, string}],
                        result = {users, {list, {res, {tuple,
                                                       [{result, atom},
                                                        {user, binary}]}}}}},
     #ejabberd_commands{name = delete_expired_messages, tags = [purge],
                        desc = "Delete expired offline messages from database",
                        module = ?MODULE, function = delete_expired_messages,
                        args = [{host, binary}], result = {res, restuple}},
     #ejabberd_commands{name = delete_old_messages, tags = [purge],
                        desc = "Delete offline messages older than DAYS",
                        module = ?MODULE, function = delete_old_messages,
                        args = [{host, binary}, {days, integer}], result = {res, restuple}},

     #ejabberd_commands{name = set_master, tags = [mnesia],
                        desc = "Set master node of the clustered Mnesia tables",
                        longdesc = "If you provide as nodename \"self\", this "
                        "node will be set as its own master.",
                        module = mnesia_api, function = set_master,
                        args = [{nodename, string}], result = {res, restuple}},
     #ejabberd_commands{name = mnesia_change_nodename, tags = [mnesia],
                        desc = "Change the erlang node name in a backup file",
                        module = mnesia_api, function = mnesia_change_nodename,
                        args = [{oldnodename, string}, {newnodename, string},
                                {oldbackup, string}, {newbackup, string}],
                        result = {res, restuple}},
     #ejabberd_commands{name = backup, tags = [mnesia],
                        desc = "Store the database to backup file (only Mnesia)",
                        module = mnesia_api, function = backup_mnesia,
                        args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = restore, tags = [mnesia],
                        desc = "Restore the database from backup file (only Mnesia)",
                        module = mnesia_api, function = restore_mnesia,
                        args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = dump, tags = [mnesia],
                        desc = "Dump the database to text file (only Mnesia)",
                        module = mnesia_api, function = dump_mnesia,
                        args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = dump_table, tags = [mnesia],
                        desc = "Dump a table to text file (only Mnesia)",
                        module = mnesia_api, function = dump_table,
                        args = [{file, string}, {table, string}], result = {res, restuple}},
     #ejabberd_commands{name = load, tags = [mnesia],
                        desc = "Restore the database from text file (only Mnesia)",
                        module = mnesia_api, function = load_mnesia,
                        args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = install_fallback, tags = [mnesia],
                        desc = "Install the database from a fallback file (only Mnesia)",
                        module = mnesia_api, function = install_fallback_mnesia,
                        args = [{file, string}], result = {res, restuple}},

     #ejabberd_commands{name = join_cluster, tags = [server],
                        desc = "Join the node to a cluster. Call it from the joining node.
                                Use `-f` or `--force` flag to avoid question prompt and force join the node",
                        module = ?MODULE, function = join_cluster,
                        args = [{node, string}],
                        result = {res, restuple}},
     #ejabberd_commands{name = leave_cluster, tags = [server],
                        desc = "Leave a cluster. Call it from the node that is going to leave.
                                Use `-f` or `--force` flag to avoid question prompt and force leave the node from cluster",
                        module = ?MODULE, function = leave_cluster,
                        args = [],
                        result = {res, restuple}},
     #ejabberd_commands{name = remove_from_cluster, tags = [server],
                        desc = "Remove dead node from the cluster. Call it from the member of the cluster.
                                Use `-f` or `--force` flag to avoid question prompt and force remove the node",
                        module = ?MODULE, function = remove_from_cluster,
                        args = [{node, string}],
                        result = {res, restuple}}
    ].


%%%
%%% Server management
%%%
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
            String = io_lib:format("The dead node ~p has been removed from the cluster~n", [DeadNode]),
            {ok, String}
    catch
        error:{node_is_alive, DeadNode} ->
            String = io_lib:format("The node ~p is alive but shoud not be.~n", [DeadNode]),
            {node_is_alive, String};
        error:{del_table_copy_schema, R} ->
            String = io_lib:format("Cannot delete table schema~n. Reason: ~p", [R]),
            {mnesia_error, String}
    end.

remove_rpc_alive_node(AliveNode) ->
    case rpc:call(AliveNode, mongoose_cluster, leave, []) of
        {badrpc, Reason} ->
            String = io_lib:format("Cannot remove the node ~p~n. RPC Reason: ~p", [AliveNode, Reason]),
            {rpc_error, String};
        ok ->
            String = io_lib:format("The node ~p has been removed from the cluster~n", [AliveNode]),
            {ok, String};
        Unknown ->
            String = io_lib:format("Unknown error: ~p~n", [Unknown]),
            {rpc_error, String}
    end.

-spec join_cluster(string()) -> {ok, string()} | {pang, string()} | {already_joined, string()} |
                                {mnesia_error, string()} | {error, string()}.
join_cluster(NodeString) ->
    NodeAtom = list_to_atom(NodeString),
    NodeList = mnesia:system_info(db_nodes),
    case lists:member(NodeAtom, NodeList) of
        true ->
            String = io_lib:format("The node ~s has already joined the cluster~n", [NodeString]),
            {already_joined, String};
        _ ->
            do_join_cluster(NodeAtom)
    end.

do_join_cluster(Node) ->
    try mongoose_cluster:join(Node) of
        ok ->
            String = io_lib:format("You have successfully joined the node ~p to the cluster with node member ~p~n", [node(), Node]),
            {ok, String}
    catch
        error:pang ->
            String = io_lib:format("Timeout while attempting to connect to node ~s~n", [Node]),
            {pang, String};
        error:{cant_get_storage_type, {T, E, R}} ->
            String = io_lib:format("Cannot get storage type for table ~p~n. Reason: ~p:~p", [T, E, R]),
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
            String = io_lib:format("The node ~p is not in the cluster~n", [node()]),
            {not_in_cluster, String};
        _ ->
            do_leave_cluster()
    end.

do_leave_cluster() ->
    try mongoose_cluster:leave() of
        ok ->
            String = io_lib:format("The node ~p has successfully left the cluster~n", [node()]),
            {ok, String}
    catch
        E:R ->
            {error, {E, R}}
    end.

-spec status() -> {'mongooseim_not_running', io_lib:chars()} | {'ok', io_lib:chars()}.
status() ->
    {InternalStatus, ProvidedStatus} = init:get_status(),
    String1 = io_lib:format("The node ~p is ~p. Status: ~p",
                            [node(), InternalStatus, ProvidedStatus]),
    {Is_running, String2} =
        case lists:keysearch(mongooseim, 1, application:which_applications()) of
            false ->
                {mongooseim_not_running, "mongooseim is not running in that node."};
            {value, {_, _, Version}} ->
                {ok, io_lib:format("mongooseim ~s is running in that node", [Version])}
        end,
    {Is_running, String1 ++ String2}.

%%%
%%% Account management
%%%

-spec register(Host :: jid:server(),
               Password :: binary()) -> mongoose_account_api:register_result().
register(Host, Password) ->
    {Result, _} = mongoose_account_api:register_generated_user(Host, Password),
    Result.

-spec register(User :: jid:user(),
               Host :: jid:server(),
               Password :: binary()) -> mongoose_account_api:register_result().
register(User, Host, Password) ->
    mongoose_account_api:register_user(User, Host, Password).

-spec unregister(User :: jid:user(),
                 Host :: jid:server()) -> mongoose_account_api:unregister_result().
unregister(User, Host) ->
    mongoose_account_api:unregister_user(User, Host).


-spec registered_users(Host :: jid:server()) -> [binary()].
registered_users(Host) ->
    mongoose_account_api:list_users(Host).

-spec import_users(Filename :: string()) -> [{ok, jid:user()} |
                                             {exists, jid:user()} |
                                             {not_allowed, jid:user()} |
                                             {invalid_jid, jid:user()} |
                                             {null_password, jid:user()} |
                                             {bad_csv, binary()}].
import_users(File) ->
    {ok, CsvStream} = erl_csv:decode_new_s(File),
    Workers = spawn_link_workers(),
    WorkersQueue = queue:from_list(Workers),
    do_import(CsvStream, WorkersQueue).

-spec do_import(erl_csv:csv_stream(), Workers :: queue:queue()) ->
    [{ok, jid:user()} |
     {exists, jid:user()} |
     {not_allowed, jid:user()} |
     {invalid_jid, jid:user()} |
     {null_password, jid:user()} |
     {bad_csv, binary()}].
do_import(stream_end, WQueue) ->
    Workers = queue:to_list(WQueue),
    lists:flatmap(fun get_results_from_registrator/1, Workers);
do_import(Stream, WQueue) ->
    {ok, Decoded, MoreStream} = erl_csv:decode_s(Stream),
    WQueue1 = send_job_to_next_worker(Decoded, WQueue),
    do_import(MoreStream, WQueue1).

-spec spawn_link_workers() -> [pid()].
spawn_link_workers() ->
    [ spawn_link(?MODULE, registrator_proc, [self()]) ||
      _ <- lists:seq(1, ?REGISTER_WORKERS_NUM)].

-spec get_results_from_registrator(Worker :: pid()) ->
    [{ok, jid:user()} |
     {exists, jid:user()} |
     {not_allowed, jid:user()} |
     {invalid_jid, jid:user()} |
     {null_password, jid:user()} |
     {bad_csv, binary()}].
get_results_from_registrator(Pid) ->
    Pid ! get_result,
    receive
        {result, Result} -> Result
    end.

send_job_to_next_worker([], WQueue) ->
    WQueue;
send_job_to_next_worker([Record], WQueue) ->
    {{value, Worker}, Q1} = queue:out(WQueue),
    Worker ! {proccess, Record},
    queue:in(Worker, Q1).

-spec registrator_proc(Manager :: pid()) -> ok.
registrator_proc(Manager) ->
    registrator_proc(Manager, []).

-spec registrator_proc(Manager :: pid(), any()) -> ok.
registrator_proc(Manager, Result) ->
    receive
        {proccess, Data} ->
            RegisterResult = do_register(Data),
            registrator_proc(Manager, [RegisterResult | Result]);
        get_result -> Manager ! {result, Result}
    end,
    ok.

-spec do_register([binary()]) -> {ok, jid:user()} |
                                 {exists, jid:user()} |
                                 {not_allowed, jid:user()} |
                                 {invalid_jid, jid:user()} |
                                 {null_password, jid:user()} |
                                 {bad_csv, binary()}.
do_register([User, Host, Password]) ->
    case ejabberd_auth:try_register(jid:make(User, Host, <<>>), Password) of
        {error, Reason} -> {Reason, User};
        _ -> {ok, User}
    end;

do_register(List) ->
    JoinBinary = fun(Elem, <<"">>) -> Elem;
                    (Elem, Acc) -> <<Elem/binary, ",", Acc/binary>>
                 end,
    Info = lists:foldr(JoinBinary, <<"">>, List),
    {bad_csv, Info}.

get_loglevel() ->
    Level = mongoose_logs:get_global_loglevel(),
    Number = mongoose_logs:loglevel_keyword_to_number(Level),
    String = io_lib:format("global loglevel is ~p, which means '~p'", [Number, Level]),
    {ok, String}.

%%%
%%% Purge DB
%%%

-spec delete_expired_messages(jid:lserver()) -> {ok, iolist()} | {error, iolist()}.
delete_expired_messages(Domain) ->
    case mod_offline_api:delete_expired_messages(Domain) of
        {ok, _} = Result -> Result;
        {_, Message} -> {error, Message}
    end.

-spec delete_old_messages(jid:lserver(), Days :: integer()) -> {ok, iolist()} | {error, iolist()}.
delete_old_messages(Domain, Days) ->
    case mod_offline_api:delete_old_messages(Domain, Days) of
        {ok, _} = Result -> Result;
        {_, Message} -> {error, Message}
    end.
