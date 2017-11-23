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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------

-module(ejabberd_admin).
-author('mickael.remond@process-one.net').

-define(REGISTER_WORKERS_NUM, 10).

-export([start/0, stop/0,
         %% Server
         status/0,
         send_service_message_all_mucs/2,
         %% Accounts
         register/3, unregister/2,
         registered_users/1,
         import_users/1,
         %% Purge DB
         delete_expired_messages/0, delete_old_messages/1,
         %% Mnesia
         set_master/1,
         backup_mnesia/1, restore_mnesia/1,
         dump_mnesia/1, dump_table/2, load_mnesia/1,
         install_fallback_mnesia/1,
         dump_to_textfile/1, dump_to_textfile/2,
         mnesia_change_nodename/4,
         restore/1, % Still used by some modules%%
         get_loglevel/0,
         join_cluster/1, leave_cluster/0,
         remove_from_cluster/1]).

-export([registrator_proc/1]).

-include("ejabberd.hrl").
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
                        result = {users, {list, {username, binary}}}},
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
                        args = [], result = {res, restuple}},
     #ejabberd_commands{name = delete_old_messages, tags = [purge],
                        desc = "Delete offline messages older than DAYS",
                        module = ?MODULE, function = delete_old_messages,
                        args = [{days, integer}], result = {res, restuple}},
     #ejabberd_commands{name = set_master, tags = [mnesia],
                        desc = "Set master node of the clustered Mnesia tables",
                        longdesc = "If you provide as nodename \"self\", this "
                        "node will be set as its own master.",
                        module = ?MODULE, function = set_master,
                        args = [{nodename, string}], result = {res, restuple}},
     #ejabberd_commands{name = mnesia_change_nodename, tags = [mnesia],
                        desc = "Change the erlang node name in a backup file",
                        module = ?MODULE, function = mnesia_change_nodename,
                        args = [{oldnodename, string}, {newnodename, string},
                                {oldbackup, string}, {newbackup, string}],
                        result = {res, restuple}},
     #ejabberd_commands{name = backup, tags = [mnesia],
                        desc = "Store the database to backup file (only Mnesia)",
                        module = ?MODULE, function = backup_mnesia,
                        args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = restore, tags = [mnesia],
                        desc = "Restore the database from backup file (only Mnesia)",
                        module = ?MODULE, function = restore_mnesia,
                        args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = dump, tags = [mnesia],
                        desc = "Dump the database to text file (only Mnesia)",
                        module = ?MODULE, function = dump_mnesia,
                        args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = dump_table, tags = [mnesia],
                        desc = "Dump a table to text file (only Mnesia)",
                        module = ?MODULE, function = dump_table,
                        args = [{file, string}, {table, string}], result = {res, restuple}},
     #ejabberd_commands{name = load, tags = [mnesia],
                        desc = "Restore the database from text file (only Mnesia)",
                        module = ?MODULE, function = load_mnesia,
                        args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = install_fallback, tags = [mnesia],
                        desc = "Install the database from a fallback file (only Mnesia)",
                        module = ?MODULE, function = install_fallback_mnesia,
                        args = [{file, string}], result = {res, restuple}},
     #ejabberd_commands{name = reload_local, tags = [server],
                        desc = "Reload configuration file on the current node",
                        module = ejabberd_config, function = reload_local,
                        args = [], result = {res, restuple}},
     #ejabberd_commands{name = reload_cluster, tags = [server],
                        desc = "Reload configuration file in the cluster",
                        module = ejabberd_config, function = reload_cluster,
                        args = [], result = {res, restuple}},
     #ejabberd_commands{name = join_cluster, tags = [server],
                        desc = "Join the node to a cluster. Call it from the joining node.
                                Use `-f` or `--force` flag to avoid question prompt and force join the node",
                        module = ?MODULE, function = join_cluster,
                        args = [{node, string}],
                        result = {res, restuple}},
     #ejabberd_commands{name = leave_cluster, tags = [server],
                        desc = "Leave a node from the cluster. Call it from the node that is going to leave.
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

-spec join_cluster(string()) -> {ok, string()} | {pang, string()} | {alread_joined, string()} |
                                {mnesia_error, string()} | {error, string()}.
join_cluster(NodeString) ->
    NodeAtom = list_to_atom(NodeString),
    NodeList = mnesia:system_info(db_nodes),
    case lists:member(NodeAtom, NodeList) of
        true ->
            String = io_lib:format("The node ~s has already joined the cluster~n", [NodeString]),
            {alread_joined, String};
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
        E:R ->
            {error, {E, R}}
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
            String = io_lib:format("You have successfully left the node ~p from the cluster~n", [node()]),
            {ok, String}
    catch
        E:R ->
            {error, {E, R}}
    end.


-spec status() -> {'ejabberd_not_running', io_lib:chars()} | {'ok', io_lib:chars()}.
status() ->
    {InternalStatus, ProvidedStatus} = init:get_status(),
    String1 = io_lib:format("The node ~p is ~p. Status: ~p",
                            [node(), InternalStatus, ProvidedStatus]),
    {Is_running, String2} =
        case lists:keysearch(ejabberd, 1, application:which_applications()) of
            false ->
                {ejabberd_not_running, "ejabberd is not running in that node."};
            {value, {_, _, Version}} ->
                {ok, io_lib:format("ejabberd ~s is running in that node", [Version])}
        end,
    {Is_running, String1 ++ String2}.


-spec send_service_message_all_mucs(Subject :: string() | binary(),
                              AnnouncementText :: string() | binary()) -> 'ok'.
send_service_message_all_mucs(Subject, AnnouncementText) ->
    Message = io_lib:format("~s~n~s", [Subject, AnnouncementText]),
    lists:foreach(
      fun(Host) ->
              MUCHost = gen_mod:get_module_opt_subhost(Host, mod_muc, mod_muc:default_host()),
              mod_muc:broadcast_service_message(MUCHost, Message)
      end,
      ?MYHOSTS).

%%%
%%% Account management
%%%

-spec register(User :: ejabberd:user(),
               Host :: ejabberd:server(),
               Password :: binary()) -> {'cannot_register', io_lib:chars()}
                                      | {'exists', io_lib:chars()}
                                      | {'ok', io_lib:chars()}.
register(User, Host, Password) ->
    case ejabberd_auth:try_register(User, Host, Password) of
        {error, exists} ->
            String = io_lib:format("User ~s@~s already registered at node ~p",
                                   [User, Host, node()]),
            {exists, String};
        {error, Reason} ->
            String = io_lib:format("Can't register user ~s@~s at node ~p: ~p",
                                   [User, Host, node(), Reason]),
            {cannot_register, String};
        _ ->
            {ok, io_lib:format("User ~s@~s successfully registered", [User, Host])}
    end.


-spec unregister(User :: ejabberd:user(),
                 Host :: ejabberd:server()) -> {'ok', []}.
unregister(User, Host) ->
    ejabberd_auth:remove_user(User, Host),
    {ok, ""}.

-spec registered_users(Host :: ejabberd:server()) -> [ejabberd:user()].
registered_users(Host) ->
    Users = ejabberd_auth:get_vh_registered_users(Host),
    SUsers = lists:sort(Users),
    lists:map(fun({U, _S}) -> U end, SUsers).

-spec import_users(Filename :: string()) -> [{ok, ejabberd:user()} |
                                             {exists, ejabberd:user()} |
                                             {not_allowed, ejabberd:user()} |
                                             {invalid_jid, ejabberd:user()} |
                                             {null_password, ejabberd:user()} |
                                             {bad_csv, binary()}].
import_users(File) ->
    FileStream = stdio:file(File),
    CsvStream = csv:stream(FileStream),
    Workers = spawn_link_workers(),
    WorkersQueue = queue:from_list(Workers),
    do_import(CsvStream, WorkersQueue).

-spec do_import(CsvStream :: stdio:stream(), Workers :: queue:queue()) ->
    [{ok, ejabberd:user()} |
     {exists, ejabberd:user()} |
     {not_allowed, ejabberd:user()} |
     {invalid_jid, ejabberd:user()} |
     {null_password, ejabberd:user()} |
     {bad_csv, binary()}].
do_import({}, WorkersQueue) ->
    Workers = queue:to_list(WorkersQueue),
    lists:flatmap(fun get_results_from_registrator/1, Workers);


do_import({s, UserData, TailFun}, WorkersQueue) ->
    {{value, Worker}, Q1} = queue:out(WorkersQueue),
    send_job_to_registrator(Worker, UserData),
    Q2 = queue:in(Worker, Q1),
    do_import(TailFun(), Q2).

-spec spawn_link_workers() -> [pid()].
spawn_link_workers() ->
    [ spawn_link(?MODULE, registrator_proc, [self()]) ||
      _ <- lists:seq(1, ?REGISTER_WORKERS_NUM)].

-spec get_results_from_registrator(Worker :: pid()) ->
    [{ok, ejabberd:user()} |
     {exists, ejabberd:user()} |
     {not_allowed, ejabberd:user()} |
     {invalid_jid, ejabberd:user()} |
     {null_password, ejabberd:user()} |
     {bad_csv, binary()}].
get_results_from_registrator(Pid) ->
    Pid ! get_result,
    receive
        {result, Result} -> Result
    end.

send_job_to_registrator(Pid, Data) ->
    Pid ! {proccess, Data}.

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

-spec do_register([binary()]) -> {ok, ejabberd:user()} |
                                 {exists, ejabberd:user()} |
                                 {not_allowed, ejabberd:user()} |
                                 {invalid_jid, ejabberd:user()} |
                                 {null_password, ejabberd:user()} |
                                 {bad_csv, binary()}.
do_register([User, Host, Password]) ->
    case ejabberd_auth:try_register(User, Host, Password) of
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
    BackendList = ejabberd_loglevel:get(),
    F = fun({Backend, Level}) ->
        {Number, Name} = Level,
        io_lib:format("loglevel for ~p is ~p which is '~p'",
                      [Backend, Number, Name])
        end,
    StringList = lists:map(F, BackendList),
    JoinedList = string:join(StringList, "\n"),
    {ok, JoinedList}.

%%%
%%% Purge DB
%%%

-spec delete_expired_messages() -> {ok, iolist()} | {error, iolist()}.
delete_expired_messages() ->
    case mod_offline:remove_expired_messages(?MYNAME) of
        {ok, C} ->
            {ok, io_lib:format("Removed ~p messages", [C])};
        {error, Reason} ->
            {error, io_lib:format("Can't delete expired messages: ~n~p", [Reason])}
    end.

-spec delete_old_messages(Days :: integer()) -> {ok, iolist()} | {error, iolist()}.
delete_old_messages(Days) ->
    case mod_offline:remove_old_messages(?MYNAME, Days) of
        {ok, C} ->
            {ok, io_lib:format("Removed ~p messages", [C])};
        {error, Reason} ->
            {error, io_lib:format("Can't remove old messages: ~n~p", [Reason])}
    end.


%%%
%%% Mnesia management
%%%

-spec set_master(Node :: atom() | string()) -> {'error', io_lib:chars()} | {'ok', []}.
set_master("self") ->
    set_master(node());
set_master(NodeString) when is_list(NodeString) ->
    set_master(list_to_atom(NodeString));
set_master(Node) when is_atom(Node) ->
    case mnesia:set_master_nodes([Node]) of
        ok ->
            {ok, ""};
        {error, Reason} ->
            String = io_lib:format("Can't set master node ~p at node ~p:~n~p",
                                   [Node, node(), Reason]),
            {error, String}
    end.

-spec backup_mnesia(file:name()) -> {'cannot_backup', io_lib:chars()} | {'ok', []}.
backup_mnesia(Path) ->
    case mnesia:backup(Path) of
        ok ->
            {ok, ""};
        {error, Reason} ->
            String = io_lib:format("Can't store backup in ~p at node ~p: ~p",
                                   [filename:absname(Path), node(), Reason]),
            {cannot_backup, String}
    end.

-spec restore_mnesia(file:name()) -> {'cannot_restore', io_lib:chars()}
                                   | {'file_not_found', io_lib:chars()}
                                   | {'ok', []}
                                   | {'table_not_exists', io_lib:chars()}.
restore_mnesia(Path) ->
    ErrorString=lists:flatten( io_lib:format("Can't restore backup from ~p at node ~p: ",
                                             [filename:absname(Path), node()])),
    case ejabberd_admin:restore(Path) of
        {atomic, _} ->
            {ok, ""};
        {aborted, {no_exists, Table}} ->
            String = io_lib:format("~sTable ~p does not exist.", [ErrorString, Table]),
            {table_not_exists, String};
        {aborted, enoent} ->
            String = ErrorString ++ "File not found.",
            {file_not_found, String};
        {aborted, Reason} ->
            String = io_lib:format("~s~p", [ErrorString, Reason]),
            {cannot_restore, String}
    end.

%% @doc Mnesia database restore
%% This function is called from ejabberd_ctl, ejabberd_web_admin and
%% mod_configure/adhoc
restore(Path) ->
    mnesia:restore(Path, [{keep_tables, keep_tables()},
                          {default_op, skip_tables}]).

%% @doc This function return a list of tables that should be kept from a
%% previous version backup.
%% Obsolete tables or tables created by module who are no longer used are not
%% restored and are ignored.
-spec keep_tables() -> [atom()].
keep_tables() ->
    lists:flatten([acl, passwd, config, local_config, disco_publish,
                   keep_modules_tables()]).

%% @doc Returns the list of modules tables in use, according to the list of
%% actually loaded modules
-spec keep_modules_tables() -> [[atom()]]. % list of lists
keep_modules_tables() ->
    lists:map(fun(Module) -> module_tables(Module) end,
              gen_mod:loaded_modules(?MYNAME)).

%% TODO: This mapping should probably be moved to a callback function in each module.
%% @doc Mapping between modules and their tables
-spec module_tables(_) -> [atom()].
module_tables(mod_announce) -> [motd, motd_users];
module_tables(mod_irc) -> [irc_custom];
module_tables(mod_last) -> [last_activity];
module_tables(mod_muc) -> [muc_room, muc_registered];
module_tables(mod_offline) -> [offline_msg];
module_tables(mod_privacy) -> [privacy];
module_tables(mod_private) -> [private_storage];
module_tables(mod_pubsub) -> [pubsub_node];
module_tables(mod_roster) -> [roster];
module_tables(mod_shared_roster) -> [sr_group, sr_user];
module_tables(mod_vcard) -> [vcard, vcard_search];
module_tables(_Other) -> [].

-spec get_local_tables() -> [any()].
get_local_tables() ->
    Tabs1 = lists:delete(schema, mnesia:system_info(local_tables)),
    Tabs = lists:filter(
             fun(T) ->
                     case mnesia:table_info(T, storage_type) of
                         disc_copies -> true;
                         disc_only_copies -> true;
                         _ -> false
                     end
             end, Tabs1),
    Tabs.

-spec dump_mnesia(file:name()) -> {'cannot_dump', io_lib:chars()} | {'ok', []}.
dump_mnesia(Path) ->
    Tabs = get_local_tables(),
    dump_tables(Path, Tabs).

-spec dump_table(file:name(), STable :: string()) ->
                        {'cannot_dump', io_lib:chars()} | {'ok', []}.
dump_table(Path, STable) ->
    Table = list_to_atom(STable),
    dump_tables(Path, [Table]).

-spec dump_tables(file:name(), Tables :: [atom()]) ->
                        {'cannot_dump', io_lib:chars()} | {'ok', []}.
dump_tables(Path, Tables) ->
    case dump_to_textfile(Path, Tables) of
        ok ->
            {ok, ""};
        {error, Reason} ->
            String = io_lib:format("Can't store dump in ~p at node ~p: ~p",
                                   [filename:absname(Path), node(), Reason]),
            {cannot_dump, String}
    end.

-spec dump_to_textfile(file:name()) -> 'ok' | {'error', atom()}.
dump_to_textfile(File) ->
    Tabs = get_local_tables(),
    dump_to_textfile(File, Tabs).

-spec dump_to_textfile(file:name(), Tabs :: list()) -> 'ok' | {'error', atom()}.
dump_to_textfile(File, Tabs) ->
    dump_to_textfile(mnesia:system_info(is_running), Tabs, file:open(File, [write])).

-spec dump_to_textfile(any(),
                      any(),
                      {'error', atom()} | {'ok', pid() | {'file_descriptor', atom() | tuple(), _}}
                      ) -> 'ok' | {'error', atom()}.
dump_to_textfile(yes, Tabs, {ok, F}) ->
    Defs = lists:map(
             fun(T) -> {T, [{record_name, mnesia:table_info(T, record_name)},
                            {attributes, mnesia:table_info(T, attributes)}]}
             end,
             Tabs),
    io:format(F, "~p.~n", [{tables, Defs}]),
    lists:foreach(fun(T) -> dump_tab(F, T) end, Tabs),
    file:close(F);
dump_to_textfile(_, _, {ok, F}) ->
    file:close(F),
    {error, mnesia_not_running};
dump_to_textfile(_, _, {error, Reason}) ->
    {error, Reason}.

-spec dump_tab(pid(), atom()) -> 'ok'.
dump_tab(F, T) ->
    W = mnesia:table_info(T, wild_pattern),
    {atomic, All} = mnesia:transaction(
                     fun() -> mnesia:match_object(T, W, read) end),
    lists:foreach(
      fun(Term) -> io:format(F, "~p.~n", [setelement(1, Term, T)]) end, All).

-spec load_mnesia(file:name()) -> {'cannot_load', io_lib:chars()} | {'ok', []}.
load_mnesia(Path) ->
    case mnesia:load_textfile(Path) of
        {atomic, ok} ->
            {ok, ""};
        {error, Reason} ->
            String = io_lib:format("Can't load dump in ~p at node ~p: ~p",
                                   [filename:absname(Path), node(), Reason]),
            {cannot_load, String}
    end.

-spec install_fallback_mnesia(file:name()) ->
                        {'cannot_fallback', io_lib:chars()} | {'ok', []}.
install_fallback_mnesia(Path) ->
    case mnesia:install_fallback(Path) of
        ok ->
            {ok, ""};
        {error, Reason} ->
            String = io_lib:format("Can't install fallback from ~p at node ~p: ~p",
                                   [filename:absname(Path), node(), Reason]),
            {cannot_fallback, String}
    end.

-spec mnesia_change_nodename(string(), string(), _, _) -> {ok, _} | {error, _}.
mnesia_change_nodename(FromString, ToString, Source, Target) ->
    From = list_to_atom(FromString),
    To = list_to_atom(ToString),
    Switch =
        fun
            (Node) when Node == From ->
                io:format("     - Replacing nodename: '~p' with: '~p'~n", [From, To]),
                To;
            (Node) when Node == To ->
                %% throw({error, already_exists});
                io:format("     - Node: '~p' will not be modified (it is already '~p')~n", [Node, To]),
                Node;
            (Node) ->
                io:format("     - Node: '~p' will not be modified (it is not '~p')~n", [Node, From]),
                Node
        end,
    Convert =
        fun
            ({schema, db_nodes, Nodes}, Acc) ->
                io:format(" +++ db_nodes ~p~n", [Nodes]),
                {[{schema, db_nodes, lists:map(Switch, Nodes)}], Acc};
            ({schema, version, Version}, Acc) ->
                io:format(" +++ version: ~p~n", [Version]),
                {[{schema, version, Version}], Acc};
            ({schema, cookie, Cookie}, Acc) ->
                io:format(" +++ cookie: ~p~n", [Cookie]),
                {[{schema, cookie, Cookie}], Acc};
            ({schema, Tab, CreateList}, Acc) ->
                io:format("~n * Checking table: '~p'~n", [Tab]),
                Keys = [ram_copies, disc_copies, disc_only_copies],
                OptSwitch =
                    fun({Key, Val}) ->
                            case lists:member(Key, Keys) of
                                true ->
                                    io:format("   + Checking key: '~p'~n", [Key]),
                                    {Key, lists:map(Switch, Val)};
                                false-> {Key, Val}
                            end
                    end,
                Res = {[{schema, Tab, lists:map(OptSwitch, CreateList)}], Acc},
                Res;
            (Other, Acc) ->
                {[Other], Acc}
        end,
    mnesia:traverse_backup(Source, Target, Convert, switched).
