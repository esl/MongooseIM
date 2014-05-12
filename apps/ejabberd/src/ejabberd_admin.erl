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

-export([start/0, stop/0,
         %% Server
         status/0, reopen_log/0,
         stop_kindly/2, send_service_message_all_mucs/2,
         %% Erlang
         update_list/0, update/1,
         %% Accounts
         register/3, unregister/2,
         registered_users/1,
         %% Migration jabberd1.4
         import_file/1, import_dir/1,
         %% Purge DB
         delete_expired_messages/0, delete_old_messages/1,
         %% Mnesia
         set_master/1,
         backup_mnesia/1, restore_mnesia/1,
         dump_mnesia/1, dump_table/2, load_mnesia/1,
         install_fallback_mnesia/1,
         dump_to_textfile/1, dump_to_textfile/2,
         mnesia_change_nodename/4,
         restore/1 % Still used by some modules
        ]).

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
     #ejabberd_commands{name = stop, tags = [server],
                        desc = "Stop ejabberd gracefully",
                        module = init, function = stop,
                        args = [], result = {res, rescode}},
     #ejabberd_commands{name = restart, tags = [server],
                        desc = "Restart ejabberd gracefully",
                        module = init, function = restart,
                        args = [], result = {res, rescode}},
%     #ejabberd_commands{name = reopen_log, tags = [logs, server],
%                       desc = "Reopen the log files",
%                       module = ?MODULE, function = reopen_log,
%                       args = [], result = {res, rescode}},
%     #ejabberd_commands{name = stop_kindly, tags = [server],
%                       desc = "Inform users and rooms, wait, and stop the server",
%                       module = ?MODULE, function = stop_kindly,
%                       args = [{delay, integer}, {announcement, string}],
%                       result = {res, rescode}},
     #ejabberd_commands{name = get_loglevel, tags = [logs, server],
                        desc = "Get the current loglevel",
                        module = ejabberd_loglevel, function = get,
                        args = [],
                        result = {leveltuple, {tuple, [{levelnumber, integer},
                                                       {levelatom, atom},
                                                       {leveldesc, string}
                                                      ]}}},

%     #ejabberd_commands{name = update_list, tags = [server],
%                       desc = "List modified modules that can be updated",
%                       module = ?MODULE, function = update_list,
%                       args = [],
%                       result = {modules, {list, {module, string}}}},
%     #ejabberd_commands{name = update, tags = [server],
%                       desc = "Update the given module, or use the keyword: all",
%                       module = ?MODULE, function = update,
%                       args = [{module, string}],
%                       result = {res, restuple}},

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

%     #ejabberd_commands{name = import_file, tags = [mnesia],
%                       desc = "Import user data from jabberd14 spool file",
%                       module = ?MODULE, function = import_file,
%                       args = [{file, string}], result = {res, restuple}},
%     #ejabberd_commands{name = import_dir, tags = [mnesia],
%                       desc = "Import users data from jabberd14 spool dir",
%                       module = ?MODULE, function = import_dir,
%                       args = [{file, string}],
%                       result = {res, restuple}},

%     #ejabberd_commands{name = import_piefxis, tags = [mnesia],
%                       desc = "Import users data from a PIEFXIS file (XEP-0227)",
%                       module = ejabberd_piefxis, function = import_file,
%                       args = [{file, string}], result = {res, rescode}},
%     #ejabberd_commands{name = export_piefxis, tags = [mnesia],
%                       desc = "Export data of all users in the server to PIEFXIS files (XEP-0227)",
%                       module = ejabberd_piefxis, function = export_server,
%                       args = [{dir, string}], result = {res, rescode}},
%     #ejabberd_commands{name = export_piefxis_host, tags = [mnesia],
%                       desc = "Export data of users in a host to PIEFXIS files (XEP-0227)",
%                       module = ejabberd_piefxis, function = export_host,
%                       args = [{dir, string}, {host, string}], result = {res, rescode}},

     #ejabberd_commands{name = delete_expired_messages, tags = [purge],
                        desc = "Delete expired offline messages from database",
                        module = ?MODULE, function = delete_expired_messages,
                        args = [], result = {res, rescode}},
     #ejabberd_commands{name = delete_old_messages, tags = [purge],
                        desc = "Delete offline messages older than DAYS",
                        module = ?MODULE, function = delete_old_messages,
                        args = [{days, integer}], result = {res, rescode}},

%     #ejabberd_commands{name = rename_default_nodeplugin, tags = [mnesia],
%                       desc = "Update PubSub table from old ejabberd trunk SVN to 2.1.0",
%                       module = mod_pubsub, function = rename_default_nodeplugin,
%                       args = [], result = {res, rescode}},

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
                        args = [{file, string}], result = {res, restuple}}
    ].


%%%
%%% Server management
%%%

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

-spec reopen_log() -> ok.
reopen_log() ->
    ejabberd_hooks:run(reopen_log_hook, []),
    %% TODO: Use the Reopen log API for logger_h ?
    ejabberd_logger_h:reopen_log(),
    case application:get_env(sasl,sasl_error_logger) of
        {ok, {file, SASLfile}} ->
            error_logger:delete_report_handler(sasl_report_file_h),
            ejabberd_logger_h:rotate_log(SASLfile),
            error_logger:add_report_handler(sasl_report_file_h,
                {SASLfile, get_sasl_error_logger_type()});
        _ -> false
        end,
    ok.

%% @doc Function copied from Erlang/OTP lib/sasl/src/sasl.erl which doesn't export it
-spec get_sasl_error_logger_type() -> 'all' | 'error' | 'progress'.
get_sasl_error_logger_type () ->
    case application:get_env (sasl, errlog_type) of
        {ok, error} -> error;
        {ok, progress} -> progress;
        {ok, all} -> all;
        {ok, Bad} -> exit ({bad_config, {sasl, {errlog_type, Bad}}});
        _ -> all
    end.

%%%
%%% Stop Kindly
%%%

-spec stop_kindly(DelaySeconds :: number(),
                  AnnouncementText :: string()) -> 'ok'.
stop_kindly(DelaySeconds, AnnouncementText) ->
    Subject = io_lib:format("Server stop in ~p seconds!", [DelaySeconds]),
    WaitingDesc = io_lib:format("Waiting ~p seconds", [DelaySeconds]),
    Steps = [
             {"Stopping ejabberd port listeners",
              ejabberd_listener, stop_listeners, []},
             {"Sending announcement to connected users",
              mod_announce, send_announcement_to_all,
              [?MYNAME, Subject, AnnouncementText]},
             {"Sending service message to MUC rooms",
              ejabberd_admin, send_service_message_all_mucs,
              [Subject, AnnouncementText]},
             {WaitingDesc, timer, sleep, [DelaySeconds * 1000]},
             {"Stopping ejabberd", application, stop, [ejabberd]},
             {"Stopping Mnesia", mnesia, stop, []},
             {"Stopping Erlang node", init, stop, []}
    ],
    NumberLast = length(Steps),
    TimestampStart = calendar:datetime_to_gregorian_seconds({date(), time()}),
    lists:foldl(
      fun({Desc, Mod, Func, Args}, NumberThis) ->
              SecondsDiff =
                  calendar:datetime_to_gregorian_seconds({date(), time()})
                  - TimestampStart,
              io:format("[~p/~p ~ps] ~s... ",
                        [NumberThis, NumberLast, SecondsDiff, Desc]),
              Result = apply(Mod, Func, Args),
              io:format("~p~n", [Result]),
              NumberThis+1
      end,
      1,
      Steps),
    ok.

-spec send_service_message_all_mucs(Subject :: string() | binary(),
                              AnnouncementText :: string() | binary()) -> 'ok'.
send_service_message_all_mucs(Subject, AnnouncementText) ->
    Message = io_lib:format("~s~n~s", [Subject, AnnouncementText]),
    lists:foreach(
      fun(ServerHost) ->
              MUCHost = gen_mod:get_module_opt_host(
                          ServerHost, mod_muc, "conference.@HOST@"),
              mod_muc:broadcast_service_message(MUCHost, Message)
      end,
      ?MYHOSTS).

%%%
%%% ejabberd_update
%%%

-spec update_list() -> [string()].
update_list() ->
    {ok, _Dir, UpdatedBeams, _Script, _LowLevelScript, _Check} =
        ejabberd_update:update_info(),
    [atom_to_list(Beam) || Beam <- UpdatedBeams].

-spec update(string()) -> [{'error',_} | {'ok',[any()]}]
                        | {'error',_} | {'ok',io_lib:chars()}.
update("all") ->
    [update_module(ModStr) || ModStr <- update_list()];
update(ModStr) ->
    update_module(ModStr).

-spec update_module(string()) -> {'error',_} | {'ok', io_lib:chars()}.
update_module(ModuleNameString) ->
    ModuleName = list_to_atom(ModuleNameString),
    case ejabberd_update:update([ModuleName]) of
          {ok, Res} -> {ok, io_lib:format("Updated: ~p", [Res])};
          {error, Reason} -> {error, Reason}
    end.

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
        {atomic, ok} ->
            {ok, io_lib:format("User ~s@~s successfully registered", [User, Host])};
        {atomic, exists} ->
            String = io_lib:format("User ~s@~s already registered at node ~p",
                                   [User, Host, node()]),
            {exists, String};
        {error, Reason} ->
            String = io_lib:format("Can't register user ~s@~s at node ~p: ~p",
                                   [User, Host, node(), Reason]),
            {cannot_register, String}
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


%%%
%%% Migration management
%%%

-spec import_file(file:name()) -> {'cannot_import_file', io_lib:chars()} | {'ok',[]}.
import_file(Path) ->
    case jd2ejd:import_file(Path) of
        ok ->
            {ok, ""};
        {error, Reason} ->
            String = io_lib:format("Can't import jabberd14 spool file ~p at node ~p: ~p",
                                   [filename:absname(Path), node(), Reason]),
            {cannot_import_file, String}
    end.

-spec import_dir(file:name()) -> {'cannot_import_dir', io_lib:chars()} | {'ok',[]}.
import_dir(Path) ->
    case jd2ejd:import_dir(Path) of
        ok ->
            {ok, ""};
        {error, Reason} ->
            String = io_lib:format("Can't import jabberd14 spool dir ~p at node ~p: ~p",
                                   [filename:absname(Path), node(), Reason]),
            {cannot_import_dir, String}
    end.


%%%
%%% Purge DB
%%%

-spec delete_expired_messages() -> 'ok'.
delete_expired_messages() ->
    {atomic, ok} = mod_offline:remove_expired_messages(),
    ok.

-spec delete_old_messages(Days :: integer()) -> 'ok'.
delete_old_messages(Days) ->
    {atomic, _} = mod_offline:remove_old_messages(Days),
    ok.


%%%
%%% Mnesia management
%%%

-spec set_master(Node :: atom() | string()) -> {'error', io_lib:chars()} | {'ok',[]}.
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

-spec backup_mnesia(file:name()) -> {'cannot_backup', io_lib:chars()} | {'ok',[]}.
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
                                   | {'ok',[]}
                                   | {'table_not_exists', io_lib:chars()}.
restore_mnesia(Path) ->
    case ejabberd_admin:restore(Path) of
        {atomic, _} ->
            {ok, ""};
        {error, Reason} ->
            String = io_lib:format("Can't restore backup from ~p at node ~p: ~p",
                                   [filename:absname(Path), node(), Reason]),
            {cannot_restore, String};
        {aborted,{no_exists,Table}} ->
            String = io_lib:format("Can't restore backup from ~p at node ~p: Table ~p does not exist.",
                                   [filename:absname(Path), node(), Table]),
            {table_not_exists, String};
        {aborted,enoent} ->
            String = io_lib:format("Can't restore backup from ~p at node ~p: File not found.",
                                   [filename:absname(Path), node()]),
            {file_not_found, String}
    end.

%% @doc Mnesia database restore
%% This function is called from ejabberd_ctl, ejabberd_web_admin and
%% mod_configure/adhoc
restore(Path) ->
    mnesia:restore(Path, [{keep_tables,keep_tables()},
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

-spec dump_tables(file:name(), Tables :: [string()]) ->
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

-spec dump_to_textfile(file:name()) -> 'ok' | {'error',atom()}.
dump_to_textfile(File) ->
    Tabs = get_local_tables(),
    dump_to_textfile(File, Tabs).

-spec dump_to_textfile(file:name(), Tabs :: list()) -> 'ok' | {'error',atom()}.
dump_to_textfile(File, Tabs) ->
    dump_to_textfile(mnesia:system_info(is_running), Tabs, file:open(File, [write])).

-spec dump_to_textfile(any(),
                      any(),
                      {'error',atom()} | {'ok',pid() | {'file_descriptor',atom() | tuple(),_}}
                      ) -> 'ok' | {'error',atom()}.
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

-spec dump_tab(pid(),atom()) -> 'ok'.
dump_tab(F, T) ->
    W = mnesia:table_info(T, wild_pattern),
    {atomic,All} = mnesia:transaction(
                     fun() -> mnesia:match_object(T, W, read) end),
    lists:foreach(
      fun(Term) -> io:format(F,"~p.~n", [setelement(1, Term, T)]) end, All).

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
                        {'cannot_fallback', io_lib:chars()} | {'ok',[]}.
install_fallback_mnesia(Path) ->
    case mnesia:install_fallback(Path) of
        ok ->
            {ok, ""};
        {error, Reason} ->
            String = io_lib:format("Can't install fallback from ~p at node ~p: ~p",
                                   [filename:absname(Path), node(), Reason]),
            {cannot_fallback, String}
    end.

-spec mnesia_change_nodename(string(),string(),_,_) -> {ok,_} | {error,_}.
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
                {[{schema, db_nodes, lists:map(Switch,Nodes)}], Acc};
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
