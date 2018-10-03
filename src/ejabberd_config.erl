%%%----------------------------------------------------------------------
%%% File    : ejabberd_config.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Load config file
%%% Created : 14 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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
%%%----------------------------------------------------------------------

-module(ejabberd_config).
-author('alexey@process-one.net').

-export([start/0,
         load_file/1,
         add_global_option/2,
         get_global_option/1,
         add_local_option/2,
         get_local_option/1,
         get_local_option/2,
         del_local_option/1,
         get_local_option_or_default/2]).
-export([get_vh_by_auth_method/1]).

%% conf reload
-export([reload_local/0,
         reload_cluster/0,
         reload_cluster_dryrun/0]).

%% Information commands
-export([print_flat_config/0]).

-export([get_local_config/0,
         get_host_local_config/0]).

%% Introspection
-export([config_info/0]).
-export([config_state/0]).
-export([config_states/0]).

-import(mongoose_config_parser, [can_be_ignored/1]).

-export([apply_reloading_change/1]).

%% For debugging
-export([assert_local_config_reloaded/0]).

-include("mongoose.hrl").
-include("ejabberd_config.hrl").

-define(CONFIG_RELOAD_TIMEOUT, 30000).

-type compare_result() :: mongoose_config_parser:compare_result().

-type host() :: any(). % TODO: specify this
-type state() :: mongoose_config_parser:state().
-type key() :: mongoose_config_parser:key().
-type value() :: mongoose_config_parser:value().

-spec start() -> ok.
start() ->
    mnesia:create_table(config,
                        [{ram_copies, [node()]},
                         {storage_properties,
                          [{ets, [{read_concurrency, true}]}]},
                         {attributes, record_info(fields, config)}]),
    mnesia:add_table_copy(config, node(), ram_copies),
    mnesia:create_table(local_config,
                        [{ram_copies, [node()]},
                         {storage_properties,
                          [{ets, [{read_concurrency, true}]}]},
                         {local_content, true},
                         {attributes, record_info(fields, local_config)}]),
    mnesia:add_table_copy(local_config, node(), ram_copies),
    Config = get_ejabberd_config_path(),
    ejabberd_config:load_file(Config),
    %% This start time is used by mod_last:
    add_local_option(node_start, p1_time_compat:timestamp()),
    ok.


%% @doc Get the filename of the ejabberd configuration file.
%% The filename can be specified with: erl -config "/path/to/mongooseim.cfg".
%% It can also be specified with the environtment variable EJABBERD_CONFIG_PATH.
%% If not specified, the default value 'mongooseim.cfg' is assumed.
-spec get_ejabberd_config_path() -> string().
get_ejabberd_config_path() ->
    DefaultPath = case os:getenv("EJABBERD_CONFIG_PATH") of
                      false ->
                          ?CONFIG_PATH;
                      Path ->
                          Path
                  end,

    application:get_env(mongooseim, config, DefaultPath).

%% @doc Load the ejabberd configuration file.
%% It also includes additional configuration files and replaces macros.
%% This function will crash if finds some error in the configuration file.
-spec load_file(File :: string()) -> ok.
load_file(File) ->
    State = parse_file(File),
    assert_required_files_exist(State),
    set_opts(State).


%% @doc Read an ejabberd configuration file and return the terms.
%% Input is an absolute or relative path to an ejabberd config file.
%% Returns a list of plain terms,
%% in which the options 'include_config_file' were parsed
%% and the terms in those files were included.
-spec get_plain_terms_file(string()) -> [term()].
get_plain_terms_file(File1) ->
    File = mongoose_config_utils:get_absolute_path(File1),
    case file:consult(File) of
        {ok, Terms} ->
            include_config_files(Terms);
        {error, {LineNumber, erl_parse, _ParseMessage} = Reason} ->
            ExitText = describe_config_problem(File, Reason, LineNumber),
            ?ERROR_MSG(ExitText, []),
            mongoose_config_utils:exit_or_halt(ExitText);
        {error, Reason} ->
            ExitText = describe_config_problem(File, Reason),
            ?ERROR_MSG(ExitText, []),
            mongoose_config_utils:exit_or_halt(ExitText)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Errors reading the config file

-type config_problem() :: atom() | {integer(), atom() | tuple(), _}. % spec me better

-spec describe_config_problem(Filename :: string(),
                              Reason :: config_problem()) -> string().
describe_config_problem(Filename, Reason) ->
    Text1 = lists:flatten("Problem loading MongooseIM config file " ++ Filename),
    Text2 = lists:flatten(" : " ++ file:format_error(Reason)),
    ExitText = Text1 ++ Text2,
    ExitText.


-spec describe_config_problem(Filename :: string(),
                              Reason :: config_problem(),
                              Line :: pos_integer()) -> string().
describe_config_problem(Filename, Reason, LineNumber) ->
    Text1 = lists:flatten("Problem loading ejabberd config file " ++ Filename),
    Text2 = lists:flatten(" approximately in the line "
                          ++ file:format_error(Reason)),
    ExitText = Text1 ++ Text2,
    Lines = mongoose_config_utils:get_config_lines(Filename, LineNumber, 10, 3),
    ?ERROR_MSG("The following lines from your configuration file might be"
               " relevant to the error: ~n~s", [Lines]),
    ExitText.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Support for 'include_config_file'

%% @doc Include additional configuration files in the list of terms.
-spec include_config_files([term()]) -> [term()].
include_config_files(Terms) ->
    Filenames = mongoose_config_parser:config_filenames_to_include(Terms),
    Configs = lists:map(fun(Filename) ->
            {Filename, get_plain_terms_file(Filename)}
        end, Filenames),
    mongoose_config_parser:include_config_files(Terms, Configs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Process terms


-spec set_opts(state()) -> 'ok' | none().
set_opts(State) ->
    case mnesia:transaction(fun() -> do_set_opts(State) end) of
        {atomic, _} -> ok;
        {aborted, {no_exists, Table}} ->
            handle_table_does_not_exist_error(Table)
    end.

-spec do_set_opts(state()) -> 'ok' | none().
do_set_opts(State) ->
    Opts = mongoose_config_parser:state_to_opts(State),
    maybe_clean_global_opts(State),
    maybe_clean_local_opts(State),
    maybe_clean_acls_opts(State),
    lists:foreach(fun(R) -> mnesia:write(R) end, Opts).

maybe_clean_global_opts(State) ->
    case mongoose_config_parser:can_override(global, State) of
        true ->
            clean_global_opts();
        _ ->
            ok
    end.

maybe_clean_local_opts(State) ->
    case mongoose_config_parser:can_override(local, State) of
        true ->
            clean_local_opts();
        _ ->
            ok
    end.

maybe_clean_acls_opts(State) ->
    case mongoose_config_parser:can_override(acls, State) of
        true ->
            clean_acls_opts();
        _ ->
            ok
    end.

clean_global_opts() ->
    Ksg = mnesia:all_keys(config),
    lists:foreach(fun(K) -> mnesia:delete({config, K}) end, Ksg).

clean_local_opts() ->
    Ksl = mnesia:all_keys(local_config),
    Ksl2 = lists:delete(node_start, Ksl),
    lists:foreach(fun(K) -> mnesia:delete({local_config, K}) end, Ksl2).

clean_acls_opts() ->
    Ksa = mnesia:all_keys(acl),
    lists:foreach(fun(K) -> mnesia:delete({acl, K}) end, Ksa).

-spec add_global_option(Opt :: key(), Val :: value()) -> {atomic|aborted, _}.
add_global_option(Opt, Val) ->
    mnesia:transaction(fun() ->
                           mnesia:write(#config{key   = Opt,
                                                value = Val})
                       end).


-spec add_local_option(Opt :: key(), Val :: value()) -> {atomic|aborted, _}.
add_local_option(Opt, Val) ->
    mnesia:transaction(fun() ->
                           mnesia:write(#local_config{key   = Opt,
                                                      value = Val})
                       end).

-spec del_local_option(Opt :: key()) -> {atomic | aborted, _}.
del_local_option(Opt) ->
    mnesia:transaction(fun mnesia:delete/1, [{local_config, Opt}]).

-spec get_global_option(key()) -> value() | undefined.
get_global_option(Opt) ->
    case ets:lookup(config, Opt) of
        [#config{value = Val}] ->
            Val;
        _ ->
            undefined
    end.

-spec get_local_option(key()) -> value() | undefined.
get_local_option(Opt) ->
    case ets:lookup(local_config, Opt) of
        [#local_config{value = Val}] ->
            Val;
        _ ->
            undefined
    end.

-spec get_local_option(key(), host()) -> value() | undefined.
get_local_option(Opt, Host) ->
    case get_local_option({Opt, Host}) of
        undefined -> get_global_option(Opt);
        Val -> Val
    end.

-spec get_local_option_or_default(key(), value()) -> value().
get_local_option_or_default(Opt, Default) ->
    case get_local_option(Opt) of
        undefined ->
            Default;
        Value ->
            Value
    end.

%% @doc Return the list of hosts handled by a given module
get_vh_by_auth_method(AuthMethod) ->
    mnesia:dirty_select(local_config,
                        [{#local_config{key   = {auth_method, '$1'},
                                        value = AuthMethod}, [], ['$1']}]).

handle_table_does_not_exist_error(Table) ->
    MnesiaDirectory = mnesia:system_info(directory),
    ?ERROR_MSG("Error reading Mnesia database spool files:~n"
               "The Mnesia database couldn't read the spool file for the table '~p'.~n"
               "ejabberd needs read and write access in the directory:~n   ~s~n"
               "Maybe the problem is a change in the computer hostname, ~n"
               "or a change in the Erlang node name, which is currently:~n   ~p~n"
               "Check the ejabberd guide for details about changing the~n"
               "computer hostname or Erlang node name.~n",
               [Table, MnesiaDirectory, node()]),
    exit("Error reading Mnesia database").

%%--------------------------------------------------------------------
%% Configuration reload
%%--------------------------------------------------------------------
-spec parse_file(file:name()) -> state().
parse_file(ConfigFile) ->
    Terms = get_plain_terms_file(ConfigFile),
    mongoose_config_parser:parse_terms(Terms).

-spec reload_local() -> {ok, iolist()} | no_return().
reload_local() ->
    reload_nodes(reload_local, [node()], false).

-spec reload_cluster() -> {ok, iolist()} | no_return().
reload_cluster() ->
    reload_nodes(reload_cluster, all_cluster_nodes(), false).

-spec reload_cluster_dryrun() -> {ok, iolist()} | no_return().
reload_cluster_dryrun() ->
    reload_nodes(reload_cluster_dryrun, all_cluster_nodes(), true).

reload_nodes(Command, Nodes, DryRun) ->
    NodeStates = config_states(Nodes),
    ReloadContext = mongoose_config_reload:states_to_reloading_context(NodeStates),
    FailedChecks = mongoose_config_reload:context_to_failed_checks(ReloadContext),
    case FailedChecks of
        [] ->
            Changes = mongoose_config_reload:context_to_changes_to_apply(ReloadContext),
            apply_reload_changes(DryRun, Nodes, ReloadContext, Changes),
            {ok, "done"};
        [no_update_required] ->
            {ok, "No update required"};
        [_|_] ->
            Filename = dump_reload_state(Command, ReloadContext),
            error(#{reason => reload_failed,
                    nodes => Nodes,
                    from_command => Command,
                    failed_checks => FailedChecks,
                    dump_filename => Filename,
                    dry_run => DryRun})
    end.

apply_reload_changes(_DryRun = false, Nodes, ReloadContext, Changes) ->
    try_reload_cluster(ReloadContext, Changes),
    assert_config_reloaded(Nodes);
apply_reload_changes(_DryRun = true, _Nodes, _ReloadContext, _Changes) ->
    ok.

print_flat_config() ->
    %% Without global opts
    FlatOptsIolist = mongoose_config_helper:get_flat_opts_iolist(),
    {ok, io_lib:format("Flat options:~n~s", [FlatOptsIolist])}.

assert_local_config_reloaded() ->
    assert_config_reloaded([node()]).

assert_config_reloaded(Nodes) ->
    NodeStates = config_states(Nodes),
    ReloadContext = mongoose_config_reload:states_to_reloading_context(NodeStates),
    FailedChecks = mongoose_config_reload:context_to_failed_checks(ReloadContext),
    case FailedChecks of
        [no_update_required] ->
            ok;
        _ ->
            Filename = dump_reload_state(assert_local_config_reloaded, ReloadContext),
            error(#{reason => assert_config_reloaded,
                    nodes => Nodes,
                    failed_checks => FailedChecks,
                    dump_filename => Filename})
    end.

dump_reload_state(From, ReloadContext) ->
    Map = ReloadContext#{what => From},
    Io = io_lib:format("~p.", [Map]),
    Filename = dump_reload_state_filename(),
    %% Wow, so important!
    ?CRITICAL_MSG("issue=dump_reload_state from=~p filename=~p",
                  [From, Filename]),
    io:format("issue=dump_reload_state from=~p filename=~p",
                  [From, Filename]),
    file:write_file(Filename, Io),
    Filename.

dump_reload_state_filename() ->
    {ok, Pwd} = file:get_cwd(),
    DateTime = jlib:now_to_utc_string(os:timestamp()),
    Filename = "reload_state_" ++ DateTime ++ ".dump",
    filename:join(Pwd, Filename).

try_reload_cluster(ReloadContext, Changes) ->
    try
        do_reload_cluster(Changes)
    catch
        Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            ?CRITICAL_MSG("issue=try_reload_cluster_failed "
                           "reason=~p:~p stacktrace=~1000p",
                          [Class, Reason, Stacktrace]),
            Filename = dump_reload_state(try_reload_cluster, ReloadContext),
            Reason2 = #{issue => try_reload_cluster_failed,
                        reason => Reason,
                        dump_filename => Filename},
            erlang:raise(Class, Reason2, Stacktrace)
    end.

do_reload_cluster(Changes) ->
    lists:map(fun(Change) -> apply_reloading_change(Change) end, Changes),
    ok.

apply_reloading_change(#{
              mongoose_node := Node,
              state_to_apply := State,
              config_diff_to_apply := ConfigDiff}) when node() =:= Node ->
    #{config_changes := ConfigChanges,
      local_config_changes := LocalConfigChanges,
      local_hosts_changes := LocalHostsChanges} = ConfigDiff,
    set_opts(State),
    reload_config(ConfigChanges),
    reload_local_config(LocalConfigChanges),
    reload_local_hosts_config(LocalHostsChanges),
    {ok, node()};
apply_reloading_change(Change=#{mongoose_node := Node}) ->
    rpc:call(Node, ?MODULE, apply_reloading_change, [Change]).

-spec reload_config(compare_result()) -> ok.
reload_config(#{to_start := CAdd,
                to_stop := CDel,
                to_reload := CChange}) ->
    lists:foreach(fun handle_config_change/1, CChange),
    lists:foreach(fun handle_config_add/1, CAdd),
    lists:foreach(fun handle_config_del/1, CDel).

-spec reload_local_config(compare_result()) -> ok.
reload_local_config(#{to_start := LCAdd,
                      to_stop := LCDel,
                      to_reload := LCChange}) ->
    lists:foreach(fun handle_local_config_change/1, LCChange),
    lists:foreach(fun handle_local_config_add/1, LCAdd),
    lists:foreach(fun handle_local_config_del/1, LCDel).

-spec reload_local_hosts_config(compare_result()) -> ok.
reload_local_hosts_config(#{to_start := LCHAdd,
                            to_stop := LCHDel,
                            to_reload := LCHChange}) ->
    lists:foreach(fun handle_local_hosts_config_change/1, LCHChange),
    lists:foreach(fun handle_local_hosts_config_add/1, LCHAdd),
    lists:foreach(fun handle_local_hosts_config_del/1, LCHDel).

%% ----------------------------------------------------------------
%% CONFIG
%% ----------------------------------------------------------------
handle_config_add(#config{key = hosts, value = Hosts}) when is_list(Hosts) ->
    lists:foreach(fun(Host) -> add_virtual_host(Host) end, Hosts).

handle_config_del(#config{key = hosts, value = Hosts}) ->
    lists:foreach(fun(Host) -> remove_virtual_host(Host) end, Hosts).

%% handle add/remove new hosts
handle_config_change({hosts, OldHosts, NewHosts}) ->
    {ToDel, ToAdd} = mongoose_config_parser:check_hosts(NewHosts, OldHosts),
    lists:foreach(fun remove_virtual_host/1, ToDel),
    lists:foreach(fun add_virtual_host/1, ToAdd);
handle_config_change({language, _Old, _New}) ->
    ok;
handle_config_change({_Key, _OldValue, _NewValue}) ->
    ok.

%% ----------------------------------------------------------------
%% LOCAL CONFIG
%% ----------------------------------------------------------------
handle_local_config_add(#local_config{key = cassandra_servers}) ->
    mongoose_cassandra:start();
handle_local_config_add(#local_config{key = Key} = El) ->
    ?WARNING_MSG_IF(not can_be_ignored(Key), "local config add ~p option unhandled", [El]).

handle_local_config_del(#local_config{key = cassandra_servers}) ->
    mongoose_cassandra:stop();
handle_local_config_del(#local_config{key = node_start}) ->
    %% do nothing with it
    ok;
handle_local_config_del(#local_config{key = Key} = El) ->
    ?WARNING_MSG_IF(not can_be_ignored(Key), "local config change: ~p unhandled", [El]).

handle_local_config_change({listen, Old, New}) ->
    reload_listeners(mongoose_config_reload:compare_listeners(Old, New));
handle_local_config_change({loglevel, _Old, Loglevel}) ->
    ejabberd_loglevel:set(Loglevel),
    ok;
handle_local_config_change({cassandra_servers, _Old, _New}) ->
    mongoose_cassandra:stop(),
    mongoose_cassandra:start(),
    ok;
handle_local_config_change({Key, _Old, _New} = El) ->
    ?WARNING_MSG_IF(not can_be_ignored(Key), "local config change: ~p unhandled", [El]).

%% ----------------------------------------------------------------
%% LOCAL HOST CONFIG
%% ----------------------------------------------------------------

handle_local_hosts_config_add({{auth, Host}, _}) ->
    ejabberd_auth:start(Host);
handle_local_hosts_config_add({{ldap, _Host}, _}) ->
    %% ignore ldap section
    ok;
handle_local_hosts_config_add({{modules, Host}, Modules}) ->
    gen_mod_deps:start_modules(Host, Modules);
handle_local_hosts_config_add({{Key, _Host}, _} = El) ->
    ?WARNING_MSG_IF(not can_be_ignored(Key), "local hosts config add option: ~p unhandled", [El]).

handle_local_hosts_config_del({{auth, Host}, Opts}) ->
    case lists:keyfind(auth_method, 1, Opts) of
        false ->
            ok;%nothing to stop?
        {auth_method, Val} ->
            AuthModules = methods_to_auth_modules(Val),
            lists:foreach(fun(M) ->
                              M:stop(Host)
                          end, AuthModules)
    end;
handle_local_hosts_config_del({{ldap, _Host}, _I}) ->
    %% ignore ldap section, only appli
    ok;
handle_local_hosts_config_del({{modules, Host}, Modules}) ->
    lists:foreach(fun({Mod, _}) -> gen_mod:stop_module(Host, Mod) end, Modules);
handle_local_hosts_config_del({{Key, _}, _} =El) ->
    ?WARNING_MSG_IF(not can_be_ignored(Key),
                    "local hosts config delete option: ~p unhandled", [El]).

handle_local_hosts_config_change({{auth, Host}, OldVals, _}) ->
    case lists:keyfind(auth_method, 1, OldVals) of
        false ->
            ejabberd_auth:stop(Host);
        {auth_method, Val} ->
            %% stop old modules
            AuthModules = methods_to_auth_modules(Val),
            lists:foreach(fun(M) ->
                              M:stop(Host)
                          end, AuthModules)
    end,
    ejabberd_auth:start(Host);
handle_local_hosts_config_change({{ldap, Host}, _OldConfig, NewConfig}) ->
    ok = ejabberd_hooks:run_fold(host_config_update, Host, ok, [Host, ldap, NewConfig]);
handle_local_hosts_config_change({{modules, Host}, OldModules, NewModules}) ->
    gen_mod_deps:replace_modules(Host, OldModules, NewModules);
handle_local_hosts_config_change({{Key, _Host}, _Old, _New} = El) ->
    ?WARNING_MSG_IF(not can_be_ignored(Key),
                    "local hosts config change option: ~p unhandled", [El]).

methods_to_auth_modules(L) when is_list(L) ->
    [list_to_atom("ejabberd_auth_" ++ atom_to_list(M)) || M <- L];
methods_to_auth_modules(A) when is_atom(A) ->
    methods_to_auth_modules([A]).


-spec add_virtual_host(Host :: jid:server()) -> any().
add_virtual_host(Host) ->
    ?DEBUG("Register host:~p", [Host]),
    ejabberd_local:register_host(Host).

-spec remove_virtual_host(jid:server()) -> any().
remove_virtual_host(Host) ->
    ?DEBUG("Unregister host :~p", [Host]),
    ejabberd_local:unregister_host(Host).

-spec reload_listeners(ChangedListeners :: compare_result()) -> 'ok'.
reload_listeners(#{to_start := Add,
                   to_stop := Del,
                   to_reload := Change} = ChangedListeners) ->
    ?DEBUG("reload listeners: ~p", [lager:pr(ChangedListeners, ?MODULE)]),
    lists:foreach(fun({{PortIP, Module}, Opts}) ->
                      ejabberd_listener:delete_listener(PortIP, Module, Opts)
                  end, Del),
    lists:foreach(fun({{PortIP, Module}, Opts}) ->
                      ejabberd_listener:add_listener(PortIP, Module, Opts)
                  end, Add),
    lists:foreach(fun({{PortIP, Module}, OldOpts, NewOpts}) ->
                      ejabberd_listener:delete_listener(PortIP, Module, OldOpts),
                      ejabberd_listener:add_listener(PortIP, Module, NewOpts)
                  end, Change).

%% match all hosts
-spec get_host_local_config() -> [{local_config, {term(), jid:server()}, term()}].
get_host_local_config() ->
    mnesia:dirty_match_object({local_config, {'_', '_'}, '_'}).

-spec get_local_config() -> [{local_config, term(), term()}].
get_local_config() ->
    Keys = lists:filter(fun mongoose_config_parser:is_not_host_specific/1, mnesia:dirty_all_keys(local_config)),
    lists:flatten(lists:map(fun(Key) ->
                                mnesia:dirty_read(local_config, Key)
                            end,
                            Keys)).

-spec get_global_config() -> [{config, term(), term()}].
get_global_config() ->
    mnesia:dirty_match_object(config, {config, '_', '_'}).

%% @doc Returns current all options in memory, grouped by category.
get_categorized_options() ->
    Config = get_global_config(),
    Local = get_local_config(),
    HostsLocal = get_host_local_config(),
    mongoose_config_reload:make_categorized_options(Config, Local, HostsLocal).

%% @doc Returns configs on disc and in memory for this node.
%% This function prepares all state data to pass into pure code part
%% (i.e. mongoose_config_parser and mongoose_config_reload).
config_state() ->
    ConfigFile = get_ejabberd_config_path(),
    Terms = get_plain_terms_file(ConfigFile),
    %% Performance optimization hint:
    %% terms_to_missing_and_required_files/1 actually parses Terms into State.
    #{missing_files := MissingFiles,
      required_files := RequiredFiles} =
        terms_to_missing_and_required_files(Terms),
    #{mongoose_node => node(),
      config_file => ConfigFile,
      loaded_categorized_options => get_categorized_options(),
      ondisc_config_terms => Terms,
      missing_files => MissingFiles,
      required_files => RequiredFiles}.

config_states() ->
    config_states(all_cluster_nodes()).

%% @doc Returns config states from all nodes in cluster
%% State from the local node comes as head of a list
config_states(Nodes) ->
    {S, F} = rpc:multicall(Nodes, ?MODULE, config_state, [], 30000),
    case F of
        [] ->
            S;
        [_|_] ->
            erlang:error(#{issue => config_state_failed,
                           cluster_nodes => Nodes,
                           failed_nodes => F})
    end.

compute_config_file_version() ->
    ConfigFile = get_ejabberd_config_path(),
    State = parse_file(ConfigFile),
    mongoose_config_reload:compute_config_file_version(State).

compute_loaded_config_version() ->
    LC = get_local_config(),
    LCH = get_host_local_config(),
    mongoose_config_reload:compute_config_version(LC, LCH).

config_info() ->
    [{config_file_version, compute_config_file_version()},
     {config_version, compute_loaded_config_version()}].


all_cluster_nodes() ->
    [node()|other_cluster_nodes()].

-spec other_cluster_nodes() -> [node()].
other_cluster_nodes() ->
    lists:filter(fun is_mongooseim_node/1, nodes()).

-spec is_mongooseim_node(node()) -> boolean().
is_mongooseim_node(Node) ->
    Apps = rpc:call(Node, application, which_applications, []),
    lists:keymember(mongooseim, 1, Apps).

assert_required_files_exist(State) ->
    RequiredFiles = mongoose_config_parser:state_to_required_files(State),
    case missing_files(RequiredFiles) of
        [] ->
            ok;
        MissingFiles ->
            erlang:error(#{issue => missing_files,
                           filenames => MissingFiles})
    end.

terms_to_missing_and_required_files(Terms) ->
    State = mongoose_config_parser:parse_terms(Terms),
    RequiredFiles = mongoose_config_parser:state_to_required_files(State),
    MissingFiles = missing_files(RequiredFiles),
    #{missing_files => MissingFiles, required_files => RequiredFiles}.

missing_files(RequiredFiles) ->
    [Filename || Filename <- RequiredFiles,
                 not mongoose_config_utils:is_file_readable(Filename)].
