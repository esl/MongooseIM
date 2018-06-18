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
         apply_changes_remote/4,
         apply_changes/5]).

-export([get_local_config/0,
         get_host_local_config/0]).

-import(mongoose_config, [can_be_ignored/1]).

-include("mongoose.hrl").
-include("ejabberd_config.hrl").

-define(CONFIG_RELOAD_TIMEOUT, 30000).

-type key() :: atom()
             | {key(), jid:server() | atom() | list()}
             | {atom(), atom(), atom()}
             | binary(). % TODO: binary is questionable here

-type value() :: atom()
               | binary()
               | integer()
               | string()
               | [value()]
               | tuple().

-export_type([key/0, value/0]).

-record(compare_result, {to_start = [] :: list(),
                         to_stop = [] :: list(),
                         to_reload = [] :: list()}).

-type compare_result() :: #compare_result{}.

-type host() :: any(). % TODO: specify this
-type state() :: mongoose_config:state().

-callback stop(host()) -> any().


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
%% The filename can be specified with: erl -config "/path/to/ejabberd.cfg".
%% It can also be specified with the environtment variable EJABBERD_CONFIG_PATH.
%% If not specified, the default value 'ejabberd.cfg' is assumed.
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
    Res = parse_file(File),
    set_opts(Res).


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
    Text1 = lists:flatten("Problem loading ejabberd config file " ++ Filename),
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
    Filenames = mongoose_config:config_filenames_to_include(Terms),
    Configs = lists:map(fun(Filename) ->
            {Filename, get_plain_terms_file(Filename)}
        end, Filenames),
    mongoose_config:include_config_files(Terms, Configs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Process terms


-spec set_opts(state()) -> 'ok' | none().
set_opts(State) ->
    case mnesia:transaction(fun() -> do_set_opts(State) end) of
        {atomic, _} -> ok;
        {aborted, {no_exists, Table}} ->
            MnesiaDirectory = mnesia:system_info(directory),
            ?ERROR_MSG("Error reading Mnesia database spool files:~n"
                       "The Mnesia database couldn't read the spool file for the table '~p'.~n"
                       "ejabberd needs read and write access in the directory:~n   ~s~n"
                       "Maybe the problem is a change in the computer hostname, ~n"
                       "or a change in the Erlang node name, which is currently:~n   ~p~n"
                       "Check the ejabberd guide for details about changing the~n"
                       "computer hostname or Erlang node name.~n",
                       [Table, MnesiaDirectory, node()]),
            exit("Error reading Mnesia database")
    end.

-spec do_set_opts(state()) -> 'ok' | none().
do_set_opts(State) ->
    Opts = mongoose_config:state_to_opts(State),
    case mongoose_config:can_override(global, State) of
        true ->
            Ksg = mnesia:all_keys(config),
            lists:foreach(fun(K) -> mnesia:delete({config, K}) end, Ksg);
        _ ->
            ok
    end,
    case mongoose_config:can_override(local, State) of
        true ->
            Ksl = mnesia:all_keys(local_config),
            lists:foreach(fun(K) -> mnesia:delete({local_config, K}) end,
                          lists:delete(node_start, Ksl));
        _ ->
            ok
    end,
    case mongoose_config:can_override(acls, State) of
        true ->
            Ksa = mnesia:all_keys(acl),
            lists:foreach(fun(K) -> mnesia:delete({acl, K}) end, Ksa);
        _ ->
            ok
    end,
    lists:foreach(fun(R) -> mnesia:write(R) end, Opts).


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

%%--------------------------------------------------------------------
%% Configuration reload
%%--------------------------------------------------------------------
-spec parse_file(file:name()) -> state().
parse_file(ConfigFile) ->
    Terms = get_plain_terms_file(ConfigFile),
    mongoose_config:parse_terms(Terms).

-spec reload_local() -> {ok, iolist()} | no_return().
reload_local() ->
    ConfigFile = get_ejabberd_config_path(),
    State0 = parse_file(ConfigFile),
    {CC, LC, LHC} = get_config_diff(State0),
    ConfigVersion = mongoose_config:compute_config_version(
                                           get_local_config(),
                                           get_host_local_config()),
    State1 = mongoose_config:allow_override_all(State0),
    try
        {ok, _} = apply_changes(CC, LC, LHC, State1, ConfigVersion),
        ?WARNING_MSG("node config reloaded from ~s", [ConfigFile]),
        {ok, io_lib:format("# Reloaded: ~s", [node()])}
    catch
        Error:Reason ->
            Msg = msg("failed to apply config on node: ~p~nreason: ~p",
                      [node(), {Error, Reason}]),
            ?WARNING_MSG("node config reload failed!~n"
                         "current config version: ~p~n"
                         "config file: ~s~n"
                         "reason: ~p~n"
                         "stacktrace: ~ts",
                         [ConfigVersion, ConfigFile, Msg,
                          msg("~p", [erlang:get_stacktrace()])]),
            error(Msg)
    end.

%% Won't be unnecessarily evaluated if used as an argument
%% to lager parse transform.
msg(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).

-spec reload_cluster() -> {ok, string()} | no_return().
reload_cluster() ->
    CurrentNode = node(),
    ConfigFile = get_ejabberd_config_path(),
    State0 = parse_file(ConfigFile),
    ConfigDiff = {CC, LC, LHC} = get_config_diff(State0),

    ConfigVersion = mongoose_config:compute_config_version(
                                           get_local_config(),
                                           get_host_local_config()),
    FileVersion = mongoose_config:compute_config_file_version(State0),
    ?WARNING_MSG("cluster config reload from ~s scheduled", [ConfigFile]),
    %% first apply on local
    State1 = mongoose_config:allow_override_all(State0),
    case catch apply_changes(CC, LC, LHC, State1, ConfigVersion) of
        {ok, CurrentNode} ->
            %% apply on other nodes
            {S, F} = rpc:multicall(nodes(), ?MODULE, apply_changes_remote,
                                   [ConfigFile, ConfigDiff,
                                    ConfigVersion, FileVersion],
                                   30000),
            {S1, F1} = group_nodes_results([{ok, node()} | S], F),
            ResultText = (groups_to_string("# Reloaded:", S1)
                          ++ groups_to_string("\n# Failed:", F1)),
            case F1 of
                [] -> ?WARNING_MSG("cluster config reloaded successfully", []);
                [_ | _] ->
                    FailedUpdateOrRPC = F ++ [Node || {error, Node, _} <- S],
                    ?WARNING_MSG("cluster config reload failed on nodes: ~p",
                                 [FailedUpdateOrRPC])
            end,
            {ok, ResultText};
        Error ->
            Reason = msg("failed to apply config on node: ~p~nreason: ~p",
                         [CurrentNode, Error]),
            ?WARNING_MSG("cluster config reload failed!~n"
                         "config file: ~s~n"
                         "reason: ~ts", [ConfigFile, Reason]),
            exit(Reason)
    end.

-spec groups_to_string(string(), [string()]) -> string().
groups_to_string(_Header, []) ->
    "";
groups_to_string(Header, S) ->
    Header ++ "\n" ++ string:join(S, "\n").

-spec group_nodes_results(list(), [atom]) -> {[string()], [string()]}.
group_nodes_results(SuccessfullRPC, FailedRPC) ->
    {S, F} = lists:foldl(fun(El, {SN, FN}) ->
                             case El of
                                 {ok, Node} ->
                                     {[atom_to_list(Node) | SN], FN};
                                 {error, Node, Reason} ->
                                     {SN, [atom_to_list(Node) ++ " " ++ Reason | FN]}
                             end
                         end, {[], []}, SuccessfullRPC),
    {S, F ++ lists:map(fun(E) -> {atom_to_list(E) ++ " " ++ "RPC failed"} end, FailedRPC)}.

-spec get_config_diff(state()) -> {ConfigChanges,
                                   LocalConfigChanges,
                                   LocalHostsChanges} when
      ConfigChanges :: compare_result(),
      LocalConfigChanges :: compare_result(),
      LocalHostsChanges :: compare_result().
get_config_diff(State) ->
    Config = get_global_config(),
    Local = get_local_config(),
    HostsLocal = get_host_local_config(),
    Args = #{global_config => Config,
             local_config => Local,
             host_local_config => HostsLocal},
    Result = mongoose_config:get_config_diff(State, Args),
    #{config_changes := CC,
      local_config_changes := LC,
      local_hosts_changes := LHC} = Result,
    {CC, LC, LHC}.

-spec apply_changes_remote(file:name(), term(), binary(), binary()) ->
                                  {ok, node()}| {error, node(), string()}.
apply_changes_remote(NewConfigFilePath, ConfigDiff,
                     DesiredConfigVersion, DesiredFileVersion) ->
    ?WARNING_MSG("remote config reload scheduled", []),
    ?DEBUG("~ndesired config version: ~p"
           "~ndesired file version: ~p",
           [DesiredConfigVersion, DesiredFileVersion]),
    Node = node(),
    {CC, LC, LHC} = ConfigDiff,
    State0 = parse_file(NewConfigFilePath),
    case mongoose_config:compute_config_file_version(State0) of
        DesiredFileVersion ->
            State1 = mongoose_config:allow_override_local_only(State0),
            case catch apply_changes(CC, LC, LHC, State1,
                                     DesiredConfigVersion) of
                {ok, Node} = R ->
                    ?WARNING_MSG("remote config reload succeeded", []),
                    R;
                UnknownResult ->
                    ?WARNING_MSG("remote config reload failed! "
                                 "can't apply desired config", []),
                    {error, Node,
                     lists:flatten(io_lib:format("~p", [UnknownResult]))}
            end;
        _ ->
            ?WARNING_MSG("remote config reload failed! "
                         "can't compute current config version", []),
            {error, Node, io_lib:format("Mismatching config file", [])}
    end.

-spec apply_changes(term(), term(), term(), state(), binary()) ->
                           {ok, node()} | {error, node(), string()}.
apply_changes(ConfigChanges, LocalConfigChanges, LocalHostsChanges,
              State, DesiredConfigVersion) ->
    ConfigVersion = mongoose_config:compute_config_version(
                                           get_local_config(),
                                           get_host_local_config()),
    ?DEBUG("config version: ~p", [ConfigVersion]),
    case ConfigVersion of
        DesiredConfigVersion ->
            ok;
        _ ->
            exit("Outdated configuration on node; cannot apply new one")
    end,
    %% apply config
    set_opts(State),

    reload_config(ConfigChanges),
    reload_local_config(LocalConfigChanges),
    reload_local_hosts_config(LocalHostsChanges),

    {ok, node()}.

reload_config(#compare_result{to_start  = CAdd, to_stop = CDel,
                              to_reload = CChange}) ->
    lists:foreach(fun handle_config_change/1, CChange),
    lists:foreach(fun handle_config_add/1, CAdd),
    lists:foreach(fun handle_config_del/1, CDel).

reload_local_config(#compare_result{to_start  = LCAdd, to_stop = LCDel,
                                    to_reload = LCChange}) ->
    lists:foreach(fun handle_local_config_change/1, LCChange),
    lists:foreach(fun handle_local_config_add/1, LCAdd),
    lists:foreach(fun handle_local_config_del/1, LCDel).

reload_local_hosts_config(#compare_result{to_start  = LCHAdd, to_stop = LCHDel,
                                          to_reload = LCHChange}) ->
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
    {ToDel, ToAdd} = mongoose_config:check_hosts(NewHosts, OldHosts),
    lists:foreach(fun remove_virtual_host/1, ToDel),
    lists:foreach(fun add_virtual_host/1, ToAdd);
handle_config_change({language, _Old, _New}) ->
    ok;
handle_config_change({_Key, _OldValue, _NewValue}) ->
    ok.

%% ----------------------------------------------------------------
%% LOCAL CONFIG
%% ----------------------------------------------------------------
handle_local_config_add(#local_config{key = riak_server}) ->
    mongoose_riak:start();
handle_local_config_add(#local_config{key = cassandra_servers}) ->
    mongoose_cassandra:start();
handle_local_config_add(#local_config{key = Key} = El) ->
    ?WARNING_MSG_IF(not can_be_ignored(Key), "local config add ~p option unhandled", [El]).

handle_local_config_del(#local_config{key = riak_server}) ->
    mongoose_riak:stop();
handle_local_config_del(#local_config{key = cassandra_servers}) ->
    mongoose_cassandra:stop();
handle_local_config_del(#local_config{key = node_start}) ->
    %% do nothing with it
    ok;
handle_local_config_del(#local_config{key = Key} = El) ->
    ?WARNING_MSG_IF(not can_be_ignored(Key), "local config change: ~p unhandled", [El]).

handle_local_config_change({listen, Old, New}) ->
    reload_listeners(mongoose_config:compare_listeners(Old, New));
handle_local_config_change({riak_server, _Old, _New}) ->
    mongoose_riak:stop(),
    mongoose_riak:start(),
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
reload_listeners(#compare_result{to_start  = Add, to_stop = Del,
                                 to_reload = Change} = ChangedListeners) ->
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
    Keys = lists:filter(fun mongoose_config:is_not_host_specific/1, mnesia:dirty_all_keys(local_config)),
    lists:flatten(lists:map(fun(Key) ->
                                mnesia:dirty_read(local_config, Key)
                            end,
                            Keys)).

-spec get_global_config() -> [{config, term(), term()}].
get_global_config() ->
    mnesia:dirty_match_object(config, {config, '_', '_'}).
