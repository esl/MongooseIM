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

-export([start/0, load_file/1,
         add_global_option/2, add_local_option/2,
         get_global_option/1, get_local_option/1]).
-export([get_vh_by_auth_method/1]).
-export([is_file_readable/1]).

-include("ejabberd.hrl").
-include("ejabberd_config.hrl").
-include_lib("kernel/include/file.hrl").

-type key() :: atom()
             | {atom(), ejabberd:server() | atom()}
             | {atom(), atom(), atom()}
             | binary(). % TODO: binary is questionable here
-type value() :: atom()
               | integer()
               | string()
               | [tuple()].

-export_type([key/0, value/0]).

-record(state, {opts = []  :: list(),
                hosts = [] :: [host()],
                override_local = false  :: boolean(),
                override_global = false :: boolean(),
                override_acls = false   :: boolean()
              }).

-type host() :: any(). % TODO: specify this
-type state() :: #state{}.
-type macro() :: {macro_key(), macro_value()}.

%% The atom must have all characters in uppercase.
-type macro_key() :: atom().

-type macro_value() :: term().

-type known_term() :: override_global
                    | override_local
                    | override_acls
                    | {acl, _, _}
                    | {alarms, _}
                    | {access, _, _}
                    | {shaper, _, _}
                    | {host, _}
                    | {hosts, _}
                    | {host_config, _, _}
                    | {listen, _}
                    | {language, _}
                    | {sm_backend, _}
                    | {outgoing_s2s_port, integer()}
                    | {outgoing_s2s_options, _, integer()}
                    | {{s2s_addr, _}, _}
                    | {s2s_dns_options, [tuple()]}
                    | {s2s_use_starttls, integer()}
                    | {s2s_certfile, _}
                    | {domain_certfile, _, _}
                    | {node_type, _}
                    | {cluster_nodes, _}
                    | {watchdog_admins, _}
                    | {watchdog_large_heap, _}
                    | {registration_timeout, integer()}
                    | {mongooseimctl_access_commands, list()}
                    | {loglevel, _}
                    | {max_fsm_queue, _}
                    | host_term().
-type host_term() :: {acl, _, _}
                  | {access, _, _}
                  | {shaper, _, _}
                  | {host, _}
                  | {hosts, _}
                  | {odbc_server, _}.


-spec start() -> ok.
start() ->
    mnesia:create_table(config,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, config)}]),
    mnesia:add_table_copy(config, node(), ram_copies),
    mnesia:create_table(local_config,
                        [{ram_copies, [node()]},
                         {local_content, true},
                         {attributes, record_info(fields, local_config)}]),
    mnesia:add_table_copy(local_config, node(), ram_copies),
    Config = get_ejabberd_config_path(),
    load_file(Config),
    %% This start time is used by mod_last:
    add_local_option(node_start, now()),
    ok.


%% @doc Get the filename of the ejabberd configuration file.
%% The filename can be specified with: erl -config "/path/to/ejabberd.cfg".
%% It can also be specified with the environtment variable EJABBERD_CONFIG_PATH.
%% If not specified, the default value 'ejabberd.cfg' is assumed.
-spec get_ejabberd_config_path() -> string().
get_ejabberd_config_path() ->
    case application:get_env(config) of
        {ok, Path} -> Path;
        undefined ->
            case os:getenv("EJABBERD_CONFIG_PATH") of
                false ->
                    ?CONFIG_PATH;
                Path ->
                    Path
            end
    end.


%% @doc Load the ejabberd configuration file.
%% It also includes additional configuration files and replaces macros.
%% This function will crash if finds some error in the configuration file.
-spec load_file(File::string()) -> ok.
load_file(File) ->
    Terms = get_plain_terms_file(File),
    State = lists:foldl(fun search_hosts/2, #state{}, Terms),
    Terms_macros = replace_macros(Terms),
    Res = lists:foldl(fun process_term/2, State, Terms_macros),
    set_opts(Res).


%% @doc Read an ejabberd configuration file and return the terms.
%% Input is an absolute or relative path to an ejabberd config file.
%% Returns a list of plain terms,
%% in which the options 'include_config_file' were parsed
%% and the terms in those files were included.
-spec get_plain_terms_file(string()) -> [term()].
get_plain_terms_file(File1) ->
    File = get_absolute_path(File1),
    case file:consult(File) of
        {ok, Terms} ->
            include_config_files(Terms);
        {error, {LineNumber, erl_parse, _ParseMessage} = Reason} ->
            ExitText = describe_config_problem(File, Reason, LineNumber),
            ?ERROR_MSG(ExitText, []),
            exit_or_halt(ExitText);
        {error, Reason} ->
            ExitText = describe_config_problem(File, Reason),
            ?ERROR_MSG(ExitText, []),
            exit_or_halt(ExitText)
    end.


%% @doc Convert configuration filename to absolute path.
%% Input is an absolute or relative path to an ejabberd configuration file.
%% And returns an absolute path to the configuration file.
-spec get_absolute_path(string()) -> string().
get_absolute_path(File) ->
    case filename:pathtype(File) of
        absolute ->
            File;
        relative ->
            {ok, Cwd} = file:get_cwd(),
            filename:absname_join(Cwd, File)
    end.


-spec search_hosts({host|hosts, [host()] | host()}, state()) -> any().
search_hosts(Term, State) ->
    case Term of
        {host, Host} ->
            if
                State#state.hosts == [] ->
                    add_hosts_to_option([Host], State);
                true ->
                    ?ERROR_MSG("Can't load config file: "
                               "too many hosts definitions", []),
                    exit("too many hosts definitions")
            end;
        {hosts, Hosts} ->
            if
                State#state.hosts == [] ->
                    add_hosts_to_option(Hosts, State);
                true ->
                    ?ERROR_MSG("Can't load config file: "
                               "too many hosts definitions", []),
                    exit("too many hosts definitions")
            end;
        _ ->
            State
    end.


-spec add_hosts_to_option(Hosts :: [host()],
                          State :: state()) -> state().
add_hosts_to_option(Hosts, State) ->
    PrepHosts = normalize_hosts(Hosts),
    add_option(hosts, PrepHosts, State#state{hosts = PrepHosts}).


-spec normalize_hosts([host()]) -> [binary() | tuple()].
normalize_hosts(Hosts) ->
    normalize_hosts(Hosts,[]).


normalize_hosts([], PrepHosts) ->
    lists:reverse(PrepHosts);
normalize_hosts([Host|Hosts], PrepHosts) ->
    case jlib:nodeprep(list_to_binary(Host)) of
        error ->
            ?ERROR_MSG("Can't load config file: "
                       "invalid host name [~p]", [Host]),
            exit("invalid hostname");
        PrepHost ->
            normalize_hosts(Hosts, [PrepHost|PrepHosts])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Errors reading the config file

-type config_problem() :: atom() | {integer(),atom() | tuple(),_}. % spec me better
-type config_line() :: [[any()] | non_neg_integer(),...]. % spec me better

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
    Lines = get_config_lines(Filename, LineNumber, 10, 3),
    ?ERROR_MSG("The following lines from your configuration file might be"
               " relevant to the error: ~n~s", [Lines]),
    ExitText.


-spec get_config_lines(Filename :: string(),
                       TargetNumber :: integer(),
                       PreContext :: 10,
                       PostContext :: 3) -> [config_line()].
get_config_lines(Filename, TargetNumber, PreContext, PostContext) ->
    {ok, Fd} = file:open(Filename, [read]),
    LNumbers = lists:seq(TargetNumber-PreContext, TargetNumber+PostContext),
    NextL = io:get_line(Fd, no_prompt),
    R = get_config_lines2(Fd, NextL, 1, LNumbers, []),
    file:close(Fd),
    R.


get_config_lines2(_Fd, eof, _CurrLine, _LNumbers, R) ->
    lists:reverse(R);
get_config_lines2(_Fd, _NewLine, _CurrLine, [], R) ->
    lists:reverse(R);
get_config_lines2(Fd, Data, CurrLine, [NextWanted | LNumbers], R) when is_list(Data) ->
    NextL = io:get_line(Fd, no_prompt),
    if
        CurrLine >= NextWanted ->
            Line2 = [integer_to_list(CurrLine), ": " | Data],
            get_config_lines2(Fd, NextL, CurrLine+1, LNumbers, [Line2 | R]);
        true ->
            get_config_lines2(Fd, NextL, CurrLine+1, [NextWanted | LNumbers], R)
    end.


%% @doc If ejabberd isn't yet running in this node, then halt the node
-spec exit_or_halt(ExitText :: string()) -> none().
exit_or_halt(ExitText) ->
    case [Vsn || {ejabberd, _Desc, Vsn} <- application:which_applications()] of
        [] ->
            timer:sleep(1000),
            halt(string:substr(ExitText, 1, 199));
        [_] ->
            exit(ExitText)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Support for 'include_config_file'

%% @doc Include additional configuration files in the list of terms.
-spec include_config_files([term()]) -> [term()].
include_config_files(Terms) ->
    include_config_files(Terms, []).


include_config_files([], Res) ->
    Res;
include_config_files([{include_config_file, Filename} | Terms], Res) ->
    include_config_files([{include_config_file, Filename, []} | Terms], Res);
include_config_files([{include_config_file, Filename, Options} | Terms], Res) ->
    Included_terms = get_plain_terms_file(Filename),
    Disallow = proplists:get_value(disallow, Options, []),
    Included_terms2 = delete_disallowed(Disallow, Included_terms),
    Allow_only = proplists:get_value(allow_only, Options, all),
    Included_terms3 = keep_only_allowed(Allow_only, Included_terms2),
    include_config_files(Terms, Res ++ Included_terms3);
include_config_files([Term | Terms], Res) ->
    include_config_files(Terms, Res ++ [Term]).


%% @doc Filter from the list of terms the disallowed.
%% Returns a sublist of Terms without the ones which first element is
%% included in Disallowed.
-spec delete_disallowed(Disallowed :: [atom()],
                        Terms :: [term()]) -> [term()].
delete_disallowed(Disallowed, Terms) ->
    lists:foldl(
      fun(Dis, Ldis) ->
              delete_disallowed2(Dis, Ldis)
      end,
      Terms,
      Disallowed).


delete_disallowed2(Disallowed, [H|T]) ->
    case element(1, H) of
        Disallowed ->
            ?WARNING_MSG("The option '~p' is disallowed, "
                         "and will not be accepted", [Disallowed]),
            delete_disallowed2(Disallowed, T);
        _ ->
            [H|delete_disallowed2(Disallowed, T)]
    end;
delete_disallowed2(_, []) ->
    [].


%% @doc Keep from the list only the allowed terms.
%% Returns a sublist of Terms with only the ones which first element is
%% included in Allowed.
-spec keep_only_allowed(Allowed :: [atom()],
                        Terms::[term()]) -> [term()].
keep_only_allowed(all, Terms) ->
    Terms;
keep_only_allowed(Allowed, Terms) ->
    {As, NAs} = lists:partition(
                  fun(Term) ->
                          lists:member(element(1, Term), Allowed)
                  end,
                  Terms),
    [?WARNING_MSG("This option is not allowed, "
                  "and will not be accepted:~n~p", [NA])
     || NA <- NAs],
    As.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Support for Macro

%% @doc Replace the macros with their defined values.
-spec replace_macros(Terms :: [term()]) -> [term()].
replace_macros(Terms) ->
    {TermsOthers, Macros} = split_terms_macros(Terms),
    replace(TermsOthers, Macros).


%% @doc Split Terms into normal terms and macro definitions.
-spec split_terms_macros(Terms :: [term()]) -> {[term()], [macro()]}.
split_terms_macros(Terms) ->
    lists:foldl(
      fun(Term, {TOs, Ms}) ->
              case Term of
                  {define_macro, Key, Value} ->
                      case is_atom(Key) and is_all_uppercase(Key) of
                          true ->
                              {TOs, Ms++[{Key, Value}]};
                          false ->
                              exit({macro_not_properly_defined, Term})
                      end;
                  Term ->
                      {TOs ++ [Term], Ms}
              end
      end,
      {[], []},
      Terms).


%% @doc Recursively replace in Terms macro usages with the defined value.
-spec replace(Terms :: [term()],
              Macros :: [macro()]) -> [term()].
replace([], _) ->
    [];
replace([Term|Terms], Macros) ->
    [replace_term(Term, Macros) | replace(Terms, Macros)].


replace_term(Key, Macros) when is_atom(Key) ->
    case is_all_uppercase(Key) of
        true ->
            case proplists:get_value(Key, Macros) of
                undefined -> exit({undefined_macro, Key});
                Value -> Value
            end;
        false ->
            Key
    end;
replace_term({use_macro, Key, Value}, Macros) ->
    proplists:get_value(Key, Macros, Value);
replace_term(Term, Macros) when is_list(Term) ->
    replace(Term, Macros);
replace_term(Term, Macros) when is_tuple(Term) ->
    List = tuple_to_list(Term),
    List2 = replace(List, Macros),
    list_to_tuple(List2);
replace_term(Term, _) ->
    Term.


-spec is_all_uppercase(atom()) -> boolean().
is_all_uppercase(Atom) ->
    String = erlang:atom_to_list(Atom),
    lists:all(fun(C) when C >= $a, C =< $z -> false;
                 (_) -> true
              end, String).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Process terms

-spec process_term(Term :: known_term(),
                   State :: state()) -> state().
process_term(Term, State) ->
    case Term of
        override_global ->
            State#state{override_global = true};
        override_local ->
            State#state{override_local = true};
        override_acls ->
            State#state{override_acls = true};
        {acl, _ACLName, _ACLData} ->
            process_host_term(Term, global, State);
        {alarms, Env} ->
            add_option(alarms, Env, State);
        {access, _RuleName, _Rules} ->
            process_host_term(Term, global, State);
        {shaper, _Name, _Data} ->
            %%lists:foldl(fun(Host, S) -> process_host_term(Term, Host, S) end,
            %%    	State, State#state.hosts);
            process_host_term(Term, global, State);
        {host, _Host} ->
            State;
        {hosts, _Hosts} ->
            State;
        {host_config, Host, Terms} ->
            lists:foldl(fun(T, S) ->
                            process_host_term(T, list_to_binary(Host), S) end,
                        State, Terms);
        {listen, Listeners} ->
            Listeners2 =
                lists:map(
                  fun({PortIP, Module, Opts}) ->
                          {Port, IPT, _, _, Proto, OptsClean} =
                              ejabberd_listener:parse_listener_portip(PortIP, Opts),
                          {{Port, IPT, Proto}, Module, OptsClean}
                  end,
                  Listeners),
            add_option(listen, Listeners2, State);
        {language, Val} ->
            add_option(language, Val, State);
        {sm_backend, Val} ->
            add_option(sm_backend, Val, State);
        {outgoing_s2s_port, Port} ->
            add_option(outgoing_s2s_port, Port, State);
        {outgoing_s2s_options, Methods, Timeout} ->
            add_option(outgoing_s2s_options, {Methods, Timeout}, State);
        {{s2s_addr, Host}, Addr} ->
            add_option({s2s_addr, list_to_binary(Host)}, Addr, State);
        {s2s_dns_options, PropList} ->
            add_option(s2s_dns_options, PropList, State);
        {s2s_use_starttls, Port} ->
            add_option(s2s_use_starttls, Port, State);
        {s2s_ciphers, Ciphers} ->
            add_option(s2s_ciphers, Ciphers, State);
        {s2s_certfile, CertFile} ->
            case ejabberd_config:is_file_readable(CertFile) of
                true -> add_option(s2s_certfile, CertFile, State);
                false ->
                    ErrorText = "There is a problem in the configuration: "
                        "the specified file is not readable: ",
                    throw({error, ErrorText ++ CertFile})
            end;
        {domain_certfile, Domain, CertFile} ->
            case ejabberd_config:is_file_readable(CertFile) of
                true -> add_option({domain_certfile, Domain}, CertFile, State);
                false ->
                    ErrorText = "There is a problem in the configuration: "
                        "the specified file is not readable: ",
                    throw({error, ErrorText ++ CertFile})
            end;
        {node_type, NodeType} ->
            add_option(node_type, NodeType, State);
        {cluster_nodes, Nodes} ->
            add_option(cluster_nodes, Nodes, State);
        {watchdog_admins, Admins} ->
            add_option(watchdog_admins, Admins, State);
        {watchdog_large_heap, LH} ->
            add_option(watchdog_large_heap, LH, State);
        {registration_timeout, Timeout} ->
            add_option(registration_timeout, Timeout, State);
        {mongooseimctl_access_commands, ACs} ->
            add_option(mongooseimctl_access_commands, ACs, State);
        {loglevel, Loglevel} ->
            ejabberd_loglevel:set(Loglevel),
            State;
        {max_fsm_queue, N} ->
            add_option(max_fsm_queue, N, State);
        {_Opt, _Val} ->
            lists:foldl(fun(Host, S) -> process_host_term(Term, Host, S) end,
                        State, State#state.hosts)
    end.


-spec process_host_term(Term :: host_term(),
                        Host :: acl:host(),
                        State :: state()) -> state().
process_host_term(Term, Host, State) ->
    case Term of
        {acl, ACLName, ACLData} ->
            State#state{opts =
                    [acl:to_record(Host, ACLName, ACLData) | State#state.opts]};
        {access, RuleName, Rules} ->
            State#state{opts = [#config{key = {access, RuleName, Host},
                                        value = Rules} |
                                State#state.opts]};
        {shaper, Name, Data} ->
            State#state{opts = [#config{key = {shaper, Name, Host},
                                        value = Data} |
                                State#state.opts]};
        {host, Host} ->
            State;
        {hosts, _Hosts} ->
            State;
        {odbc_server, ODBC_server} ->
            add_option({odbc_server, Host}, ODBC_server, State);
        {Opt, Val} ->
            add_option({Opt, Host}, Val, State)
    end.


-spec add_option(Opt :: hosts | language | sm_backend,
                 Val :: term(),
                 State :: state()) -> state().
add_option(Opt, Val, State) ->
    Table = case Opt of
                hosts ->
                    config;
                language ->
                    config;
                sm_backend ->
                    config;
                _ ->
                    local_config
            end,
    case Table of
        config ->
            State#state{opts = [#config{key = Opt, value = Val} |
                                State#state.opts]};
        local_config ->
            case Opt of
                {{add, OptName}, Host} ->
                    State#state{opts = compact({OptName, Host}, Val,
                                               State#state.opts, [])};
                _ ->
                    State#state{opts = [#local_config{key = Opt, value = Val} |
                                        State#state.opts]}
            end
    end.


compact({OptName, Host} = Opt, Val, [], Os) ->
    ?WARNING_MSG("The option '~p' is defined for the host ~p using host_config "
                 "before the global '~p' option. This host_config option may "
                 "get overwritten.", [OptName, Host, OptName]),
    [#local_config{key = Opt, value = Val}] ++ Os;
%% Traverse the list of the options already parsed
compact(Opt, Val, [O | Os1], Os2) ->
    case catch O#local_config.key of
        %% If the key of a local_config matches the Opt that wants to be added
        Opt ->
            %% Then prepend the new value to the list of old values
            Os2 ++ [#local_config{key = Opt,
                                  value = Val++O#local_config.value}
                   ] ++ Os1;
        _ ->
            compact(Opt, Val, Os1, Os2++[O])
    end.


-spec set_opts(state()) -> 'ok' | none().
set_opts(State) ->
    Opts = lists:reverse(State#state.opts),
    F = fun() ->
                if
                    State#state.override_global ->
                        Ksg = mnesia:all_keys(config),
                        lists:foreach(fun(K) ->
                                              mnesia:delete({config, K})
                                      end, Ksg);
                    true ->
                        ok
                end,
                if
                    State#state.override_local ->
                        Ksl = mnesia:all_keys(local_config),
                        lists:foreach(fun(K) ->
                                              mnesia:delete({local_config, K})
                                      end, Ksl);
                    true ->
                        ok
                end,
                if
                    State#state.override_acls ->
                        Ksa = mnesia:all_keys(acl),
                        lists:foreach(fun(K) ->
                                              mnesia:delete({acl, K})
                                      end, Ksa);
                    true ->
                        ok
                end,
                lists:foreach(fun(R) ->
                                      mnesia:write(R)
                              end, Opts)
        end,
    case mnesia:transaction(F) of
        {atomic, _} -> ok;
        {aborted,{no_exists,Table}} ->
            MnesiaDirectory = mnesia:system_info(directory),
            ?ERROR_MSG("Error reading Mnesia database spool files:~n"
                       "The Mnesia database couldn't read the spool file for the table '~p'.~n"
                       "ejabberd needs read and write access in the directory:~n   ~s~n"
                       "Maybe the problem is a change in the computer hostname,~n"
                       "or a change in the Erlang node name, which is currently:~n   ~p~n"
                       "Check the ejabberd guide for details about changing the~n"
                       "computer hostname or Erlang node name.~n",
                       [Table, MnesiaDirectory, node()]),
            exit("Error reading Mnesia database")
    end.


-spec add_global_option(Opt :: key(), Val :: value()) -> {atomic|aborted, _}.
add_global_option(Opt, Val) ->
    mnesia:transaction(fun() ->
                               mnesia:write(#config{key = Opt,
                                                    value = Val})
                       end).


-spec add_local_option(Opt :: key(), Val :: value()) -> {atomic|aborted, _}.
add_local_option(Opt, Val) ->
    mnesia:transaction(fun() ->
                               mnesia:write(#local_config{key = Opt,
                                                          value = Val})
                       end).


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


%% @doc Return the list of hosts handled by a given module
get_vh_by_auth_method(AuthMethod) ->
    mnesia:dirty_select(local_config,
                        [{#local_config{key = {auth_method, '$1'},
                                        value=AuthMethod},[],['$1']}]).


-spec is_file_readable(Path :: string()) -> boolean().
is_file_readable(Path) ->
    case file:read_file_info(Path) of
        {ok, FileInfo} ->
            case {FileInfo#file_info.type, FileInfo#file_info.access} of
                {regular, read} -> true;
                {regular, read_write} -> true;
                _ -> false
            end;
        {error, _Reason} ->
            false
    end.
