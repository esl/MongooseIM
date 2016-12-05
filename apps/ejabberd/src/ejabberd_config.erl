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
-export([is_file_readable/1]).

%% for unit tests
-export([check_hosts/2,
         compare_modules/2,
         compare_listeners/2,
         group_host_changes/1]).

%% conf reload
-export([reload_local/0,
         reload_cluster/0,
         apply_changes_remote/4,
         apply_changes/5]).

-export([compute_config_version/2,
         get_local_config/0,
         get_host_local_config/0]).

-include("ejabberd.hrl").
-include("ejabberd_config.hrl").
-include_lib("kernel/include/file.hrl").

-define(CONFIG_RELOAD_TIMEOUT, 30000).

-type key() :: atom()
             | {key(), ejabberd:server() | atom() | list()}
             | {atom(), atom(), atom()}
             | binary(). % TODO: binary is questionable here

-type value() :: atom()
               | binary()
               | integer()
               | string()
               | [value()]
               | tuple().

-export_type([key/0, value/0]).

-record(state, {opts = [] :: list(),
                hosts = [] :: [host()],
                override_local = false :: boolean(),
                override_global = false :: boolean(),
                override_acls = false :: boolean()}).

-record(compare_result, {to_start = [] :: list(),
                         to_stop = [] :: list(),
                         to_reload = [] :: list()}).

-type compare_result() :: #compare_result{}.

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
                    | {registration_timeout, integer()}
                    | {mongooseimctl_access_commands, list()}
                    | {loglevel, _}
                    | {max_fsm_queue, _}
                    | {sasl_mechanisms, _}
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
    add_local_option(node_start, now()),
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

    application:get_env(ejabberd, config, DefaultPath).

%% @doc Load the ejabberd configuration file.
%% It also includes additional configuration files and replaces macros.
%% This function will crash if finds some error in the configuration file.
-spec load_file(File :: string()) -> ok.
load_file(File) ->
    Terms = get_plain_terms_file(File),
    State = lists:foldl(fun search_hosts/2, #state{}, Terms),
    TermsMacros = replace_macros(Terms),
    Res = lists:foldl(fun process_term/2, State, TermsMacros),
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
            case State of
                #state{hosts = []} ->
                    add_hosts_to_option([Host], State);
                _ ->
                    ?ERROR_MSG("Can't load config file: "
                               "too many hosts definitions", []),
                    exit("too many hosts definitions")
            end;
        {hosts, Hosts} ->
            case State of
                #state{hosts = []} ->
                    add_hosts_to_option(Hosts, State);
                _ ->
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
    normalize_hosts(Hosts, []).


normalize_hosts([], PrepHosts) ->
    lists:reverse(PrepHosts);
normalize_hosts([Host | Hosts], PrepHosts) ->
    case jid:nodeprep(host_to_binary(Host)) of
        error ->
            ?ERROR_MSG("Can't load config file: "
                       "invalid host name [~p]", [Host]),
            exit("invalid hostname");
        PrepHost ->
            normalize_hosts(Hosts, [PrepHost | PrepHosts])
    end.

host_to_binary(Host) ->
    unicode:characters_to_binary(Host).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Errors reading the config file

-type config_problem() :: atom() | {integer(), atom() | tuple(), _}. % spec me better
-type config_line() :: [[any()] | non_neg_integer(), ...]. % spec me better

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
    LNumbers = lists:seq(TargetNumber - PreContext, TargetNumber + PostContext),
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
            get_config_lines2(Fd, NextL, CurrLine + 1, LNumbers, [Line2 | R]);
        true ->
            get_config_lines2(Fd, NextL, CurrLine + 1, [NextWanted | LNumbers], R)
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
    IncludedTerms = get_plain_terms_file(Filename),
    Disallow = proplists:get_value(disallow, Options, []),
    IncludedTerms2 = delete_disallowed(Disallow, IncludedTerms),
    AllowOnly = proplists:get_value(allow_only, Options, all),
    IncludedTerms3 = keep_only_allowed(AllowOnly, IncludedTerms2),
    include_config_files(Terms, Res ++ IncludedTerms3);
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


delete_disallowed2(Disallowed, [H | T]) ->
    case element(1, H) of
        Disallowed ->
            ?WARNING_MSG("The option '~p' is disallowed, "
                         "and will not be accepted", [Disallowed]),
            delete_disallowed2(Disallowed, T);
        _ ->
            [H | delete_disallowed2(Disallowed, T)]
    end;
delete_disallowed2(_, []) ->
    [].


%% @doc Keep from the list only the allowed terms.
%% Returns a sublist of Terms with only the ones which first element is
%% included in Allowed.
-spec keep_only_allowed(Allowed :: [atom()],
                        Terms :: [term()]) -> [term()].
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
                              {TOs, Ms ++ [{Key, Value}]};
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
replace([Term | Terms], Macros) ->
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
            %%          State, State#state.hosts);
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
            add_option(language, list_to_binary(Val), State);
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
        {routing_modules, Mods} ->
            add_option(routing_modules, Mods, State);
        {loglevel, Loglevel} ->
            ejabberd_loglevel:set(Loglevel),
            State;
        {max_fsm_queue, N} ->
            add_option(max_fsm_queue, N, State);
        {sasl_mechanisms, Mechanisms} ->
            add_option(sasl_mechanisms, Mechanisms, State);
        {http_connections, HttpConnections} ->
            add_option(http_connections, HttpConnections, State);
        {all_metrics_are_global, Value} ->
            add_option(all_metrics_are_global, Value, State);
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
            State#state{opts = [#config{key   = {access, RuleName, Host},
                                        value = Rules} |
                                State#state.opts]};
        {shaper, Name, Data} ->
            State#state{opts = [#config{key   = {shaper, Name, Host},
                                        value = Data} |
                                State#state.opts]};
        {host, Host} ->
            State;
        {hosts, _Hosts} ->
            State;
        {odbc_server, ODBCServer} ->
            add_option({odbc_server, Host}, ODBCServer, State);
        {riak_server, RiakConfig} ->
            add_option(riak_server, RiakConfig, State);
        {cassandra_servers, CassandraConfig} ->
            add_option(cassandra_servers, CassandraConfig, State);
        {Opt, Val} ->
            add_option({Opt, Host}, Val, State)
    end.


-spec add_option(Opt :: key(),
                 Val :: value(),
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
                                  value = Val ++ O#local_config.value}
                   ] ++ Os1;
        _ ->
            compact(Opt, Val, Os1, Os2 ++ [O])
    end.


-spec set_opts(state()) -> 'ok' | none().
set_opts(State) ->
    Opts = lists:reverse(State#state.opts),
    F = fun() ->
                case State of
                    #state{override_global = true} ->
                        Ksg = mnesia:all_keys(config),
                        lists:foreach(fun(K) ->
                                              mnesia:delete({config, K})
                                      end, Ksg);
                    _ ->
                        ok
                end,
                case State of
                    #state{override_local = true} ->
                        Ksl = mnesia:all_keys(local_config),
                        lists:foreach(fun(K) ->
                                              mnesia:delete({local_config, K})
                                      end, lists:delete(node_start, Ksl));
                    _ ->
                        ok
                end,
                case State of
                    #state{override_acls = true} ->
                        Ksa = mnesia:all_keys(acl),
                        lists:foreach(fun(K) ->
                                              mnesia:delete({acl, K})
                                      end, Ksa);
                    _ ->
                        ok
                end,
                lists:foreach(fun(R) ->
                                      mnesia:write(R)
                              end, Opts)
        end,
    case mnesia:transaction(F) of
        {atomic, _} -> ok;
        {aborted, {no_exists, Table}} ->
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

%%--------------------------------------------------------------------
%% Configuration reload
%%--------------------------------------------------------------------
-spec parse_file(file:name()) -> state().
parse_file(ConfigFile) ->
    Terms = get_plain_terms_file(ConfigFile),
    State = lists:foldl(fun search_hosts/2, #state{}, Terms),
    TermsWExpandedMacros = replace_macros(Terms),
    lists:foldl(fun process_term/2, State, TermsWExpandedMacros).

-spec reload_local() -> {ok, iolist()} | no_return().
reload_local() ->
    ConfigFile = get_ejabberd_config_path(),
    State0 = parse_file(ConfigFile),
    {CC, LC, LHC} = get_config_diff(State0),
    ConfigVersion = compute_config_version(get_local_config(),
                                           get_host_local_config()),
    State1 = State0#state{override_global = true,
                          override_local  = true,
                          override_acls   = true},
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

    ConfigVersion = compute_config_version(get_local_config(),
                                           get_host_local_config()),
    FileVersion = compute_config_file_version(State0),
    ?WARNING_MSG("cluster config reload from ~s scheduled", [ConfigFile]),
    %% first apply on local
    State1 = State0#state{override_global = true,
                          override_local  = true, override_acls = true},
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
                                                % New Options
    {NewConfig, NewLocal, NewHostsLocal} = categorize_options(State#state.opts),
                                                % Current Options
    Config = get_global_config(),
    Local = get_local_config(),
    HostsLocal = get_host_local_config(),
    %% global config diff
    CC = compare_terms(Config, NewConfig, 2, 3),
    LC = compare_terms(Local, NewLocal, 2, 3),
    LHC = compare_terms(group_host_changes(HostsLocal), group_host_changes(NewHostsLocal), 1, 2),
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
    case compute_config_file_version(State0) of
        DesiredFileVersion ->
            State1 = State0#state{override_global = false,
                                  override_local  = true,
                                  override_acls   = false},
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
    ConfigVersion = compute_config_version(get_local_config(),
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
    {ToDel, ToAdd} = check_hosts(NewHosts, OldHosts),
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
    case can_be_ignored(Key) of
        true ->
            ok;
        false ->
            ?WARNING_MSG("local config add ~p option unhandled", [El])
    end.

handle_local_config_del(#local_config{key = riak_server}) ->
    mongoose_riak:stop();
handle_local_config_del(#local_config{key = cassandra_servers}) ->
    mongoose_cassandra:start();
handle_local_config_del(#local_config{key = node_start}) ->
    %% do nothing with it
    ok;
handle_local_config_del(#local_config{key = Key} = El) ->
    case can_be_ignored(Key) of
        true ->
            ok;
        false ->
            ?WARNING_MSG("local config change: ~p unhandled", [El])
    end.

handle_local_config_change({listen, Old, New}) ->
    reload_listeners(compare_listeners(Old, New));
handle_local_config_change({riak_server, _Old, _New}) ->
    mongoose_riak:stop(),
    mongoose_riak:start(),
    ok;
handle_local_config_change({cassandra_servers, _Old, _New}) ->
    mongoose_cassandra:stop(),
    mongoose_cassandra:start(),
    ok;
handle_local_config_change({Key, _Old, _New} = El) ->
    case can_be_ignored(Key) of
        true ->
            ok;
        false ->
            ?WARNING_MSG("local config change: ~p unhandled", [El])
    end.

%% ----------------------------------------------------------------
%% LOCAL HOST CONFIG
%% ----------------------------------------------------------------

handle_local_hosts_config_add({{auth, Host}, _}) ->
    ejabberd_auth:start(Host);
handle_local_hosts_config_add({{odbc, Host}, _}) ->
    ejabberd_rdbms:start(Host);
handle_local_hosts_config_add({{ldap, _Host}, _}) ->
    %% ignore ldap section
    ok;
handle_local_hosts_config_add({{modules, Host}, Modules}) ->
    lists:foreach(
      fun({Module, Args}) ->
              gen_mod:start_module(Host, Module, Args)
      end, Modules);
handle_local_hosts_config_add({{Key, _Host}, _} = El) ->
    case can_be_ignored(Key) of
        true ->
            ok;
        false ->
            ?WARNING_MSG("local hosts config add option: ~p unhandled", [El])
    end.

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
handle_local_hosts_config_del({{odbc, Host}, _}) ->
    ejabberd_rdbms:stop_odbc(Host);
handle_local_hosts_config_del({{ldap, _Host}, _I}) ->
    %% ignore ldap section, only appli
    ok;
handle_local_hosts_config_del({{modules, Host}, Modules}) ->
    lists:foreach(
      fun({Module, _Args}) ->
              gen_mod:stop_module(Host, Module)
      end, Modules);
handle_local_hosts_config_del({{Key, _}, _} = El) ->
    case can_be_ignored(Key) of
        true ->
            ok;
        false ->
            ?WARNING_MSG("local hosts config delete option: ~p unhandled", [El])
    end.

handle_local_hosts_config_change({{odbc, Host}, Old, _}) ->
    %% stop rdbms
    case lists:keyfind({odbc_server, Host}, 1, Old) of
        false ->
            ok;
        #local_config{} ->
            ejabberd_rdbms:stop_odbc(Host)
    end,
    ejabberd_rdbms:start(Host);
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
    Res = compare_modules(OldModules, NewModules),
    reload_modules(Host, Res);
handle_local_hosts_config_change({{Key, _Host}, _Old, _New} = El) ->
    case can_be_ignored(Key) of
        true ->
            ok;
        false ->
            ?WARNING_MSG("local hosts config change option: ~p unhandled", [El])
    end.

methods_to_auth_modules(L) when is_list(L) ->
    [list_to_atom("ejabberd_auth_" ++ atom_to_list(M)) || M <- L];
methods_to_auth_modules(A) when is_atom(A) ->
    methods_to_auth_modules([A]).


compute_config_version(LC, LCH) ->
    L0 = lists:filter(mk_node_start_filter(), LC ++ LCH),
    L1 = sort_config(L0),
    crypto:hash(sha, term_to_binary(L1)).

compute_config_file_version(#state{opts = Opts, hosts = Hosts}) ->
    L = sort_config(Opts ++ Hosts),
    crypto:hash(sha, term_to_binary(L)).

-spec check_hosts([ejabberd:server()], [ejabberd:server()]) -> {[ejabberd:server()],
                                                                [ejabberd:server()]}.
check_hosts(NewHosts, OldHosts) ->
    Old = sets:from_list(OldHosts),
    New = sets:from_list(NewHosts),
    ListToAdd = sets:to_list(sets:subtract(New, Old)),
    ListToDel = sets:to_list(sets:subtract(Old, New)),
    {ListToDel, ListToAdd}.

-spec add_virtual_host(Host :: ejabberd:server()) -> any().
add_virtual_host(Host) ->
    ?DEBUG("Register host:~p", [Host]),
    ejabberd_local:register_host(Host).

-spec can_be_ignored(Key :: atom()) -> boolean().
can_be_ignored(Key) when is_atom(Key) ->
    L = [domain_certfile, s2s, all_metrics_are_global],
    lists:member(Key, L).

-spec remove_virtual_host(ejabberd:server()) -> any().
remove_virtual_host(Host) ->
    ?DEBUG("Unregister host :~p", [Host]),
    ejabberd_local:unregister_host(Host).

-spec reload_modules(Host :: ejabberd:server(),
                     ChangedModules :: compare_result()) -> 'ok'.
reload_modules(Host, #compare_result{to_start  = Start, to_stop = Stop,
                                     to_reload = Reload} = ChangedModules) ->
    ?DEBUG("reload modules: ~p", [lager:pr(ChangedModules, ?MODULE)]),
    lists:foreach(fun({M, _}) ->
                          gen_mod:stop_module(Host, M)
                  end, Stop),
    lists:foreach(fun({M, Args}) ->
                          gen_mod:start_module(Host, M, Args)
                  end, Start),
    lists:foreach(fun({M, _, Args}) ->
                          gen_mod:reload_module(Host, M, Args)
                  end, Reload).

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

-spec compare_modules(term(), term()) -> compare_result().
compare_modules(OldMods, NewMods) ->
    compare_terms(OldMods, NewMods, 1, 2).

-spec compare_listeners(term(), term()) -> compare_result().
compare_listeners(OldListeners, NewListeners) ->
    compare_terms(map_listeners(OldListeners), map_listeners(NewListeners), 1, 2).

map_listeners(Listeners) ->
    lists:map(fun({PortIP, Module, Opts}) ->
                      {{PortIP, Module}, Opts}
              end, Listeners).

                                                % group values which can be grouped like odbc ones
-spec group_host_changes([term()]) -> [{atom(), [term()]}].
group_host_changes(Changes) when is_list(Changes) ->
    D = lists:foldl(fun(#local_config{key = {Key, Host}, value = Val}, Dict) ->
                            BKey = atom_to_binary(Key, utf8),
                            case get_key_group(BKey, Key) of
                                Key ->
                                    dict:append({Key, Host}, Val, Dict);
                                NewKey ->
                                    dict:append({NewKey, Host}, {Key, Val}, Dict)
                            end
                    end, dict:new(), Changes),
    [{Group, lists:sort(lists:flatten(MaybeDeepList))}
     || {Group, MaybeDeepList} <- dict:to_list(D)].

%% match all hosts
-spec get_host_local_config() -> [{local_config, {term(), ejabberd:server()}, term()}].
get_host_local_config() ->
    mnesia:dirty_match_object({local_config, {'_', '_'}, '_'}).

-spec get_local_config() -> [{local_config, term(), term()}].
get_local_config() ->
    Keys = lists:filter(fun is_not_host_specific/1, mnesia:dirty_all_keys(local_config)),
    lists:flatten(lists:map(fun(Key) ->
                                    mnesia:dirty_read(local_config, Key)
                            end,
                            Keys)).

-spec get_global_config() -> [{config, term(), term()}].
get_global_config() ->
    mnesia:dirty_match_object(config, {config, '_', '_'}).

-spec is_not_host_specific(atom() | {atom(), ejabberd:server()}) -> boolean().
is_not_host_specific(Key) when is_atom(Key) ->
    true;
is_not_host_specific({Key, Host}) when is_atom(Key), is_binary(Host) ->
    false.

-spec categorize_options([term()]) -> {GlobalConfig, LocalConfig, HostsConfig} when
      GlobalConfig :: list(),
      LocalConfig :: list(),
      HostsConfig :: list().
categorize_options(Opts) ->
    lists:foldl(fun({config, _, _} = El, Acc) ->
                        as_global(El, Acc);
                   ({local_config, {Key, Host}, _} = El, Acc)
                      when is_atom(Key), is_binary(Host) ->
                        as_hosts(El, Acc);
                   ({local_config, _, _} = El, Acc) ->
                        as_local(El, Acc);
                   ({acl, _, _}, R) ->
                        %% no need to do extra work here
                        R;
                   (R, R2) ->
                        ?ERROR_MSG("not matched ~p", [R]),
                        R2
                end, {[], [], []}, Opts).

as_global(El, {Config, Local, HostLocal}) -> {[El | Config], Local, HostLocal}.
as_local(El, {Config, Local, HostLocal}) -> {Config, [El | Local], HostLocal}.
as_hosts(El, {Config, Local, HostLocal}) -> {Config, Local, [El | HostLocal]}.

-spec get_key_group(binary(), atom()) -> atom().
get_key_group(<<"ldap_", _/binary>>, _) ->
    ldap;
get_key_group(<<"odbc_", _/binary>>, _) ->
    odbc;
get_key_group(<<"pgsql_", _/binary>>, _) ->
    odbc;
get_key_group(<<"auth_", _/binary>>, _) ->
    auth;
get_key_group(<<"ext_auth_", _/binary>>, _) ->
    auth;
get_key_group(<<"s2s_", _/binary>>, _) ->
    s2s;
get_key_group(_, Key) when is_atom(Key) ->
    Key.

-spec compare_terms(OldTerms :: [tuple()],
                    NewTerms :: [tuple()],
                    KeyPos :: non_neg_integer(),
                    ValuePos :: non_neg_integer()) -> compare_result().
compare_terms(OldTerms, NewTerms, KeyPos, ValuePos)
  when is_integer(KeyPos), is_integer(ValuePos) ->
    {ToStop, ToReload} = lists:foldl(pa:bind(fun find_modules_to_change/5,
                                             KeyPos, NewTerms, ValuePos),
                                     {[], []}, OldTerms),
    ToStart = lists:foldl(pa:bind(fun find_modules_to_start/4,
                                  KeyPos, OldTerms), [], NewTerms),
    #compare_result{to_start  = ToStart,
                    to_stop   = ToStop,
                    to_reload = ToReload}.

find_modules_to_start(KeyPos, OldTerms, Element, ToStart) ->
    case lists:keyfind(element(KeyPos, Element), KeyPos, OldTerms) of
        false -> [Element | ToStart];
        _ -> ToStart
    end.

find_modules_to_change(KeyPos, NewTerms, ValuePos,
                       Element, {ToStop, ToReload}) ->
    case lists:keyfind(element(KeyPos, Element), KeyPos, NewTerms) of
        false ->
            {[Element | ToStop], ToReload};
        NewElement ->
            OldVal = element(ValuePos, Element),
            NewVal = element(ValuePos, NewElement),
            case OldVal == NewVal of
                true ->
                    {ToStop, ToReload};
                false ->
                    %% add also old value
                    {ToStop,
                     [{element(KeyPos, Element), OldVal, NewVal} | ToReload]}
            end
    end.

mk_node_start_filter() ->
    fun(#local_config{key = node_start}) ->
            false;
       (_) ->
            true
    end.

sort_config(Config) when is_list(Config) ->
    L = lists:map(fun(ConfigItem) when is_list(ConfigItem) ->
                          sort_config(ConfigItem);
                     (ConfigItem) when is_tuple(ConfigItem) ->
                          sort_config(tuple_to_list(ConfigItem));
                     (ConfigItem) ->
                          ConfigItem
                  end, Config),
    lists:sort(L).

