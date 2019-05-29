%% @doc Pure config logic.
%% No ets table manipulations, no Mnesia, no starting modules, no file reading here.
%% Everything here is safe, side effect free.
%% OK, logging is possible, but keep it to minimum.
-module(mongoose_config_parser).
-export([parse_terms/1]).
-export([check_hosts/2]).
-export([can_be_ignored/1]).
-export([is_not_host_specific/1]).

-export([allow_override_all/1,
         allow_override_local_only/1,
         state_to_opts/1,
         state_to_host_opts/1,
         state_to_global_opt/3,
         state_to_required_files/1,
         can_override/2]).

%% for unit tests
-export([group_host_changes/1]).

%% Support for 'include_config_file'
-export([config_filenames_to_include/1,
         include_config_files/2]).

-include("mongoose.hrl").
-include("ejabberd_config.hrl").

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

-export_type([state/0, key/0, value/0]).

-record(state, {opts = [] :: list(),
                hosts = [] :: [host()],
                override_local = false :: boolean(),
                override_global = false :: boolean(),
                override_acls = false :: boolean()}).

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
                   | {hosts, _}.

-spec search_hosts_and_pools({host|hosts, [host()] | host()} , state()) -> any().
search_hosts_and_pools({host, Host}, State) ->
    search_hosts_and_pools({hosts, [Host]}, State);
search_hosts_and_pools({hosts, Hosts}, State=#state{hosts = []}) ->
    add_hosts_to_option(Hosts, State);
search_hosts_and_pools({hosts, Hosts}, #state{hosts = OldHosts}) ->
    ?ERROR_MSG("event=\"too many host definitions\" "
               "new_hosts=~1000p old_hosts=~1000p", []),
    exit(#{issue => "too many hosts definitions",
           new_hosts => Hosts,
           old_hosts => OldHosts});
search_hosts_and_pools(_Term, State) ->
    State.

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
            ?ERROR_MSG("event=invalid_hostname_in_config "
                       "hostname=~p", [Host]),
            erlang:error(#{issue => invalid_hostname,
                           hostname => Host});
        PrepHost ->
            normalize_hosts(Hosts, [PrepHost | PrepHosts])
    end.

host_to_binary(Host) ->
    unicode:characters_to_binary(Host).

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
    lists:foldl(fun split_terms_macros_fold/2, {[], []}, Terms).

-spec split_terms_macros_fold(any(), Acc) -> Acc when
      Acc :: {[term()], [{Key :: any(), Value :: any()}]}.
split_terms_macros_fold({define_macro, Key, Value} = Term, {TOs, Ms}) ->
    case is_macro_name(Key) of
        true ->
            {TOs, Ms ++ [{Key, Value}]};
        false ->
            exit({macro_not_properly_defined, Term})
    end;
split_terms_macros_fold(Term, {TOs, Ms}) ->
    {TOs ++ [Term], Ms}.


%% @doc Recursively replace in Terms macro usages with the defined value.
-spec replace(Terms :: [term()],
              Macros :: [macro()]) -> [term()].
replace([], _) ->
    [];
replace([Term | Terms], Macros) ->
    [replace_term(Term, Macros) | replace(Terms, Macros)].


replace_term(Key, Macros) when is_atom(Key) ->
    case is_macro_name(Key) of
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

%% Check is the term is a config macro
-spec is_macro_name(atom()) -> boolean().
is_macro_name(Atom) when is_atom(Atom) ->
    is_all_uppercase(Atom) andalso has_any_uppercase(Atom);
is_macro_name(_) ->
    false.

-spec is_all_uppercase(atom()) -> boolean().
is_all_uppercase(Atom) ->
    String = erlang:atom_to_list(Atom),
    lists:all(fun(C) when C >= $a, C =< $z -> false;
                 (_) -> true
              end, String).

has_any_uppercase(Atom) ->
    String = erlang:atom_to_list(Atom),
    lists:any(fun(C) when C >= $A, C =< $Z -> true;
                 (_) -> false
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
        {rdbms_server_type, Val} ->
            add_option(rdbms_server_type, Val, State);
        {outgoing_s2s_port, Port} ->
            add_option(outgoing_s2s_port, Port, State);
        {outgoing_s2s_options, Methods, Timeout} ->
            add_option(outgoing_s2s_options, {Methods, Timeout}, State);
        {{s2s_addr, Host}, Addr} ->
            add_option({s2s_addr, list_to_binary(Host)}, Addr, State);
        {{global_distrib_addr, Host}, Addr} ->
            add_option({global_distrib_addr, list_to_binary(Host)}, Addr, State);
        {s2s_dns_options, PropList} ->
            add_option(s2s_dns_options, PropList, State);
        {s2s_use_starttls, Port} ->
            add_option(s2s_use_starttls, Port, State);
        {s2s_ciphers, Ciphers} ->
            add_option(s2s_ciphers, Ciphers, State);
        {s2s_certfile, CertFile} ->
            State2 = compact_global_option(required_files, [CertFile], State),
            add_option(s2s_certfile, CertFile, State2);
        {domain_certfile, Domain, CertFile} ->
            State2 = compact_global_option(required_files, [CertFile], State),
            add_option({domain_certfile, Domain}, CertFile, State2);
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
            add_option(loglevel, Loglevel, State);
        {max_fsm_queue, N} ->
            add_option(max_fsm_queue, N, State);
        {sasl_mechanisms, Mechanisms} ->
            add_option(sasl_mechanisms, Mechanisms, State);
        {all_metrics_are_global, Value} ->
            add_option(all_metrics_are_global, Value, State);
        {cowboy_server_name, Value} ->
            add_option(cowboy_server_name, Value, State);
        {services, Value} ->
            add_option(services, Value, State);
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
            OptRec = acl:to_record(Host, ACLName, ACLData),
            append_option(OptRec, State);
        {access, RuleName, Rules} ->
            append_global_opt({access, RuleName, Host}, Rules, State);
        {shaper, Name, Data} ->
            append_global_opt({shaper, Name, Host}, Data, State);
        {host, Host} ->
            State;
        {hosts, _Hosts} ->
            State;
        {outgoing_pools, Pools} when is_list(Pools) ->
            add_option(outgoing_pools, Pools, State);
        {node_specific_options, NodeOpts} ->
            add_option(node_specific_options, NodeOpts, State);
        {Opt, Val} ->
            add_option({Opt, Host}, Val, State)
    end.

-spec add_option(Opt :: key(),
                 Val :: value(),
                 State :: state()) -> state().
add_option(Opt, Val, State) ->
    Table = opt_table(Opt),
    add_option(Table, Opt, Val, State).

add_option(config, Opt, Val, State) ->
    append_global_opt(Opt, Val, State);
add_option(local_config, {{add, OptName}, Host}, Val, State) ->
    compact_option({OptName, Host}, Val, State);
add_option(local_config, Opt, Val, State) ->
    append_local_opt(Opt, Val, State).

append_global_opt(OptName, OptValue, State) ->
    OptRec = #config{key = OptName, value = OptValue},
    append_option(OptRec, State).

append_local_opt(OptName, OptValue, State) ->
    OptRec = #local_config{key = OptName, value = OptValue},
    append_option(OptRec, State).

append_option(OptRec, State = #state{opts = Opts}) ->
    State#state{ opts = [OptRec | Opts] }.

%% Merges two values of a local option
compact_option(Opt, Val, State) ->
    Opts2 = compact(Opt, Val, State#state.opts, []),
    State#state{opts = Opts2}.

compact({OptName, Host} = Opt, Val, [], Os) ->
    %% The option is defined for host using host_config before the global option.
    %% The host_option can be overwritten.
    %% TODO or maybe not. We need a test.
    ?WARNING_MSG("event=host_config_option_can_be_overwritten "
                 "option_name=~1000p host=~p "
                 "solution=\"define global options before host options\"",
                 [OptName, Host]),
    [#local_config{key = Opt, value = Val}] ++ Os;
%% Traverse the list of the options already parsed
compact(Opt, Val, [#local_config{key = Opt, value = OldVal} | Os1], Os2) ->
    %% If the key of a local_config matches the Opt that wants to be added
    OptRec = #local_config{key = Opt, value = Val ++ OldVal},
    %% Then prepend the new value to the list of old values
    Os2 ++ [OptRec] ++ Os1;
compact(Opt, Val, [O | Os1], Os2) ->
    compact(Opt, Val, Os1, Os2 ++ [O]).


%% Merges two values of a global option
compact_global_option(Opt, Val, State) when is_list(Val) ->
    Opts2 = compact_global(Opt, Val, State#state.opts, []),
    State#state{opts = Opts2}.

compact_global(Opt, Val, [], Os) ->
    [#config{key = Opt, value = Val}] ++ Os;
%% Traverse the list of the options already parsed
compact_global(Opt, Val, [#config{key = Opt, value = OldVal} | Os1], Os2) ->
    %% If the key of a local_config matches the Opt that wants to be added
    OptRec = #config{key = Opt, value = Val ++ OldVal},
    %% Then prepend the new value to the list of old values
    Os2 ++ [OptRec] ++ Os1;
compact_global(Opt, Val, [O | Os1], Os2) ->
    compact_global(Opt, Val, Os1, Os2 ++ [O]).


opt_table(Opt) ->
    case is_global_option(Opt) of
        true ->
            config;
        false ->
            local_config
    end.

is_global_option(Opt) ->
    lists:member(Opt, global_options()).

global_options() ->
    [
        hosts,
        language,
        sm_backend,
        node_specific_options
    ].


%%--------------------------------------------------------------------
%% Configuration parsing
%%--------------------------------------------------------------------

-spec parse_terms(term()) -> state().
parse_terms(Terms) ->
    State = just_parse_terms(Terms),
    State2 = dedup_state_opts(State),
    add_dep_modules(State2).

just_parse_terms(Terms) ->
    State = lists:foldl(fun search_hosts_and_pools/2, #state{}, Terms),
    TermsWExpandedMacros = replace_macros(Terms),
    lists:foldl(fun process_term/2, State, TermsWExpandedMacros).

-spec check_hosts([jid:server()], [jid:server()]) ->
    {[jid:server()], [jid:server()]}.
check_hosts(NewHosts, OldHosts) ->
    Old = sets:from_list(OldHosts),
    New = sets:from_list(NewHosts),
    ListToAdd = sets:to_list(sets:subtract(New, Old)),
    ListToDel = sets:to_list(sets:subtract(Old, New)),
    {ListToDel, ListToAdd}.


-spec can_be_ignored(Key :: atom() | tuple()) -> boolean().
can_be_ignored(Key) when is_atom(Key);
                         is_tuple(Key) ->
    L = [domain_certfile, s2s, all_metrics_are_global, rdbms],
    lists:member(Key, L).

% group values which can be grouped like rdbms ones
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


-spec is_not_host_specific(atom()
                           | {atom(), jid:server()}
                           | {atom(), atom(), atom()}) -> boolean().
is_not_host_specific(Key) when is_atom(Key) ->
    true;
is_not_host_specific({Key, Host}) when is_atom(Key), is_binary(Host) ->
    false;
is_not_host_specific({Key, PoolType, PoolName})
  when is_atom(Key), is_atom(PoolType), is_atom(PoolName) ->
    true.

-spec get_key_group(binary(), atom()) -> atom().
get_key_group(<<"ldap_", _/binary>>, _) ->
    ldap;
get_key_group(<<"rdbms_", _/binary>>, _) ->
    rdbms;
get_key_group(<<"pgsql_", _/binary>>, _) ->
    rdbms;
get_key_group(<<"auth_", _/binary>>, _) ->
    auth;
get_key_group(<<"ext_auth_", _/binary>>, _) ->
    auth;
get_key_group(<<"s2s_", _/binary>>, _) ->
    s2s;
get_key_group(_, Key) when is_atom(Key) ->
    Key.

%% -----------------------------------------------------------------
%% State API
%% -----------------------------------------------------------------

allow_override_all(State = #state{}) ->
    State#state{override_global = true,
                override_local  = true,
                override_acls   = true}.

allow_override_local_only(State = #state{}) ->
    State#state{override_global = false,
                override_local  = true,
                override_acls   = false}.

state_to_opts(#state{opts = Opts}) ->
    lists:reverse(Opts).

state_to_host_opts(#state{hosts = Hosts}) ->
    Hosts.

can_override(global, #state{override_global = Override}) ->
    Override;
can_override(local, #state{override_local = Override}) ->
    Override;
can_override(acls, #state{override_acls = Override}) ->
    Override.

state_to_global_opt(OptName, State, Default) ->
    Opts = state_to_opts(State),
    opts_to_global_opt(Opts, OptName, Default).

%% @doc Files, that are required to be present on disc.
-spec state_to_required_files(state()) -> list(file:filename()).
state_to_required_files(State) ->
    Opts = state_to_opts(State),
    opts_to_global_opt(Opts, required_files, []).

opts_to_global_opt([{config, OptName, OptValue}|_], OptName, _Default) ->
    OptValue;
opts_to_global_opt([_|Opts], OptName, Default) ->
    opts_to_global_opt(Opts, OptName, Default);
opts_to_global_opt([], _OptName, Default) ->
    Default.


%% -----------------------------------------------------------------
%% Support for 'include_config_file'
%% -----------------------------------------------------------------

config_filenames_to_include([{include_config_file, Filename} | Terms]) ->
    [Filename|config_filenames_to_include(Terms)];
config_filenames_to_include([{include_config_file, Filename, _Options} | Terms]) ->
    [Filename|config_filenames_to_include(Terms)];
config_filenames_to_include([_Other | Terms]) ->
    config_filenames_to_include(Terms);
config_filenames_to_include([]) ->
    [].

include_config_files(Terms, Configs) ->
    include_config_files(Terms, Configs, []).

include_config_files([], _Configs, Res) ->
    Res;
include_config_files([{include_config_file, Filename} | Terms], Configs, Res) ->
    include_config_files([{include_config_file, Filename, []} | Terms],
                         Configs, Res);
include_config_files([{include_config_file, Filename, Options} | Terms],
                     Configs, Res) ->
    IncludedTerms = find_plain_terms_for_file(Filename, Configs),
    Disallow = proplists:get_value(disallow, Options, []),
    IncludedTerms2 = delete_disallowed(Disallow, IncludedTerms),
    AllowOnly = proplists:get_value(allow_only, Options, all),
    IncludedTerms3 = keep_only_allowed(AllowOnly, IncludedTerms2),
    include_config_files(Terms, Configs, Res ++ IncludedTerms3);
include_config_files([Term | Terms], Configs, Res) ->
    include_config_files(Terms, Configs, Res ++ [Term]).

find_plain_terms_for_file(Filename, Configs) ->
    case lists:keyfind(Filename, 1, Configs) of
        false ->
            %% Terms were not provided by caller for this file
            erlang:error({config_not_found, Filename});
        {Filename, Terms} ->
            Terms
    end.

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
            ?WARNING_MSG("event=ignore_disallowed_option option=~p", [Disallowed]),
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
    [?WARNING_MSG("event=ignore_disallowed_option option=~p", [NA])
     || NA <- NAs],
    As.


dedup_state_opts(State = #state{opts = RevOpts}) ->
    {RevOpts2, _Removed} = dedup_state_opts_list(RevOpts, [], [], sets:new()),
    State#state{opts = RevOpts2}.

dedup_state_opts_list([{Type, K, _V} = H|List], Removed, Keep, Set)
  when Type =:= config; Type =:= local_config ->
    Element = {Type, K},
    case sets:is_element(Element, Set) of
        true ->
            dedup_state_opts_list(List, [H|Removed], Keep, Set);
        false ->
            Set1 = sets:add_element(Element, Set),
            dedup_state_opts_list(List, Removed, [H|Keep], Set1)
    end;
dedup_state_opts_list([H|List], Removed, Keep, Set) ->
    dedup_state_opts_list(List, Removed, [H|Keep], Set);
dedup_state_opts_list([], Removed, Keep, _Set) ->
    {Keep, Removed}.


add_dep_modules(State = #state{opts = Opts}) ->
    Opts2 = add_dep_modules_opts(Opts),
    State#state{opts = Opts2}.

add_dep_modules_opts(Opts) ->
    lists:map(fun add_dep_modules_opt/1, Opts).

add_dep_modules_opt({local_config, {modules, Host}, Modules}) ->
    Modules2 = gen_mod_deps:add_deps(Host, Modules),
    {local_config, {modules, Host}, Modules2};
add_dep_modules_opt(Other) ->
    Other.
