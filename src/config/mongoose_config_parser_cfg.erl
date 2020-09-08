%% @doc Config parsing and processing for the 'cfg' format
-module(mongoose_config_parser_cfg).

-behaviour(mongoose_config_parser).

-export([parse_file/1]).

%% For tests
-export([parse_terms/1]).

-include("mongoose.hrl").
-include("ejabberd_config.hrl").

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

%%--------------------------------------------------------------------
%% Configuration parsing
%%--------------------------------------------------------------------

-spec parse_file(FileName :: string()) -> mongoose_config_parser:state().
parse_file(FileName) ->
    Terms = mongoose_config_terms:get_plain_terms_file(FileName),
    parse_terms(Terms).

-spec parse_terms(term()) -> mongoose_config_parser:state().
parse_terms(Terms) ->
    State = just_parse_terms(Terms),
    State2 = mongoose_config_parser:dedup_state_opts(State),
    mongoose_config_parser:add_dep_modules(State2).

just_parse_terms(Terms) ->
    State = lists:foldl(fun search_hosts_and_pools/2, mongoose_config_parser:new_state(), Terms),
    TermsWExpandedMacros = replace_macros(Terms),
    lists:foldl(fun process_term/2, State, TermsWExpandedMacros).

-spec search_hosts_and_pools({host|hosts,
                              [mongoose_config_parser:host()] | mongoose_config_parser:host()},
                             mongoose_config_parser:state()) -> any().
search_hosts_and_pools({host, Host}, State) ->
    search_hosts_and_pools({hosts, [Host]}, State);
search_hosts_and_pools({hosts, Hosts}, State) ->
    case mongoose_config_parser:state_to_host_opts(State) of
        [] ->
            add_hosts_to_option(Hosts, State);
        OldHosts ->
            ?LOG_ERROR(#{what => too_many_hosts_definitions,
                 new_hosts => Hosts, old_hosts => OldHosts}),
            exit(#{issue => "too many hosts definitions",
                   new_hosts => Hosts,
                   old_hosts => OldHosts})
    end;
search_hosts_and_pools(_Term, State) ->
    State.

-spec add_hosts_to_option(Hosts :: [mongoose_config_parser:host()],
                          State :: mongoose_config_parser:state()) ->
          mongoose_config_parser:state().
add_hosts_to_option(Hosts, State) ->
    PrepHosts = normalize_hosts(Hosts),
    add_option(hosts, PrepHosts, mongoose_config_parser:set_hosts(PrepHosts, State)).

-spec normalize_hosts([mongoose_config_parser:host()]) -> [binary() | tuple()].
normalize_hosts(Hosts) ->
    normalize_hosts(Hosts, []).


normalize_hosts([], PrepHosts) ->
    lists:reverse(PrepHosts);
normalize_hosts([Host | Hosts], PrepHosts) ->
    case jid:nodeprep(host_to_binary(Host)) of
        error ->
            ?LOG_ERROR(#{what => invalid_hostname_in_config, hostname => Host}),
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
                   State :: mongoose_config_parser:state()) -> mongoose_config_parser:state().
process_term(Term, State) ->
    case Term of
        override_global ->
            mongoose_config_parser:override_global(State);
        override_local ->
            mongoose_config_parser:override_local(State);
        override_acls ->
            mongoose_config_parser:override_acls(State);
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
            State1 = add_option(outgoing_s2s_families, Methods, State),
            add_option(outgoing_s2s_timeout, Timeout, State1);
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
                        State, mongoose_config_parser:state_to_host_opts(State))
    end.

-spec process_host_term(Term :: host_term(),
                        Host :: acl:host(),
                        State :: mongoose_config_parser:state()) -> mongoose_config_parser:state().
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

-spec add_option(Opt :: mongoose_config_parser:key(),
                 Val :: mongoose_config_parser:value(),
                 State :: mongoose_config_parser:state()) -> mongoose_config_parser:state().
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

append_option(OptRec, State) ->
    Opts = mongoose_config_parser:get_opts(State),
    mongoose_config_parser:set_opts([OptRec | Opts], State).

%% Merges two values of a local option
compact_option(Opt, Val, State) ->
    Opts = mongoose_config_parser:get_opts(State),
    Opts2 = compact(Opt, Val, Opts, []),
    mongoose_config_parser:set_opts(Opts2, State).

compact({OptName, Host} = Opt, Val, [], Os) ->
    %% The option is defined for host using host_config before the global option.
    %% The host_option can be overwritten.
    %% TODO or maybe not. We need a test.
    ?LOG_WARNING(#{what => host_config_option_can_be_overwritten,
                   text => <<"define global options before host options">>,
                   option_name => OptName, host => Host}),
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
    Opts2 = compact_global(Opt, Val, mongoose_config_parser:get_opts(State), []),
    mongoose_config_parser:set_opts(Opts2, State).

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
