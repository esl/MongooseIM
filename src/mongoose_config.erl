%% @doc Pure config logic.
%% No ets table manipulations, no Mnesia, no starting modules, no file reading here.
%% Everything here is safe, side effect free.
%% OK, logging is possible, but keep it to minimum.
-module(mongoose_config).
-export([parse_terms/1]).
-export([get_config_diff/2]).
-export([compute_config_version/2]).
-export([compute_config_file_version/1]).
-export([check_hosts/2]).
-export([can_be_ignored/1]).
-export([compare_modules/2]).
-export([compare_listeners/2]).
-export([is_not_host_specific/1]).

-export([allow_override_all/1,
         allow_override_local_only/1,
         state_to_opts/1,
         state_to_global_opt/3,
         can_override/2]).

%% for unit tests
-export([group_host_changes/1]).

%% Support for 'include_config_file'
-export([config_filenames_to_include/1,
         include_config_files/2]).

-export([flatten_opts/2,
         state_to_flatten_local_opts/1,
         expand_opts/1]).

-export([does_pattern_match/2]).
-export([make_categorized_options/3]).
-export([state_to_categorized_options/1]).
-export([cluster_reload_strategy/1]).
-export([strategy_to_failed_checks/1]).
-export([strategy_to_changes_to_apply/1]).


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

-export_type([key/0, value/0]).

-record(state, {opts = [] :: list(),
                hosts = [] :: [host()],
                odbc_pools = [] :: [atom()],
                override_local = false :: boolean(),
                override_global = false :: boolean(),
                override_acls = false :: boolean()}).

-type compare_result() :: #{
        to_start => list(),
        to_stop => list(),
        to_reload => list()
       }.

-type config_diff() :: #{
        config_changes => compare_result(),
        local_config_changes => compare_result(),
        local_hosts_changes => compare_result()
       }.

-type categorized_options() :: #{
        global_config => list(),
        local_config => list(),
        host_local_config => list()}.

-type node_state() :: #{
        mongoose_node => node(),
        config_file => string(),
        loaded_categorized_options => categorized_options(),
        ondisc_config_terms => list()}.

-type reloading_strategy() :: map().

-type reloading_change() :: #{
        mongoose_node => node(),
        state_to_apply => state(),
        config_diff_to_apply => config_diff()
       }.

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

-type flatten_option() :: term().
-type flatten_options() :: list(flatten_option()).

-callback stop(host()) -> any().


-spec search_hosts_and_pools({host|hosts, [host()] | host()}
                             | {pool, odbc, atom()}
                             | {pool, odbc, atom(), list()}, state()) -> any().
search_hosts_and_pools(Term, State) ->
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
        {pool, PoolType, PoolName, _Options} ->
            search_hosts_and_pools({pool, PoolType, PoolName}, State);
        {pool, odbc, PoolName} ->
            add_odbc_pool_to_option(PoolName, State);
        _ ->
            State
    end.


-spec add_hosts_to_option(Hosts :: [host()],
                          State :: state()) -> state().
add_hosts_to_option(Hosts, State) ->
    PrepHosts = normalize_hosts(Hosts),
    add_option(hosts, PrepHosts, State#state{hosts = PrepHosts}).

add_odbc_pool_to_option(PoolName, State) ->
    Pools = State#state.odbc_pools,
    State#state{odbc_pools = [PoolName | Pools]}.

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
        {pool, odbc, _PoolName} ->
            State;
        {pool, odbc, PoolName, Options} ->
            lists:foldl(fun(T, S) ->
                            process_db_pool_term(T, PoolName, S)
                        end,
                        State, Options);
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
        {{global_distrib_addr, Host}, Addr} ->
            add_option({global_distrib_addr, list_to_binary(Host)}, Addr, State);
        {s2s_dns_options, PropList} ->
            add_option(s2s_dns_options, PropList, State);
        {s2s_use_starttls, Port} ->
            add_option(s2s_use_starttls, Port, State);
        {s2s_ciphers, Ciphers} ->
            add_option(s2s_ciphers, Ciphers, State);
        {s2s_certfile, CertFile} ->
            case mongoose_config_utils:is_file_readable(CertFile) of
                true -> add_option(s2s_certfile, CertFile, State);
                false ->
                    ErrorText = "There is a problem in the configuration: "
                        "the specified file is not readable: ",
                    throw({error, ErrorText ++ CertFile})
            end;
        {domain_certfile, Domain, CertFile} ->
            case mongoose_config_utils:is_file_readable(CertFile) of
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
        {services, Value} ->
            add_option(services, Value, State);
        {_Opt, _Val} ->
            process_term_for_hosts_and_pools(Term, State)
    end.

process_term_for_hosts_and_pools(Term = {Key, _Val}, State) ->
    BKey = atom_to_binary(Key, utf8),
    case get_key_group(BKey, Key) of
        odbc ->
            ok = check_pools(State#state.odbc_pools),
            lists:foldl(fun(Pool, S) -> process_db_pool_term(Term, Pool, S) end,
                        State, State#state.odbc_pools);
        _ ->
            lists:foldl(fun(Host, S) -> process_host_term(Term, Host, S) end,
                        State, State#state.hosts)
    end.

check_pools([]) ->
    ?CRITICAL_MSG("Config file invalid: ODBC defined with no pools", []),
    exit(no_odbc_pools);
check_pools(Pools) when is_list(Pools) ->
    ok.

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
        {odbc_pool, Pool} when is_atom(Pool) ->
            add_option({odbc_pool, Host}, Pool, State);
        {riak_server, RiakConfig} ->
            add_option(riak_server, RiakConfig, State);
        {cassandra_servers, CassandraConfig} ->
            add_option(cassandra_servers, CassandraConfig, State);
        {elasticsearch_server, ESConfig} ->
            add_option(elasticsearch_server, ESConfig, State);
        {node_specific_options, NodeOpts} ->
            add_option(node_specific_options, NodeOpts, State);
        {Opt, Val} ->
            add_option({Opt, Host}, Val, State)
    end.

process_db_pool_term({Opt, Val}, Pool, State) when is_atom(Pool) ->
    add_option({Opt, odbc_pool, Pool}, Val, State).

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
                node_specific_options ->
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


%%--------------------------------------------------------------------
%% Configuration parsing
%%--------------------------------------------------------------------

-spec parse_terms(term()) -> state().
parse_terms(Terms) ->
    State = lists:foldl(fun search_hosts_and_pools/2, #state{}, Terms),
    TermsWExpandedMacros = replace_macros(Terms),
    lists:foldl(fun process_term/2,
                add_option(odbc_pools, State#state.odbc_pools, State),
                TermsWExpandedMacros).

-spec get_config_diff(state(), categorized_options()) -> config_diff().
get_config_diff(State, #{global_config := Config,
                         local_config := Local,
                         host_local_config := HostsLocal}) ->
    #{global_config := NewConfig,
      local_config := NewLocal,
      host_local_config := NewHostsLocal} =
        state_to_categorized_options(State),
    %% global config diff
    CC = compare_terms(Config, NewConfig, 2, 3),
    LC = compare_terms(skip_special_config_opts(Local), NewLocal, 2, 3),
    LHC = compare_terms(group_host_changes(HostsLocal), group_host_changes(NewHostsLocal), 1, 2),
    #{config_changes => CC,
      local_config_changes => LC,
      local_hosts_changes => LHC}.



%% Config version hash does not depend on order of arguments in the config
compute_config_version(LC, LCH) ->
    LC1 = skip_special_config_opts(LC),
    L1 = lists:sort(flatten_opts(LC1, LCH)),
    ShaBin = crypto:hash(sha, term_to_binary(L1)),
    bin_to_hex:bin_to_hex(ShaBin).

flatten_global_opts_version(FlattenOpts) ->
    Sorted = lists:sort(FlattenOpts),
    ShaBin = crypto:hash(sha, term_to_binary(Sorted)),
    bin_to_hex:bin_to_hex(ShaBin).

compute_config_file_version(#state{opts = Opts, hosts = Hosts}) ->
    compute_config_version(Opts, Hosts).

skip_special_config_opts(Opts) ->
    lists:filter(fun(Opt) -> not is_special_config_opt(Opt) end, Opts).

is_special_config_opt(#local_config{key = Key}) ->
    lists:member(Key, special_local_config_opts());
is_special_config_opt(_) -> %% There are also #config{} and acls
    false.

special_local_config_opts() ->
    [node_start].

-spec check_hosts([jid:server()], [jid:server()]) -> {[jid:server()],
                                                                [jid:server()]}.
check_hosts(NewHosts, OldHosts) ->
    Old = sets:from_list(OldHosts),
    New = sets:from_list(NewHosts),
    ListToAdd = sets:to_list(sets:subtract(New, Old)),
    ListToDel = sets:to_list(sets:subtract(Old, New)),
    {ListToDel, ListToAdd}.


-spec can_be_ignored(Key :: atom() | tuple()) -> boolean().
can_be_ignored(Key) when is_atom(Key);
                         is_tuple(Key) ->
    L = [domain_certfile, s2s, all_metrics_are_global, odbc],
    lists:member(Key, L).


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

-spec state_to_categorized_options(state()) -> categorized_options().
state_to_categorized_options(#state{opts = RevOpts}) ->
    {GlobalConfig, LocalConfig, HostsConfig} = categorize_options(RevOpts),
    #{global_config => GlobalConfig,
      local_config => LocalConfig,
      host_local_config => HostsConfig}.

%% Takes opts in reverse order
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
    #{to_start  => ToStart,
      to_stop   => ToStop,
      to_reload => ToReload}.

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

can_override(global, #state{override_global = Override}) ->
    Override;
can_override(local, #state{override_local = Override}) ->
    Override;
can_override(acls, #state{override_acls = Override}) ->
    Override.

state_to_global_opt(OptName, State, Default) ->
    Opts = state_to_opts(State),
    opts_to_global_opt(Opts, OptName, Default).

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
    include_config_files([{include_config_file, Filename, []} | Terms], Configs, Res);
include_config_files([{include_config_file, Filename, Options} | Terms], Configs, Res) ->
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



%% @doc Convert LocalConfig and HostLocalConfig to a flat list of options
flatten_opts(LC, LCH) ->
    flatten_local_config_opts(LC) ++ flatten_local_config_host_opts(LCH).

%% @doc It ignores global options
state_to_flatten_local_opts(State) ->
    CatOptions = state_to_categorized_options(State),
    categorize_options_to_flatten_local_config_opts(CatOptions).

%% Be aware, that
%% categorize_options_to_flatten_local_config_opts
%% and
%% categorize_options_to_flatten_local_config_opts
%% returns different sets of flatten_options
-spec categorize_options_to_flatten_local_config_opts(categorized_options()) ->
    flatten_options().
categorize_options_to_flatten_local_config_opts(
    #{local_config := Local,
      host_local_config := HostsLocal}) ->
    Local2 = skip_special_config_opts(Local),
    mongoose_config:flatten_opts(Local2, HostsLocal).

%% Flat categorize_options for global options only
-spec categorize_options_to_flatten_global_config_opts(categorized_options()) ->
    flatten_options().
categorize_options_to_flatten_global_config_opts(#{global_config := GlobalConfig}) ->
    flatten_global_config_opts(GlobalConfig).

flatten_global_config_opts(LC) ->
    lists:flatmap(fun(#config{key  = K, value = V}) ->
                          flatten_global_config_opt(K, V);
                     ({acl, K, V}) ->
                          flatten_acl_config_opt(K, V);
                     (Other) ->
                     ?ERROR_MSG("issue=strange_global_config value=~1000p",
                                [Other]),
                     []
                  end, LC).

flatten_local_config_opts(LC) ->
    lists:flatmap(fun(#local_config{key = K, value = V}) ->
                          flatten_local_config_opt(K, V);
                     (#config{key  = K, value = V}) ->
                          flatten_global_config_opt(K, V);
                     ({acl, K, V}) ->
                          flatten_acl_config_opt(K, V);
                     (Other) ->
                     ?ERROR_MSG("issue=strange_local_config value=~1000p",
                                [Other]),
                     []
                  end, LC).

flatten_local_config_host_opts(LCH) ->
    lists:flatmap(fun(#local_config{key = {K, Host}, value = V}) ->
                          flatten_local_config_host_opt(K, Host, V);
                     (Host) when is_binary(Host) ->
                     [{hostname, Host}];
                     (Other) ->
                     ?ERROR_MSG("issue=strange_local_host_config value=~1000p",
                                [Other]),
                     []
                  end, LCH).

flatten_global_config_opt(K, V) ->
    [{[g, K], V}].

flatten_acl_config_opt(K, V) ->
    [{[a, K], V}].

flatten_local_config_opt(listen, V) ->
    [{[l, listen], flatten}] ++
    lists:append([flatten_listen(Address, Module, Opts)
                  || {Address, Module, Opts} <- V]);
flatten_local_config_opt(K, V) ->
    [{[l, K], V}].

flatten_listen(Address, Module, Opts) ->
    [{[l, listener, Address, Module], flatten}
     | flatten_listen_opts(Address, Module, Opts)].

flatten_listen_opts(Address, Module, Opts) ->
    lists:map(fun({OptName, OptValue}) ->
                {[l, listener_opt, Address, Module, OptName], OptValue};
                 (Opt) -> %% not key-value option, example: starttls
                {[l, listener_simple_opt, Address, Module, Opt], simple}
              end, Opts).

flatten_local_config_host_opt(modules, Host, V) ->
    [{[h, Host, modules], flatten}] ++
    lists:append([flatten_module(Host, Module, Opts) || {Module, Opts} <- V]);
flatten_local_config_host_opt(K, Host, V) ->
    [{[h, Host, K], V}].

flatten_module(H, Module, Opts) ->
    [{[h, H, module, Module], flatten}|flatten_module_opts(H, Module, Opts)].

flatten_module_opts(H, Module, Opts) ->
    lists:map(fun({OptName, OptValue}) ->
                {[h, H, module_opt, Module, OptName], OptValue};
                  (OptName) -> %% Special case, flag is the same as {flag, true}
                {[h, H, module_opt, Module, OptName], true}
        end, Opts).


%% @doc Convert flat list of options back to LocalConfig and HostLocalConfig
expand_opts(FlattenOpts) ->
    Groups = group_flat_opts(FlattenOpts),
    GlobalConfigGroup = maps:get(global_config, Groups, []),
    LocalConfigGroup = maps:get(local_config, Groups, []),
    HostConfigGroup = maps:get(host_config, Groups, []),
    LocalConfig = expand_local_config_group(LocalConfigGroup, Groups),
    HostConfig = expand_host_config_group(HostConfigGroup, Groups),
    {LocalConfig, HostConfig}.

expand_global_config_group(LocalConfigGroup, Groups) ->
    [expand_global_config_group_item(K, V, Groups)
     || {K, V} <- LocalConfigGroup].

expand_global_config_group_item(K, V, Groups) ->
    Value = expand_local_config_group_item_value(K, V, Groups),
    #config{key = K, value = Value}.

%% @doc Rewrite LocalConfigGroup to `{local_config, _, _}' format
%% using `Groups' for quick lookups
expand_local_config_group(LocalConfigGroup, Groups) ->
    [expand_local_config_group_item(Type, K, V, Groups)
     || {Type, K, V} <- LocalConfigGroup].

expand_local_config_group_item(local, K, V, Groups) ->
    Value = expand_local_config_group_item_value(K, V, Groups),
    #local_config{key = K, value = Value};
expand_local_config_group_item(global, K, V, Groups) ->
    #config{key = K, value = V};
expand_local_config_group_item(acl, K, V, Groups) ->
    {acl, K, V};
expand_local_config_group_item(hostname, Host, simple, Groups) ->
    Host.

expand_local_config_group_item_value(listen, flatten, Groups) ->
    make_listeners_from_groups(Groups);
expand_local_config_group_item_value(_K, V, _Groups) ->
    V.

make_listeners_from_groups(Groups) ->
    Listeners = maps:get(listeners, Groups, []),
    [{Address, Module, make_listener_opts_from_groups(Address, Module, Groups)}
     || {Address, Module} <- Listeners].

make_listener_opts_from_groups(Address, Module, Groups) ->
    GroupName = {listener, Address, Module},
    maps:get(GroupName, Groups, []).

expand_host_config_group(HostConfigGroup, Groups) ->
    [expand_host_config_group_item(Host, K, V, Groups)
     || {Host, K, V} <- HostConfigGroup].

expand_host_config_group_item(Host, K, V, Groups) ->
    Value = expand_host_config_group_item_value(Host, K, V, Groups),
    {local_config, {K, Host}, Value}.

expand_host_config_group_item_value(Host, modules, flatten, Groups) ->
    make_modules_for_host_from_groups(Host, Groups);
expand_host_config_group_item_value(_Host, _K, V, _Groups) ->
    V.

make_modules_for_host_from_groups(Host, Groups) ->
    GroupName = {modules, Host},
    Modules = maps:get(GroupName, Groups, []),
    [{Module, make_module_opts_from_groups(Host, Module, Groups)}
     || Module <- Modules].

make_module_opts_from_groups(Host, Module, Groups) ->
    GroupName = {module_opts, Host, Module},
    maps:get(GroupName, Groups, []).

%% @doc group opts for faster lookups in expand_opts
group_flat_opts(FlattenOpts) ->
    %% Process FlattenOpts in reverse order
    lists:foldr(fun group_flat_opt/2, #{}, FlattenOpts).

group_flat_opt({[g, K], V}, Groups) ->
    GroupName  = local_config,
    GroupValue = {global, K, V},
    add_to_group(GroupName, GroupValue, Groups);

group_flat_opt({[l, K], V}, Groups) ->
    GroupName  = local_config,
    GroupValue = {local, K, V},
    add_to_group(GroupName, GroupValue, Groups);

group_flat_opt({[a, K], V}, Groups) ->
    GroupName  = local_config,
    GroupValue = {acl, K, V},
    add_to_group(GroupName, GroupValue, Groups);

group_flat_opt({[h, Host, K], V}, Groups) ->
    GroupName  = host_config,
    GroupValue = {Host, K, V},
    add_to_group(GroupName, GroupValue, Groups);

group_flat_opt({[h, Host, module, Module], flatten}, Groups) ->
    GroupName  = {modules, Host},
    GroupValue = Module,
    add_to_group(GroupName, GroupValue, Groups);

group_flat_opt({[h, Host, module_opt, Module, OptName], OptValue}, Groups) ->
    GroupName  = {module_opts, Host, Module},
    GroupValue = {OptName, OptValue},
    add_to_group(GroupName, GroupValue, Groups);

group_flat_opt({[l, listener, Address, Module], flatten}, Groups) ->
    GroupName  = listeners,
    GroupValue = {Address, Module},
    add_to_group(GroupName, GroupValue, Groups);

group_flat_opt({[l, listener_opt, Address, Module, OptName], OptValue}, Groups) ->
    GroupName  = {listener, Address, Module},
    GroupValue = {OptName, OptValue},
    add_to_group(GroupName, GroupValue, Groups);

group_flat_opt({[l, listener_simple_opt, Address, Module, Opt], simple}, Groups) ->
    GroupName  = {listener, Address, Module},
    GroupValue = Opt,
    add_to_group(GroupName, GroupValue, Groups);

group_flat_opt({[hostname, Host], simple}, Groups) ->
    GroupName  = local_config,
    GroupValue = {hostname, Host, simple},
    add_to_group(GroupName, GroupValue, Groups).

%% @doc Append into a group for later use
%% Returns updated Groups
-spec add_to_group(term(), term(), Groups :: map()) -> Groups :: map().
add_to_group(Group, Value, Groups) ->
    Values = maps:get(Group, Groups, []),
    maps:put(Group, [Value|Values], Groups).


%% @doc Use erlang match spec to check if the Subject fits the Pattern.
%% http://erlang.org/doc/apps/erts/match_spec.html
-spec does_pattern_match(term(), ets:match_pattern()) -> boolean().
does_pattern_match(Subject, Pattern) ->
    case ets:test_ms({Subject}, [ {{Pattern}, [], [true]} ]) of
        {ok, Matches} ->
            Matches;
        Other ->
            erlang:error(#{issue => does_pattern_match_failed,
                           subject => Subject, patten => Pattern})
    end.

%% @doc Make a categorize_options() map
%% Does not work with acls
-spec make_categorized_options(list(), list(), list()) -> categorized_options().
make_categorized_options(GlobalConfig, LocalConfig, HostLocalConfig) ->
    #{global_config => GlobalConfig,
      local_config => LocalConfig,
      host_local_config => HostLocalConfig}.

-spec cluster_reload_strategy([node_state()]) -> reloading_strategy().
cluster_reload_strategy([CoordinatorNodeState|_] = NodeStates) ->
    Data = prepare_data_for_cluster_reload_strategy(NodeStates),
    Data2 = cluster_reload_version_check(Data),
    calculate_changes(Data2).

-spec strategy_to_failed_checks(reloading_strategy()) -> boolean().
strategy_to_failed_checks(#{failed_checks := FailedChecks}) ->
    lists:map(fun(#{check := CheckName}) -> CheckName end, FailedChecks).

-spec strategy_to_changes_to_apply(reloading_strategy()) -> [reloading_change()].
strategy_to_changes_to_apply(#{coordinatior_node := Coordinator,
                               changes_to_apply := Changes}) ->
    [Change#{coordinatior_node => Coordinator} || Change <- Changes].

%% Checks what to pass into ejabberd_config:apply_changes/3
%% for each node
calculate_changes(Data=#{extended_node_states := ExtNodeStates}) ->
    [CoordinatorNodeState|OtherNodeStates] = ExtNodeStates,
    CoordinatorChanges = calculate_changes_on_coordinator(CoordinatorNodeState),
    OtherNodeChanges = lists:map(fun calculate_changes_on_remote_node/1,
                                 OtherNodeStates),
    Changes = [CoordinatorChanges|OtherNodeChanges],
    %% Changes that we need to apply for cluster_reload,
    %% if all checks are green.
    Data#{changes_to_apply => Changes}.

%% Coordinator node applies global changes
-spec calculate_changes_on_coordinator(map()) -> reloading_change().
calculate_changes_on_coordinator(ExtNodeState = #{
          mongoose_node := Node,
          ondisc_config_state := State,
          loaded_categorized_options := CatOptions}) ->
    State1 = allow_override_all(State),
    Diff = get_config_diff(State, CatOptions),
    #{
        mongoose_node => Node,
        state_to_apply => State1,
        config_diff_to_apply => Diff
    }.

-spec calculate_changes_on_remote_node(map()) -> reloading_change().
calculate_changes_on_remote_node(ExtNodeState = #{
          mongoose_node := Node,
          ondisc_config_state := State,
          loaded_categorized_options := CatOptions}) ->
    State1 = allow_override_local_only(State),
    Diff = get_config_diff(State, CatOptions),
    #{
        mongoose_node => Node,
        state_to_apply => State1,
        config_diff_to_apply => Diff
    }.

cluster_reload_version_check(Data) ->
    FailedChecks = lists:append(cluster_reload_version_checks(Data)),
    %% If there is at least one FailedChecks, we are not allowed to
    %% run reload_cluster.
    Data#{failed_checks => FailedChecks}.

cluster_reload_version_checks(Data) ->
    [check(inconsistent_loaded_global_versions,
           "Runtime configuration inconsistency! "
           "Global configs should be the same for all nodes in cluster. "
           "loaded_global_versions contains more than one unique config version. "
           "cluster_reload is not allowed to continue. "
           "How to fix: stop nodes, that run wrong config version.",
           not all_same(loaded_global_versions, Data)),

     check(inconsistent_ondisc_global_versions,
           "Ondisc configuration inconsistency. "
           "Global configs should be the same for all nodes in cluster. "
           "ondisc_global_versions contains more than one unique config version. "
           "cluster_reload is not allowed to continue. "
           "How to fix: ensure that ejabberd.cfg-s are the same for all nodes.",
           not all_same(loaded_global_versions, Data)),

     check(inconsistent_loaded_local_versions,
           "Runtime configuration inconsistency! "
           "Local configs should be the same for all nodes in cluster. "
           "Only node-specific parameters can be different. "
           "loaded_local_versions contains more than one unique config version. "
           "cluster_reload is not allowed to continue. "
           "How to fix: stop nodes, that run wrong config version.",
           not all_same(loaded_local_versions, Data)),

     check(inconsistent_ondisc_global_versions,
           "Ondisc configuration inconsistency. "
           "Local configs should be the same for all nodes in cluster. "
           "ondisc_local_versions contains more than one unique config version. "
           "cluster_reload is not allowed to continue. "
           "How to fix: ensure that ejabberd.cfg-s are the same for all nodes.",
           not all_same(ondisc_local_versions, Data)),

     check(no_update_required,
           "No nodes need cluster reload.",
           all_empty(version_transitions_lists, Data))
    ].

check(CheckName, Message, _Triggered=true) ->
    [#{check => CheckName, message => Message}];
check(_CheckName, _Message, _Triggered=false) ->
    [].

all_empty(Key, Data) ->
    all_equal_to(Key, Data, []).

all_equal_to(Key, Data, ExpectedValue) ->
    List = maps:get(Key, Data),
    Values = lists:map(fun({_Node, Value}) -> Value end, List),
    case Values of
        [ExpectedValue] ->
            true;
        _ ->
            false
    end.

all_same(Key, Data) ->
    List = maps:get(Key, Data),
    Values = lists:map(fun({_Node, Value}) -> Value end, List),
    case lists:usort(Values) of
        [_] ->
            true;
        _ ->
            false
    end.

prepare_data_for_cluster_reload_strategy([CoordinatorNodeState|_] = NodeStates) ->
    ExtNodeStates = extend_node_states(NodeStates),
    %% check that global options are the same everywhere:
    %% - same before
    %% - same after
    #{coordinatior_node => maps:get(mongoose_node, CoordinatorNodeState),
      %% All of these versions should be the same for reload_cluster to continue
      loaded_global_versions => node_values(loaded_global_version, ExtNodeStates),
      %% All of these versions should be the same for reload_cluster to continue
      ondisc_global_versions => node_values(loaded_global_version, ExtNodeStates),

      %% All of these versions should be the same for reload_cluster to continue
      %% Doest not count node-specific options
      loaded_local_versions => node_values(loaded_local_version, ExtNodeStates),
      %% All of these versions should be the same for reload_cluster to continue
      %% Doest not count node-specific options
      ondisc_local_versions => node_values(loaded_local_version, ExtNodeStates),

      %% These versions can be different
      loaded_local_node_specific_versions =>
            node_values(loaded_local_node_specific_version, ExtNodeStates),
      %% These versions can be different
      ondisc_local_node_specific_versions =>
            node_values(ondisc_local_node_specific_version, ExtNodeStates),

      %% If all are empty lists, we don't need to update
      version_transitions_lists =>
            node_values(version_transitions, ExtNodeStates),

      extended_node_states => ExtNodeStates
      }.

node_values(Key, NodeStates) ->
    [{maps:get(mongoose_node, NodeState), maps:get(Key, NodeState)}
     || NodeState <- NodeStates].

%% mongoose_node => node(),
%% config_file => string(),
%% loaded_categorized_options => categorized_options(),
%% ondisc_config_terms => list()

extend_node_states(NodeStates) ->
    lists:map(fun(NodeState) -> extend_node_state(NodeState) end, NodeStates).

extend_node_state(NodeState=#{
                    loaded_categorized_options := LoadedCatOptions,
                    ondisc_config_terms := OndiscTerms}) ->
    OndiscState = parse_terms(OndiscTerms),
    OndiscCatOptions = state_to_categorized_options(OndiscState),
    NodeSpecificPatterns = mongoose_config:state_to_global_opt(node_specific_options, OndiscState, []),
    LoadedFlattenGlobalOptions = categorize_options_to_flatten_global_config_opts(LoadedCatOptions),
    OndiscFlattenGlobalOptions = categorize_options_to_flatten_global_config_opts(OndiscCatOptions),
    LoadedFlattenLocalOptions = categorize_options_to_flatten_local_config_opts(LoadedCatOptions),
    OndiscFlattenLocalOptions = categorize_options_to_flatten_local_config_opts(OndiscCatOptions),
    #{node_specific_options := LoadedFlattenLocalNodeSpecificOptions,
      common_options := LoadedFlattenLocalCommonOptions,
      matched_patterns := LoadedMatchedPatterns} =
        split_node_specific_options(NodeSpecificPatterns, LoadedFlattenLocalOptions),
    #{node_specific_options := OndiscFlattenLocalNodeSpecificOptions,
      common_options := OndiscFlattenLocalCommonOptions,
      matched_patterns := OndiscMatchedPatterns} =
        split_node_specific_options(NodeSpecificPatterns, OndiscFlattenLocalOptions),
    LoadedGlobalVersion = flatten_global_opts_version(LoadedFlattenGlobalOptions),
    OndiscGlobalVersion = flatten_global_opts_version(OndiscFlattenGlobalOptions),
    LoadedLocalVersion = flatten_global_opts_version(LoadedFlattenLocalCommonOptions),
    OndiscLocalVersion = flatten_global_opts_version(OndiscFlattenLocalCommonOptions),
    LoadedNodeSpecificVersion = flatten_global_opts_version(LoadedFlattenLocalNodeSpecificOptions),
    OndiscNodeSpecificVersion = flatten_global_opts_version(OndiscFlattenLocalNodeSpecificOptions),
    VersionTransitions = [#{type => global,
                            loaded => LoadedGlobalVersion,
                            ondisc => OndiscGlobalVersion},
                          #{type => local,
                            loaded => LoadedLocalVersion,
                            ondisc => OndiscLocalVersion},
                          #{type => node_specific,
                            loaded => LoadedNodeSpecificVersion,
                            ondisc => OndiscNodeSpecificVersion}],
    %% Modified versions
    %% Ignore same version transitions
    NeededVersionTransitions = [Transition ||
                                Transition = #{loaded := Loaded, ondisc := Ondisc}
                                <- VersionTransitions,
                                Loaded =/= Ondisc],
    NodeState#{
      version_transitions => NeededVersionTransitions,
      ondisc_config_state => OndiscState,
      ondisc_categorized_options => OndiscCatOptions,
      loaded_global_flatten_opts => LoadedFlattenGlobalOptions,
      ondisc_global_flatten_opts => OndiscFlattenGlobalOptions,
      loaded_global_version => LoadedGlobalVersion,
      ondisc_global_version => OndiscGlobalVersion,
      %% Doest not count node-specific options
      loaded_local_version => LoadedLocalVersion,
      %% Doest not count node-specific options
      ondisc_local_version => OndiscLocalVersion,
      loaded_local_node_specific_version => LoadedNodeSpecificVersion,
      ondisc_local_node_specific_version => OndiscNodeSpecificVersion,
      %% Just node specific options
      loaded_local_node_specific_options => LoadedFlattenLocalNodeSpecificOptions,
      ondisc_local_node_specific_options => OndiscFlattenLocalNodeSpecificOptions,
      loaded_local_common_options => LoadedFlattenLocalCommonOptions,
      ondisc_local_common_options => OndiscFlattenLocalCommonOptions,
      loaded_matched_patterns => LoadedMatchedPatterns,
      ondisc_matched_patterns => OndiscMatchedPatterns,
      %% For debugging
      flatten_diff => #{
        ondisc_global_only => subtract_lists(OndiscFlattenGlobalOptions, LoadedFlattenGlobalOptions),
        loaded_global_only => subtract_lists(LoadedFlattenGlobalOptions, OndiscFlattenGlobalOptions),
        ondisc_local_only => subtract_lists(OndiscFlattenLocalOptions, LoadedFlattenLocalOptions),
        loaded_local_only => subtract_lists(LoadedFlattenLocalOptions, OndiscFlattenLocalOptions)
       }
     }.

subtract_lists(List, Except) ->
    SetList = ordsets:from_list(List),
    SetExcept = ordsets:from_list(Except),
    ordsets:subtract(SetList, SetExcept).

%% Split local options to:
%% - node specific (can be different for different nodes)
%% - common local options (same for all nodes, but not global)
%% Collects information about matches for debugging
split_node_specific_options([], FlattenOpts) ->
    %% No patterns case
    #{node_specific_options => [],
      common_options => FlattenOpts,
      matched_patterns => []};
split_node_specific_options(NodeSpecificPatterns, FlattenOpts) ->
    split_node_specific_options(NodeSpecificPatterns, FlattenOpts, [], [], []).

split_node_specific_options(NodeSpecificPatterns,
                            [{OptKey, Value}=Opt|FlattenOpts],
                            NodeSpecificOpts,
                            CommonOpts,
                            MatchedPatterns) ->
    case find_matching_node_specific_pattern(NodeSpecificPatterns, OptKey) of
        nomatch ->
            CommonOpts2 = [Opt|CommonOpts],
            split_node_specific_options(NodeSpecificPatterns, FlattenOpts,
                                        NodeSpecificOpts, CommonOpts2,
                                        MatchedPatterns);
        {match, Pattern} ->
            NodeSpecificOpts2 = [Opt|NodeSpecificOpts],
            %% For debugging
            MPattern = #{option_key => OptKey,
                         option_value => Value,
                         matched_pattern => Pattern},
            MatchedPatterns2 = [MPattern|MatchedPatterns],
            split_node_specific_options(NodeSpecificPatterns, FlattenOpts,
                                        NodeSpecificOpts2, CommonOpts,
                                        MatchedPatterns2)
    end;
split_node_specific_options(_NodeSpecificPatterns,
                            [],
                            NodeSpecificOpts,
                            CommonOpts,
                            MatchedPatterns) ->
    #{node_specific_options => NodeSpecificOpts,
      common_options => CommonOpts,
      matched_patterns => MatchedPatterns}.

find_matching_node_specific_pattern([Pattern|NodeSpecificPatterns], OptKey) ->
    case does_pattern_match(OptKey, Pattern) of
        true ->
            {match, Pattern};
        false ->
            find_matching_node_specific_pattern(NodeSpecificPatterns, OptKey)
    end;
find_matching_node_specific_pattern([], _OptKey) ->
    nomatch.
