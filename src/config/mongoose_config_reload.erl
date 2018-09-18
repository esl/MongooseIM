%% @doc Pure config reloading logic.
%% No ets table manipulations, no Mnesia, no starting modules, no file reading here.
%% Everything here is safe, side effect free.
%% OK, logging is possible, but keep it to minimum.
-module(mongoose_config_reload).
-export([get_config_diff/2]).
-export([compute_config_version/2]).
-export([compute_config_file_version/1]).
-export([compare_listeners/2]).
-export([state_to_flat_local_opts/1]).
-export([make_categorized_options/3]).
-export([state_to_categorized_options/1]).
-export([states_to_reloading_context/1]).
-export([context_to_failed_checks/1]).
-export([context_to_changes_to_apply/1]).

-include("mongoose.hrl").
-include("ejabberd_config.hrl").

-type config_version() :: binary().

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
        host_config => list()}.

-type node_state() :: #{
        mongoose_node => node(),
        config_file => string(),
        loaded_categorized_options => categorized_options(),
        ondisc_config_terms => list(),
        missing_files => list(file:filename()),
        required_files => list(file:filename())}.

-type reloading_context() :: map().

-type reloading_change() :: #{
        coordinator_node => node() | undefined,
        mongoose_node => node(),
        state_to_apply => state(),
        config_diff_to_apply => config_diff()
       }.

-type state() :: mongoose_config_parser:state().


-spec get_config_diff(state(), categorized_options()) -> config_diff().
get_config_diff(State, #{global_config := Config,
                         local_config := Local,
                         host_config := HostsLocal}) ->
    #{global_config := NewConfig,
      local_config := NewLocal,
      host_config := NewHostsLocal} =
        state_to_categorized_options(State),
    %% global config diff
    CC = compare_terms(Config, NewConfig, 2, 3),
    LC = compare_terms(skip_special_config_opts(Local), NewLocal, 2, 3),
    LHC = compare_terms(group_host_changes(HostsLocal),
                        group_host_changes(NewHostsLocal), 1, 2),
    #{config_changes => CC,
      local_config_changes => LC,
      local_hosts_changes => LHC}.



%% Config version hash does not depend on order of arguments in the config
compute_config_version(LC, LCH) ->
    LC1 = skip_special_config_opts(LC),
    L1 = lists:sort(mongoose_config_flat:flatten_opts(LC1, LCH)),
    ShaBin = crypto:hash(sha, term_to_binary(L1)),
    bin_to_hex:bin_to_hex(ShaBin).

flat_global_opts_version(FlatOpts) ->
    Sorted = lists:sort(FlatOpts),
    ShaBin = crypto:hash(sha, term_to_binary(Sorted)),
    bin_to_hex:bin_to_hex(ShaBin).

-spec compute_config_file_version(state()) -> config_version().
compute_config_file_version(State) ->
    Opts = mongoose_config_parser:state_to_opts(State),
    Hosts = mongoose_config_parser:state_to_host_opts(State),
    compute_config_version(Opts, Hosts).

skip_special_config_opts(Opts) ->
    lists:filter(fun(Opt) -> not is_special_config_opt(Opt) end, Opts).

is_special_config_opt(#local_config{key = Key}) ->
    lists:member(Key, special_local_config_opts());
is_special_config_opt(_) -> %% There are also #config{} and acls
    false.

special_local_config_opts() ->
    [node_start].


-spec compare_listeners(term(), term()) -> compare_result().
compare_listeners(OldListeners, NewListeners) ->
    compare_terms(map_listeners(OldListeners), map_listeners(NewListeners), 1, 2).

map_listeners(Listeners) ->
    lists:map(fun({PortIP, Module, Opts}) ->
                  {{PortIP, Module}, Opts}
              end, Listeners).

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

-spec state_to_categorized_options(state()) -> categorized_options().
state_to_categorized_options(State) ->
    Opts = mongoose_config_parser:state_to_opts(State),
    RevOpts = lists:reverse(Opts),
    {GlobalConfig, LocalConfig, HostsConfig} = categorize_options(RevOpts),
    #{global_config => GlobalConfig,
      local_config => LocalConfig,
      host_config => HostsConfig}.

%% Takes opts in reverse order
-spec categorize_options([term()]) -> {GlobalConfig, LocalConfig, HostsConfig} when
      GlobalConfig :: list(),
      LocalConfig :: list(),
      HostsConfig :: list().
categorize_options(Opts) ->
    lists:foldl(fun categorize_option/2, {[], [], []}, Opts).

categorize_option({config, _, _} = El, Acc) ->
    as_global(El, Acc);
categorize_option({local_config, {Key, Host}, _} = El, Acc)
        when is_atom(Key), is_binary(Host) ->
    as_hosts(El, Acc);
categorize_option({local_config, _, _} = El, Acc) ->
    as_local(El, Acc);
categorize_option({acl, _, _}, Acc) ->
    %% no need to do extra work here
    Acc;
categorize_option(Opt, Acc) ->
    ?ERROR_MSG("event=uncategorized_option option=~p", [Opt]),
    Acc.

as_global(El, {Config, Local, HostLocal}) -> {[El | Config], Local, HostLocal}.
as_local(El, {Config, Local, HostLocal}) -> {Config, [El | Local], HostLocal}.
as_hosts(El, {Config, Local, HostLocal}) -> {Config, Local, [El | HostLocal]}.

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

%% @doc It ignores global options
state_to_flat_local_opts(State) ->
    CatOptions = state_to_categorized_options(State),
    categorize_options_to_flat_local_config_opts(CatOptions).

%% Be aware, that
%% categorize_options_to_flat_local_config_opts
%% and
%% categorize_options_to_flat_local_config_opts
%% returns different sets of flat_options
-spec categorize_options_to_flat_local_config_opts(categorized_options()) ->
    mongoose_config_flat:flat_options().
categorize_options_to_flat_local_config_opts(
    #{local_config := Local,
      host_config := HostsLocal}) ->
    Local2 = skip_special_config_opts(Local),
    mongoose_config_flat:flatten_opts(Local2, HostsLocal).

%% Flat categorize_options for global options only
-spec categorize_options_to_flat_global_config_opts(categorized_options()) ->
    mongoose_config_flat:flat_options().
categorize_options_to_flat_global_config_opts(#{global_config := GlobalConfig}) ->
    mongoose_config_flat:flatten_global_config_opts(GlobalConfig).


%% @doc Make a categorize_options() map
%% Does not work with acls
-spec make_categorized_options(list(), list(), list()) -> categorized_options().
make_categorized_options(GlobalConfig, LocalConfig, HostLocalConfig) ->
    #{global_config => GlobalConfig,
      local_config => LocalConfig,
      host_config => HostLocalConfig}.

-spec states_to_reloading_context([node_state()]) -> reloading_context().
states_to_reloading_context(NodeStates) ->
    Data = prepare_data_for_cluster_reloading_context(NodeStates),
    Data2 = cluster_reload_version_check(Data),
    Data3 = calculate_changes(Data2),
    calculate_inconsistent_opts(Data3).

calculate_inconsistent_opts(Data=#{extended_node_states := ExtNodeStates}) ->
    NodeOpts = node_values(loaded_local_common_options, ExtNodeStates),
    Inconsistent = inconsistent_node_opts(NodeOpts),
    Data#{inconsistent_loaded_local_common_options => Inconsistent}.

%% Return a list with inconsistent options for each node.
%% Useful for inconsistent mongooseim.cfg debugging (when just hash is not enough).
inconsistent_node_opts(NodeOpts) ->
    {Nodes, OptLists} = lists:unzip(NodeOpts),
    OptSets = lists:map(fun sets:from_list/1, OptLists),
    AllConsistentSet = sets:intersection(OptSets),
    InconsistentOpts = lists:map(fun(OptSet) ->
                      sets:to_list(sets:subtract(OptSet, AllConsistentSet))
              end, OptSets),
    filter_out_empty_value_pairs(lists:zip(Nodes, InconsistentOpts)).

filter_out_empty_value_pairs(KVs) ->
    [KV || {_K,V} = KV <- KVs, V =/= []].

-spec context_to_failed_checks(reloading_context()) -> list().
context_to_failed_checks(#{failed_checks := FailedChecks}) ->
    lists:map(fun(#{check := CheckName}) -> CheckName end, FailedChecks).

-spec context_to_changes_to_apply(reloading_context()) -> [reloading_change()].
context_to_changes_to_apply(#{coordinator_node := Coordinator,
                               changes_to_apply := Changes}) ->
    [Change#{coordinator_node => Coordinator} || Change <- Changes].

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
calculate_changes_on_coordinator(_ExtNodeState = #{
          mongoose_node := Node,
          ondisc_config_state := State,
          loaded_categorized_options := CatOptions}) ->
    State1 = mongoose_config_parser:allow_override_all(State),
    Diff = get_config_diff(State, CatOptions),
    #{
        mongoose_node => Node,
        state_to_apply => State1,
        config_diff_to_apply => Diff
    }.

-spec calculate_changes_on_remote_node(map()) -> reloading_change().
calculate_changes_on_remote_node(_ExtNodeState = #{
          mongoose_node := Node,
          ondisc_config_state := State,
          loaded_categorized_options := CatOptions}) ->
    State1 = mongoose_config_parser:allow_override_local_only(State),
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

%% Returns a list of lists failed checks
cluster_reload_version_checks(Data) ->
    [check(inconsistent_loaded_global_versions,
           "Runtime configuration inconsistency! "
           "Global configs should be the same for all nodes in cluster. "
           "loaded_global_versions contains more than one unique config version. "
           "cluster_reload is not allowed to continue. "
           "How to fix: stop nodes, that run wrong config version.",
           %% To pass the check it should be ...
           all_same(loaded_global_versions, Data)),

     check(inconsistent_ondisc_global_versions,
           "Ondisc configuration inconsistency. "
           "Global configs should be the same for all nodes in cluster. "
           "ondisc_global_versions contains more than one unique config version. "
           "cluster_reload is not allowed to continue. "
           "How to fix: ensure that mongooseim.cfg-s are the same for all nodes.",
           %% To pass the check it should be ...
           all_same(loaded_global_versions, Data)),

     check(inconsistent_loaded_local_versions,
           "Runtime configuration inconsistency! "
           "Local configs should be the same for all nodes in cluster. "
           "Only node-specific parameters can be different. "
           "loaded_local_versions contains more than one unique config version. "
           "cluster_reload is not allowed to continue. "
           "Compare loaded_local_common_options for all nodes. "
           "Check inconsistent_loaded_local_common_options, it should be empty. "
           "How to fix: stop nodes, that run wrong config version.",
           %% To pass the check it should be ...
           all_same(loaded_local_versions, Data)),

     check(inconsistent_ondisc_local_versions,
           "Ondisc configuration inconsistency. "
           "Local configs should be the same for all nodes in cluster. "
           "ondisc_local_versions contains more than one unique config version. "
           "cluster_reload is not allowed to continue. "
           "How to fix: ensure that mongooseim.cfg-s are the same for all nodes.",
           %% To pass the check it should be ...
           all_same(ondisc_local_versions, Data)),

     check(no_update_required,
           "No nodes need cluster reload.",
           %% To pass the check it should be ...
           not all_empty(version_transitions_lists, Data)),

     check(some_required_files_are_missing,
           "Some files are missing on disc. Check missing_files_lists in dump.",
           %% To pass the check it should be ...
           all_empty(missing_files_lists, Data))
    ].

%% Returns failed check
check(_CheckName, _Message, _CheckPass=true) ->
    [];
check(CheckName, Message, _CheckPass=false) ->
    %% Check failed
    [#{check => CheckName, message => Message}].

all_empty(Key, Data) ->
    all_equal_to(Key, Data, []).

all_equal_to(Key, Data, ExpectedValue) ->
    List = maps:get(Key, Data),
    Values = lists:map(fun({_Node, Value}) -> Value end, List),
    case lists:usort(Values) of
        [ExpectedValue] ->
            true;
        [] ->
            true; %% empty node list case
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

prepare_data_for_cluster_reloading_context([CoordinatorNodeState|_] = NodeStates) ->
    ExtNodeStates = extend_node_states(NodeStates),
    %% check that global options are the same everywhere:
    %% - same before
    %% - same after
    #{coordinator_node => maps:get(mongoose_node, CoordinatorNodeState),
      %% All of these versions should be the same for reload_cluster to continue
      loaded_global_versions => node_values(loaded_global_version, ExtNodeStates),
      %% All of these versions should be the same for reload_cluster to continue
      ondisc_global_versions => node_values(ondisc_global_version, ExtNodeStates),

      %% All of these versions should be the same for reload_cluster to continue
      %% Doest not count node-specific options
      loaded_local_versions => node_values(loaded_local_version, ExtNodeStates),
      %% All of these versions should be the same for reload_cluster to continue
      %% Doest not count node-specific options
      ondisc_local_versions => node_values(ondisc_local_version, ExtNodeStates),

      %% These versions can be different
      loaded_local_node_specific_versions =>
            node_values(loaded_local_node_specific_version, ExtNodeStates),
      %% These versions can be different
      ondisc_local_node_specific_versions =>
            node_values(ondisc_local_node_specific_version, ExtNodeStates),

      %% If all are empty lists, we don't need to update
      version_transitions_lists =>
            node_values(version_transitions, ExtNodeStates),

      missing_files_lists =>
            node_values(missing_files, ExtNodeStates),

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
    OndiscState = mongoose_config_parser:parse_terms(OndiscTerms),
    OndiscCatOptions = state_to_categorized_options(OndiscState),
    NodeSpecificPatterns = mongoose_config_parser:state_to_global_opt(node_specific_options, OndiscState, []),
    LoadedFlatGlobalOptions = categorize_options_to_flat_global_config_opts(LoadedCatOptions),
    OndiscFlatGlobalOptions = categorize_options_to_flat_global_config_opts(OndiscCatOptions),
    LoadedFlatLocalOptions = categorize_options_to_flat_local_config_opts(LoadedCatOptions),
    OndiscFlatLocalOptions = categorize_options_to_flat_local_config_opts(OndiscCatOptions),
    #{node_specific_options := LoadedFlatLocalNodeSpecificOptions,
      common_options := LoadedFlatLocalCommonOptions,
      matched_patterns := LoadedMatchedPatterns,
      node_specific_orphans := LoadedNodeSpecificOrphans} =
        mongoose_config_flat:split_node_specific_options_tree(NodeSpecificPatterns, LoadedFlatLocalOptions),
    #{node_specific_options := OndiscFlatLocalNodeSpecificOptions,
      common_options := OndiscFlatLocalCommonOptions,
      matched_patterns := OndiscMatchedPatterns,
      node_specific_orphans := OndiscNodeSpecificOrphans} =
        mongoose_config_flat:split_node_specific_options_tree(NodeSpecificPatterns, OndiscFlatLocalOptions),
    LoadedGlobalVersion = flat_global_opts_version(LoadedFlatGlobalOptions),
    OndiscGlobalVersion = flat_global_opts_version(OndiscFlatGlobalOptions),
    LoadedLocalVersion = flat_global_opts_version(LoadedFlatLocalCommonOptions),
    OndiscLocalVersion = flat_global_opts_version(OndiscFlatLocalCommonOptions),
    LoadedNodeSpecificVersion = flat_global_opts_version(LoadedFlatLocalNodeSpecificOptions),
    OndiscNodeSpecificVersion = flat_global_opts_version(OndiscFlatLocalNodeSpecificOptions),
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
      loaded_global_flat_opts => LoadedFlatGlobalOptions,
      ondisc_global_flat_opts => OndiscFlatGlobalOptions,
      loaded_global_version => LoadedGlobalVersion,
      ondisc_global_version => OndiscGlobalVersion,
      %% Doest not count node-specific options
      loaded_local_version => LoadedLocalVersion,
      %% Doest not count node-specific options
      ondisc_local_version => OndiscLocalVersion,
      loaded_local_node_specific_version => LoadedNodeSpecificVersion,
      ondisc_local_node_specific_version => OndiscNodeSpecificVersion,
      %% Just node specific options
      loaded_local_node_specific_options => LoadedFlatLocalNodeSpecificOptions,
      ondisc_local_node_specific_options => OndiscFlatLocalNodeSpecificOptions,
      loaded_node_specific_orphans => LoadedNodeSpecificOrphans,
      ondisc_node_specific_orphans => OndiscNodeSpecificOrphans,
      loaded_local_common_options => LoadedFlatLocalCommonOptions,
      ondisc_local_common_options => OndiscFlatLocalCommonOptions,
      loaded_matched_patterns => LoadedMatchedPatterns,
      ondisc_matched_patterns => OndiscMatchedPatterns,
      %% For debugging
      flat_diff => #{
        ondisc_global_only => subtract_lists(OndiscFlatGlobalOptions, LoadedFlatGlobalOptions),
        loaded_global_only => subtract_lists(LoadedFlatGlobalOptions, OndiscFlatGlobalOptions),
        ondisc_local_only => subtract_lists(OndiscFlatLocalOptions, LoadedFlatLocalOptions),
        loaded_local_only => subtract_lists(LoadedFlatLocalOptions, OndiscFlatLocalOptions)
       }
     }.

subtract_lists(List, Except) ->
    SetList = ordsets:from_list(List),
    SetExcept = ordsets:from_list(Except),
    ordsets:subtract(SetList, SetExcept).
