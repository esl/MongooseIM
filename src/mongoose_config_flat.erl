%% @doc Pure flat config logic.
%% No ets table manipulations, no Mnesia, no starting modules, no file reading here.
%% Everything here is safe, side effect free.
%% OK, logging is possible, but keep it to minimum.
%%
%% Flat format is a list of pairs, corresponding to some part of config tree.
%% Keys are:
%% - [g, K] - global option;
%% - [a, K] - acl list;
%% - [l, listener, Address, Module] - enabled listener;
%% - [l, listener_opt, Address, Module, OptName] - option with value;
%% - [l, listener_simple_opt, Address, Module, Opt] - option without value;
%% - [h, Host, modules] - we need this to build tree (expand options back);
%% - [h, Host, module, Module] - for an enabled module;
%% - [h, H, module_opt, Module, OptName] - option of module;
%% - [h, Host, module_subopt, Module, OptName|Path] - if value of OptName is
%%   a proplist, we split is into module_subopt-s recursively.
-module(mongoose_config_flat).

-export([flatten_opts/2,
         flatten_all_opts/1,
         expand_opts/1,
         expand_all_opts/1]).

-export([does_pattern_match/2]).
-export([split_node_specific_options_tree/2]).
-export([flatten_global_config_opts/1]).

-include("mongoose.hrl").
-include("ejabberd_config.hrl").

-type flat_option() :: term().
-type flat_options() :: list(flat_option()).

-export_type([flat_option/0, flat_options/0]).


%% @doc Convert LocalConfig and HostLocalConfig to a flat list of options
flatten_opts(LC, LCH) ->
    flatten_local_config_opts(LC) ++ flatten_local_config_host_opts(LCH).

flatten_all_opts(#{
        global_config := GlobalConfig,
        local_config := LocalConfig,
        host_config := HostConfig
    }) ->
    flatten_global_config_opts(GlobalConfig)
    ++
    flatten_local_config_opts(LocalConfig)
    ++
    flatten_local_config_host_opts(HostConfig).

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
    [{[l, listen], 'FLAT'}] ++
    lists:append([flatten_listen(Address, Module, Opts)
                  || {Address, Module, Opts} <- V]);
flatten_local_config_opt(K, V) ->
    [{[l, K], V}].

flatten_listen(Address, Module, Opts) ->
    [{[l, listener, Address, Module], 'FLAT'}
     | flatten_listen_opts(Address, Module, Opts)].

flatten_listen_opts(Address, Module, Opts) ->
    lists:map(fun({OptName, OptValue}) ->
                {[l, listener_opt, Address, Module, OptName], OptValue};
                 (Opt) -> %% not key-value option, example: starttls
                {[l, listener_simple_opt, Address, Module, Opt], simple}
              end, Opts).

flatten_local_config_host_opt(modules, Host, V) ->
    [{[h, Host, modules], 'FLAT'}] ++
    lists:append([flatten_module(Host, Module, Opts) || {Module, Opts} <- V]);
flatten_local_config_host_opt(K, Host, V) ->
    [{[h, Host, K], V}].

flatten_module(H, Module, Opts) ->
    [{[h, H, module, Module], 'FLAT'}|flatten_module_opts(H, Module, Opts)].

flatten_module_opts(H, Module, Opts) ->
    lists:flatmap(fun({OptName, OptValue}) ->
                {OptValue2, FlatOpts} =
                    flatten_module_subopts(H, OptName, OptValue, Module),
                [ {[h, H, module_opt, Module, OptName], OptValue2} | FlatOpts ];
                  (OptName) -> %% Special case, flag is the same as {flag, true}
                [ {[h, H, module_opt, Module, OptName], true} ]
        end, Opts).

flatten_module_subopts(Host, OptName, OptValue, Module) ->
    Path = [h, Host, module_subopt, Module, OptName],
    flatten_subopts(Path, OptValue).

flatten_subopts(Path, OptValue) ->
    case can_be_flattened_value(OptValue) of
        false ->
            {OptValue, []};
        true ->
            FlatOpts = lists:flatmap(fun({SubOptName, SubOptValue}) ->
                                            flatten_subopt(Path, SubOptName, SubOptValue)
                                    end, OptValue),
            {'FLAT', FlatOpts}
    end.

flatten_subopt(Path, SubOptName, SubOptValue) ->
    Path2 = Path ++ [SubOptName],
    case can_be_flattened_value(SubOptValue) of
        false ->
            [{Path2, SubOptValue}];
        true ->
            FlatOpts = lists:flatmap(fun({SubOptName2, SubOptValue2}) ->
                                            flatten_subopt(Path2, SubOptName2, SubOptValue2)
                                    end, SubOptValue),
            [{Path2, 'FLAT'}|FlatOpts]
    end.



can_be_flattened_value([_|_] = List) ->
    lists:all(fun({_,_}) -> true; (_) -> false end, List);
can_be_flattened_value(_) ->
    false.

%% @doc Convert flat list of options back to LocalConfig and HostLocalConfig
%% Useful, but not used
expand_opts(FlatOpts) ->
    Groups = group_flat_opts(FlatOpts),
    %% We ignore global options here
    LocalConfigGroup = maps:get(local_config, Groups, []),
    HostConfigGroup = maps:get(host_config, Groups, []),
    LocalConfig = expand_local_config_group(LocalConfigGroup, Groups),
    HostConfig = expand_host_config_group(HostConfigGroup, Groups),
    {LocalConfig, HostConfig}.

%% Useful, but not used
expand_all_opts(FlatOpts) ->
    Groups = group_flat_opts(FlatOpts),
    GlobalConfigGroup = maps:get(global_config, Groups, []),
    LocalConfigGroup = maps:get(local_config, Groups, []),
    HostConfigGroup = maps:get(host_config, Groups, []),
    GlobalConfig = expand_global_config_group(GlobalConfigGroup, Groups),
    LocalConfig = expand_local_config_group(LocalConfigGroup, Groups),
    HostConfig = expand_host_config_group(HostConfigGroup, Groups),
    #{
        global_config => GlobalConfig,
        local_config => LocalConfig,
        host_config => HostConfig
    }.

expand_global_config_group(GlobalConfigGroup, Groups) ->
    [expand_global_config_group_item(K, V, Groups)
     || {K, V} <- GlobalConfigGroup].

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
expand_local_config_group_item(global, K, V, _Groups) ->
    #config{key = K, value = V};
expand_local_config_group_item(acl, K, V, _Groups) ->
    {acl, K, V};
expand_local_config_group_item(hostname, Host, simple, _Groups) ->
    Host.

expand_local_config_group_item_value(listen, 'FLAT', Groups) ->
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

expand_host_config_group_item_value(Host, modules, 'FLAT', Groups) ->
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
    ModuleOpts = maps:get(GroupName, Groups, []),
    expand_module_opts(ModuleOpts, Host, Module, Groups).

expand_module_opts([{OptName, 'FLAT'} | ModuleOpts], Host, Module, Groups) ->
    OptValue = expand_module_subopts(OptName, Host, Module, Groups),
    Opt = {OptName, OptValue},
    [Opt | expand_module_opts(ModuleOpts, Host, Module, Groups)];
expand_module_opts([Opt | ModuleOpts], Host, Module, Groups) ->
    [Opt | expand_module_opts(ModuleOpts, Host, Module, Groups)];
expand_module_opts([], _Host, _Module, _Groups) ->
    [].

expand_module_subopts(OptName, Host, Module, Groups) ->
    Path = [OptName],
    expand_module_subopts_path(Path, Host, Module, Groups).

expand_module_subopts_path(Path, Host, Module, Groups) ->
    GroupName = {module_subopts, Host, Module, Path},
    SubOpts = maps:get(GroupName, Groups, []),
    lists:map(fun({SubOptName, 'FLAT'}) ->
                      Path2 = Path ++ [SubOptName],
                      SubOptValue = expand_module_subopts_path(Path2, Host, Module, Groups),
                      {SubOptName, SubOptValue};
                 (SubOpt) ->
                      SubOpt
              end, SubOpts).

%% @doc group opts for faster lookups in expand_opts
group_flat_opts(FlatOpts) ->
    %% Process FlatOpts in reverse order
    lists:foldr(fun group_flat_opt/2, #{}, FlatOpts).

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

group_flat_opt({[h, Host, module, Module], 'FLAT'}, Groups) ->
    GroupName  = {modules, Host},
    GroupValue = Module,
    add_to_group(GroupName, GroupValue, Groups);

group_flat_opt({[h, Host, module_opt, Module, OptName], OptValue}, Groups) ->
    GroupName  = {module_opts, Host, Module},
    GroupValue = {OptName, OptValue},
    add_to_group(GroupName, GroupValue, Groups);

group_flat_opt({[h, Host, module_subopt, Module, OptName|Path], OptValue}, Groups) ->
    {Last, Init} = last_init(Path),
    GroupName  = {module_subopts, Host, Module,  [OptName|Init]},
    GroupValue = {Last, OptValue},
    add_to_group(GroupName, GroupValue, Groups);

group_flat_opt({[l, listener, Address, Module], 'FLAT'}, Groups) ->
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

last_init(List) ->
    [Last|RevInit] = lists:reverse(List),
    {Last, lists:reverse(RevInit)}.

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
                           subject => Subject, patten => Pattern,
                           test_ms_result => Other})
    end.

subtract_lists(List, Except) ->
    SetList = ordsets:from_list(List),
    SetExcept = ordsets:from_list(Except),
    ordsets:subtract(SetList, SetExcept).

%% split_node_specific_options does not count nested options.
%% split_node_specific_options_tree works for nested options too.
%% I.e. options of node specific module would be filtered by this function.
split_node_specific_options_tree(NodeSpecificPatterns, FlatOpts) ->
    Split = split_node_specific_options(NodeSpecificPatterns, FlatOpts),
    case maps:get(matched_patterns, Split) of
        [] ->
            Split#{node_specific_orphans => []}; %% no node specific stuff
        [_|_] ->
            do_split_node_specific_options_tree(Split)
    end.

do_split_node_specific_options_tree(Split=#{common_options := CommonOpts}) ->
    Orphans = node_specific_orphans(CommonOpts),
    CommonOpts2 = subtract_lists(CommonOpts, Orphans),
    Split#{common_options => CommonOpts2,
           node_specific_orphans => Orphans}.

%% node_specific_orphans are for example options of node-specific modules.
node_specific_orphans(FlatOpts) ->
    CatOpts = expand_all_opts(FlatOpts),
    ReflatOpts = flatten_all_opts(CatOpts),
    subtract_lists(FlatOpts, ReflatOpts).

%% Split local options to:
%% - node specific (can be different for different nodes)
%% - common local options (same for all nodes, but not global)
%% Collects information about matches for debugging
split_node_specific_options([], FlatOpts) ->
    %% No patterns case
    #{node_specific_options => [],
      common_options => FlatOpts,
      matched_patterns => []};
split_node_specific_options(NodeSpecificPatterns, FlatOpts) ->
    split_node_specific_options(NodeSpecificPatterns, FlatOpts, [], [], []).

split_node_specific_options(NodeSpecificPatterns,
                            [{OptKey, Value}=Opt|FlatOpts],
                            NodeSpecificOpts,
                            CommonOpts,
                            MatchedPatterns) ->
    case find_matching_node_specific_pattern(NodeSpecificPatterns, OptKey) of
        nomatch ->
            CommonOpts2 = [Opt|CommonOpts],
            split_node_specific_options(NodeSpecificPatterns, FlatOpts,
                                        NodeSpecificOpts, CommonOpts2,
                                        MatchedPatterns);
        {match, Pattern} ->
            NodeSpecificOpts2 = [Opt|NodeSpecificOpts],
            %% For debugging
            MPattern = #{option_key => OptKey,
                         option_value => Value,
                         matched_pattern => Pattern},
            MatchedPatterns2 = [MPattern|MatchedPatterns],
            split_node_specific_options(NodeSpecificPatterns, FlatOpts,
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

