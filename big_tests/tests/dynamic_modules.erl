-module(dynamic_modules).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, rpc/4]).

save_modules(HostTypes, Config) when is_list(HostTypes) ->
    save_modules(mim(), HostTypes, Config);
save_modules(HostType, Config) when is_binary(HostType) ->
    save_modules(mim(), [HostType], Config).

%% Save modules from Node for HostTypes, overwriting previously saved modules
save_modules(Node = #{node := NodeName}, HostTypes, Config) ->
    lists:foldl(fun(HostType, ConfigIn) ->
                        Key = {saved_modules, NodeName, HostType},
                        Value = get_current_modules(Node, HostType),
                        lists:keystore(Key, 1, ConfigIn, {Key, Value})
                end, Config, HostTypes).

get_saved_config(HostType, Module, Config) ->
    get_saved_config(mim(), HostType, Module, Config).

get_saved_config(#{node := NodeName}, HostType, Module, Config) ->
    SavedModules = proplists:get_value({saved_modules, NodeName, HostType}, Config),
    proplists:get_value(Module, SavedModules).

ensure_modules(HostType, RequiredModules) ->
    ensure_modules(mim(), HostType, RequiredModules).

ensure_modules(Node, HostType, RequiredModules) ->
    CurrentModules = get_current_modules(Node, HostType),
    {ToReplace, ReplaceWith} = to_replace(RequiredModules, CurrentModules, [], []),
    ok = rpc(Node, gen_mod_deps, replace_modules, [HostType, ToReplace, ReplaceWith]).

ensure_stopped(HostType, ModulesToStop) ->
    ensure_stopped(mim(), HostType, ModulesToStop).

ensure_stopped(Node, HostType, ModulesToStop) ->
    CurrentModules = get_current_modules(HostType),
    [stop(Node, HostType, Mod) || {Mod, _Opts} <- CurrentModules,
                                  lists:member(Mod, ModulesToStop)].

to_replace([], _CurrentModules, ReplaceAcc, ReplaceWithAcc) ->
    {lists:usort(ReplaceAcc), ReplaceWithAcc};
to_replace([RequiredModule | Rest], CurrentModules, ReplaceAcc, ReplaceWithAcc) ->
    {Mod, Opts} = RequiredModule,
    {NewReplaceAcc, NewReplaceWithAcc} =
        case lists:keyfind(Mod, 1, CurrentModules) of
            false when Opts =:= stopped -> {ReplaceAcc, ReplaceWithAcc};
            false -> {ReplaceAcc, [RequiredModule | ReplaceWithAcc]};
            {Mod, Opts} -> {ReplaceAcc, ReplaceWithAcc};
            ExistingMod when Opts =:= stopped -> {[ExistingMod | ReplaceAcc], ReplaceWithAcc};
            ExistingMod -> {[ExistingMod | ReplaceAcc], [RequiredModule | ReplaceWithAcc]}
        end,
    to_replace(Rest, CurrentModules, NewReplaceAcc, NewReplaceWithAcc).

restore_modules(Config) ->
    restore_modules(#{}, Config).

restore_modules(RPCSpec, Config) when is_map(RPCSpec) ->
    [restore_modules(RPCSpec#{node => NodeName}, HostType, SavedModules)
     || {{saved_modules, NodeName, HostType}, SavedModules} <- Config],
    Config.

restore_modules(Node, HostType, SavedModules) ->
    CurrentModules = get_current_modules(Node, HostType),
    rpc(Node, gen_mod_deps, replace_modules, [HostType, CurrentModules, SavedModules]).

get_current_modules(HostType) ->
    get_current_modules(mim(), HostType).

get_current_modules(Node, HostType) ->
    rpc(Node, gen_mod, loaded_modules_with_opts, [HostType]).

stop(HostType, Mod) ->
    stop(mim(), HostType, Mod).

stop(Node, HostType, Mod) ->
    IsLoaded = rpc(Node, gen_mod, is_loaded, [HostType, Mod]),
    case IsLoaded of
        true -> unsafe_stop(Node, HostType, Mod);
        false -> {error, stopped}
    end.

unsafe_stop(Node, HostType, Mod) ->
    case rpc(Node, gen_mod, stop_module, [HostType, Mod]) of
        {badrpc, Reason} ->
            ct:fail("Cannot stop module ~p reason ~p", [Mod, Reason]);
        R -> R
    end.

start(HostType, Mod, Args) ->
    start(mim(), HostType, Mod, Args).

start(Node, HostType, Mod, Args) ->
    case rpc(Node, gen_mod, start_module, [HostType, Mod, Args]) of
        {ok, _} = R ->
            R;
        Reason ->
            ct:fail("Cannot start module ~p reason ~p", [Mod, Reason])
    end.

restart(HostType, Mod, Args) ->
    restart(mim(), HostType, Mod, Args).

restart(Node, HostType, Mod, Args) ->
    stop(Node, HostType, Mod),
    ModStr = atom_to_list(Mod),
    case lists:reverse(ModStr) of
        "smbdr_" ++ Str -> %%check if we need to start rdbms module or regular
            stop(Node, HostType, list_to_atom(lists:reverse(Str)));
        Str ->
            stop(Node, HostType, list_to_atom(lists:reverse(Str)++"_rdbms"))
    end,
    start(Node, HostType, Mod, Args).
