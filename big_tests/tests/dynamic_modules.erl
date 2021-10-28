-module(dynamic_modules).

-include_lib("common_test/include/ct.hrl").

-export([save_modules_for_host_types/2,
         save_modules/2, get_saved_config/3, ensure_modules/2, ensure_stopped/2,
         restore_modules/2, restore_modules/1]).
-export([stop/2, stop/3, start/3, start/4, restart/3, stop_running/2, start_running/1]).

-import(distributed_helper, [mim/0,
                             rpc/4]).

save_modules_for_host_types([HostType|HostTypes], Config) ->
    Config2 = save_modules(HostType, Config),
    save_modules_for_host_types(HostTypes, Config2);
save_modules_for_host_types([], Config) ->
    Config.

save_modules(HostType, Config) ->
    [{{saved_modules, HostType}, get_current_modules(HostType)} | Config].

get_saved_config(HostType, Module, Config) ->
    SavedModules = proplists:get_value({saved_modules, HostType}, Config),
    proplists:get_value(Module, SavedModules).

ensure_modules(HostType, RequiredModules) ->
    CurrentModules = get_current_modules(HostType),
    {ToReplace, ReplaceWith} = to_replace(RequiredModules, CurrentModules, [], []),
    ok = rpc(mim(), gen_mod_deps, replace_modules, [HostType, ToReplace, ReplaceWith]).

ensure_stopped(HostType, ModulesToStop) ->
    CurrentModules = get_current_modules(HostType),
    [stop(HostType, Mod) || {Mod, _Opts} <- CurrentModules,
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

restore_modules(HostType, Config) ->
    SavedModules = ?config({saved_modules, HostType}, Config),
    CurrentModules = get_current_modules(HostType),
    rpc(mim(), gen_mod_deps, replace_modules, [HostType, CurrentModules, SavedModules]).

restore_modules(Config) ->
    [restore_modules(HostType, Config)
     || {{saved_modules, HostType}, _SavedModules} <- Config],
    Config.

get_current_modules(HostType) ->
    rpc(mim(), gen_mod, loaded_modules_with_opts, [HostType]).

stop(HostType, Mod) ->
    Node = escalus_ct:get_config(ejabberd_node),
    stop(Node, HostType, Mod).

stop(#{node := Node}, HostType, Mod) ->
    stop(Node, HostType, Mod);
stop(Node, HostType, Mod) ->
    Cookie = escalus_ct:get_config(ejabberd_cookie),
    IsLoaded = escalus_rpc:call(Node, gen_mod, is_loaded, [HostType, Mod], 5000, Cookie),
    case IsLoaded of
        true -> unsafe_stop(Node, Cookie, HostType, Mod);
        false -> {error, stopped}
    end.

unsafe_stop(Node, Cookie, HostType, Mod) ->
    case escalus_rpc:call(Node, gen_mod, stop_module, [HostType, Mod], 5000, Cookie) of
        {badrpc, Reason} ->
            ct:fail("Cannot stop module ~p reason ~p", [Mod, Reason]);
        R -> R
    end.

start(HostType, Mod, Args) ->
    Node = escalus_ct:get_config(ejabberd_node),
    start(Node, HostType, Mod, Args).

start(#{node := Node}, HostType, Mod, Args) ->
    start(Node, HostType, Mod, Args);
start(Node, HostType, Mod, Args) ->
    Cookie = escalus_ct:get_config(ejabberd_cookie),
    case escalus_rpc:call(Node, gen_mod, start_module, [HostType, Mod, Args], 5000, Cookie) of
        {badrpc, Reason} ->
            ct:fail("Cannot start module ~p reason ~p", [Mod, Reason]);
        {ok, _} = R ->
            R;
        Reason ->
            ct:fail("Cannot start module ~p reason ~p", [Mod, Reason])
    end.

restart(HostType, Mod, Args) ->
    stop(HostType, Mod),
    ModStr = atom_to_list(Mod),
    case lists:reverse(ModStr) of
        "smbdr_" ++ Str -> %%check if we need to start rdbms module or regular
            stop(HostType, list_to_atom(lists:reverse(Str)));
        Str ->
            stop(HostType, list_to_atom(lists:reverse(Str)++"_rdbms"))
    end,
    start(HostType, Mod, Args).

start_running(Config) ->
    HostType = domain_helper:host_type(mim),
    case ?config(running, Config) of
        List when is_list(List) ->
            _ = [start(HostType, Mod, Args) || {Mod, Args} <- List];
        _ ->
            ok
    end.

stop_running(Mod, Config) ->
    ModL = atom_to_list(Mod),
    HostType = domain_helper:host_type(mim),
    Modules = rpc(mim(), mongoose_config, get_opt, [{modules, HostType}]),
    Filtered = lists:filter(fun({Module, _}) ->
                    ModuleL = atom_to_list(Module),
                    case lists:sublist(ModuleL, 1, length(ModL)) of
                        ModL -> true;
                        _ -> false
                    end;
                (_) -> false
            end, Modules),
    case Filtered of
        [] ->
            Config;
        [{Module,_Args}=Head|_] ->
            stop(HostType, Module),
            [{running, [Head]} | Config]
    end.
