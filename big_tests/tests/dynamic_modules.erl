-module(dynamic_modules).

-include_lib("common_test/include/ct.hrl").

-export([save_modules/2, ensure_modules/2, restore_modules/2]).
-export([stop/2, stop/3, start/3, start/4, restart/3, stop_running/2, start_running/1]).

-import(distributed_helper, [mim/0,
                             rpc/4]).

save_modules(Domain, Config) ->
    [{saved_modules, get_current_modules(Domain)} | Config].

ensure_modules(Domain, RequiredModules) ->
    CurrentModules = get_current_modules(Domain),
    {ToReplace, ReplaceWith} = to_replace(RequiredModules, CurrentModules, [], []),
    ok = rpc(mim(), gen_mod_deps, replace_modules, [Domain, ToReplace, ReplaceWith]).

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

restore_modules(Domain, Config) ->
    SavedModules = ?config(saved_modules, Config),
    CurrentModules = get_current_modules(Domain),
    rpc(mim(), gen_mod_deps, replace_modules, [Domain, CurrentModules, SavedModules]).

get_current_modules(Domain) ->
    rpc(mim(), gen_mod, loaded_modules_with_opts, [Domain]).

stop(Domain, Mod) ->
    Node = escalus_ct:get_config(ejabberd_node),
    stop(Node, Domain, Mod).

stop(Node, Domain, Mod) ->
    Cookie = escalus_ct:get_config(ejabberd_cookie),
    IsLoaded = escalus_rpc:call(Node, gen_mod, is_loaded, [Domain, Mod], 5000, Cookie),
    case IsLoaded of
        true -> unsafe_stop(Node, Cookie, Domain, Mod);
        false -> {error, stopped}
    end.

unsafe_stop(Node, Cookie, Domain, Mod) ->
    case escalus_rpc:call(Node, gen_mod, stop_module, [Domain, Mod], 5000, Cookie) of
        {badrpc, Reason} ->
            ct:fail("Cannot stop module ~p reason ~p", [Mod, Reason]);
        R -> R
    end.

start(Domain, Mod, Args) ->
    Node = escalus_ct:get_config(ejabberd_node),
    start(Node, Domain, Mod, Args).

start(Node, Domain, Mod, Args) ->
    Cookie = escalus_ct:get_config(ejabberd_cookie),
    case escalus_rpc:call(Node, gen_mod, start_module, [Domain, Mod, Args], 5000, Cookie) of
        {badrpc, Reason} ->
            ct:fail("Cannot start module ~p reason ~p", [Mod, Reason]);
        R -> R
    end.

restart(Domain, Mod, Args) ->
    stop(Domain, Mod),
    ModStr = atom_to_list(Mod),
    case lists:reverse(ModStr) of
        "smbdr_" ++ Str -> %%check if we need to start rdbms module or regular
            stop(Domain, list_to_atom(lists:reverse(Str)));
        Str ->
            stop(Domain, list_to_atom(lists:reverse(Str)++"_rdbms"))
    end,
    start(Domain, Mod, Args).

start_running(Config) ->
    Domain = ct:get_config({hosts, mim, domain}),
    case ?config(running, Config) of
        List when is_list(List) ->
            _ = [start(Domain, Mod, Args) || {Mod, Args} <- List];
        _ ->
            ok
    end.

stop_running(Mod, Config) ->
    ModL = atom_to_list(Mod),
    Domain = escalus_ejabberd:unify_str_arg(
               ct:get_config({hosts, mim, domain})),
    Modules = rpc(mim(), ejabberd_config, get_local_option, [{modules, Domain}]),
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
            stop(Domain, Module),
            [{running, [Head]} | Config]
    end.
