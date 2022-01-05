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
    maps:get(Module, SavedModules).

ensure_modules(HostType, RequiredModules) ->
    ensure_modules(mim(), HostType, RequiredModules).

ensure_modules(Node, HostType, RequiredModules) ->
    ToStop = [M || {M, stopped} <- RequiredModules],
    ToEnsure = maps:without(ToStop, maps:from_list(RequiredModules)),
    rpc(Node, mongoose_modules, replace_modules, [HostType, ToStop, ToEnsure]).

ensure_stopped(HostType, ModulesToStop) ->
    ensure_stopped(mim(), HostType, ModulesToStop).

ensure_stopped(Node, HostType, ModulesToStop) ->
    [{Mod, stop(Node, HostType, Mod)} || Mod <- ModulesToStop].

restore_modules(Config) ->
    restore_modules(#{}, Config).

restore_modules(RPCSpec, Config) when is_map(RPCSpec) ->
    [restore_modules(RPCSpec#{node => NodeName}, HostType, SavedModules)
     || {{saved_modules, NodeName, HostType}, SavedModules} <- Config],
    Config.

restore_modules(Node, HostType, SavedModules) ->
    CurrentModules = get_current_modules(Node, HostType),
    ToStop = maps:keys(CurrentModules) -- maps:keys(SavedModules),
    rpc(Node, mongoose_modules, replace_modules, [HostType, ToStop, SavedModules]).

get_current_modules(HostType) ->
    get_current_modules(mim(), HostType).

get_current_modules(Node, HostType) ->
    rpc(Node, gen_mod, loaded_modules_with_opts, [HostType]).

stop(HostType, Mod) ->
    stop(mim(), HostType, Mod).

stop(Node, HostType, Mod) ->
    rpc(Node, mongoose_modules, ensure_stopped, [HostType, Mod]).

start(HostType, Mod, Args) ->
    start(mim(), HostType, Mod, Args).

start(Node, HostType, Mod, Args) ->
    rpc(Node, mongoose_modules, ensure_started, [HostType, Mod, Args]).

restart(HostType, Mod, Args) ->
    restart(mim(), HostType, Mod, Args).

restart(Node, HostType, Mod, Args) ->
    stop(Node, HostType, Mod),
    start(Node, HostType, Mod, Args).
