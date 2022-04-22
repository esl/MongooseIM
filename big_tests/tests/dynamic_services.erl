-module(dynamic_services).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, rpc/4]).

save_services(Config)  ->
    save_services(mim(), Config).

%% Save services from Node, overwriting previously saved services
save_services(Nodes, Config) when is_list(Nodes) ->
    lists:foldl(fun save_services/2, Config, Nodes);
save_services(Node = #{node := NodeName}, Config) ->
    Key = {saved_services, NodeName},
    Value = get_current_services(Node),
    lists:keystore(Key, 1, Config, {Key, Value}).

get_saved_config(Service, Config) ->
    get_saved_config(mim(), Service, Config).

get_saved_config(#{node := NodeName}, Service, Config) ->
    SavedServices = proplists:get_value({saved_services, NodeName}, Config),
    maps:get(Service, SavedServices).

restore_services(Config) ->
    restore_services(#{}, Config).

restore_services(RPCSpec, Config) when is_map(RPCSpec) ->
    [restore_saved_services(RPCSpec#{node => NodeName}, SavedServices)
     || {{saved_services, NodeName}, SavedServices} <- Config],
    Config.

restore_saved_services(Node, SavedServices) ->
    CurrentServices = get_current_services(Node),
    ToStop = maps:keys(CurrentServices) -- maps:keys(SavedServices),
    rpc(Node, mongoose_service, replace_services, [ToStop, SavedServices]).

get_current_services() ->
    get_current_services(mim()).

get_current_services(Node) ->
    rpc(Node, mongoose_service, loaded_services_with_opts, []).

ensure_services(Node, RequiredServices) ->
    ToStop = [M || {M, stopped} <- RequiredServices],
    ToEnsure = maps:without(ToStop, maps:from_list(RequiredServices)),
    rpc(Node, mongoose_service, replace_services, [ToStop, ToEnsure]).

ensure_stopped(Service) ->
    ensure_stopped(mim(), Service).

ensure_stopped(Node, Service) ->
    rpc(Node, mongoose_service, ensure_stopped, [Service]).

ensure_started(Service, Opts) ->
    ensure_started(mim(), Service, Opts).

ensure_started(Node, Service, Opts) ->
    rpc(Node, mongoose_service, ensure_started, [Service, Opts]).
