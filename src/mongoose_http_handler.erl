%% @doc Manage the configuration and initialization of HTTP handlers

-module(mongoose_http_handler).

-export([config_spec/0, process_config/2, cowboy_host/1, get_routes/1]).

-type options() :: #{host := '_' | string(),
                     path := string(),
                     module := module(),
                     atom() => any()}.

-type path() :: iodata().
-type routes() :: [{path(), module(), #{atom() => any()}}].

-export_type([options/0, path/0, routes/0]).

-callback config_spec() -> mongoose_config_spec:config_section().
-callback routes(options()) -> routes().

-optional_callbacks([config_spec/0, routes/1]).

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

%% @doc Return a config section with a list of sections for each handler type
-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    Items = maps:from_list([{atom_to_binary(Module),
                             #list{items = handler_config_spec(Module),
                                   wrap = none}}
                            || Module <- configurable_handler_modules()]),
    #section{items = Items#{default => #list{items = common_handler_config_spec(),
                                             wrap = none}},
             format_items = list,
             validate_keys = module,
             include = always}.

%% @doc Return a config section with handler options
-spec handler_config_spec(module()) -> mongoose_config_spec:config_section().
handler_config_spec(Module) ->
    mongoose_config_utils:merge_sections(common_handler_config_spec(), Module:config_spec()).

common_handler_config_spec() ->
    #section{items = #{<<"host">> => #option{type = string,
                                             validate = non_empty,
                                             process = fun ?MODULE:cowboy_host/1},
                       <<"path">> => #option{type = string}
                      },
             required = [<<"host">>, <<"path">>],
             process = fun ?MODULE:process_config/2}.

process_config([item, HandlerType | _], Opts) ->
    Opts#{module => binary_to_atom(HandlerType)}.

%% @doc Return the list of Cowboy routes for the specified handler configuration.
%% Cowboy will search for a matching Host, then for a matching Path. If no Path matches,
%% Cowboy will not search for another matching Host. So, we must merge all Paths for each Host,
%% add any wildcard Paths to each Host, and ensure that the wildcard Host '_' is listed last.
%% A proplist ensures that the user can influence Host ordering if wildcards like "[...]" are used.
-spec get_routes([options()]) -> cowboy_router:routes().
get_routes(Handlers) ->
    Routes = lists:foldl(fun add_handler_routes/2, [], Handlers),
    WildcardPaths = proplists:get_value('_', Routes, []),
    Merge = fun(Paths) -> Paths ++ WildcardPaths end,
    Merged = lists:keymap(Merge, 2, proplists:delete('_', Routes)),
    Final = Merged ++ [{'_', WildcardPaths}],
    ?LOG_DEBUG(#{what => configured_cowboy_routes, routes => Final}),
    Final.

add_handler_routes(#{host := Host, path := Path, module := Module} = HandlerOpts, Routes) ->
    HandlerRoutes = case mongoose_lib:is_exported(Module, routes, 1) of
                        true -> Module:routes(HandlerOpts);
                        false -> [{Path, Module, HandlerOpts}]
                    end,
    HostRoutes = proplists:get_value(Host, Routes, []),
    lists:keystore(Host, 1, Routes, {Host, HostRoutes ++ HandlerRoutes}).

%% @doc Translate "_" used in TOML to '_' expected by COwboy
cowboy_host("_") -> '_';
cowboy_host(Host) -> Host.

%% @doc All handlers implementing config_spec/0 are listed here
configurable_handler_modules() ->
    [mod_websockets,
     mongoose_client_api,
     mongoose_admin_api,
     mongoose_graphql_handler].
