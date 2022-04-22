-module(mongoose_modules).

-include("mongoose.hrl").

%% API
-export([start/0, stop/0]).

%% Module management utilities for tests
-export([replace_modules/3, ensure_stopped/2, ensure_started/3]).
-ignore_xref([replace_modules/3, ensure_stopped/2, ensure_started/3]).

-type module_opts() :: gen_mod:module_opts().
-type module_map() :: gen_mod_deps:module_map().
-type module_list() :: gen_mod_deps:module_list().

%% @doc Start all configured modules in the dependency order.
-spec start() -> 'ok'.
start() ->
    [gen_mod:start_module(HostType, Mod, Opts) || HostType <- ?ALL_HOST_TYPES,
                                                  {Mod, Opts} <- sorted_modules(HostType)],
    ok.

%% @doc Stop all configured modules in the reverse dependency order
%% to avoid stopping modules which have other modules dependent on them.
-spec stop() -> 'ok'.
stop() ->
    [gen_mod:stop_module(HostType, Mod) || HostType <- ?ALL_HOST_TYPES,
                                           {Mod, _} <- lists:reverse(sorted_modules(HostType))],
    ok.

%% @doc Replace modules at runtime - only for testing and debugging.
%% Running modules from ToStop are stopped and modules from ToEnsure are (re)started when needed.
%% Unused dependencies are stopped if no running modules depend on them anymore.
%% To prevent an unused dependency from being stopped, you need to include it in ToEnsure.
-spec replace_modules(mongooseim:host_type(), [module()], module_map()) -> ok.
replace_modules(HostType, ToStop, ToEnsure) ->
    Current = get_modules(HostType),
    Old = maps:with(ToStop ++ maps:keys(ToEnsure), Current),
    OldWithDeps = gen_mod_deps:resolve_deps(HostType, Old),
    SortedOldWithDeps = gen_mod_deps:sort_deps(HostType, OldWithDeps),
    WithoutOld = maps:without(maps:keys(OldWithDeps), Current),
    WithNew = maps:merge(WithoutOld, ToEnsure),
    Target = gen_mod_deps:resolve_deps(HostType, WithNew),

    %% Stop each affected module if it is not in Target (stop deps first)
    [ensure_stopped(HostType, Module) || {Module, _} <- lists:reverse(SortedOldWithDeps),
        not maps:is_key(Module, Target)],

    %% Ensure each module from Target
    [ensure_started(HostType, Module, Opts) ||
        {Module, Opts} <- gen_mod_deps:sort_deps(HostType, Target)],
    ok.

%% @doc Make sure the module is stopped.
-spec ensure_stopped(mongooseim:host_type(), module()) ->
          already_stopped | {stopped, module_opts()}.
ensure_stopped(HostType, Module) ->
    Modules = get_modules(HostType),
    case maps:find(Module, Modules) of
        error ->
            already_stopped;
        {_, Opts} ->
            stop_module(HostType, Module, Modules),
            {stopped, Opts}
    end.

%% @doc Make sure the module is running with the provided options.
-spec ensure_started(mongooseim:host_type(), module(), module_opts()) ->
          already_started | {started, term()} | {restarted, module_opts(), term()}.
ensure_started(HostType, Module, Opts) ->
    Modules = get_modules(HostType),
    case maps:find(Module, Modules) of
        error ->
            {ok, Result} = start_module(HostType, Module, Opts, Modules),
            {started, Result};
        {_, Opts} ->
            already_started;
        {_, PrevOpts} ->
            stop_module(HostType, Module, Modules),
            {ok, Result} = start_module(HostType, Module, Opts, Modules),
            {restarted, PrevOpts, Result}
    end.

%% Helpers

-spec start_module(mongooseim:host_type(), module(), module_opts(), module_map()) -> {ok, term()}.
start_module(HostType, Module, Opts, Modules) ->
    set_modules(HostType, Modules#{Module => Opts}),
    try
        gen_mod:start_module(HostType, Module, Opts)
    catch
        C:R:S ->
            set_modules(HostType, Modules),
            erlang:raise(C, R, S)
    end.

-spec stop_module(mongooseim:host_type(), module(), module_map()) -> ok.
stop_module(HostType, Module, Modules) ->
    gen_mod:stop_module(HostType, Module),
    set_modules(HostType, maps:remove(Module, Modules)).

-spec sorted_modules(mongooseim:host_type()) -> module_list().
sorted_modules(HostType) ->
    gen_mod_deps:sort_deps(HostType, get_modules(HostType)).

-spec get_modules(mongooseim:host_type()) -> module_map().
get_modules(HostType) ->
    mongoose_config:get_opt({modules, HostType}).

-spec set_modules(mongooseim:host_type(), module_map()) -> ok.
set_modules(HostType, Modules) ->
    mongoose_config:set_opt({modules, HostType}, Modules).
