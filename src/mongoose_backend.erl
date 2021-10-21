-module(mongoose_backend).

%% API
-export([init_per_host_type/4,
         call/4,
         call_tracked/4,
         get_backend_name/2]).

%% remove after mongoose_rdbms is refactored not to use dynamically compiled backend
-ignore_xref([get_backend_name/2]).

%% Legacy call from backend_module
-export([ensure_backend_metrics/2]).

%% For debugging and tests
-export([get_backend_module/2]).
-ignore_xref([get_backend_module/2]).

-type function_name() :: atom().
-type main_module() :: module().
-type backend_module() :: module().

-spec init_per_host_type(HostType :: mongooseim:host_type(),
                         MainModule :: main_module(),
                         TrackedFuns :: [function_name()],
                         Opts :: gen_mod:module_opts()) -> ok.
init_per_host_type(HostType, MainModule, TrackedFuns, Opts) ->
    ensure_backend_metrics(MainModule, TrackedFuns),
    Backend = gen_mod:get_opt(backend, Opts, mnesia),
    BackendModule = backend_module(MainModule, Backend),
    persist_backend_name(HostType, MainModule, Backend, BackendModule),
    ok.

backend_module(Module, Backend) ->
    list_to_atom(atom_to_list(Module) ++ "_" ++ atom_to_list(Backend)).

call_metric(MainModule, FunName) ->
    [backends, MainModule, calls, FunName].

time_metric(MainModule, FunName) ->
    [backends, MainModule, FunName].

backend_key(HostType, MainModule) ->
    {backend_module, HostType, MainModule}.

backend_name_key(HostType, MainModule) ->
    {backend_name, HostType, MainModule}.

-spec ensure_backend_metrics(MainModule :: main_module(),
                             FunNames :: [function_name()]) -> ok.
ensure_backend_metrics(MainModule, FunNames) ->
    EnsureFun = fun(FunName) ->
                        CM = call_metric(MainModule, FunName),
                        TM = time_metric(MainModule, FunName),
                        mongoose_metrics:ensure_metric(global, CM, spiral),
                        mongoose_metrics:ensure_metric(global, TM, histogram)
                end,
    lists:foreach(EnsureFun, FunNames).

persist_backend_name(HostType, MainModule, Backend, BackendModule) ->
    ModuleKey = backend_key(HostType, MainModule),
    persistent_term:put(ModuleKey, BackendModule),
    NameKey = backend_name_key(HostType, MainModule),
    persistent_term:put(NameKey, Backend).

%% @doc Get a backend module, stored in init_per_host_type.
-spec get_backend_module(HostType :: mongooseim:host_type(),
                         MainModule :: main_module()) ->
    BackendModule :: backend_module().
get_backend_module(HostType, MainModule) ->
    ModuleKey = backend_key(HostType, MainModule),
    persistent_term:get(ModuleKey).

%% @doc Get a backend name, like `pgsql', stored in init_per_host_type.
-spec get_backend_name(HostType :: mongooseim:host_type(),
                       MainModule :: main_module()) -> BackendName :: atom().
get_backend_name(HostType, MainModule) ->
    Key = backend_name_key(HostType, MainModule),
    persistent_term:get(Key).

-spec call(HostType :: mongooseim:host_type(),
           MainModule :: main_module(),
           FunName :: function_name(),
           Args :: [term()]) -> term().
call(HostType, MainModule, FunName, Args) ->
    BackendModule = get_backend_module(HostType, MainModule),
    erlang:apply(BackendModule, FunName, Args).

-spec call_tracked(HostType :: mongooseim:host_type(),
                   MainModule :: main_module(),
                   FunName :: function_name(),
                   Args :: [term()]) -> term().
call_tracked(HostType, MainModule, FunName, Args) ->
    BackendModule = get_backend_module(HostType, MainModule),
    CM = call_metric(MainModule, FunName),
    TM = time_metric(MainModule, FunName),
    mongoose_metrics:update(global, CM, 1),
    {Time, Result} = timer:tc(BackendModule, FunName, Args),
    mongoose_metrics:update(global, TM, Time),
    Result.