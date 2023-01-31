-module(mongoose_backend).

%% API
-export([init/4,
         call/4,
         call_tracked/4,
         is_exported/4,
         get_backend_name/2]).

%% remove after mongoose_rdbms is refactored not to use dynamically compiled backend
-ignore_xref([get_backend_name/2]).

%% For debugging and tests
-export([get_backend_module/2]).
-ignore_xref([get_backend_module/2]).

-type function_name() :: atom().
-type main_module() :: module().
-type backend_module() :: module().
-type host_type_or_global() :: mongooseim:host_type_or_global().

-spec init(HostType :: host_type_or_global(),
           MainModule :: main_module(),
           TrackedFuns :: [function_name()],
           Opts :: map()) -> ok.
init(HostType, MainModule, TrackedFuns, Opts) ->
    ensure_backend_metrics(MainModule, TrackedFuns),
    Backend = maps:get(backend, Opts, mnesia),
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

%% @doc Get a backend module, stored in init.
-spec get_backend_module(HostType :: host_type_or_global(),
                         MainModule :: main_module()) ->
    BackendModule :: backend_module().
get_backend_module(HostType, MainModule) ->
    ModuleKey = backend_key(HostType, MainModule),
    persistent_term:get(ModuleKey).

%% @doc Get a backend name, like `pgsql', stored in init.
-spec get_backend_name(HostType :: host_type_or_global(),
                       MainModule :: main_module()) -> BackendName :: atom().
get_backend_name(HostType, MainModule) ->
    Key = backend_name_key(HostType, MainModule),
    persistent_term:get(Key).

-spec call(HostType :: host_type_or_global(),
           MainModule :: main_module(),
           FunName :: function_name(),
           Args :: [term()]) -> term().
call(HostType, MainModule, FunName, Args) ->
    BackendModule = get_backend_module(HostType, MainModule),
    erlang:apply(BackendModule, FunName, Args).

-spec call_tracked(HostType :: host_type_or_global(),
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

-spec is_exported(HostType :: host_type_or_global(),
                  MainModule :: main_module(),
                  FunName :: function_name(),
                  Arity :: integer()) -> boolean().
is_exported(HostType, MainModule, Function, Arity) ->
    BackendModule = get_backend_module(HostType, MainModule),
    erlang:function_exported(BackendModule, Function, Arity).
