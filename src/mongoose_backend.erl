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
    Backend = maps:get(backend, Opts, mnesia),
    BackendModule = backend_module(MainModule, Backend),
    persist_backend_name(HostType, MainModule, Backend, BackendModule),
    try
        mongoose_instrument:set_up(instrumentation(HostType, BackendModule, TrackedFuns))
    catch error:#{what := event_already_registered} ->
            %% The same backend can be initialized more than once because:
            %% - either it is global, and a module initializes it for each host type
            %% - or the module (or the entire app) was restarted
            ok
    end.

backend_module(Module, Backend) ->
    list_to_atom(atom_to_list(Module) ++ "_" ++ atom_to_list(Backend)).

backend_key(HostType, MainModule) ->
    {backend_module, HostType, MainModule}.

backend_name_key(HostType, MainModule) ->
    {backend_name, HostType, MainModule}.

-spec instrumentation(host_type_or_global(), module(), [function_name()]) ->
          [mongoose_instrument:spec()].
instrumentation(HostType, BackendModule, FunNames) ->
    [{BackendModule, labels(HostType, FunName),
      #{metrics => #{count => spiral, time => histogram}}} || FunName <- FunNames].

-spec labels(host_type_or_global(), function_name()) -> mongoose_instrument:labels().
labels(global, FunName) ->
    #{function => FunName};
labels(HostType, FunName) ->
    #{function => FunName, host_type => HostType}.

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
    mongoose_instrument:span(BackendModule, labels(HostType, FunName),
                             BackendModule, FunName, Args,
                             fun(Time, _Result) -> #{args => Args, time => Time, count => 1} end).

-spec is_exported(HostType :: host_type_or_global(),
                  MainModule :: main_module(),
                  FunName :: function_name(),
                  Arity :: integer()) -> boolean().
is_exported(HostType, MainModule, Function, Arity) ->
    BackendModule = get_backend_module(HostType, MainModule),
    erlang:function_exported(BackendModule, Function, Arity).
