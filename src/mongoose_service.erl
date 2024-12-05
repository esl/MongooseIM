%% @doc Behaviour for sub-systems that are global to the node.
%%
%% This plays in counterpart to `m:mongoose_modules', which enables sub-systems that
%% are scoped to a specific `t:mongooseim:host_type/0'.
%%
%% Examples of services are
%% <ul>
%%   <li>`m:service_domain_db'</li>
%%   <li>`m:service_mongoose_system_metrics'</li>
%% </ul>
-module(mongoose_service).

-include("mongoose.hrl").

%% API
-export([start/0,
         stop/0,
         config_spec/1,
         get_deps/1,
         is_loaded/1,
         assert_loaded/1]).

%% Shell utilities
-export([loaded_services_with_opts/0]).

%% Service management utilities for tests
-export([replace_services/2, ensure_stopped/1, ensure_started/2]).

-ignore_xref([loaded_services_with_opts/0, replace_services/2, ensure_stopped/1, ensure_started/2]).

-type service() :: module().
-type opt_key() :: atom().
-type opt_value() :: mongoose_config:value().
-type options() :: #{opt_key() => opt_value()}.
-type start_result() :: any().
-type service_list() :: [{service(), options()}].
-type service_map() :: #{service() => options()}.

-export_type([service/0, service_list/0, service_map/0, options/0]).

-callback start(options()) -> start_result().
-callback stop() -> any().
-callback config_spec() -> mongoose_config_spec:config_section().
-callback deps() -> [service()].

-optional_callbacks([deps/0]).

%% @doc Start all configured services in the dependency order.
-spec start() -> ok.
start() ->
    [start_service(Service, Opts) || {Service, Opts} <- sorted_services()],
    ok.

%% @doc Stop all configured services in the reverse dependency order
%% to avoid stopping services which have other services dependent on them.
-spec stop() -> ok.
stop() ->
    [stop_service(Service) || {Service, _Opts} <- lists:reverse(sorted_services())],
    ok.

%% @doc Replace services at runtime - only for testing and debugging.
%% Running services from ToStop are stopped and services from ToEnsure are (re)started when needed.
%% Unused dependencies are stopped if no running services depend on them anymore.
%% To prevent an unused dependency from being stopped, you need to include it in ToEnsure.
-spec replace_services([service()], service_map()) -> ok.
replace_services(ToStop, ToEnsure) ->
    Current = loaded_services_with_opts(),
    Old = maps:with(ToStop ++ maps:keys(ToEnsure), Current),
    OldWithDeps = mongoose_service_deps:resolve_deps(Old),
    SortedOldWithDeps = mongoose_service_deps:sort_deps(OldWithDeps),
    WithoutOld = maps:without(maps:keys(OldWithDeps), Current),
    WithNew = maps:merge(WithoutOld, ToEnsure),
    Target = mongoose_service_deps:resolve_deps(WithNew),

    %% Stop each affected service if it is not in Target (stop deps first)
    [ensure_stopped(Service) || {Service, _} <- lists:reverse(SortedOldWithDeps),
        not maps:is_key(Service, Target)],

    %% Ensure each service from Target
    [ensure_started(Service, Opts) || {Service, Opts} <- mongoose_service_deps:sort_deps(Target)],
    ok.

-spec config_spec(service()) -> mongoose_config_spec:config_section().
config_spec(Service) ->
    Service:config_spec().

-spec get_deps(service()) -> [service()].
get_deps(Service) ->
    case mongoose_lib:is_exported(Service, deps, 0) of
        true -> Service:deps();
        false -> []
    end.

-spec sorted_services() -> service_list().
sorted_services() ->
    mongoose_service_deps:sort_deps(loaded_services_with_opts()).

-spec set_services(service_map()) -> ok.
set_services(Services) ->
    mongoose_config:set_opt(services, Services).

-spec ensure_stopped(service()) -> {stopped, options()} | already_stopped.
ensure_stopped(Service) ->
    case loaded_services_with_opts() of
        #{Service := Opts} = Services ->
            stop_service(Service, Services),
            {stopped, Opts};
        _Services ->
            already_stopped
    end.

-spec ensure_started(service(), options()) ->
          {started, start_result()} | {restarted, options(), start_result()} | already_started.
ensure_started(Service, Opts) ->
    case loaded_services_with_opts() of
        #{Service := Opts} ->
            already_started;
        #{Service := PrevOpts} = Services ->
            stop_service(Service, Services),
            {ok, Result} = start_service(Service, Opts, Services),
            {restarted, PrevOpts, Result};
        Services ->
            {ok, Result} = start_service(Service, Opts, Services),
            {started, Result}
    end.

-spec start_service(service(), options(), service_map()) -> {ok, start_result()}.
start_service(Service, Opts, Services) ->
    set_services(Services#{Service => Opts}),
    try
        start_service(Service, Opts)
    catch
        C:R:S ->
            set_services(Services),
            erlang:raise(C, R, S)
    end.

-spec stop_service(service(), service_map()) -> ok.
stop_service(Service, Services) ->
    stop_service(Service),
    set_services(maps:remove(Service, Services)).

start_service(Service, Opts) ->
    assert_loaded(Service),
    try
        Res = Service:start(Opts),
        ?LOG_INFO(#{what => service_started, service => Service,
                    text => <<"Started MongooseIM service">>}),
        case Res of
            {ok, _} -> Res;
            _ -> {ok, Res}
        end
    catch Class:Reason:Stacktrace ->
            ?LOG_CRITICAL(#{what => service_startup_failed,
                            text => <<"Failed to start MongooseIM service">>,
                            service => Service, opts => Opts,
                            class => Class, reason => Reason, stacktrace => Stacktrace}),
            erlang:raise(Class, Reason, Stacktrace)
    end.

stop_service(Service) ->
    assert_loaded(Service),
    try Service:stop() of
        _ ->
            ?LOG_INFO(#{what => service_stopped, service => Service,
                        text => <<"Stopped MongooseIM service">>}),
            ok
    catch Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => service_stop_failed, service => Service,
                         text => <<"Failed to stop MongooseIM service">>,
                         class => Class, reason => Reason, stacktrace => Stacktrace}),
            erlang:raise(Class, Reason, Stacktrace)
    end.

-spec assert_loaded(service()) -> ok.
assert_loaded(Service) ->
    case is_loaded(Service) of
        true ->
            ok;
        false ->
            error(#{what => service_not_loaded,
                    text => <<"Service missing from mongoose_config">>,
                    service => Service})
    end.

-spec is_loaded(service()) -> boolean().
is_loaded(Service) ->
    case mongoose_config:lookup_opt([services, Service]) of
        {ok, _Opts} -> true;
        {error, not_found} -> false
    end.

-spec loaded_services_with_opts() -> service_map().
loaded_services_with_opts() ->
    mongoose_config:get_opt(services).
