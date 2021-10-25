%%==============================================================================
%% Copyright 2018 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(mongoose_service).

-include("mongoose.hrl").

-export([start/0, stop/0,
         start_service/2,
         stop_service/1,
         config_spec/1,
         is_loaded/1,
         assert_loaded/1,
         ensure_loaded/1,
         ensure_loaded/2,
         purge_service/1,
         get_service_opts/1,
         loaded_services_with_opts/0
         ]).

-export([check_deps/1]). % for testing

-ignore_xref([behaviour_info/1, check_deps/1, ensure_loaded/1, purge_service/1,
              get_service_opts/1, start_service/2, stop/0]).

%%Question marks:
%%do we need the 'keep config' facility?
%%does anybody use the 'wait' option in stopping gen_mod?

-define(ETAB, mongoose_services).

-type service() :: atom().
-type options() :: [term()].

-callback start(Opts :: list()) -> any().
-callback stop() -> any().
-callback config_spec() -> mongoose_config_spec:config_section().
%%optional:
%%-callback deps() -> [service()].

-spec start() -> ok.
start() ->
    ets:new(?ETAB, [named_table, public, {read_concurrency, true}]),
    ok.

-spec stop() -> ok.
stop() -> catch ets:delete(?ETAB), ok.

-spec start_service(service(), options() | undefined) -> ok | {error, already_started}.
start_service(Service, undefined) ->
    error({service_not_configured, Service});
start_service(Service, Opts) when is_list(Opts) ->
    case is_loaded(Service) of
        true -> {error, already_started};
        false -> run_start_service(Service, Opts)
    end.

-spec stop_service(service()) -> ok | {error, not_running}.
stop_service(Service) ->
    case is_loaded(Service) of
        false -> {error, not_running};
        true -> run_stop_service(Service)
    end.

-spec config_spec(service()) -> mongoose_config_spec:config_section().
config_spec(Service) ->
    Service:config_spec().

-spec ensure_loaded(service()) -> ok.
ensure_loaded(Service) ->
    Options = mongoose_config:get_opt(services, []),
    start_service(Service, proplists:get_value(Service, Options)),
    ok.

-spec ensure_loaded(service(), options()) -> ok.
ensure_loaded(Service, Opts) ->
    start_service(Service, Opts),
    ok.

-spec assert_loaded(service()) -> ok.
assert_loaded(Service) ->
    case is_loaded(Service) of
        true ->
            ok;
        false ->
            error({service_not_loaded, Service})
    end.

-spec is_loaded(service()) -> boolean().
is_loaded(Service) ->
    ets:member(?ETAB, Service).

-spec get_service_opts(service()) -> options().
get_service_opts(Service) ->
    case ets:lookup(?ETAB, Service) of
        [] -> [];
        [{Service, Opts}] -> Opts
    end.

-spec loaded_services_with_opts() -> [{service(), options()}].
loaded_services_with_opts() ->
    ets:tab2list(?ETAB).

%% @doc to be used as an emergency feature if serviced crashed while stopping and is not
%% running but still lingers in the services tab
-spec purge_service(service()) -> ok.
purge_service(Service) ->
    ets:delete(?ETAB, Service),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_start_service(Service, Opts0) ->
    start_deps(Service),
    Opts = proplists:unfold(Opts0),
    ets:insert(?ETAB, {Service, Opts}),
    try
        Res = Service:start(Opts),
        ?LOG_INFO(#{what => service_startup_started, service => Service,
                    text => <<"Started MongooseIM service">>}),
        case Res of
            {ok, _} -> Res;
            _ -> {ok, Res}
        end
    catch
        Class:Reason:Stacktrace ->
            ets:delete(?ETAB, Service),
            ?LOG_CRITICAL(#{what => service_startup_failed,
                            text => <<"Failed to start MongooseIM service">>,
                            service => Service, opts => Opts,
                            class => Class, reason => Reason, stacktrace => Stacktrace}),
            case is_app_running(mongooseim) of
                true ->
                    erlang:raise(Class, Reason, Stacktrace);
                false ->
                    ?LOG_CRITICAL(#{what => stopping_mongooseim,
                                    text => <<"mongooseim initialization was aborted "
                                              "because a service start failed.">>,
                                    service => Service, opts => Opts,
                                    class => Class, reason => Reason, stacktrace => Stacktrace}),
                    timer:sleep(3000),
                    ErrorText = io_lib:format("Stopping MongooseIM because of bad service~n"
                                              "service=~p ~nreason=~0p ~nstactrace=~0p",
                                              [Service, Reason, Stacktrace]),
                    erlang:halt(string:substr(lists:flatten(ErrorText), 1, 199))
            end
    end.

run_stop_service(Service) ->
    try Service:stop() of
        _ ->
            ?LOG_INFO(#{what => service_stopped, service => Service,
                        text => <<"Stopped MongooseIM service">>}),
            ets:delete(?ETAB, Service),
            ok
    catch Class:Reason:Stacktrace ->
            ets:delete(?ETAB, Service),
            ?LOG_ERROR(#{what => service_stop_failed, service => Service,
                         text => <<"Failed to stop MongooseIM service">>,
                         class => Class, reason => Reason, stacktrace => Stacktrace})
    end.

-spec is_app_running(_) -> boolean().
is_app_running(AppName) ->
    %% Use a high timeout to prevent a false positive in a high load system
    Timeout = 15000,
    lists:keymember(AppName, 1, application:which_applications(Timeout)).

-spec start_deps(service()) -> ok.
start_deps(Service) ->
    check_deps(Service), % make sure there are no circular deps
    lists:map(fun ensure_loaded/1, get_deps(Service)),
    ok.

check_deps(Service) ->
    check_deps(Service, []).

check_deps(Service, Stack) ->
    case lists:member(Service, Stack) of
        true ->
            error({circular_deps_detected, Service});
        false ->
            lists:foreach(fun(Serv) -> check_deps(Serv, [Service | Stack]) end,
                          get_deps(Service))
    end.

-spec get_deps(service()) -> [service()].
get_deps(Service) ->
    %% the module has to be loaded,
    %% otherwise the erlang:function_exported/3 returns false
    code:ensure_loaded(Service),
    case erlang:function_exported(Service, deps, 0) of
        true ->
            Service:deps();
        _ ->
            []
    end.
