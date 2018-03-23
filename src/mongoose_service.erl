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
         is_loaded/1,
         ensure_loaded/1,
         purge_service/1,
         get_service_opts/1,
         loaded_services_with_opts/0
         ]).

-export([check_required/1]). % for testing

%%Question marks:
%%do we need the 'keep config' facility?
%%does anybody use the 'wait' option in stopping gen_mod?

-define(ETAB, mongoose_services).

-type service() :: atom().
-type options() :: [term()].

-callback start(Opts :: list()) -> any().
-callback stop() -> any().
%%optional:
%%-callback requires() -> [service()].

-spec start() -> ok.
start() ->
    ets:new(?ETAB, [named_table, public, {read_concurrency, true}]),
    ok.

-spec stop() -> ok.
stop() -> catch ets:delete(?ETAB), ok.

-spec start_service(service(), options()) -> ok | {error, already_started}.
start_service(Service, Opts) ->
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

-spec ensure_loaded(service()) -> ok.
ensure_loaded(Service) ->
    case is_loaded(Service) of
        true ->
            ok;
        false ->
            Options = ejabberd_config:get_local_option_or_default(services, []),
            start_service(Service, proplists:get_value(Service, Options, [])),
            ok
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
    [Z || [Z] <- ets:match(?ETAB, '$1')].

%% @doc to be used as an emergency feature if serviced crashed while stopping and is not
%% running but still lingers in the services tab
-spec purge_service(service()) -> ok.
purge_service(Service) ->
    ets:delete(?ETAB, Service),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_start_service(Service, Opts0) ->
    start_required(Service),
    Opts = proplists:unfold(Opts0),
    ets:insert(?ETAB, {Service, Opts}),
    try
        ?INFO_MSG("Starting service: ~p with ~p...", [Service, Opts]),
        Res = Service:start(Opts),
        ?INFO_MSG("...done~n", []),
        case Res of
            {ok, _} -> Res;
            _ -> {ok, Res}
        end
    catch
        Class:Reason ->
            ets:delete(?ETAB, Service),
            ErrorText = io_lib:format("Problem starting service ~p~n options: ~p~n ~p: ~p~n~p",
                                      [Service, Opts, Class, Reason, erlang:get_stacktrace()]),
            ?CRITICAL_MSG(ErrorText, []),
            case is_app_running(mongooseim) of
                true ->
                    erlang:raise(Class, Reason, erlang:get_stacktrace());
                false ->
                    ?CRITICAL_MSG("mongooseim initialization was aborted "
                    "because a service start failed.~n"
                    "The trace is ~p.", [erlang:get_stacktrace()]),
                    timer:sleep(3000),
                    erlang:halt(string:substr(lists:flatten(ErrorText),
                        1, 199))
            end
    end.

run_stop_service(Service) ->
    ?INFO_MSG("stopping service: ~p~n", [Service]),
    case catch Service:stop() of
        {'EXIT', Reason} ->
            ?ERROR_MSG("Failed to stop service ~p, reason: ~p~n", [Service, Reason]);
        _ ->
            ets:delete(?ETAB, Service),
            ok
    end.

-spec is_app_running(_) -> boolean().
is_app_running(AppName) ->
    %% Use a high timeout to prevent a false positive in a high load system
    Timeout = 15000,
    lists:keymember(AppName, 1, application:which_applications(Timeout)).

-spec start_required(service()) -> ok.
start_required(Service) ->
    check_required(Service), % make sure there are no circular requirements
    lists:map(fun ensure_loaded/1, get_required(Service)),
    ok.

check_required(Service) ->
    check_required(Service, []).

check_required(Service, Stack) ->
    case lists:member(Service, Stack) of
        true ->
            error(circular_requirements_detected);
        false ->
            lists:foreach(fun(Serv) -> check_required(Serv, [Service | Stack]) end,
                          get_required(Service))
    end.

-spec get_required(service()) -> [service()].
get_required(Service) ->
    %% the module has to be loaded,
    %% otherwise the erlang:function_exported/3 returns false
    code:ensure_loaded(Service),
    case erlang:function_exported(Service, requires, 0) of
        true ->
            Service:requires();
        _ ->
            []
    end.
