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
         get_service_opts/1,
         loaded_services_with_opts/0
         ]).

%%Question marks:
%%do we need the 'keep config' facility?
%%does anybody use the 'wait' option in stopping gen_mod?

-define(ETAB, mongoose_services).

-callback start(Opts :: list()) -> any().
-callback stop() -> any().

-type service() :: atom().
-type options() :: [term()].

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_start_service(Service, Opts0) ->
    Opts = proplists:unfold(Opts0),
    ets:insert(?ETAB, {Service, Opts}),
    try
        Res = Service:start(Opts),
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


