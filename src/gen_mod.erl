%%%----------------------------------------------------------------------
%%% File    : gen_mod.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose :
%%% Created : 24 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(gen_mod).
-author('alexey@process-one.net').

-export_type([key_path/0, opt_key/0, opt_value/0, module_opts/0]).

-export([
         % Modules start & stop, do NOT use in the tests, use mongoose_modules API instead
         start_module/3,
         stop_module/2,
         does_module_support/2,
         config_spec/1,
         % Get/set opts by host or from a list
         get_opt/2,
         get_opt/3,
         lookup_module_opt/3,
         get_module_opt/3,
         get_module_opt/4,
         get_module_opts/2,
         get_loaded_module_opts/2,

         loaded_modules/0,
         loaded_modules/1,
         loaded_modules_with_opts/0,
         loaded_modules_with_opts/1,
         hosts_with_module/1,
         hosts_and_opts_with_module/1,
         get_module_proc/2,
         is_loaded/2,
         get_deps/3]).

-export([is_app_running/1]). % we have to mock it in some tests

-ignore_xref([loaded_modules_with_opts/0,
              loaded_modules_with_opts/1, hosts_and_opts_with_module/1]).

-include("mongoose.hrl").

-type module_feature() :: atom().
-type host_type() :: mongooseim:host_type().
-type key_path() :: mongoose_config:key_path().
-type opt_key() :: atom().
-type opt_value() :: mongoose_config:value().
-type module_opts() ::  #{opt_key() => opt_value()}.

-callback start(HostType :: host_type(), Opts :: module_opts()) -> any().
-callback stop(HostType :: host_type()) -> any().
-callback hooks(HostType :: host_type()) -> gen_hook:hook_list().
-callback supported_features() -> [module_feature()].
-callback config_spec() -> mongoose_config_spec:config_section().

%% Optional callback specifying module dependencies.
%% The dependent module can specify parameters with which the dependee should be
%% started (the parameters will be merged with params given in user config and
%% by other modules).
%% The last element of the tuple specifies whether the ordering can be broken in
%% case of cycle (in that case soft dependency may be started after the
%% dependent module).
%%
%% TODO: think about getting rid of HostType param for deps/2 interface, currently
%% it's used only by global_distrib modules (see mod_global_distrib_utils:deps/4
%% function).
-callback deps(host_type(), module_opts()) -> gen_mod_deps:deps().

-optional_callbacks([hooks/1, config_spec/0, supported_features/0, deps/2]).

%% @doc This function should be called by mongoose_modules only.
%% To start a new module at runtime, use mongoose_modules:ensure_module/3 instead.
-spec start_module(host_type(), module(), module_opts()) -> {ok, term()}.
start_module(HostType, Module, Opts) ->
    assert_loaded(HostType, Module),
    start_module_for_host_type(HostType, Module, Opts).

start_module_for_host_type(HostType, Module, Opts) ->
    {links, LinksBefore} = erlang:process_info(self(), links),
    try
        lists:map(fun mongoose_service:assert_loaded/1,
                  get_required_services(HostType, Module, Opts)),
        check_dynamic_domains_support(HostType, Module),
        Res = Module:start(HostType, Opts),
        run_for_hooks(HostType, fun gen_hook:add_handlers/1, Module),
        {links, LinksAfter} = erlang:process_info(self(), links),
        case lists:sort(LinksBefore) =:= lists:sort(LinksAfter) of
            true -> ok;
            false ->
                %% TODO: grepping for "fail_ci_build=true" is bad option
                %% for ci testing, rework this.
                CIInfo = "fail_ci_build=true ",
                %% Note for programmers:
                %% Never call start_link directly from your_module:start/2 function!
                %% The process will be killed if we start modules remotely or in shell
                ?LOG_ERROR(#{what => unexpected_links, ci_info => CIInfo,
                             links_before => LinksBefore, links_after => LinksAfter})
        end,
        ?LOG_DEBUG(#{what => module_started, module => Module, host_type => HostType}),
        % normalise result
        case Res of
            {ok, R} -> {ok, R};
            _ -> {ok, Res}
        end
    catch
        Class:Reason:StackTrace ->
            ErrorText = io_lib:format("Problem starting the module ~p for "
                                      "host_type ~p~n options: ~p~n ~p: ~p~n~p",
                                      [Module, HostType, Opts, Class, Reason,
                                       StackTrace]),
            ?LOG_CRITICAL(#{what => module_start_failed, module => Module,
                            host_type => HostType, opts => Opts, class => Class,
                            reason => Reason, stacktrace => StackTrace}),
            case is_mim_or_ct_running() of
                true ->
                    erlang:raise(Class, Reason, StackTrace);
                false ->
                    ?LOG_CRITICAL(#{what => mim_initialization_aborted,
                                    text => <<"mongooseim initialization was aborted "
                                              "because a module start failed.">>,
                                    class => Class, reason => Reason,
                                    stacktrace => StackTrace}),
                    timer:sleep(3000),
                    erlang:halt(string:substr(lists:flatten(ErrorText),
                                              1, 199))
            end
    end.

run_for_hooks(HostType, Fun, Module) ->
    case erlang:function_exported(Module, hooks, 1) of
        true -> Fun(Module:hooks(HostType));
        false -> ok
    end.

check_dynamic_domains_support(HostType, Module) ->
    case lists:member(HostType, ?MYHOSTS) of
        true -> ok;
        false ->
            case gen_mod:does_module_support(Module, dynamic_domains) of
                true -> ok;
                false ->
                    error({Module, HostType, dynamic_domains_feature_is_not_supported})
            end
    end.

is_mim_or_ct_running() ->
    ?MODULE:is_app_running(mongooseim)
    %% Common tests would be very confused if we kill the whole node
    orelse is_common_test_running().

is_common_test_running() ->
    try
        is_list(ct:get_status())
    catch _:_ ->
        false
    end.

-spec is_app_running(_) -> boolean().
is_app_running(AppName) ->
    %% Use a high timeout to prevent a false positive in a high load system
    Timeout = 15000,
    lists:keymember(AppName, 1, application:which_applications(Timeout)).

%% @doc This function should be called by mongoose_modules only.
%% To stop a module at runtime, use mongoose_modules:ensure_stopped/2 instead.
-spec stop_module(host_type(), module()) -> ok.
stop_module(HostType, Module) ->
    assert_loaded(HostType, Module),
    stop_module_for_host_type(HostType, Module).

-spec stop_module_for_host_type(host_type(), module()) -> ok.
stop_module_for_host_type(HostType, Module) ->
    run_for_hooks(HostType, fun gen_hook:delete_handlers/1, Module),
    try Module:stop(HostType) of
        {wait, ProcList} when is_list(ProcList) ->
            lists:foreach(fun wait_for_process/1, ProcList);
        {wait, Process} ->
            wait_for_process(Process);
        _ ->
            ok
    catch Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => module_stopping_failed,
                         host_type => HostType, stop_module => Module,
                         class => Class, reason => Reason, stacktrace => Stacktrace}),
            erlang:raise(Class, Reason, Stacktrace)
    end.

-spec does_module_support(module(), module_feature()) -> boolean().
does_module_support(Module, Feature) ->
    lists:member(Feature, get_supported_features(Module)).

-spec get_supported_features(module()) -> [module_feature()].
get_supported_features(Module) ->
    %% if module is not loaded, erlang:function_exported/3 returns false
    case erlang:function_exported(Module, supported_features, 0) of
        true -> apply(Module, supported_features, []);
        false -> []
    end.

-spec config_spec(module()) -> mongoose_config_spec:config_section().
config_spec(Module) ->
    Module:config_spec().

-spec wait_for_process(atom() | pid() | {atom(), atom()}) -> 'ok'.
wait_for_process(Process) ->
    MonitorReference = erlang:monitor(process, Process),
    case wait_for_stop(MonitorReference) of
        ok -> ok;
        timeout ->
            catch exit(whereis(Process), kill),
            wait_for_stop(MonitorReference),
            ok
    end.

-spec wait_for_stop(reference()) -> 'ok' | timeout.
wait_for_stop(MonitorReference) ->
    receive
        {'DOWN', MonitorReference, _Type, _Object, _Info} ->
            ok
    after 5000 ->
            timeout
    end.

-spec get_opt(opt_key() | key_path(), module_opts()) -> opt_value().
get_opt(Path, Opts) when is_list(Path), is_map(Opts) ->
    lists:foldl(fun maps:get/2, Opts, Path);
get_opt(Opt, Opts) when is_map(Opts) ->
    maps:get(Opt, Opts).

-spec get_opt(opt_key() | key_path(), module_opts(), opt_value()) -> opt_value().
get_opt(Path, Opts, Default) ->
    try
        get_opt(Path, Opts)
    catch
        error:{badkey, _} -> Default
    end.

-spec lookup_module_opt(mongooseim:host_type(), module(), opt_key() | key_path()) ->
          {ok, opt_value()} | {error, not_found}.
lookup_module_opt(HostType, Module, Key) ->
    mongoose_config:lookup_opt(config_path(HostType, Module, Key)).

-spec get_module_opt(mongooseim:host_type(), module(), opt_key() | key_path(), opt_value()) ->
          opt_value().
get_module_opt(HostType, Module, Key, Default) ->
    %% Fail in dev builds.
    %% It protects against passing something weird as a Module argument
    %% or against wrong argument order.
    ?ASSERT_MODULE(Module),
    mongoose_config:get_opt(config_path(HostType, Module, Key), Default).

-spec get_module_opt(mongooseim:host_type(), module(), opt_key() | key_path()) -> opt_value().
get_module_opt(HostType, Module, Key) ->
    mongoose_config:get_opt(config_path(HostType, Module, Key)).

-spec config_path(mongooseim:host_type(), module(), opt_key() | key_path()) -> key_path().
config_path(HostType, Module, Path) when is_list(Path) ->
    [{modules, HostType}, Module] ++ Path;
config_path(HostType, Module, Key) when is_atom(Key) ->
    [{modules, HostType}, Module, Key].

-spec get_module_opts(mongooseim:host_type(), module()) -> module_opts().
get_module_opts(HostType, Module) ->
    ?ASSERT_MODULE(Module),
    mongoose_config:get_opt([{modules, HostType}, Module], #{}).

-spec get_loaded_module_opts(mongooseim:host_type(), module()) -> module_opts().
get_loaded_module_opts(HostType, Module) ->
    mongoose_config:get_opt([{modules, HostType}, Module]).

-spec loaded_modules() -> [module()].
loaded_modules() ->
    lists:usort(lists:flatmap(fun loaded_modules/1, ?ALL_HOST_TYPES)).

-spec loaded_modules(host_type()) -> [module()].
loaded_modules(HostType) ->
    maps:keys(mongoose_config:get_opt({modules, HostType})).

-spec loaded_modules_with_opts(host_type()) -> #{module() => module_opts()}.
loaded_modules_with_opts(HostType) ->
    mongoose_config:get_opt({modules, HostType}).

-spec loaded_modules_with_opts() -> #{host_type() => #{module() => module_opts()}}.
loaded_modules_with_opts() ->
    maps:from_list([{HostType, loaded_modules_with_opts(HostType)} || HostType <- ?ALL_HOST_TYPES]).

-spec hosts_with_module(module()) -> [host_type()].
hosts_with_module(Module) ->
    [HostType || HostType <- ?ALL_HOST_TYPES, is_loaded(HostType, Module)].

-spec hosts_and_opts_with_module(module()) -> #{host_type() => module_opts()}.
hosts_and_opts_with_module(Module) ->
    maps:from_list(
      lists:flatmap(fun(HostType) ->
                            case mongoose_config:lookup_opt([{modules, HostType}, Module]) of
                                {error, not_found} -> [];
                                {ok, Opts} -> [{HostType, Opts}]
                            end
                    end, ?ALL_HOST_TYPES)).

-spec get_module_proc(binary() | string(), module()) -> atom().
%% TODO:
%% split this interface into 2:
%%   * create_module_proc_name/2 - which can create new atoms by calling list_to_atom/1
%%   * get_module_proc_name/2 - which should use safe list_to_existing_atom/1 function
get_module_proc(Host, Base) when is_binary(Host) ->
    get_module_proc(binary_to_list(Host), Base);
get_module_proc(Host, Base) ->
    list_to_atom(atom_to_list(Base) ++ "_" ++ Host).

-spec assert_loaded(mongooseim:host_type(), module()) -> ok.
assert_loaded(HostType, Module) ->
    case is_loaded(HostType, Module) of
        true ->
            ok;
        false ->
            error(#{what => module_not_loaded,
                    text => <<"Module missing from mongoose_config">>,
                    host_type => HostType,
                    module => Module})
    end.

-spec is_loaded(HostType :: binary(), Module :: atom()) -> boolean().
is_loaded(HostType, Module) ->
    maps:is_key(Module, loaded_modules_with_opts(HostType)).

-spec get_deps(host_type(), module(), module_opts()) -> gen_mod_deps:module_deps().
get_deps(HostType, Module, Opts) ->
    case mongoose_lib:is_exported(Module, deps, 2) of
        true ->
            Deps = Module:deps(HostType, Opts),
            lists:filter(fun(D) -> element(1, D) =/= service end, Deps);
        _ ->
            []
    end.

-spec get_required_services(host_type(), module(), module_opts()) -> [mongoose_service:service()].
get_required_services(HostType, Module, Options) ->
    case mongoose_lib:is_exported(Module, deps, 2) of
        true ->
            [Service || {service, Service} <- Module:deps(HostType, Options)];
        _ ->
            []
    end.
