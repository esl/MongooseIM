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

-type dep_arguments() :: proplists:proplist().
-type deps_list() :: [
                      {module(), dep_arguments(), gen_mod_deps:hardness()} |
                      {module(), gen_mod_deps:hardness()} |
                      {service, mongoose_service:service()}
                     ].

-type module_deps_list() :: [
                              {module(), dep_arguments(), gen_mod_deps:hardness()} |
                              {module(), gen_mod_deps:hardness()}
                             ].

-type service_deps_list() :: [atom()].

-export_type([deps_list/0,
              module_opts/0]).

-export([
         % Modules start & stop
         start/0,
         start_module/3,
         start_backend_module/2,
         start_backend_module/3,
         stop_module/2,
         stop_module/3,
         stop_module_keep_config/2,
         stop_module_keep_config/3,
         reload_module/3,
         does_module_support/2,
         config_spec/1,
         % Get/set opts by host or from a list
         get_opt/2,
         get_opt/3,
         get_opt/4,
         set_opt/3,
         get_module_opt/4,
         set_module_opt/4,
         set_module_opts/3,
         get_module_opts/2,
         get_opt_subhost/3,
         get_module_opt_subhost/3,

         loaded_modules/0,
         loaded_modules/1,
         loaded_modules_with_opts/0,
         loaded_modules_with_opts/1,
         hosts_with_module/1,
         get_module_proc/2,
         is_loaded/2,
         get_deps/3]).

-export([is_app_running/1]). % we have to mock it in some tests

-ignore_xref([behaviour_info/1, loaded_modules_with_opts/0,
              loaded_modules_with_opts/1, set_module_opts/3]).

-include("mongoose.hrl").

-record(ejabberd_module, {
          module_host_type, % {module(), host_type()},
          opts % list()
         }).

-type module_feature() :: atom().
-type domain_name() :: mongooseim:domain_name().
-type host_type() :: mongooseim:host_type().
-type module_opts() :: list().

%% -export([behaviour_info/1]).
%% behaviour_info(callbacks) ->
%%     [{start, 2},
%%      {stop, 1}];
%% behaviour_info(_Other) ->
%%     undefined.
-callback start(HostType :: host_type(), Opts :: module_opts()) -> any().
-callback stop(HostType :: host_type()) -> any().
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
-callback deps(HostType :: host_type(), Opts :: proplists:list()) -> deps_list().

-optional_callbacks([config_spec/0, supported_features/0, deps/2]).

-spec start() -> 'ok'.
start() ->
    ets:new(ejabberd_modules, [named_table, public, {read_concurrency, true},
                               {keypos, #ejabberd_module.module_host_type}]),
    ok.


-spec start_module(HostType :: host_type(),
                   Module :: module(),
                   Opts :: [any()]) -> {ok, term()} | {error, already_started}.
start_module(HostType, Module, Opts) ->
    case is_loaded(HostType, Module) of
        true -> {error, already_started};
        false -> start_module_for_host_type(HostType, Module, Opts)
    end.

start_module_for_host_type(HostType, Module, Opts0) ->
    {links, LinksBefore} = erlang:process_info(self(), links),
    Opts = proplists:unfold(Opts0),
    set_module_opts_mnesia(HostType, Module, Opts),
    ets:insert(ejabberd_modules, #ejabberd_module{module_host_type = {Module, HostType},
                                                  opts = Opts}),
    try
        lists:map(fun mongoose_service:assert_loaded/1,
                  get_required_services(HostType, Module, Opts)),
        check_dynamic_domains_support(HostType, Module),
        Res = Module:start(HostType, Opts),
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
            del_module_mnesia(HostType, Module),
            ets:delete(ejabberd_modules, {Module, HostType}),
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

-spec start_backend_module(module(), list()) -> any().
start_backend_module(Module, Opts) ->
    start_backend_module(Module, Opts, []).

start_backend_module(Module, Opts, TrackedFuncs) ->
    Backend = gen_mod:get_opt(backend, Opts, mnesia),
    backend_module:create(Module, Backend, TrackedFuncs).

-spec is_app_running(_) -> boolean().
is_app_running(AppName) ->
    %% Use a high timeout to prevent a false positive in a high load system
    Timeout = 15000,
    lists:keymember(AppName, 1, application:which_applications(Timeout)).


%% @doc Stop the module for host type, and forget its configuration.
-spec stop_module(host_type(), module()) ->
    {ok, list()} | {error, not_loaded} | {error, term()}.
stop_module(HostType, Module) ->
    stop_module(HostType, Module, []).

-spec stop_module(host_type(), module(), list()) ->
    {ok, list()} | {error, term()}.
stop_module(HostType, Module, Opts) ->
    case is_loaded(HostType, Module) of
        false -> {error, not_loaded};
        true -> stop_module_for_host_type(HostType, Module, Opts)
    end.

stop_module_for_host_type(HostType, Module, Opts) ->
    case stop_module_keep_config(HostType, Module, Opts) of
        {ok, ModOpts} ->
            case del_module_mnesia(HostType, Module) of
                {atomic, _} -> {ok, ModOpts};
                E -> {error, E}
            end;
        {error, E} ->
            {error, E}
    end.

%% @doc Stop the module for a host type, but keep its configuration. As the module
%% configuration is kept in the Mnesia local_config table, when ejabberd is
%% restarted the module will be started again. This function is useful when
%% ejabberd is being stopped and it stops all modules.
-spec stop_module_keep_config(host_type(), module()) -> {error, term()} | {ok, list()}.
stop_module_keep_config(HostType, Module) ->
    stop_module_keep_config(HostType, Module, [{stop_dependents, true}]).

-spec stop_module_keep_config(host_type(), module(), list()) -> {error, term()} | {ok, list()}.
stop_module_keep_config(HostType, Module, Opts) ->
    ModOpts = get_module_opts(HostType, Module),
    case {get_opt(stop_dependents, Opts, false),
          get_dependents(HostType, Module)} of
        {false, Dependents} ->
            [] =:= Dependents andalso ?LOG_WARNING(#{what => module_dependency_broken,
                                                     module => Module, depends_on => Dependents}),
            try Module:stop(HostType) of
                {wait, ProcList} when is_list(ProcList) ->
                    lists:foreach(fun wait_for_process/1, ProcList),
                    ets:delete(ejabberd_modules, {Module, HostType}),
                    {ok, ModOpts};
                {wait, Process} ->
                    wait_for_process(Process),
                    ets:delete(ejabberd_modules, {Module, HostType}),
                    {ok, ModOpts};
                _ ->
                    ets:delete(ejabberd_modules, {Module, HostType}),
                    {ok, ModOpts}
            catch Class:Reason:Stacktrace ->
                      ?LOG_ERROR(#{what => module_stopping_failed,
                                   host_type => HostType, stop_module => Module,
                                   class => Class, reason => Reason, stacktrace => Stacktrace}),
                      {error, Reason}
            end;
        {true, Dependents} -> {error, {dependents, Dependents}}
    end.

-spec get_dependents(host_type(), module()) -> [module()].
get_dependents(HostType, Module) ->
    LoadedModules = loaded_modules(HostType),
    Dependencies = lists:map(fun(Mod) ->
                                     {Mod, get_deps(HostType, Mod, [])}
                                 end,
                                 LoadedModules),
    lists:filtermap(fun({Mod, Deps}) ->
                            lists:any(fun(Elem) when element(1, Elem) =:= Module -> true;
                                         (_) -> false
                                      end, Deps) andalso {true, Mod}
                    end, Dependencies).


-spec reload_module(host_type(), module(), [any()]) ->
    {ok, term()} | {error, already_started}.
reload_module(HostType, Module, Opts) ->
    stop_module_keep_config(HostType, Module),
    start_module(HostType, Module, Opts).

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

get_opt(Opt, Opts) ->
    case lists:keysearch(Opt, 1, Opts) of
        false ->
            throw({undefined_option, Opt});
        {value, {_, Val}} ->
            Val
    end.

get_opt(Opt, Opts, Default) ->
    case lists:keysearch(Opt, 1, Opts) of
        false ->
            Default;
        {value, {_, Val}} ->
            Val
    end.

get_opt(Opt, Opts, F, Default) ->
    case lists:keysearch(Opt, 1, Opts) of
        false ->
            Default;
        {value, {_, Val}} ->
            F(Val)
    end.

-spec set_opt(_, [tuple()], _) -> [tuple(), ...].
set_opt(Opt, Opts, Value) ->
    lists:keystore(Opt, 1, Opts, {Opt, Value}).


%%% TODO Make Opt an atom. Fix in mod_auth_token:
%%% 374: The call gen_mod:get_module_opt(Domain::any(), 'mod_auth_token',
%%% {'validity_period','access' | 'refresh'}, {1 | 25,'days' | 'hours'})
%%% breaks the contract (mongooseim:host_type(), module(), atom(), term()) -> term()
-spec get_module_opt(mongooseim:host_type(), module(), term(), term()) -> term().
get_module_opt(HostType, Module, Opt, Default) ->
    %% Fail in dev builds.
    %% It protects against passing something weird as a Module argument
    %% or against wrong argument order.
    ?ASSERT_MODULE(Module),
    ModuleOpts = get_module_opts(HostType, Module),
    get_opt(Opt, ModuleOpts, Default).


get_module_opts(HostType, Module) ->
    OptsList = ets:lookup(ejabberd_modules, {Module, HostType}),
    case OptsList of
        [] -> [];
        [#ejabberd_module{opts = Opts} | _] -> Opts
    end.

%% @doc use this function only on init stage
%% Non-atomic! You have been warned.
-spec set_module_opt(host_type(), module(), _Opt, _Value) -> boolean().
set_module_opt(HostType, Module, Opt, Value) ->
    case ets:lookup(ejabberd_modules, {Module, HostType}) of
        [] ->
            false;
        [#ejabberd_module{opts = Opts}] ->
            Updated = set_opt(Opt, Opts, Value),
            set_module_opts(HostType, Module, Updated)
    end.


%% @doc Replaces all module options, use  this function
%% for integration tests only
-spec set_module_opts(host_type(), module(), _Opts) -> true.
set_module_opts(HostType, Module, Opts0) ->
    Opts = proplists:unfold(Opts0),
    ets:insert(ejabberd_modules,
               #ejabberd_module{module_host_type = {Module, HostType},
                                opts = Opts}).

-spec get_opt_subhost(domain_name(),
                      list(),
                      mongoose_subdomain_utils:subdomain_pattern()) ->
    domain_name().
get_opt_subhost(Host, Opts, Default) ->
    %% TODO: try to get rid of this interface
    Val = get_opt(host, Opts, Default),
    mongoose_subdomain_utils:get_fqdn(Val, Host).

-spec get_module_opt_subhost(domain_name(),
                             module(),
                             mongoose_subdomain_utils:subdomain_pattern()) ->
    domain_name().
get_module_opt_subhost(Host, Module, Default) ->
    %% TODO: try to get rid of this interface
    %% note that get_module_opt/4 requires host_type(), while
    %% mongoose_subdomain_utils:get_fqdn/2 expects domain_name()
    Spec = get_module_opt(Host, Module, host, Default),
    mongoose_subdomain_utils:get_fqdn(Spec, Host).

-spec loaded_modules() -> [module()].
loaded_modules() ->
    ModSet = ets:foldl(fun(#ejabberd_module{module_host_type = {Mod, _}}, Set) ->
                               gb_sets:add_element(Mod, Set)
                       end, gb_sets:new(), ejabberd_modules),
    gb_sets:to_list(ModSet).

-spec loaded_modules(host_type()) -> [module()].
loaded_modules(HostType) ->
    ets:select(ejabberd_modules,
               [{#ejabberd_module{_ = '_', module_host_type = {'$1', HostType}},
                 [],
                 ['$1']}]).


-spec loaded_modules_with_opts(host_type()) -> [{module(), list()}].
loaded_modules_with_opts(HostType) ->
    ets:select(ejabberd_modules,
               [{#ejabberd_module{_ = '_', module_host_type = {'$1', HostType},
                                  opts = '$2'},
                 [],
                 [{{'$1', '$2'}}]}]).

-spec loaded_modules_with_opts() -> #{host_type() => [{module(), list()}]}.
loaded_modules_with_opts() ->
    Res = ets:select(ejabberd_modules,
               [{#ejabberd_module{_ = '_', module_host_type = {'$1', '$2'},
                                  opts = '$3'},
                 [],
                 [{{'$2', '$1', '$3'}}]}]),
    Hosts = lists:usort([H || {H, _, _} <- Res]),
    maps:from_list([{H, [{M, Opts}
                         || {HH, M, Opts} <- Res,
                            H =:= HH]}
                    || H <- Hosts]).

-spec hosts_with_module(module()) -> [host_type()].
hosts_with_module(Module) ->
    ets:select(ejabberd_modules,
               [{#ejabberd_module{_ = '_', module_host_type = {Module, '$1'}},
                 [], ['$1']}]).

-spec set_module_opts_mnesia(host_type(), module(), [any()]) ->
    {'aborted', _} | {'atomic', _}.
set_module_opts_mnesia(HostType, Module, Opts0) ->
    %% this function is not atomic!
    Opts = proplists:unfold(Opts0),
    Modules = case ejabberd_config:get_local_option({modules, HostType}) of
        undefined ->
            [];
        Ls ->
            Ls
    end,
    Modules1 = lists:keydelete(Module, 1, Modules),
    Modules2 = [{Module, Opts} | Modules1],
    ejabberd_config:add_local_option({modules, HostType}, Modules2).


-spec del_module_mnesia(host_type(), module()) -> {'aborted', _} | {'atomic', _}.
del_module_mnesia(HostType, Module) ->
    %% this function is not atomic!
    Modules = case ejabberd_config:get_local_option({modules, HostType}) of
                  undefined ->
                      [];
                  Ls ->
                      Ls
              end,
    case lists:keydelete(Module, 1, Modules) of
        [] ->
            ejabberd_config:del_local_option({modules, HostType});
        OtherModules ->
            ejabberd_config:add_local_option({modules, HostType}, OtherModules)
    end.

-spec get_module_proc(binary() | string(), module()) -> atom().
%% TODO:
%% split this interface into 2:
%%   * create_module_proc_name/2 - which can create new atoms by calling list_to_atom/1
%%   * get_module_proc_name/2 - which should use safe list_to_existing_atom/1 function
get_module_proc(Host, Base) when is_binary(Host) ->
    get_module_proc(binary_to_list(Host), Base);
get_module_proc(Host, Base) ->
    list_to_atom(atom_to_list(Base) ++ "_" ++ Host).


-spec is_loaded(HostType :: binary(), Module :: atom()) -> boolean().
is_loaded(HostType, Module) ->
    ets:member(ejabberd_modules, {Module, HostType}).

-spec get_deps(HostType :: host_type(), Module :: module(),
               Opts :: proplists:proplist()) -> module_deps_list().
get_deps(HostType, Module, Opts) ->
    %% the module has to be loaded,
    %% otherwise the erlang:function_exported/3 returns false
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, deps, 2) of
        true ->
            Deps = Module:deps(HostType, Opts),
            lists:filter(fun(D) -> element(1, D) =/= service end, Deps);
        _ ->
            []
    end.

-spec get_required_services(host_type(), module(), proplists:proplist()) ->
    service_deps_list().
get_required_services(HostType, Module, Options) ->
    %% the module has to be loaded,
    %% otherwise the erlang:function_exported/3 returns false
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, deps, 2) of
        true ->
            [Service || {service, Service} <- Module:deps(HostType, Options)];
        _ ->
            []
    end.
