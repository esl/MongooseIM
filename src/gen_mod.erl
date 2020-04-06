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

-export_type([deps_list/0]).

-export([
         % Modules start & stop
         start/0,
         start_module/3,
         start_backend_module/2,
         start_backend_module/3,
         stop_module/2,
         stop_module_keep_config/2,
         reload_module/3,
         % Get/set opts by host or from a list
         get_opt/2,
         get_opt/3,
         get_opt/4,
         set_opt/3,
         get_module_opt/4,
         set_module_opt/4,
         set_module_opts/3,
         get_module_opts/2,
         make_subhosts/2,
         get_opt_subhost/3,
         get_opt_subhost/4,
         get_module_opt_subhost/3,
         % Get/set opts by subhost
         get_module_opt_by_subhost/4,
         set_module_opt_by_subhost/4,

         loaded_modules/1,
         loaded_modules_with_opts/1,
         opts_for_module/2,
         get_module_proc/2,
         is_loaded/2,
         get_deps/3]).

-export([is_app_running/1]). % we have to mock it in some tests

-include("mongoose.hrl").

-record(ejabberd_module, {
          module_host, % {module(), jid:server()},
          opts % list()
         }).

%% -export([behaviour_info/1]).
%% behaviour_info(callbacks) ->
%%     [{start, 2},
%%      {stop, 1}];
%% behaviour_info(_Other) ->
%%     undefined.
-callback start(Host :: jid:server(), Opts :: list()) -> any().
-callback stop(Host :: jid:server()) -> any().

%% Optional callback specifying module dependencies.
%% The dependent module can specify parameters with which the dependee should be
%% started (the parameters will be merged with params given in user config and
%% by other modules).
%% The last element of the tuple specifies whether the ordering can be broken in
%% case of cycle (in that case soft dependency may be started after the
%% dependent module).
%%
%% -callback deps(Host :: jid:server(), Opts :: proplists:list()) ->
%%     deps_list().

-spec start() -> 'ok'.
start() ->
    ets:new(ejabberd_modules, [named_table, public, {keypos, #ejabberd_module.module_host},
                               {read_concurrency, true}]),
    ok.


-spec start_module(Host :: jid:server(),
                   Module :: module(),
                   Opts :: [any()]) -> {ok, term()} | {error, already_started}.
start_module(Host, Module, Opts) ->
    case is_loaded(Host, Module) of
        true -> {error, already_started};
        false -> start_module_for_host(Host, Module, Opts)
    end.

start_module_for_host(Host, Module, Opts0) ->
    {links, LinksBefore} = erlang:process_info(self(), links),
    Opts = clear_opts(Module, Opts0),
    set_module_opts_mnesia(Host, Module, Opts),
    ets:insert(ejabberd_modules, #ejabberd_module{module_host = {Module, Host}, opts = Opts}),
    try
        lists:map(fun mongoose_service:assert_loaded/1, get_required_services(Host, Module, Opts)),
        Res = Module:start(Host, Opts),
        {links, LinksAfter} = erlang:process_info(self(), links),
        case lists:sort(LinksBefore) =:= lists:sort(LinksAfter) of
            true -> ok;
            false ->
                TravisInfo = "fail_ci_build=true ",
                %% Note for programmers:
                %% Never call start_link directly from your_module:start/2 function!
                %% The process will be killed if we start modules remotely or in shell
                ?ERROR_MSG("msg=unexpected_links ~s"
                            " links_before=~p links_after=~p",
                           [TravisInfo, LinksBefore, LinksAfter])
        end,
        ?DEBUG("Module ~p started for ~p.", [Module, Host]),
        % normalise result
        case Res of
            {ok, R} -> {ok, R};
            _ -> {ok, Res}
        end
    catch
        Class:Reason:StackTrace ->
            del_module_mnesia(Host, Module),
            ets:delete(ejabberd_modules, {Module, Host}),
            ErrorText = io_lib:format("Problem starting the module ~p for "
                                      "host ~p~n options: ~p~n ~p: ~p~n~p",
                                      [Module, Host, Opts, Class, Reason,
                                       StackTrace]),
            ?CRITICAL_MSG(ErrorText, []),
            case is_mim_or_ct_running() of
                true ->
                    erlang:raise(Class, Reason, StackTrace);
                false ->
                    ?CRITICAL_MSG("mongooseim initialization was aborted "
                                  "because a module start failed.~n"
                                  "The trace is ~p.", [StackTrace]),
                    timer:sleep(3000),
                    erlang:halt(string:substr(lists:flatten(ErrorText),
                                              1, 199))
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


%% @doc Stop the module in a host, and forget its configuration.
-spec stop_module(jid:server(), module()) ->
    {ok, list()} | {error, not_loaded} | {error, term()}.
stop_module(Host, Module) ->
    case is_loaded(Host, Module) of
        false -> {error, not_loaded};
        true -> stop_module_for_host(Host, Module)
    end.

stop_module_for_host(Host, Module) ->
    case stop_module_keep_config(Host, Module) of
        {ok, Opts} ->
            case del_module_mnesia(Host, Module) of
                {atomic, _} -> {ok, Opts};
                E -> {error, E}
            end;
        {error, E} ->
            {error, E}
    end.

%% @doc Stop the module in a host, but keep its configuration. As the module
%% configuration is kept in the Mnesia local_config table, when ejabberd is
%% restarted the module will be started again. This function is useful when
%% ejabberd is being stopped and it stops all modules.
-spec stop_module_keep_config(jid:server(), module()) -> {error, term()} | {ok, list()}.
stop_module_keep_config(Host, Module) ->
    Opts = opts_for_module(Host, Module),
    case catch Module:stop(Host) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p", [Reason]),
            {error, Reason};
        {wait, ProcList} when is_list(ProcList) ->
            lists:foreach(fun wait_for_process/1, ProcList),
            ets:delete(ejabberd_modules, {Module, Host}),
            {ok, Opts};
        {wait, Process} ->
            wait_for_process(Process),
            ets:delete(ejabberd_modules, {Module, Host}),
            {ok, Opts};
        _ ->
            ets:delete(ejabberd_modules, {Module, Host}),
            {ok, Opts}
    end.

-spec reload_module(jid:server(), module(), [any()]) -> {ok, term()} | {error, already_started}.
reload_module(Host, Module, Opts) ->
    stop_module_keep_config(Host, Module),
    start_module(Host, Module, Opts).

-spec wait_for_process(atom() | pid() | {atom(), atom()}) -> 'ok'.
wait_for_process(Process) ->
    MonitorReference = erlang:monitor(process, Process),
    wait_for_stop(Process, MonitorReference).


-spec wait_for_stop(atom() | pid() | {atom(), atom()}, reference()) -> 'ok'.
wait_for_stop(Process, MonitorReference) ->
    receive
        {'DOWN', MonitorReference, _Type, _Object, _Info} ->
            ok
    after 5000 ->
            catch exit(whereis(Process), kill),
            wait_for_stop1(MonitorReference)
    end.


-spec wait_for_stop1(reference()) -> 'ok'.
wait_for_stop1(MonitorReference) ->
    receive
        {'DOWN', MonitorReference, _Type, _Object, _Info} ->
            ok
    after 5000 ->
            ok
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


get_module_opt(global, Module, Opt, Default) ->
    [Value | Values] = [get_module_opt(Host, Module, Opt, Default)
                        || Host <- ?MYHOSTS],
    AllSame = lists:all(fun(Other) -> Other == Value end, Values),
    case AllSame of
        true ->
            Value;
        false ->
            Default
    end;
get_module_opt(Host, Module, Opt, Default) ->
    ModuleOpts = get_module_opts(Host, Module),
    get_opt(Opt, ModuleOpts, Default).


get_module_opts(Host, Module) ->
    OptsList = ets:lookup(ejabberd_modules, {Module, Host}),
    case OptsList of
        [] -> [];
        [#ejabberd_module{opts = Opts} | _] -> Opts
    end.


-spec get_module_opt_by_subhost(
        SubHost :: jid:server(),
        Module :: module(),
        Opt :: term(),
        Default :: term()) -> term().
get_module_opt_by_subhost(SubHost, Module, Opt, Default) ->
    {ok, Host} = mongoose_subhosts:get_host(SubHost),
    get_module_opt(Host, Module, Opt, Default).


%% @doc Non-atomic! You have been warned.
-spec set_module_opt(jid:server(), module(), _Opt, _Value) -> boolean().
set_module_opt(Host, Module, Opt, Value) ->
    case ets:lookup(ejabberd_modules, {Module, Host}) of
        [] ->
            false;
        [#ejabberd_module{opts = Opts}] ->
            Updated = set_opt(Opt, Opts, Value),
            set_module_opts(Host, Module, Updated)
    end.


%% @doc Replaces all module options
-spec set_module_opts(ejabberd:server(), module(), _Opts) -> true.
set_module_opts(Host, Module, Opts0) ->
    Opts = proplists:unfold(Opts0),
    ets:insert(ejabberd_modules,
               #ejabberd_module{module_host = {Module, Host},
                                opts = Opts}).


-spec set_module_opt_by_subhost(
        SubHost :: jid:server(),
        Module :: module(),
        Opt :: term(),
        Value :: term()) -> boolean().
set_module_opt_by_subhost(SubHost, Module, Opt, Value) ->
    {ok, Host} = mongoose_subhosts:get_host(SubHost),
    set_module_opt(Host, Module, Opt, Value).

-spec make_subhost(Spec :: iodata() | unicode:charlist(), Host :: jid:server()) -> jid:server().
make_subhost(Spec, Host) ->
    re:replace(Spec, "@HOST@", Host, [global, {return, binary}]).

-spec make_subhosts(Spec :: iodata() | unicode:charlist(), Host :: jid:server()) -> [jid:server()].
make_subhosts(Spec, Host) ->
    [make_subhost(S, Host) || S <- expand_hosts(Spec)].

-spec expand_hosts(iodata()) -> [iodata()].
expand_hosts(Spec) ->
    case re:run(Spec, "@HOSTS@") of
        nomatch -> [Spec];
        {match, _} -> [re:replace(Spec, "@HOSTS@", Host) || Host <- ?MYHOSTS]
    end.

-spec get_opt_subhost(jid:server(), list(), list() | binary()) -> jid:server().
get_opt_subhost(Host, Opts, Default) ->
    get_opt_subhost(Host, host, Opts, Default).

-spec get_opt_subhost(jid:server(), atom(), list(), list() | binary()) -> jid:server().
get_opt_subhost(Host, OptName, Opts, Default) ->
    Val = get_opt(OptName, Opts, Default),
    make_subhost(Val, Host).

-spec get_module_opt_subhost(jid:server(), module(), list() | binary()) -> jid:server().
get_module_opt_subhost(Host, Module, Default) ->
    Spec = get_module_opt(Host, Module, host, Default),
    make_subhost(Spec, Host).

-spec loaded_modules(jid:server()) -> [module()].
loaded_modules(Host) ->
    ets:select(ejabberd_modules,
               [{#ejabberd_module{_ = '_', module_host = {'$1', Host}},
                 [],
                 ['$1']}]).


-spec loaded_modules_with_opts(jid:server()) -> [{module(), list()}].
loaded_modules_with_opts(Host) ->
    ets:select(ejabberd_modules,
               [{#ejabberd_module{_ = '_', module_host = {'$1', Host},
                                  opts = '$2'},
                 [],
                 [{{'$1', '$2'}}]}]).

-spec opts_for_module(ejabberd:server(), module()) -> list().
opts_for_module(Host, Module) ->
    case ets:select(ejabberd_modules,
                    [{#ejabberd_module{_ = '_',
                                       module_host = {Module, Host},
                                       opts = '$1'},
                      [],
                      [{{'$1'}}]}]) of
        [{Opts}] -> Opts;
        []       -> []
    end.

-spec set_module_opts_mnesia(jid:server(), module(), [any()]
                            ) -> {'aborted', _} | {'atomic', _}.
set_module_opts_mnesia(Host, Module, Opts0) ->
    Opts = proplists:unfold(Opts0),
    Modules = case ejabberd_config:get_local_option({modules, Host}) of
        undefined ->
            [];
        Ls ->
            Ls
    end,
    Modules1 = lists:keydelete(Module, 1, Modules),
    Modules2 = [{Module, Opts} | Modules1],
    ejabberd_config:add_local_option({modules, Host}, Modules2).


-spec del_module_mnesia(jid:server(), module()) -> {'aborted', _} | {'atomic', _}.
del_module_mnesia(Host, Module) ->
    Modules = case ejabberd_config:get_local_option({modules, Host}) of
                  undefined ->
                      [];
                  Ls ->
                      Ls
              end,
    case lists:keydelete(Module, 1, Modules) of
        [] ->
            ejabberd_config:del_local_option({modules, Host});
        OtherModules ->
            ejabberd_config:add_local_option({modules, Host}, OtherModules)
    end.

-spec get_module_proc(binary() | string(), module()) -> atom().
get_module_proc(Host, Base) when is_binary(Host) ->
    get_module_proc(binary_to_list(Host), Base);
get_module_proc(Host, Base) ->
    list_to_atom(atom_to_list(Base) ++ "_" ++ Host).


-spec is_loaded(Host :: binary(), Module :: atom()) -> boolean().
is_loaded(Host, Module) ->
    ets:member(ejabberd_modules, {Module, Host}).


-spec clear_opts(atom(), list()) -> list().
clear_opts(Module, Opts0) ->
    Opts = proplists:unfold(Opts0),
    %% the module has to be loaded,
    %% otherwise the erlang:function_exported/3 returns false
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, clean_opts, 1) of
        true ->
            Module:clean_opts(Opts);
        _ ->
            Opts
    end.

-spec get_deps(Host :: jid:server(), Module :: module(),
               Opts :: proplists:proplist()) -> module_deps_list().
get_deps(Host, Module, Opts) ->
    %% the module has to be loaded,
    %% otherwise the erlang:function_exported/3 returns false
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, deps, 2) of
        true ->
            Deps = Module:deps(Host, Opts),
            lists:filter(fun(D) -> element(1, D) =/= service end, Deps);
        _ ->
            []
    end.

-spec get_required_services(jid:server(), module(), proplists:proplist()) -> service_deps_list().
get_required_services(Host, Module, Options) ->
    %% the module has to be loaded,
    %% otherwise the erlang:function_exported/3 returns false
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, deps, 2) of
        true ->
            [Service || {service, Service} <- Module:deps(Host, Options)];
        _ ->
            []
    end.
