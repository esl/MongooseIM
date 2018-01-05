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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(gen_mod).
-author('alexey@process-one.net').

-type dep_arguments() :: proplists:proplist().
-type dep_hardness() :: soft | hard.
-type deps_list() :: [
                      {module(), dep_arguments(), dep_hardness()} |
                      {module(), dep_hardness()}
                     ].

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
         get_opt_subhost/3,
         get_opt_subhost/4,
         get_module_opt_subhost/3,
         % Get/set opts by subhost
         get_module_opt_by_subhost/4,
         set_module_opt_by_subhost/4,

         loaded_modules/1,
         loaded_modules_with_opts/1,
         get_module_proc/2,
         is_loaded/2,
         get_deps/3]).

-include("ejabberd.hrl").

-record(ejabberd_module, {
          module_host, % {module(), ejabberd:server()},
          opts % list()
         }).

%% -export([behaviour_info/1]).
%% behaviour_info(callbacks) ->
%%     [{start, 2},
%%      {stop, 1}];
%% behaviour_info(_Other) ->
%%     undefined.
-callback start(Host :: ejabberd:server(), Opts :: list()) -> any().
-callback stop(Host :: ejabberd:server()) -> any().

%% Optional callback specifying module dependencies.
%% The dependent module can specify parameters with which the dependee should be
%% started (the parameters will be merged with params given in user config and
%% by other modules).
%% The last element of the tuple specifies whether the ordering can be broken in
%% case of cycle (in that case soft dependency may be started after the
%% dependent module).
%%
%% -callback deps(Host :: ejabberd:server(), Opts :: proplists:list()) ->
%%     deps_list().

-spec start() -> 'ok'.
start() ->
    ets:new(ejabberd_modules, [named_table, public, {keypos, #ejabberd_module.module_host},
                               {read_concurrency, true}]),
    ok.


-spec start_module(Host :: ejabberd:server(),
                   Module :: module(),
                   Opts :: [any()]) -> any().
start_module(Host, Module, Opts0) ->
    {links, LinksBefore} = erlang:process_info(self(), links),
    Opts = clear_opts(Module, Opts0),
    set_module_opts_mnesia(Host, Module, Opts),
    ets:insert(ejabberd_modules, #ejabberd_module{module_host = {Module, Host}, opts = Opts}),
    try
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
        Res
    catch
        Class:Reason ->
            del_module_mnesia(Host, Module),
            ets:delete(ejabberd_modules, {Module, Host}),
            ErrorText = io_lib:format("Problem starting the module ~p for "
                                      "host ~p~n options: ~p~n ~p: ~p~n~p",
                                      [Module, Host, Opts, Class, Reason,
                                       erlang:get_stacktrace()]),
            ?CRITICAL_MSG(ErrorText, []),
            case is_app_running(mongooseim) of
                true ->
                    erlang:raise(Class, Reason, erlang:get_stacktrace());
                false ->
                    ?CRITICAL_MSG("ejabberd initialization was aborted "
                                  "because a module start failed.~n"
                                  "The trace is ~p.", [erlang:get_stacktrace()]),
                    timer:sleep(3000),
                    erlang:halt(string:substr(lists:flatten(ErrorText),
                                              1, 199))
            end
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
-spec stop_module(ejabberd:server(), module()) -> 'error' | {'aborted', _} | {'atomic', _}.
stop_module(Host, Module) ->
    case stop_module_keep_config(Host, Module) of
        error ->
            error;
        ok ->
            del_module_mnesia(Host, Module)
    end.


%% @doc Stop the module in a host, but keep its configuration. As the module
%% configuration is kept in the Mnesia local_config table, when ejabberd is
%% restarted the module will be started again. This function is useful when
%% ejabberd is being stopped and it stops all modules.
-spec stop_module_keep_config(ejabberd:server(), module()) -> 'error' | 'ok'.
stop_module_keep_config(Host, Module) ->
    case catch Module:stop(Host) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p", [Reason]),
            error;
        {wait, ProcList} when is_list(ProcList) ->
            lists:foreach(fun wait_for_process/1, ProcList),
            ets:delete(ejabberd_modules, {Module, Host}),
            ok;
        {wait, Process} ->
            wait_for_process(Process),
            ets:delete(ejabberd_modules, {Module, Host}),
            ok;
        _ ->
            ets:delete(ejabberd_modules, {Module, Host}),
            ok
    end.

-spec reload_module(ejabberd:server(), module(), [any()]) -> 'error' | 'ok'.
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
            % TODO: replace with more appropriate function
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
        SubHost :: ejabberd:server(),
        Module :: module(),
        Opt :: term(),
        Default :: term()) -> term().
get_module_opt_by_subhost(SubHost, Module, Opt, Default) ->
    {ok, Host} = mongoose_subhosts:get_host(SubHost),
    get_module_opt(Host, Module, Opt, Default).

%% @doc Non-atomic! You have been warned.
-spec set_module_opt(ejabberd:server(), module(), _Opt, _Value) -> boolean().
set_module_opt(Host, Module, Opt, Value) ->
    Key = {Module, Host},
    OptsList = ets:lookup(ejabberd_modules, Key),
    case OptsList of
        [] ->
            false;
        [#ejabberd_module{opts = Opts}] ->
            Updated = set_opt(Opt, Opts, Value),
            ets:update_element(ejabberd_modules, Key,
                               {#ejabberd_module.opts, Updated})
    end.

%% @doc Non-atomic! You have been warned.
-spec set_module_opts(ejabberd:server(), module(), _Opts) -> boolean().
set_module_opts(Host, Module, Opts0) ->
    Opts = proplists:unfold(Opts0),
    Key = {Module, Host},
    OptsList = ets:lookup(ejabberd_modules, Key),
    case OptsList of
        [] ->
            false;
        [#ejabberd_module{opts = _Opts}] ->
            ets:update_element(ejabberd_modules, Key,
                               {#ejabberd_module.opts, Opts})
    end.

-spec set_module_opt_by_subhost(
        SubHost :: ejabberd:server(),
        Module :: module(),
        Opt :: term(),
        Value :: term()) -> boolean().
set_module_opt_by_subhost(SubHost, Module, Opt, Value) ->
    {ok, Host} = mongoose_subhosts:get_host(SubHost),
    set_module_opt(Host, Module, Opt, Value).


-spec get_opt_subhost(ejabberd:server(), list(), list() | binary()) -> ejabberd:server().
get_opt_subhost(Host, Opts, Default) ->
    get_opt_subhost(Host, host, Opts, Default).

-spec get_opt_subhost(ejabberd:server(), atom(), list(), list() | binary()) -> ejabberd:server().
    get_opt_subhost(Host, OptName, Opts, Default) ->
    Val = get_opt(OptName, Opts, Default),
    re:replace(Val, "@HOST@", Host, [global, {return, binary}]).

-spec get_module_opt_subhost(ejabberd:server(), module(), list() | binary()) -> ejabberd:server().
get_module_opt_subhost(Host, Module, Default) ->
    Subject = get_module_opt(Host, Module, host, Default),
    re:replace(Subject, "@HOST@", Host, [global, {return, binary}]).

-spec loaded_modules(ejabberd:server()) -> [module()].
loaded_modules(Host) ->
    ets:select(ejabberd_modules,
               [{#ejabberd_module{_ = '_', module_host = {'$1', Host}},
                 [],
                 ['$1']}]).


-spec loaded_modules_with_opts(ejabberd:server()) -> [{module(), list()}].
loaded_modules_with_opts(Host) ->
    ets:select(ejabberd_modules,
               [{#ejabberd_module{_ = '_', module_host = {'$1', Host},
                                  opts = '$2'},
                 [],
                 [{{'$1', '$2'}}]}]).


-spec set_module_opts_mnesia(ejabberd:server(), module(), [any()]
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


-spec del_module_mnesia(ejabberd:server(), module()) -> {'aborted', _} | {'atomic', _}.
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


-spec get_deps(Host :: ejabberd:server(), Module :: module(),
               Opts :: proplists:proplist()) -> deps_list().
get_deps(Host, Module, Opts) ->
    %% the module has to be loaded,
    %% otherwise the erlang:function_exported/3 returns false
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, deps, 2) of
        true ->
            Module:deps(Host, Opts);
        _ ->
            []
    end.
