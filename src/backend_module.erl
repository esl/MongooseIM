%%%----------------------------------------------------------------------
%%% File    : backend_module.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%% Copyright 2016 Erlang Solutions Ltd.
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

-module(backend_module).
-author('alexey@process-one.net').
-author('konrad.zemek@erlang-solutions.com').

-export([create/2, create/3]).

%% Callback implemented by proxy modules.
-callback backend() -> module().

%% API

-spec create(For :: module(), Name :: atom()) ->
                    {ok, module()} | {error, already_loaded}.
create(For, Name) ->
    create(For, Name, []).

-spec create(For :: module(), Name :: atom(), TrackedFuns :: [atom()]) ->
                    {ok, module()} | {error, already_loaded}.
create(Module, Backend, TrackedFuns) ->
    ProxyModule = proxy_module(Module),
    BackendModule = backend_module(Module, Backend),
    ensure_backend_metrics(Module, TrackedFuns),
    case catch ProxyModule:backend() of
        BackendModule ->
            {error, already_loaded};
        _ ->
            {ProxyModuleStr, CodeString} = backend_code(Module, Backend, TrackedFuns),
            {Mod, Code} = dynamic_compile:from_string(CodeString),
            code:load_binary(Mod, ProxyModuleStr ++ ".erl", Code),
            {ok, ProxyModule}
    end.

%% Internal functions

-spec proxy_module(Module :: module()) -> module().
proxy_module(Module) ->
    list_to_atom(atom_to_list(Module) ++ "_backend").

-spec backend_module(Module :: module(), Backend :: atom()) -> module().
backend_module(Module, Backend) ->
    list_to_atom(atom_to_list(Module) ++ "_" ++ atom_to_list(Backend)).

-spec backend_code(module(), atom(), list()) -> {nonempty_string(), list()}.
backend_code(Module, Backend, TrackedFuns) when is_atom(Backend) ->
    Callbacks = Module:behaviour_info(callbacks),
    ModuleStr = atom_to_list(Module),
    ProxyModuleName = ModuleStr ++ "_backend",
    RealBackendModule = ModuleStr ++ "_" ++ atom_to_list(Backend),
    BehaviourExports = [generate_export(F, A) || {F, A} <- Callbacks],

    BehaviourImpl = [generate_fun(Module, RealBackendModule, F, A, TrackedFuns) ||
                        {F, A} <- Callbacks],
    Code = lists:flatten(
             ["-module(", ProxyModuleName, ").\n",
              "-behaviour(backend_module).\n"
              "-export([backend/0, backend_name/0]).\n",
              BehaviourExports,


              "-spec backend() -> atom().\n",
              "backend() ->", RealBackendModule, ".\n",
              "backend_name() ->", atom_to_list(Backend), ".\n",
              BehaviourImpl
             ]),
    {ProxyModuleName, Code}.

generate_export(F, A) ->
    "-export([" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A) ++ "]).\n".

generate_fun(BaseModule, RealBackendModule, F, A, TrackedFuns) ->
    Args = string:join(["A" ++ integer_to_list(I) || I <- lists:seq(1, A)], ", "),
    IsTracked = lists:member(F, TrackedFuns),
    [fun_header(F, Args), " ->\n",
     generate_fun_body(IsTracked, BaseModule, RealBackendModule, F, Args)].

fun_header(F, Args) ->
    [atom_to_list(F), "(", Args, ")"].

time_metric(Module, Op) -> [backends, Module, Op].
calls_metric(Module, Op) -> [backends, Module, calls, Op].

generate_fun_body(false, _, RealBackendModule, F, Args) ->
    ["    ", RealBackendModule, ":", fun_header(F, Args), ".\n"];
generate_fun_body(true, BaseModule, RealBackendModule, F, Args) ->
    FS = atom_to_list(F),
    %%     returned is the following
    %%     mongoose_metrics:update(global, calls_metric(Backend, F), 1),
    %%     {Time, Result} = timer:tc(Backend, F, Args),
    %%     mongoose_metrics:update(global, time_metric(Backend, F), Time),
    %%     Result.
    CallsMetric = io_lib:format("~p", [calls_metric(BaseModule, F)]),
    TimeMetric = io_lib:format("~p", [time_metric(BaseModule, F)]),
    ["    mongoose_metrics:update(global, ", CallsMetric, ", 1), \n",
     "    {Time, Result} = timer:tc(", RealBackendModule, ", ", FS, ", [", Args, "]), \n",
     "    mongoose_metrics:update(global, ", TimeMetric, ", Time), \n",
     "    Result.\n"].

ensure_backend_metrics(Module, Ops) ->
    EnsureFun = fun(Op) ->
                        mongoose_metrics:ensure_metric(global, calls_metric(Module, Op), spiral),
                        mongoose_metrics:ensure_metric(global, time_metric(Module, Op), histogram)
                end,
    lists:foreach(EnsureFun, Ops).
