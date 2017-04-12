%%==============================================================================
%% Copyright 2017 Erlang Solutions Ltd.
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

-module(mod_global_distrib_utils).
-author('konrad.zemek@erlang-solutions.com').

-export([start/4, deps/4, stop/3, opt/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start(Module, Host, Opts, StartFun) ->
    {global_host, GlobalHostList} = lists:keyfind(global_host, 1, Opts),
    case unicode:characters_to_binary(GlobalHostList) of
        Host ->
            create_opts_ets(Module, Opts),
            StartFun();
        _ ->
            ok
    end.

stop(Module, Host, StopFun) ->
    case opt(Module, global_host) of
        Host ->
            StopFun(),
            ets:delete(Module);
        _ ->
            ok
    end.

deps(_Module, Host, Opts, DepsFun) ->
    {global_host, GlobalHostList} = lists:keyfind(global_host, 1, Opts),
    case unicode:characters_to_binary(GlobalHostList) of
        Host -> DepsFun(Opts);
        _ -> []
    end.

opt(Module, Key) ->
    ets:lookup_element(Module, Key, 2).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

create_opts_ets(Module, Opts) ->
    Self = self(),
    Heir = case whereis(ejabberd_sup) of
               undefined -> none;
               Self -> none;
               Pid -> Pid
           end,

    ets:new(Module, [named_table, public, {read_concurrency, true}, {heir, Heir, testing}]),
    [ets:insert(Module, {Key, translate_opt(Value)}) || {Key, Value} <- Opts].

translate_opt(Opt) when is_list(Opt) ->
    case catch unicode:characters_to_binary(Opt) of
        Bin when is_binary(Bin) -> Bin;
        _ -> Opt
    end;
translate_opt(Opt) ->
    Opt.
