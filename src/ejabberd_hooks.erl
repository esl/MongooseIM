%%%----------------------------------------------------------------------
%%% File    : ejabberd_hooks.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage hooks
%%% Created :  8 Aug 2004 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_hooks).
-author('alexey@process-one.net').

%% External exports
-export([add/5,
         delete/5,
         run_fold/4]).

-export([add/1,
         delete/1]).

-export([gen_hook_fn_wrapper/3]).

-include("mongoose.hrl").

-type hook() :: {HookName :: atom(),
                 HostType :: mongooseim:host_type() | global,
                 Module :: module(),
                 Fn :: atom(),
                 Priority:: integer()}.

-export_type([hook/0]).
%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% @doc Add a module and function to the given hook.
%% The integer Priority is used to sort the calls:
%% low numbers are executed before high numbers.
-spec add(HookName :: atom(),
          HostType :: mongooseim:host_type() | global,
          Module :: module(),
          Function :: atom(),
          Priority :: integer()) -> ok.
add(HookName, HostType, Module, Function, Priority) ->
    add_hook({HookName, HostType, Module, Function, Priority}).

-spec add([hook()]) -> ok.
add(Hooks) when is_list(Hooks) ->
    [add_hook(Hook) || Hook <- Hooks],
    ok.

%% @doc Delete a module and function from this hook.
%% It is important to indicate exactly the same information as when the call was added.
-spec delete(HookName :: atom(),
             HostType :: mongooseim:host_type() | global,
             Module :: module(),
             Function :: atom(),
             Priority :: integer()) -> ok.
delete(HookName, HostType, Module, Function, Priority) ->
    delete_hook({HookName, HostType, Module, Function, Priority}).

-spec delete([hook()]) -> ok.
delete(Hooks) when is_list(Hooks) ->
    [delete_hook(Hook) || Hook <- Hooks],
    ok.


%% @doc run the hook.
-spec run_fold(HookName :: atom(),
               HostType :: mongooseim:host_type() | global,
               Acc :: term(),
               Args :: [term()]) ->
    NewAcc :: term() | stopped.
run_fold(HookName, HostType, Acc, Args) when is_binary(HostType); HostType =:= global ->
    {_, RetValue} = gen_hook:run_fold(HookName, HostType, Acc, #{args => Args}),
    RetValue.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

-spec add_hook(hook()) -> ok.
add_hook({HookName, HostType, Module, Function, Priority}) when is_atom(Function) ->
    gen_hook:add_handler(HookName, HostType,
                         fun ?MODULE:gen_hook_fn_wrapper/3,
                         #{module => Module, function =>Function},
                         Priority).

-spec delete_hook(hook()) -> ok.
delete_hook({HookName, HostType, Module, Function, Priority}) when is_atom(Function) ->
    gen_hook:delete_handler(HookName, HostType,
                            fun ?MODULE:gen_hook_fn_wrapper/3,
                            #{module => Module, function => Function},
                            Priority).

gen_hook_fn_wrapper(Acc, #{args := Args}, #{module := Module, function := Function}) ->
    case apply(Module, Function, [Acc | Args]) of
        stop -> {stop, stopped};
        {stop, NewAcc} -> {stop, NewAcc};
        NewAcc -> {ok, NewAcc}
    end.
