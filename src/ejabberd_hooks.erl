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

-behaviour(gen_server).

%% External exports
-export([start_link/0,
         add/4,
         add/5,
         delete/4,
         delete/5,
         run_global/3,
         run_for_host_type/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         code_change/3,
         handle_info/2,
         terminate/2]).

-export([add/1,
         delete/1]).

-export([error_running_hook/3]).

-include("mongoose.hrl").

-type hook() :: {HookName :: atom(),
                 HostType :: binary() | global,
                 Module :: module() | undefined,
                 Fn :: fun() | atom(),
                 Priority:: integer()}.

-type key() :: {HookName :: atom(),
                HostType :: binary() | global}.

-type element() :: {Priority:: integer(), %% must be first to ensure proper sorting
                    Module :: module() | undefined,
                    Fn :: fun() | atom()}.

-record(state, {}).

-export_type([hook/0]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ejabberd_hooks}, ejabberd_hooks, [], []).

%% @doc Add a fun to the given hook.
%% The integer Priority is used to sort the calls:
%% low numbers are executed before high numbers.
-spec add(HookName :: atom(),
          HostType :: binary() | global,
          Function :: fun() | atom(),
          Priority :: integer()) -> ok.
add(HookName, HostType, Function, Priority) when is_function(Function) ->
    add_hook({HookName, HostType, undefined, Function, Priority}).

%% @doc Add a module and function to the given hook.
%% The integer Priority is used to sort the calls:
%% low numbers are executed before high numbers.
-spec add(HookName :: atom(),
          HostType :: binary() | global,
          Module :: atom(),
          Function :: fun() | atom(),
          Priority :: integer()) -> ok.
add(HookName, HostType, Module, Function, Priority) ->
    add_hook({HookName, HostType, Module, Function, Priority}).

-spec add([hook()]) -> ok.
add(Hooks) when is_list(Hooks) ->
    [add_hook(Hook) || Hook <- Hooks],
    ok.

-spec add_hook(hook()) -> ok.
add_hook(Hook) ->
    gen_server:call(ejabberd_hooks, {add, Hook}).

%% @doc Delete a module and function from this hook.
%% It is important to indicate exactly the same information than when the call was added.
-spec delete(HookName :: atom(),
             HostType :: binary() | global,
             Function :: fun() | atom(),
             Priority :: integer()) -> ok.
delete(HookName, HostType, Function, Priority) when is_function(Function) ->
    delete_hook({HookName, HostType, undefined, Function, Priority}).

-spec delete(HookName :: atom(),
             HostType :: binary() | global,
             Module :: atom(),
             Function :: fun() | atom(),
             Priority :: integer()) -> ok.
delete(HookName, HostType, Module, Function, Priority) ->
    delete_hook({HookName, HostType, Module, Function, Priority}).

-spec delete([hook()]) -> ok.
delete(Hooks) when is_list(Hooks) ->
    [delete_hook(Hook) || Hook <- Hooks],
    ok.

-spec delete_hook(hook()) -> ok.
delete_hook(Hook) ->
    gen_server:call(ejabberd_hooks, {delete, Hook}).

%% @doc run global hook, for more details see run_fold/4 documentation.
-spec run_global(HookName :: atom(), Acc :: term(), Args :: [term()]) ->
    NewAcc :: term() | stopped.
run_global(HookName, Acc, Args) ->
    run_fold(HookName, global, Acc, Args).

%% @doc run hook for the host type, for more details see run_fold/4 documentation.
-spec run_for_host_type(HookName :: atom(),
                        HostType :: binary() | undefined,
                        Acc :: term(),
                        Args :: [term()]) ->
    NewAcc :: term() | stopped.
run_for_host_type(HookName, undefined, Acc, Args) ->
    ?LOG_ERROR(#{what => undefined_host_type,
                 text => <<"Running hook for an undefined host type">>,
                 hook_name => HookName, hook_acc => Acc, hook_args => Args}),
    Acc;
run_for_host_type(HookName, HostType, Acc, Args) when is_binary(HostType) ->
    run_fold(HookName, HostType, Acc, Args).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    ets:new(hooks, [named_table, {read_concurrency, true}]),
    {ok, #state{}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({add, Hook}, _From, State) ->
    {Key, El} = get_key_and_el(Hook),
    Reply = case ets:lookup(hooks, Key) of
                [{_, Ls}] ->
                    case lists:member(El, Ls) of
                        true ->
                            ok;
                        false ->
                            %% NB: lists:merge/2 returns sorted list!
                            NewLs = lists:merge(Ls, [El]),
                            ets:insert(hooks, {Key, NewLs}),
                            ok
                    end;
                [] ->
                    NewLs = [El],
                    ets:insert(hooks, {Key, NewLs}),
                    create_hook_metric(Key),
                    ok
            end,
    {reply, Reply, State};

handle_call({delete, Hook}, _From, State) ->
    {Key, El} = get_key_and_el(Hook),
    Reply = case ets:lookup(hooks, Key) of
                [{_, Ls}] ->
                    NewLs = lists:delete(El, Ls),
                    ets:insert(hooks, {Key, NewLs}),
                    ok;
                [] ->
                    ok
            end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ets:delete(hooks),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% @doc Run the handlers of the hook in order.
%% The arguments passed to the hook handler function are: [Acc | Args].
%% The result of a call is used as Acc for the next call.
%% If a call returns 'stop', no more calls are performed and 'stopped' is returned.
%% If a call returns {stop, NewAcc}, no more calls are performed and NewAcc is returned.
%% If hook doesn't need accumulator and doesn't care about the return value. The 'ok'
%% atom can be used as initial Acc value.
%%
%% Note that every hook handler MUST return a valid Acc. if any hook handler is not
%% interested in Acc parameter (or even if Acc is not used for a hook at all), it must
%% return (pass through) an unchanged input accumulator value.
-spec run_fold(HookName :: atom(),
               HostType :: global | binary(),
               Acc :: term(),
               Args :: [term()]) ->
                  term() | stopped.
run_fold(HookName, HostType, Acc, Args) ->
    Key = {HookName, HostType},
    case ets:lookup(hooks, Key) of
        [{_, Ls}] ->
            mongoose_metrics:increment_generic_hook_metric(HostType, HookName),
            run_hook(Ls, Key, Acc, Args);
        [] ->
            Acc
    end.

run_hook([], _Key, Val, _Args) ->
    Val;
run_hook([{_Priority, Module, Function} | Ls], Key, Acc, Args) ->
    Res = hook_apply_function(Module, Function, Acc, Args),
    case Res of
        {'EXIT', Reason} ->
            error_running_hook(Reason, Key, Args),
            run_hook(Ls, Key, Acc, Args);
        stop ->
            stopped;
        {stop, NewAcc} ->
            NewAcc;
        NewAcc ->
            run_hook(Ls, Key, NewAcc, Args)
    end.

hook_apply_function(_Module, Function, Acc, Args) when is_function(Function) ->
    safely:apply(Function, [Acc | Args]);
hook_apply_function(Module, Function, Acc, Args) ->
    safely:apply(Module, Function, [Acc | Args]).

error_running_hook(Reason, Key, Args) ->
    ?LOG_ERROR(#{what => hook_failed,
                 text => <<"Error running hook">>,
                 hook_key => Key, args => Args, reason => Reason}).

-spec get_key_and_el(hook()) -> {key(), element()}.
get_key_and_el({HookName, HostType, Module, Function, Priority}) ->
    Key = {HookName, HostType},
    El = {Priority, Module, Function},
    {Key, El}.

-spec create_hook_metric(Key :: key()) -> any().
create_hook_metric({HookName, HostType}) ->
    mongoose_metrics:create_generic_hook_metric(HostType, HookName).
