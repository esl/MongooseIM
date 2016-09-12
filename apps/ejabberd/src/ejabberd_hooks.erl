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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_hooks).
-author('alexey@process-one.net').

-behaviour(gen_server).

%% External exports
-export([start_link/0,
         add/4,
         add/5,
         add2/4,
         add2/5,
         delete/4,
         delete/5,
         run/2,
         run/3,
         run_fold/3,
         run_fold/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         code_change/3,
         handle_info/2,
         terminate/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").


-record(state, {}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ejabberd_hooks}, ejabberd_hooks, [], []).

%% @doc Add a fun to the given hook.
%% The integer sequence is used to sort the calls:
%% low numbers are executed before high numbers.
-spec add(Hook :: atom(),
          Host :: ejabberd:server() | global,
          Function :: fun() | atom(),
          Seq :: integer()) -> ok.
add(Hook, Host, Function, Seq) when is_function(Function) ->
    add(Hook, Host, undefined, Function, Seq).

add2(Hook, Host, Function, Seq) when is_function(Function) ->
    add2(Hook, Host, undefined, Function, Seq).
add2(Hook, Host, Module, Function, Seq) ->
    gen_server:call(ejabberd_hooks, {add, Hook, Host, Module, Function, Seq, new}).
%% @doc Add a module and function to the given hook.
%% The integer sequence is used to sort the calls:
%% low numbers are executed before high numbers.
-spec add(Hook :: atom(),
          Host :: ejabberd:server() | global,
          Module :: atom(),
          Function :: fun() | atom(),
          Seq :: integer()) -> ok.
add(Hook, Host, Module, Function, Seq) ->
    gen_server:call(ejabberd_hooks, {add, Hook, Host, Module, Function, Seq, old}).

%% @doc Delete a module and function from this hook.
%% It is important to indicate exactly the same information than when the call was added.
-spec delete(Hook :: atom(),
             Host :: ejabberd:server() | global,
             Function :: fun() | atom(),
             Seq :: integer()) -> ok.
delete(Hook, Host, Function, Seq) when is_function(Function) ->
    delete(Hook, Host, undefined, Function, Seq).

-spec delete(Hook :: atom(),
             Host :: ejabberd:server() | global,
             Module :: atom(),
             Function :: fun() | atom(),
             Seq :: integer()) -> ok.
delete(Hook, Host, Module, Function, Seq) ->
    gen_server:call(ejabberd_hooks, {delete, Hook, Host, Module, Function, Seq}).

%% @doc Run the calls of this hook in order, don't care about function results.
%% If a call returns stop, no more calls are performed.
-spec run(Hook :: atom(),
          Args :: [any()]) -> ok.
run(Hook, Args) ->
    run(Hook, global, Args).

-spec run(Hook :: atom(),
          Host :: ejabberd:server() | global,
          Args :: [any()]) -> ok.
run(Hook, Host, Args) ->
    % if {packet,P} not given as the first arg, prepend 'nopacket'
    CArgs = case Args of
                [{packet, #xmlel{}}|_] ->
                    Args;
                _ ->
                    [nopacket|Args]
            end,
    case ets:lookup(hooks, {Hook, Host}) of
        [{_, Ls}] ->
            mongoose_metrics:increment_generic_hook_metric(Host, Hook),
            run1(Ls, Hook, CArgs);
        [] ->
            ok
    end.

%% @spec (Hook::atom(), Val, Args) -> Val | stopped | NewVal
%% @doc Run the calls of this hook in order.
%% The arguments passed to the function are: [Val | Args].
%% The result of a call is used as Val for the next call.
%% If a call returns 'stop', no more calls are performed and 'stopped' is returned.
%% If a call returns {stop, NewVal}, no more calls are performed and NewVal is returned.
run_fold(Hook, Val, Args) ->
    run_fold(Hook, global, Val, Args).

run_fold(Hook, Host, Val, Args) ->
    % if {packet,P} not given as the first arg, prepend 'nopacket'
    CArgs = case Args of
                [{packet, #xmlel{}}|_] ->
                    Args;
                _ ->
                    [nopacket|Args]
            end,
    case ets:lookup(hooks, {Hook, Host}) of
        [{_, Ls}] ->
            mongoose_metrics:increment_generic_hook_metric(Host, Hook),
            run_fold1(Ls, Hook, Val, CArgs);
        [] ->
            Val
    end.

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
    ets:new(hooks, [named_table, {read_concurrency,true}]),
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
handle_call({add, Hook, Host, Module, Function, Seq, Mode}, _From, State) ->
    Reply = case ets:lookup(hooks, {Hook, Host}) of
                [{_, Ls}] ->
                    El = {Seq, Module, Function, Mode},
                    case lists:member(El, Ls) of
                        true ->
                            ok;
                        false ->
                            NewLs = lists:merge(Ls, [El]),
                            ets:insert(hooks, {{Hook, Host}, NewLs}),
                            ok
                    end;
                [] ->
                    NewLs = [{Seq, Module, Function, Mode}],
                    ets:insert(hooks, {{Hook, Host}, NewLs}),
                    mongoose_metrics:create_generic_hook_metric(Host, Hook),
                    ok
            end,
    {reply, Reply, State};

handle_call({delete, Hook, Host, Module, Function, Seq}, _From, State) ->
    Reply = case ets:lookup(hooks, {Hook, Host}) of
                [{_, Ls}] ->
                    NewLs = lists:delete({Seq, Module, Function}, Ls),
                    ets:insert(hooks, {{Hook, Host}, NewLs}),
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

%% @doc for old-style hooks, strip leading {packet, #xmlel} or 'nopacket' if present
cook_args(old, [{packet, #xmlel{}}|A]) ->
    A;
cook_args(old, [nopacket|A]) ->
    A;
cook_args(new, A) ->
    A.

cook_return(_, {'EXIT', Reason}, A) ->
    {'EXIT', Reason, A};
%% legal return from old style hook: anything, it is ignored
%% stopping
cook_return(old, stop, [nopacket|_]) ->
    % to keep old api
    stop;
cook_return(old, stop, [{packet, #xmlel{} = P}|_]) ->
    {stop, P};
cook_return(old, _, A) ->
    A;
cook_return(new, {stop, nopacket}, _) ->
    stopped;
cook_return(new, {stop, {packet, #xmlel{} = P}}, _) ->
    {stop, P};
%% new-style: if there was nopacket, must be nopacket or {nopacket, V}, V is ignored and we go on
cook_return(new, nopacket, [nopacket|A]) ->
    [nopacket|A];
cook_return(new, {nopacket, _}, [nopacket|A]) ->
    [nopacket|A];
%% new-style: if there was a packet, must be a {packet, NewPacket} or {{packet, NewPacket}, V}, V is ignored and we go on WITH THE NEW PACKET
cook_return(new, {packet, #xmlel{} = NewPacket}, [{packet, #xmlel{}}|A]) ->
    [{packet, NewPacket}|A];
cook_return(new, {{packet, #xmlel{} = NewPacket, _}}, [{packet, #xmlel{}}|A]) ->
    [{packet, NewPacket}|A].

run1([], _Hook, [nopacket|_Args]) ->
    ok;
run1([], _Hook, [{packet, P}|_Args]) ->
    P;
run1([{_Seq, Module, Function, Mode} | Ls], Hook, Args) ->
    CArgs = cook_args(Mode, Args),
    Res = if is_function(Function) ->
                  safely:apply(Function, CArgs);
             true ->
                  safely:apply(Module, Function, CArgs)
          end,
    NRes = cook_return(Mode, Res, Args),
    case NRes of
        {'EXIT', Reason, NArgs} ->
            ?ERROR_MSG("~p~n    Running hook: ~p~n    Callback: ~p:~p, mode ~p",
                       [Reason, {Hook, Args}, Module, Function, Mode]),
            run1(Ls, Hook, NArgs);
        stop ->
            stopped;
        {stop, R} ->
            R;
        NArgs ->
            run1(Ls, Hook, NArgs)
    end.

%% @doc for old-style hooks, strip leading {packet, #xmlel} or 'nopacket' if present
%% insert value passed to run_fold as the first/second arg, depending on mode
cook_args(old, V, [{packet, #xmlel{}}|A]) ->
    [V|A];
cook_args(old, V, [nopacket|A]) ->
    [V|A];
cook_args(new, V, [{packet, #xmlel{} = P}|A]) ->
    [{packet, P}, V|A];
cook_args(new, V, [nopacket|A]) ->
    [nopacket, V|A].


cook_fold_result(_, {'EXIT', Reason}, Val, Args) ->
    {'EXIT', Reason, Val, Args};
%% brutal stop
cook_fold_result(_, stop, _, _) ->
    stop;
%% old handler, old way, stop with value
cook_fold_result(old, {stop, V}, _, [nopacket|_]) ->
    {stop, V};
%% old handler, new way, stop with value
cook_fold_result(old, {stop, V}, _, [{packet, #xmlel{} = P}|_]) ->
    {stop, P, V};
%% old handler, any way, returned something
cook_fold_result(old, NVal, _, Args) ->
    {NVal, Args};
%% new handler, old way, stop with value
cook_fold_result(new, {stop, V}, _, [nopacket|_]) ->
    {stop, V};
%% new handler, old way, just modify value
cook_fold_result(new, {nopacket, NVal}, _, [nopacket|Args]) ->
    {NVal, [nopacket|Args]};
%% new handler,new way, stop with value
cook_fold_result(new, {stop, {packet, P}, V}, _, _) ->
    {stop, P, V};
%% new handler, new way, modify value and packet
cook_fold_result(new, {{packet, #xmlel{} = NPacket}, NVal}, _, [{packet, _}|Args]) ->
    {NVal, [{packet, NPacket}|Args]}.

run_fold1([], _Hook, Val, [nopacket|_Args]) ->
    Val;
run_fold1([], _Hook, Val, [{packet, #xmlel{} = Packet}|_Args]) ->
    {Packet, Val};
run_fold1([{_Seq, Module, Function, Mode} | Ls], Hook, Val, Args) ->
    CArgs = cook_args(Mode, Val, Args),
    Res = if is_function(Function) ->
                  safely:apply(Function, CArgs);
             true ->
                  safely:apply(Module, Function, CArgs)
          end,
    NRes = cook_fold_result(Mode, Res, Val, Args),
    case NRes of
        {'EXIT', Reason, NVal, NArgs} ->
            ?ERROR_MSG("~p~nrunning hook: ~p in mode ~p",
                       [Reason, {Hook, Args}, Mode]),
            run_fold1(Ls, Hook, NVal, NArgs);
        stop ->
            stopped;
        {stop, NewVal} ->
            NewVal;
        {stop, P, NewVal} ->
            {P, NewVal};
        {NewVal, NArgs} ->
            run_fold1(Ls, Hook, NewVal, NArgs)
    end.
