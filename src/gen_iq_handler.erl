%%%----------------------------------------------------------------------
%%% File    : gen_iq_handler.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : IQ handler support
%%% Created : 22 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(gen_iq_handler).
-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start_link/3,
         add_iq_handler/6,
         remove_iq_handler/3,
         stop_iq_handler/3,
         handle/8,
         process_iq/7,
         check_type/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("mongoose.hrl").

-record(state, {host     :: jid:server(),
                module   :: module(),
                function :: atom()
               }).
-type state()     :: #state{}.
-type component() :: atom() | tuple().
-type ns()        :: binary().
-type type()      :: 'no_queue' | 'one_queue' | 'parallel' | {'queues', integer()}.
-type options()   :: atom() | {one_queue | queues, pid() | [pid()]}.

%%====================================================================
%% API
%%====================================================================

%% @doc Starts the server
-spec start_link(jid:server(), atom(), atom()
                ) -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link(Host, Module, Function) ->
    gen_server:start_link(?MODULE, [Host, Module, Function], []).


-spec add_iq_handler(component(), Host :: jid:server(), NS :: ns(),
    Module :: atom(), Function :: atom(), Type :: type()) -> any().
add_iq_handler(Component, Host, NS, Module, Function, Type) ->
    case Type of
        no_queue ->
            Component:register_iq_handler(Host, NS, Module, Function, no_queue);
        one_queue ->
            {ok, Pid} = supervisor:start_child(ejabberd_iq_sup,
                                               [Host, Module, Function]),
            Component:register_iq_handler(Host, NS, Module, Function,
                                          {one_queue, Pid});
        {queues, N} ->
            Pids =
                lists:map(
                  fun(_) ->
                          {ok, Pid} = supervisor:start_child(
                                        ejabberd_iq_sup,
                                        [Host, Module, Function]),
                          Pid
                  end, lists:seq(1, N)),
            Component:register_iq_handler(Host, NS, Module, Function,
                                          {queues, Pids});
        parallel ->
            Component:register_iq_handler(Host, NS, Module, Function, parallel)
    end.


-spec remove_iq_handler(Component :: component(),
                        Host :: jid:server(),
                        NS :: ns()) -> any().
remove_iq_handler(Component, Host, NS) ->
    Component:unregister_iq_handler(Host, NS).


-spec stop_iq_handler(M :: atom(), F :: atom(), Opts :: options()) -> any().
stop_iq_handler(_Module, _Function, Opts) ->
    case Opts of
        {one_queue, Pid} ->
            gen_server:call(Pid, stop);
        {queues, Pids} ->
            lists:foreach(fun(Pid) ->
                                  catch gen_server:call(Pid, stop)
                          end, Pids);
        _ ->
            ok
    end.


-spec handle(Host :: jid:server(), Module :: atom(), Function :: atom(),
             Opts :: options(), From :: jid:jid(), To :: jid:jid(),
             mongoose_acc:t(),
             IQ :: jlib:iq()) -> mongoose_acc:t().
handle(Host, Module, Function, Opts, From, To, Acc, IQ) ->
    case Opts of
        no_queue ->
            process_iq(Host, Module, Function, From, To, Acc, IQ);
        {one_queue, Pid} ->
            Pid ! {process_iq, From, To, Acc, IQ},
            Acc;
        {queues, Pids} ->
            Pid = lists:nth(erlang:phash(erlang:unique_integer(), length(Pids)), Pids),
            Pid ! {process_iq, From, To, Acc, IQ},
            Acc;
        parallel ->
            spawn(?MODULE, process_iq, [Host, Module, Function, From, To, Acc, IQ]),
            Acc;
        _ ->
            Acc
    end.


-spec process_iq(Host :: jid:server(), Module :: atom(), Function :: atom(),
                 From :: jid:jid(), To :: jid:jid(), Acc :: mongoose_acc:t(),
                 IQ :: jlib:iq()) -> mongoose_acc:t().
process_iq(_Host, Module, Function, From, To, Acc, IQ) ->
    case catch Module:Function(From, To, Acc, IQ) of
        {'EXIT', {Reason, StackTrace}} ->
            ?WARNING_MSG("event=process_iq_error,reason=~p,stack_trace=~p",
                         [Reason, StackTrace]),
            Acc;
        {Acc1, ignore} ->
            Acc1;
        {Acc1, ResIQ} ->
            ejabberd_router:route(To, From, Acc1,
                                  jlib:iq_to_xml(ResIQ))
    end.

-spec check_type(type()) -> type().

check_type(no_queue) -> no_queue;
check_type(one_queue) -> one_queue;
check_type(N) when is_integer(N), N>0 -> N;
check_type(parallel) -> parallel.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initiates the server
-spec init([atom() | binary(), ...]) -> {'ok', state()}.
init([Host, Module, Function]) ->
    {ok, #state{host = Host,
                module = Module,
                function = Function}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    Reply = ok,
    {stop, normal, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({process_iq, From, To, Acc, IQ},
            #state{host = Host,
                   module = Module,
                   function = Function} = State) ->
    process_iq(Host, Module, Function, From, To, Acc, IQ),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
