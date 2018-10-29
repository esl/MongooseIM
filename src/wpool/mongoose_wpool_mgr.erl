%%==============================================================================
%% Copyright 2018 Erlang Solutions Ltd.
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
%%==============================================================================
%% @doc
%% This gen_server is responsible for starting and restarting pools of
%% given type
%%==============================================================================
-module(mongoose_wpool_mgr).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([name/1]).
-export([start/5]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("mongoose.hrl").

-record(state, {type}).

-type state() :: #state{type :: atom()}.

-type request() :: {start_pool,
                    mongoose_wpool:host(), mongoose_wpool:tag(),
                    [any()], [any()]}.

-type reply() :: ok.

%%%===================================================================
%%% API
%%%===================================================================

start_link(Type) ->
    gen_server:start_link({local, name(Type)}, ?MODULE, [Type], []).

start(Type, Host, Tag, PoolOpts, ConnOpts) ->
    gen_server:call(name(Type), {start_pool, Host, Tag, PoolOpts, ConnOpts}).

-spec name(mongoose_wpool:type()) -> mongoose_wpool:name().
name(Type) ->
    list_to_atom("mongoose_wpool_" ++ atom_to_list(Type) ++ "_mgr").
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([any()]) -> {ok, state()}.
init([Type]) ->
    {ok, #state{type = Type}}.

-spec handle_call(request(), {pid(), term()}, state()) ->
    {reply, reply(), state()}.
handle_call({start_pool, Host, Tag, PoolOpts, ConnOpts}, _From,
            #state{type = Type} = State) ->
    logger:warning("Starting pool {~p,~p,~p}, with pool opts: ~p and conn opts: ~p",
                   Type, Host, Tag, PoolOpts, ConnOpts),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

