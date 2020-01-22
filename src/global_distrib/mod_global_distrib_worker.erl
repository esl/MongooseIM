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

%% Routing worker

-module(mod_global_distrib_worker).
-author('konrad.zemek@erlang-solutions.com').

-behaviour(gen_server).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("global_distrib_metrics.hrl").

-define(TIMEOUT, 60000). % TODO

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link(Name :: atom()) -> {ok, pid()} | {error, any()}.
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% gen_server API
%%--------------------------------------------------------------------

init(_) ->
    {ok, state, ?TIMEOUT}.

handle_call({data, Host, TransferTime, Stamp, Data}, From, State) ->
    gen_server:reply(From, ok),
    handle_cast({data, Host, TransferTime, Stamp, Data}, State).

handle_cast({route, {From, To, Acc, Packet}}, State) ->
    ejabberd_router:route(From, To, Acc, Packet),
    {noreply, State, ?TIMEOUT};
handle_cast({data, Host, TransferTime, Stamp, Data}, State) ->
    QueueTimeNative = erlang:monotonic_time() - Stamp,
    QueueTimeUS = erlang:convert_time_unit(QueueTimeNative, native, microsecond),
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_RECV_QUEUE_TIME, QueueTimeUS),
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_TRANSFER_TIME(Host), TransferTime),
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_MESSAGES_RECEIVED(Host), 1),
    do_work(Data),
    {noreply, State, ?TIMEOUT}.

handle_info(timeout, State) ->
    {stop, {shutdown, spinning_down_idle_worker}, State};
handle_info(_, State) ->
    {noreply, State, ?TIMEOUT}.

code_change(_FromVersion, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec do_work(Data :: binary()) -> any().
do_work(Data) ->
    {From, To, Acc, Packet} = erlang:binary_to_term(Data),
    mod_global_distrib_utils:maybe_update_mapping(From, Acc),
    ?DEBUG("event=route_incoming_gd,id=~s,stanza=~s",
           [mod_global_distrib:get_metadata(Acc, id, "unknown"),
            exml:to_binary(mongoose_acc:element(Acc))]),
    ejabberd_router:route(From, To, Acc, Packet).
