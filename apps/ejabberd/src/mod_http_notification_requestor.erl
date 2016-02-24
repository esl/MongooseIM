%%%===================================================================
%%% @copyright (C) 2016, Erlang Solutions Ltd.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%===================================================================
-module(mod_http_notification_requestor).
-author("bartek").

-behaviour(gen_server).

%% API
-export([start_link/2, send_request/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(SERVER, ?MODULE).

-record(state, {pool_name}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(RequestorName, PoolName) ->
  gen_server:start_link({local, RequestorName}, ?MODULE, [PoolName], []).

send_request(RequestorName, Request) ->
  gen_server:cast(RequestorName, {request, Request}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([PoolName]) ->
  {ok, #state{pool_name = PoolName}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({request, Request}, #state{pool_name = PoolName} = State) ->
  make_req(PoolName, Request),
  {noreply, State};
handle_cast(_Request, State) ->
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

make_req(PoolName, {Host, Path, Sender, Receiver, Message}) ->
  Connection = cuesport:get_worker(PoolName),
  Query = <<"author=", Sender/binary, "&server=", Host/binary, "&receiver=", Receiver/binary, "&message=", Message/binary>>,
  ?INFO_MSG("Making request '~s' for user ~s@~s...", [Path, Sender, Host]),
  Header = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
  case fusco:request(Connection, <<Path/binary>>, "POST", Header, Query, 2, 5000) of
    {ok, {{Code, _Reason}, _RespHeaders, RespBody, _, _}} ->
      ?INFO_MSG("Request result: ~s: ~p", [Code, RespBody]);
    Else ->
      ?CRITICAL_MSG("Request failed: ~p", [[Else]])
  end,
  ok.

