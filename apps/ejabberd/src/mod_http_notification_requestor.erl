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
-export([start/3, request/4]).

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

-record(state, {parameters = []}).

%%% @doc This module starts a process which holds parameters for hosts
%%% and does the actual requesting.

%%%===================================================================
%%% API
%%%===================================================================

start(Host, PoolName, Path) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []), % ignore if already started
    gen_server:call(?MODULE, {register_host, Host, PoolName, Path}),
    ensure_metrics(Host).

request(Host, Sender, Receiver, Message) ->
    gen_server:cast(?MODULE, {request, Host, Sender, Receiver, Message}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_Opts) ->
    {ok, #state{}}.

%% @doc Remember parameters for a host, replace if already there
handle_call({register_host, Host, PoolName, Path}, _From, #state{parameters = Params} = State) ->
    Nparams = [{Host, {PoolName, fix_path(Path)}} | proplists:delete(Host, Params)],
    {reply, ok, State#state{parameters = Nparams}}.

handle_cast({request, Host, Sender, Receiver, Message}, State) ->
    {PoolName, Path} = proplists:get_value(Host, State#state.parameters),
    make_req(Host, PoolName, Path, Sender, Receiver, Message),
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

make_req(Host, PoolName, Path, Sender, Receiver, Message) ->
    Pool = mongoose_http_client:get_pool(PoolName),
    Query = <<"author=", Sender/binary, "&server=", Host/binary, "&receiver=", Receiver/binary, "&message=",
        Message/binary>>,
    ?INFO_MSG("Making request '~p' for user ~s@~s...", [Sender, Host]),
    Headers = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
    T0 = os:timestamp(),
    {Res, Elapsed} = case mongoose_http_client:post(Pool, Path, Headers, Query) of
                         {ok, _} ->
                             {ok, timer:now_diff(os:timestamp(), T0)};
                         {error, Reason} ->
                             {{error, Reason}, 0}
                     end,
    record_result(Host, Res, Elapsed),
    ok.

ensure_metrics(Host) ->
    mongoose_metrics:ensure_metric([Host, mod_http_notifications, sent], spiral),
    mongoose_metrics:ensure_metric([Host, mod_http_notifications, failed], spiral),
    mongoose_metrics:ensure_metric([Host, mod_http_notifications, response_time], histogram),
    ok.
record_result(Host, ok, Elapsed) ->
    mongoose_metrics:update([Host, mod_http_notifications, sent], 1),
    mongoose_metrics:update([Host, mod_http_notifications, response_time], Elapsed),
    ok;
record_result(Host, {error, Reason}, _) ->
    mongoose_metrics:update([Host, mod_http_notifications, failed], 1),
    ?WARNING_MSG("Sending http notification failed: ~p", [Reason]),
    ok.

%% @doc Convert to binary, strip initial slash (it is added by mongoose_http_client)
fix_path(P) when is_list(P) ->
    fix_path(list_to_binary(P));
fix_path(P) when is_binary(P) ->
    case P of
        <<47/integer, R/binary>> ->
            R;
        Path ->
            Path
    end.
