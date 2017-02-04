%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
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
-module(http_helper).

-export([start/3, stop/0, init/3, handle/2, terminate/3]).

start(Port, Path, HandleFun) ->
    application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([{'_', [{Path, http_helper, [HandleFun]}]}]),
    {ok, _} = cowboy:start_http(http_helper_listener, 100, [{port, Port}],
                                [{env, [{dispatch, Dispatch}]}]).

stop() ->
    cowboy:stop_listener(http_helper_listener).

%% Cowboy handler callbacks

init(_Type, Req, [HandleFun]) ->
    {ok, Req, HandleFun}.

handle(Req, HandleFun) ->
    Req2 = HandleFun(Req),
    {ok, Req2, HandleFun}.

terminate(_Reason, _Req, _State) ->
    ok.
