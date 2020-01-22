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

-export([start/3, stop/0, port/0, init/2]).

-spec start(
        non_neg_integer(),
        string(),
        fun((cowboy_req:req()) -> {ok, cowboy_req:req(), any()})) ->
    {ok, pid()}.
start(Port, Path, HandleFun) ->
    application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([{'_', [{Path, http_helper, [HandleFun]}]}]),
    {ok, _} = cowboy:start_clear(http_helper_listener, [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}).

stop() ->
    cowboy:stop_listener(http_helper_listener).

port() ->
    ranch:get_port(http_helper_listener).

%% Cowboy handler callbacks
init(Req, [HandleFun] = State) ->
    Req2 = HandleFun(Req),
    {ok, Req2, State}.
