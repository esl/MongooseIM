%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
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
-module(mongoose_http_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%%------------------------------------------------------------------------------
%% API

-spec(start_link(term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(SupName) ->
    supervisor:start_link({local, SupName}, ?MODULE, []).

%%------------------------------------------------------------------------------
%% supervisor callbacks

-spec init(Args :: term()) -> {ok, {{one_for_all, pos_integer(), pos_integer()}, []}}.
init([]) ->
    RestartStrategy = one_for_all,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, []}}.
