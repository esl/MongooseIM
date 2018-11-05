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
-module(mongoose_wpool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([child_spec/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2, 3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, {#{strategy => one_for_one, intensity => 100, period => 5},
                                    []}}.
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 100,
                 period => 5},

    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec child_spec(mongoose_wpool:type()) ->
    #{id := mongoose_wpool:name(),
      start := {mongoose_wpool_type_sup, start_link, [mongoose_wpool:type()]},
      restart => transient,
      shutdown => brutal_kill,
      type => supervisor,
      modules => [module()]}.
child_spec(Type) ->
    #{id => mongoose_wpool_type_sup:name(Type),
      start => {mongoose_wpool_type_sup, start_link, [Type]},
      restart => transient,
      shutdown => brutal_kill,
      type => supervisor,
      modules => [mongoose_wpool_type_sup]
     }.

