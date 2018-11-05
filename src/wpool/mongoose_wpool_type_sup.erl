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
-module(mongoose_wpool_type_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([name/1]).

%% Supervisor callbacks
-export([init/1]).

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
-spec start_link(mongoose_wpool:type()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Type) ->
    supervisor:start_link({local, name(Type)}, ?MODULE, [Type]).

-spec name(mongoose_wpool:type()) -> mongoose_wpool:name().
name(Type) ->
    list_to_atom("mongoose_wpool_" ++ atom_to_list(Type) ++ "_sup").

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
                                    [#{id := mongoose_wpool:name(),
                                       start := {mongoose_wpool_mgr, start_link, [mongoose_wpool:type()]},
                                       restart => transient,
                                       shutdown => brutal_kill,
                                       type => worker,
                                       modules => [module()]}]}}.
init([Type]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 100,
                 period => 5},

    ChildSpec = #{id => mongoose_wpool_mgr:name(Type),
                  start => {mongoose_wpool_mgr, start_link, [Type]},
                  restart => transient,
                  shutdown => brutal_kill,
                  type => worker,
                  modules => [?MODULE]},

    {ok, {SupFlags, [ChildSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
