%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
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
-module(mongoose_riak_sup).

-behaviour(supervisor).

%% API
-export([start/4]).
-export([start_link/4]).
-export([stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-include("ejabberd.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

start(Workers, Addr, Port, PBOpts) ->
    ChildSpec = {?MODULE, {?MODULE, start_link, [Workers, Addr, Port, PBOpts]},
        transient, infinity, supervisor, [?MODULE]},
    {ok, _} = supervisor:start_child(ejabberd_sup, ChildSpec).
%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
%-spec(start_link(integer(), inet:ip(), ) ->
%    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Workers, Addr, Port, PBOpts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Workers, Addr, Port, PBOpts]).

-spec stop() -> _.
stop() ->
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
          MaxR :: non_neg_integer(), MaxT :: pos_integer()},
          [ChildSpec :: supervisor:child_spec()]
         }}).
init([Workers, Address, Port, PBOpts]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, [child_spec(Workers, [Address, Port, PBOpts])] }}.

child_spec(Workers, RiakOpts) ->
    Restart = transient,
    Shutdown = 2000,
    Type = supervisor,
    ChildMods = [mongoose_riak, riakc_pb_socket],
    ChildMF = {mongoose_riak, start_worker},
    RiakPoolName = mongoose_riak:pool_name(),
    ChildArgs = {for_all, RiakOpts},
    AChild = {RiakPoolName, {cuesport, start_link, [RiakPoolName, Workers, ChildMods, ChildMF, ChildArgs]},
        Restart, Shutdown, Type, ChildMods},
    AChild.

%%%===================================================================
%%% Internal functions
%%%===================================================================

