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
-module(mod_global_distrib_server_sup).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(supervisor).

-include("mongoose.hrl").

-export([start_link/1, init/1]).
-export([get_connection/1, is_available/1]).
-export([start_pool/3, stop_pool/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link(Server :: jid:lserver()) -> {ok, pid()} | {error, any()}.
start_link(Server) ->
    SupName = mod_global_distrib_utils:server_to_sup_name(Server),
    supervisor:start_link({local, SupName}, ?MODULE, [Server]).

-spec get_connection(Server :: jid:lserver()) -> {ok, pid()} | {error, not_available}.
get_connection(Server) ->
    try mod_global_distrib_server_mgr:get_connection(Server)
        %% Possible issues:
        %% - {'EXIT', {noproc, _}}
        %%  - {case_clause,{'EXIT',{no_connections...
    catch Class:Reason:Stacktrace ->
            ?ERROR_MSG("event=get_gd_connection_failed "
                       "server=~ts reason=~p:~p stacktrace=~1000p",
                       [Server, Class, Reason, Stacktrace]),
            %% May be caused by missing server_sup or missing connection manager
            %% The former occurs when a process tries to send a message to Server
            %% for the first time.
            %% The latter occurs when some other process already started server_sup,
            %% which hasn't started manager yet.
            %% In both cases the caller should attempt to start server_sup,
            %% so the main outgoing_conns_sup becomes a synchronisation point
            %% because it's impossible to learn that the server_sup is `already_started`
            %% without it finishing the init first (thus finishing the init of mgr as well).
            %%
            %% TODO: Write a test for it, once we establish a good way to reproduce
            %%       race conditions in tests!
            {error, not_available}
    end.

-spec is_available(Server :: jid:lserver()) -> boolean().
is_available(Server) ->
    pong == mod_global_distrib_server_mgr:ping_proc(Server).

-spec start_pool(Supervisor :: pid(),
                 Endpoint :: mod_global_distrib_utils:endpoint(),
                 Server :: jid:lserver()) ->
    {ok, atom(), pid()} | {error, any()}.
start_pool(Supervisor, Endpoint, Server) ->
    PoolRef = endpoint_to_atom(Endpoint),
    PoolParams = [
                  PoolRef,
                  {mod_global_distrib_connection, start_link, [Endpoint, Server]},
                  [{pool_size, mod_global_distrib_utils:opt(mod_global_distrib_sender,
                                                            connections_per_endpoint)}]
                 ],
    PoolSpec = #{
      id => Endpoint,
      start => {cpool, new_pool_sup, PoolParams},
      restart => temporary,
      shutdown => 5000,
      type => supervisor,
      modules => dynamic
     },
    {ok, PoolPid} = supervisor:start_child(Supervisor, PoolSpec),
    {ok, PoolRef, PoolPid}.

-spec stop_pool(Supervisor :: pid(), Endpoint :: mod_global_distrib_utils:endpoint()) ->
    ok | {error, any()}.
stop_pool(Supervisor, Endpoint) ->
    ok = supervisor:terminate_child(Supervisor, Endpoint),
    supervisor:delete_child(Supervisor, Endpoint).

%%--------------------------------------------------------------------
%% supervisor callback
%%--------------------------------------------------------------------

init([Server]) ->
    SupFlags = #{ strategy => rest_for_one, intensity => 5, period => 5 },
    MgrName = mod_global_distrib_utils:server_to_mgr_name(Server),
    ServerMgrSpec = #{
      id => MgrName,
      start => {mod_global_distrib_server_mgr, start_link, [Server, self()]},
      restart => transient,
      shutdown => 5000,
      type => worker,
      modules => dynamic
     },
    {ok, {SupFlags, [ServerMgrSpec]}}.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec endpoint_to_atom(mod_global_distrib_utils:endpoint()) -> atom().
endpoint_to_atom({IP, Port}) ->
    list_to_atom(inet:ntoa(IP) ++ "_" ++ integer_to_list(Port)).
