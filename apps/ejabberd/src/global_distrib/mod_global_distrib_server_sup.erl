-module(mod_global_distrib_server_sup).

-behaviour(supervisor).

-include("ejabberd.hrl").

-export([start_link/1, init/1]).
-export([get_connection/1]).
-export([start_pool/3, stop_pool/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link(Server :: ejabberd:lserver()) -> {ok, pid()} | {error, any()}.
start_link(Server) ->
    SupName = mod_global_distrib_utils:server_to_sup_name(Server),
    supervisor:start_link({local, SupName}, ?MODULE, [Server]).

-spec get_connection(Server :: ejabberd:lserver()) -> pid().
get_connection(Server) ->
    mod_global_distrib_server_mgr:get_connection(Server).

-spec start_pool(Supervisor :: pid(),
                 Endpoint :: mod_global_distrib_utils:endpoint(),
                 Server :: ejabberd:lserver()) ->
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

