-module(mod_global_distrib_outgoing_conns_sup).

-behaviour(supervisor).

-include("mongoose.hrl").

-export([start_link/0, init/1]).
-export([add_server/1, get_connection/1, ensure_server_started/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec add_server(Server :: jid:lserver()) -> ok | {error, any()}.
add_server(Server) ->
    SupName = mod_global_distrib_utils:server_to_sup_name(Server),
    ServerSupSpec = #{
      id => SupName,
      start => {mod_global_distrib_server_sup, start_link, [Server]},
      restart => temporary,
      shutdown => 5000,
      type => supervisor,
      modules => dynamic
     },
    ?INFO_MSG("event=outgoing_conn_start_progress,server='~s',state='starting'", [Server]),
    case supervisor:start_child(?MODULE, ServerSupSpec) of
        {ok, Pid} ->
            ?INFO_MSG("event=outgoing_conn_start_progress,server='~s',"
                      "state='started',pid='~p'", [Server, Pid]),
            ok;
        {error, {already_started, Pid}} ->
            ?INFO_MSG("event=outgoing_conn_start_progress,server='~s',"
                      "state='started_by_other',pid='~p'", [Server, Pid]),
            ok;
        Error ->
            ?ERROR_MSG("event=outgoing_conn_start_progress,server='~s',"
                       "state='failed',error='~p'", [Server, Error]),
            Error
    end.

%% Call to get_connection blocks until a connection is available.
%% Currently the timeout is infinity.
%% This function is safe for concurrent calls if the outgoing pool is not present yet.
%% The first caller will be the one initiating pool startup and the others are blocked
%% in the meantime; then, everyone will use the pool initiated by the first caller.
%% TODO: Revise negative cases for this function.
-spec get_connection(Server :: jid:lserver()) -> pid().
get_connection(Server) ->
    get_connection(Server, 5).

-spec get_connection(Server :: jid:lserver(), RetriesLeft :: non_neg_integer()) ->
    pid() | no_return().
get_connection(Server, 0) ->
    ?ERROR_MSG("event=cannot_acquire_outgoing_connection,server='~s'", [Server]),
    throw({error, {cannot_acquire_outgoing_connection, Server}});
get_connection(Server, RetriesLeft) ->
    case mod_global_distrib_server_sup:get_connection(Server) of
        {error, not_available} ->
            %% add_server is a synchronous call that, if succeeds,
            %% returns after the outgoing conn layer is ready,
            %% so we may retry immediately
            add_server(Server),
            get_connection(Server, RetriesLeft - 1);
        {ok, Pid} ->
            Pid
    end.

-spec ensure_server_started(Server :: jid:lserver()) -> ok | {error, any()}.
ensure_server_started(Server) ->
    case mod_global_distrib_server_sup:is_available(Server) of
        false -> add_server(Server);
        true -> ok
    end.

%%--------------------------------------------------------------------
%% supervisor callback
%%--------------------------------------------------------------------

init(_) ->
    SupFlags = #{ strategy => one_for_one, intensity => 5, period => 5 },
    {ok, {SupFlags, []}}.

