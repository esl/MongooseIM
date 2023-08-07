-module(service_domain_db).

-behaviour(mongoose_service).

-include("mongoose_config_spec.hrl").
-include("mongoose_logger.hrl").

%% Use a separate pg scope, which is started by ejabberd_sup
%% This prevents a bug when a default pg server is not running
-define(SCOPE, mim_scope).
-define(GROUP, service_domain_db_group).

-export([start/1, stop/0, restart/0, config_spec/0]).
-export([start_link/0]).
-export([enabled/0]).
-export([force_check_for_updates/0]).
-export([sync_local/0, ping/1]).

-ignore_xref([start_link/0, sync_local/0, ping/1,
              init/1, handle_call/3, handle_cast/2, handle_info/2,
              code_change/3, terminate/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ---------------------------------------------------------------------------
%% Client code

-spec start(mongoose_service:options()) -> ok.
start(Opts) ->
    mongoose_domain_sql:start(Opts),
    ChildSpec =
        {?MODULE,
         {?MODULE, start_link, []},
         permanent, infinity, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec),
    mongoose_domain_db_cleaner:start(Opts),
    ok.

-spec stop() -> ok.
stop() ->
    mongoose_domain_db_cleaner:stop(),
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE),
    supervisor:terminate_child(ejabberd_sup, domain_pg),
    supervisor:delete_child(ejabberd_sup, domain_pg),
    ok.

restart() ->
    %% if service goes out of sync with DB this interface
    %% can be used to restart the service.
    %% it's enough to just shut down gen_server, supervisor
    %% will restart it.
    gen_server:cast(?MODULE, reset_and_shutdown).

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{<<"event_cleaning_interval">> => #option{type = integer,
                                                                validate = positive},
                       <<"event_max_age">> => #option{type = integer,
                                                      validate = positive},
                       <<"db_pool">> => #option{type = atom,
                                                validate = pool_name}
                      },
             defaults = #{<<"event_cleaning_interval">> => 1800, % 30 minutes
                          <<"event_max_age">> => 7200, % 2 hours
                          <<"db_pool">> => global}}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

enabled() ->
    mongoose_service:is_loaded(?MODULE).

all_members() ->
    pg:get_members(?SCOPE, ?GROUP).

force_check_for_updates() ->
    %% Send a broadcast message.
    case all_members() of
        [_|_] = Pids ->
            [Pid ! check_for_updates || Pid <- Pids],
            ok;
        _ ->
            ok
    end.

%% Ensure that all pending check_for_updates messages are received by this node,
%% even if they are sent by other nodes in the cluster.
%% We have to do an RPC to ensure there is nothing pending in the distributed communication buffers.
%% Used in tests.
sync_local() ->
    LocalPid = whereis(?MODULE),
    true = is_pid(LocalPid),
    Nodes = [node(Pid) || Pid <- all_members()],
    %% Ping from all nodes in the cluster
    [pong = rpc:call(Node, ?MODULE, ping, [LocalPid]) || Node <- Nodes],
    pong.

-spec ping(pid()) -> pong.
ping(Pid) ->
    gen_server:call(Pid, ping).

%% ---------------------------------------------------------------------------
%% Server callbacks

init([]) ->
    pg:join(?SCOPE, ?GROUP, self()),
    gen_server:cast(self(), initial_loading),
    %% initial state will be set on initial_loading processing
    {ok, #{}}.

handle_call(ping, _From, State) ->
    {reply, pong, State};
handle_call(Request, From, State) ->
    ?UNEXPECTED_CALL(Request, From),
    {reply, ok, State}.

handle_cast(initial_loading, State) ->
    mongoose_domain_loader:initial_load(),
    NewState = State#{check_for_updates_interval => 30000},
    {noreply, handle_check_for_updates(NewState, true)};
handle_cast(reset_and_shutdown, State) ->
    %% to ensure that domains table is re-read from
    %% scratch, we must reset the last event id.
    mongoose_loader_state:reset(),
    {stop, shutdown, State};
handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

handle_info(check_for_updates, State) ->
    {noreply, handle_check_for_updates(State, false)};
handle_info(Info, State) ->
    ?UNEXPECTED_INFO(Info),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Server helpers

handle_check_for_updates(State = #{check_for_updates_interval := Interval},
                         IsInitial) ->
    maybe_cancel_timer(IsInitial, State),
    receive_all_check_for_updates(),
    mongoose_domain_loader:check_for_updates(),
    TRef = erlang:send_after(Interval, self(), check_for_updates),
    State#{check_for_updates_tref => TRef}.

maybe_cancel_timer(IsInitial, State) ->
    TRef = maps:get(check_for_updates_tref, State, undefined),
    case {IsInitial, TRef} of
        {true, undefined} -> ok; %% TRef is not set the first time
        {false, _} -> erlang:cancel_timer(TRef)
    end.

receive_all_check_for_updates() ->
    receive check_for_updates -> receive_all_check_for_updates() after 0 -> ok end.
