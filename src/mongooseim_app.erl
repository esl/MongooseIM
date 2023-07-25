-module(mongooseim_app).

-behaviour(application).

-export([start/2, prep_stop/1, stop/1]).

-ignore_xref([prep_stop/1]).

-include("mongoose.hrl").

-spec start(_,_) -> {ok, pid()} | {error,_}.
start(normal, _Args) ->
    {TotalStartTime, Supervisor} = timer:tc(fun do_start/0),
    ?LOG_NOTICE(#{what => mongooseim_node_started, startup_time_ms => TotalStartTime div 1000,
                  version => ?MONGOOSE_VERSION, node => node()}),
    Supervisor;
start(_, _) ->
    {error, badarg}.

%% @doc Prepare the application for termination.
%% This function is called when an application is about to be stopped,
%% before shutting down the processes of the application.
-spec prep_stop(term()) -> term().
prep_stop(State) ->
    mongoose_deprecations:stop(),
    broadcast_c2s_shutdown_listeners(),
    mongoose_listener:stop(),
    mongoose_modules:stop(),
    mongoose_service:stop(),
    broadcast_c2s_shutdown_sup(),
    mongoose_wpool:stop(),
    mongoose_metrics:remove_all_metrics(),
    mongoose_config:stop(),
    mongoose_graphql_commands:stop(),
    State.

%% All the processes were killed when this function is called
-spec stop(_) -> ok.
stop(_State) ->
    ?LOG_NOTICE(#{what => mongooseim_node_stopped, version => ?MONGOOSE_VERSION, node => node()}),
    ejabberd:delete_pid_file(),
    ejabberd:update_status_file(stopped),
    ok.

do_start() ->
    mongoose_fips:notify(),
    ejabberd:write_pid_file(),
    ejabberd:update_status_file(starting),
    mongoose_config:start(),
    mongoose_metrics:init(),
    mnesia_init(),
    application:start(cache_tab),

    mongoose_graphql:init(),
    translate:start(),
    ejabberd_node_id:start(),
    ejabberd_commands:init(),
    mongoose_graphql_commands:start(),
    mongoose_router:start(),
    mongoose_logs:set_global_loglevel(mongoose_config:get_opt(loglevel)),
    mongoose_deprecations:start(),
    {ok, _} = Sup = mongooseim_sup:start_link(),
    mongoose_domain_api:init(),
    mongoose_wpool:ensure_started(),
    mongoose_wpool:start_configured_pools(),
    %% ejabberd_sm is started separately because it may use one of the outgoing_pools
    %% but some outgoing_pools should be started only with ejabberd_sup already running
    ejabberd_sm:start(),
    ejabberd_auth:start(),
    mongoose_cluster_id:start(),
    mongoose_service:start(),
    mongoose_modules:start(),
    service_mongoose_system_metrics:verify_if_configured(),
    mongoose_listener:start(),
    ejabberd_admin:start(),
    mongoose_metrics:init_mongooseim_metrics(),
    ejabberd:update_status_file(started),
    ?LOG_NOTICE(#{what => mongooseim_node_started, version => ?MONGOOSE_VERSION, node => node()}),
    Sup.

%% Internal functions
mnesia_init() ->
    case mnesia:system_info(extra_db_nodes) of
        [] ->
            application:stop(mnesia),
            mnesia:create_schema([node()]),
            application:start(mnesia, permanent);
        _ ->
            ok
    end,
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity).

-spec broadcast_c2s_shutdown_listeners() -> ok.
broadcast_c2s_shutdown_listeners() ->
    Children = supervisor:which_children(mongoose_listener_sup),
    Listeners = [Ref || {Ref, _, _, [mongoose_c2s_listener]} <- Children],
    lists:foreach(
        fun(Listener) ->
            ranch:suspend_listener(Listener),
            [mongoose_c2s:exit(Pid, system_shutdown) || Pid <- ranch:procs(Listener, connections)],
            mongoose_lib:wait_until(
                fun() ->
                    length(ranch:procs(Listener, connections))
                end,
                0)
        end,
        Listeners).

-spec broadcast_c2s_shutdown_sup() -> ok.
broadcast_c2s_shutdown_sup() ->
    Children = supervisor:which_children(mongoose_c2s_sup),
    lists:foreach(
        fun({_, Pid, _, _}) ->
            mongoose_c2s:exit(Pid, system_shutdown)
        end,
        Children),
    mongoose_lib:wait_until(
        fun() ->
              Res = supervisor:count_children(mongoose_c2s_sup),
              proplists:get_value(active, Res)
        end,
        0).
