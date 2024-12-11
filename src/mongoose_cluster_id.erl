-module(mongoose_cluster_id).

-include("mongoose.hrl").

-export([start/0, get_cached_cluster_id/0]).

% For testing purposes only
-export([clean_table/0, clean_cache/0, get_backend_cluster_id/0]).

-ignore_xref([clean_table/0, clean_cache/0, get_backend_cluster_id/0]).

-record(mongoose_cluster_id, {key :: atom(), value :: cluster_id()}).
-type cluster_id() :: binary().
-type maybe_cluster_id() :: {ok, cluster_id()} | {error, any()}.
-type persistent_backend() :: rdbms | {error, none}.
-type volatile_backend() :: mnesia | cets.

-spec start() -> maybe_cluster_id().
start() ->
    %% Consider rewriting this logic, so it does not block the starting process.
    %% Currently, we have to do an SQL query each time we restart MongooseIM
    %% application in the tests.
    init_cache(),
    PersistentBackend = which_persistent_backend_enabled(),
    VolatileBackend = which_volatile_backend_available(),
    maybe_prepare_queries(PersistentBackend),
    mongoose_task:run_tracked(#{task => wait_for_any_backend,
                                backend => PersistentBackend,
                                volatile_backend => VolatileBackend},
                          fun() -> wait_for_any_backend(PersistentBackend, VolatileBackend) end),
    CachedRes = get_cached_cluster_id(VolatileBackend),
    BackendRes = get_backend_cluster_id(PersistentBackend),
    case {CachedRes, BackendRes} of
        {{ok, ID}, {ok, ID}} ->
            {ok, ID};
        {{ok, ID}, {error, _}} ->
            persist_cluster_id(ID, PersistentBackend);
        {{error, _}, {ok, ID}} ->
            cache_cluster_id(ID, VolatileBackend);
        {{error, _}, {error, _}} ->
            make_and_set_new_cluster_id(PersistentBackend, VolatileBackend);
        {{ok, CachedID}, {ok, BackendID}} ->
            ?LOG_ERROR(#{what => cluster_id_setup_conflict,
                         text => <<"Mnesia and Backend have different cluster IDs">>,
                         cached_id => CachedID, backend_id => BackendID}),
            {error, conflict}
    end.


%% If RDBMS is available before CETS - it is enough for us to continue
%% the starting procedure
wait_for_any_backend(PersistentBackend, VolatileBackend) ->
    Alias = erlang:alias([reply]),
    Pids = lists:append([wait_for_backend_promise(B, Alias)
                         || B <- lists:sort([PersistentBackend, VolatileBackend])]),
    wait_for_first_reply(Alias),
    %% Interrupt other waiting calls to reduce the logging noise
    [erlang:exit(Pid, shutdown) || Pid <- Pids],
    clear_pending_replies(Alias),
    ok.

wait_for_first_reply(Alias) ->
    receive
        {ready, Alias} ->
            ok
    end.

clear_pending_replies(Alias) ->
    receive
        {ready, Alias} -> clear_pending_replies(Alias)
    after
        0 -> ok
    end.

wait_for_backend_promise(cets, Alias) ->
    [spawn(fun() ->
            %% We have to do it, because we want to read from across the cluster
            %% in the start/0 function.
            ok = cets_discovery:wait_for_ready(mongoose_cets_discovery, infinity),
            Alias ! {ready, Alias}
        end)];
wait_for_backend_promise(rdbms, Alias) ->
    [spawn(fun() ->
            mongoose_task:run_tracked(#{task => wait_for_rdbms},
                                      fun wait_for_rdbms/0),
            Alias ! {ready, Alias}
        end)];
wait_for_backend_promise(_, Alias) ->
    Alias ! {ready, Alias},
    [].

wait_for_rdbms() ->
    case get_backend_cluster_id(rdbms) of
        {ok, _} ->
            ok;
        _ ->
            timer:sleep(100),
            wait_for_rdbms()
     end.

%% Get cached version
-spec get_cached_cluster_id() -> maybe_cluster_id().
get_cached_cluster_id() ->
    IntBackend = which_volatile_backend_available(),
    get_cached_cluster_id(IntBackend).

get_cached_cluster_id(mnesia) ->
    T = fun() -> mnesia:read(mongoose_cluster_id, cluster_id) end,
    case mnesia:transaction(T) of
        {atomic, [#mongoose_cluster_id{value = ClusterID}]} ->
            {ok, ClusterID};
        {atomic, []} ->
            {error, cluster_id_not_in_cache};
        {aborted, Reason} ->
            {error, Reason}
    end;
get_cached_cluster_id(cets) ->
    case ets:lookup(cets_cluster_id, cluster_id) of
        [{cluster_id, ClusterID}] ->
            {ok, ClusterID};
        [] ->
            {error, cluster_id_not_in_cache}
    end.

%% ====================================================================
%% Internal getters and setters
%% ====================================================================
-spec get_backend_cluster_id() -> maybe_cluster_id().
get_backend_cluster_id() ->
    get_backend_cluster_id(which_persistent_backend_enabled()).

-spec make_and_set_new_cluster_id(persistent_backend(), volatile_backend()) ->
    maybe_cluster_id().
make_and_set_new_cluster_id(PersistentBackend, VolatileBackend) ->
    NewID = make_cluster_id(PersistentBackend),
    persist_cluster_id(NewID, PersistentBackend),
    cache_cluster_id(NewID, VolatileBackend).

%% ====================================================================
%% Internal functions
%% ====================================================================
init_cache() ->
    init_cache(which_volatile_backend_available()).

init_cache(mnesia) ->
    mongoose_mnesia:create_table(mongoose_cluster_id,
                        [{type, set},
                         {record_name, mongoose_cluster_id},
                         {attributes, record_info(fields, mongoose_cluster_id)},
                         {ram_copies, [node()]}
                        ]);
init_cache(cets) ->
    cets:start(cets_cluster_id, #{}),
    cets_discovery:add_table(mongoose_cets_discovery, cets_cluster_id).

-spec maybe_prepare_queries(persistent_backend()) -> any().
maybe_prepare_queries(rdbms) ->
    mongoose_rdbms:prepare(cluster_insert_new, mongoose_cluster_id, [v],
        <<"INSERT INTO mongoose_cluster_id(k,v) VALUES ('cluster_id', ?)">>),
    mongoose_rdbms:prepare(cluster_select, mongoose_cluster_id, [],
        <<"SELECT v FROM mongoose_cluster_id WHERE k='cluster_id'">>);
maybe_prepare_queries(_) ->
    ok.

-spec execute_cluster_insert_new(binary()) -> mongoose_rdbms:query_result().
execute_cluster_insert_new(ID) ->
    mongoose_rdbms:execute_successfully(global, cluster_insert_new, [ID]).

%% If there's no persistent backend, cluster IDs will be recreated on every cluster restart,
%% hence prefix them as ephemeral to re-classify them later.
-spec make_cluster_id(persistent_backend()) -> cluster_id().
make_cluster_id(rdbms) ->
    uuid:uuid_to_string(uuid:get_v4(), binary_standard);
make_cluster_id({error, none}) ->
    <<"ephemeral-", (uuid:uuid_to_string(uuid:get_v4(), binary_standard))/binary>>.

%% Which persistent backend is enabled
-spec which_persistent_backend_enabled() -> persistent_backend().
which_persistent_backend_enabled() ->
    case mongoose_wpool:get_pool_settings(rdbms, global, default) of
        undefined -> {error, none};
        _ -> rdbms
    end.

-spec which_volatile_backend_available() -> volatile_backend().
which_volatile_backend_available() ->
    case mongoose_config:get_opt(internal_databases) of
        #{cets := _} ->
            cets;
        #{mnesia := _} ->
            mnesia
    end.

-spec persist_cluster_id(cluster_id(), persistent_backend()) -> maybe_cluster_id().
persist_cluster_id(ID, rdbms) ->
    try execute_cluster_insert_new(ID) of
        {updated, 1} ->
            {ok, ID}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_WARNING(#{what => cluster_id_set_failed,
                           text => <<"Error inserting cluster ID into RDBMS">>,
                           cluster_id => ID,
                           class => Class, reason => Reason, stacktrace => Stacktrace}),
            {error, {Class, Reason}}
    end;
persist_cluster_id(ID, {error, none}) ->
    {ok, ID}.

-spec cache_cluster_id(cluster_id(), volatile_backend()) -> maybe_cluster_id().
cache_cluster_id(ID, mnesia) ->
    T = fun() -> mnesia:write(#mongoose_cluster_id{key = cluster_id, value = ID}) end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, ID};
        {aborted, Reason} ->
            {error, Reason}
    end;
cache_cluster_id(ID, cets) ->
    cets:insert_serial(cets_cluster_id, {cluster_id, ID}),
    {ok, ID}.

%% Get cluster ID
-spec get_backend_cluster_id(persistent_backend()) -> maybe_cluster_id().
get_backend_cluster_id(rdbms) ->
    try mongoose_rdbms:execute_successfully(global, cluster_select, []) of
        {selected, [{ID}]} -> {ok, ID};
        {selected, []} -> {error, no_value_in_backend}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_WARNING(#{what => cluster_id_get_failed,
                           text => <<"Error getting cluster ID from RDBMS">>,
                           class => Class, reason => Reason, stacktrace => Stacktrace}),
            {error, {Class, Reason}}
    end;
get_backend_cluster_id({error, none}) ->
    {error, no_value_in_backend}.

clean_table() ->
    clean_table(which_persistent_backend_enabled()).

-spec clean_table(persistent_backend()) -> ok | {error, any()}.
clean_table(rdbms) ->
    SQLQuery = [<<"TRUNCATE TABLE mongoose_cluster_id;">>],
    try mongoose_rdbms:sql_query(global, SQLQuery) of
        {selected, _} -> ok;
        {updated, _} -> ok;
        {error, _} = Err -> Err
    catch
        Class:Reason:Stacktrace ->
            ?LOG_WARNING(#{what => cluster_id_clean_failed,
                           text => <<"Error truncating mongoose_cluster_id table">>,
                           sql_query => SQLQuery,
                           class => Class, reason => Reason, stacktrace => Stacktrace}),
            {error, {Class, Reason}}
    end;
clean_table(_) -> ok.

clean_cache() ->
    clean_cache(which_volatile_backend_available()).

clean_cache(mnesia) ->
    mnesia:dirty_delete(mongoose_cluster_id, cluster_id);
clean_cache(cets) ->
    cets:delete(cets_cluster_id, cluster_id).
