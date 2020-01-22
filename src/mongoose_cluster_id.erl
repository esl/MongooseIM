-module(mongoose_cluster_id).

-include("mongoose.hrl").

-export([
         start/0,
         get_cached_cluster_id/0,
         get_backend_cluster_id/0
        ]).

% For testing purposes only
-export([clean_table/0]).

-record(mongoose_cluster_id, {key :: atom(), value :: cluster_id()}).
-type cluster_id() :: binary().
-type maybe_cluster_id() :: {ok, cluster_id()} | {error, any()}.
-type mongoose_backend() :: rdbms
                          | mnesia.

-spec start() -> maybe_cluster_id().
start() ->
    init_mnesia_cache(),
    run_steps(
      [fun get_cached_cluster_id/0,
       fun get_backend_cluster_id/0,
       fun make_and_set_new_cluster_id/0],
      {error, no_cluster_id_yet}).

-spec run_steps(ListOfFuns, MaybeCID) -> maybe_cluster_id() when
      ListOfFuns :: [fun(() -> maybe_cluster_id())],
      MaybeCID :: maybe_cluster_id().
run_steps(_, {ok, ID}) when is_binary(ID) ->
    store_cluster_id(ID);
run_steps([Fun | NextFuns], {error, _}) ->
    run_steps(NextFuns, Fun());
run_steps([], {error, _} = E) ->
    E.

%% Get cached version
-spec get_cached_cluster_id() -> maybe_cluster_id().
get_cached_cluster_id() ->
    T = fun() -> mnesia:read(mongoose_cluster_id, cluster_id) end,
    case mnesia:transaction(T) of
        {atomic, [#mongoose_cluster_id{value = ClusterID}]} ->
            {ok, ClusterID};
        {atomic, []} ->
            {error, cluster_id_not_in_mnesia};
        {aborted, Reason} ->
            {error, Reason}
    end.

%% ====================================================================
%% Internal getters and setters
%% ====================================================================
-spec get_backend_cluster_id() -> maybe_cluster_id().
get_backend_cluster_id() ->
    get_backend_cluster_id(which_backend_available()).

-spec set_new_cluster_id(cluster_id()) -> maybe_cluster_id().
set_new_cluster_id(ID) ->
    set_new_cluster_id(ID, which_backend_available()).

-spec make_and_set_new_cluster_id() -> maybe_cluster_id().
make_and_set_new_cluster_id() ->
    NewID = make_cluster_id(),
    set_new_cluster_id(NewID).

%% ====================================================================
%% Internal functions
%% ====================================================================
init_mnesia_cache() ->
    mnesia:create_table(mongoose_cluster_id,
                        [{type, set},
                         {record_name, mongoose_cluster_id},
                         {attributes, record_info(fields, mongoose_cluster_id)},
                         {ram_copies, [node()]}
                        ]),
    mnesia:add_table_copy(mongoose_cluster_id, node(), ram_copies).

-spec make_cluster_id() -> cluster_id().
make_cluster_id() ->
    uuid:uuid_to_string(uuid:get_v4(), binary_standard).

%% Which backend is enabled
-spec which_backend_available() -> mongoose_backend().
which_backend_available() ->
    case mongoose_rdbms:db_engine(<<>>) of
        undefined -> mnesia;
        _ -> rdbms
    end.

-spec store_cluster_id(cluster_id()) -> maybe_cluster_id().
store_cluster_id(ID) ->
    which_backend_available() == rdbms andalso set_new_cluster_id(ID, rdbms),
    set_new_cluster_id(ID, mnesia).

-spec set_new_cluster_id(cluster_id(), mongoose_backend()) -> ok | {error, any()}.
set_new_cluster_id(ID, rdbms) ->
    SQLQuery = [<<"INSERT INTO mongoose_cluster_id(k,v) "
                  "VALUES ('cluster_id',">>,
                mongoose_rdbms:use_escaped(mongoose_rdbms:escape_string(ID)), ");"],
    try mongoose_rdbms:sql_query(?MYNAME, SQLQuery) of
        {updated, 1} -> {ok, ID};
        {error, _} = Err -> Err
    catch
        E:R:Stack ->
            ?WARNING_MSG("issue=error_setting_cluster_id_from_rdbms, class=~p, reason=~p, stack=~p",
                         [E, R, Stack]),
            {error, {E,R}}
    end;
set_new_cluster_id(ID, mnesia) ->
    T = fun() -> mnesia:write(#mongoose_cluster_id{key = cluster_id, value = ID}) end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, ID};
        {aborted, Reason} ->
            {error, Reason}
    end.

%% Get cluster ID
-spec get_backend_cluster_id(mongoose_backend()) -> maybe_cluster_id().
get_backend_cluster_id(rdbms) ->
    SQLQuery = [<<"SELECT v FROM mongoose_cluster_id WHERE k='cluster_id'">>],
    try mongoose_rdbms:sql_query(?MYNAME, SQLQuery) of
        {selected, [{Row}]} -> {ok, Row};
        {selected, []} -> {error, no_value_in_backend};
        {error, _} = Err -> Err
    catch
        E:R:Stack ->
            ?WARNING_MSG("issue=error_getting_cluster_id_from_rdbms, class=~p, reason=~p, stack=~p",
                         [E, R, Stack]),
            {error, {E,R}}
    end;
get_backend_cluster_id(mnesia) ->
    get_cached_cluster_id().

clean_table() ->
    clean_table(which_backend_available()).

-spec clean_table(mongoose_backend()) -> ok | {error, any()}.
clean_table(rdbms) ->
    SQLQuery = [<<"TRUNCATE TABLE mongoose_cluster_id;">>],
    try mongoose_rdbms:sql_query(?MYNAME, SQLQuery) of
        {selected, _} -> ok;
        {updated, _} -> ok;
        {error, _} = Err -> Err
    catch
        E:R:Stack ->
            ?WARNING_MSG("issue=error_truncating_cluster_id_from_rdbms,
                         class=~p, reason=~p, stack=~p", [E, R, Stack]),
            {error, {E,R}}
    end;
clean_table(_) -> ok.
