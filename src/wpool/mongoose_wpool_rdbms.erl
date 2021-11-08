-module(mongoose_wpool_rdbms).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4]).
-export([default_opts/0]).
-export([stop/2]).

%% --------------------------------------------------------------
%% mongoose_wpool callbacks
init() ->
    case ets:info(prepared_statements) of
        undefined ->
            Heir = case whereis(ejabberd_sup) of
                       undefined -> [];
                       Pid -> [{heir, Pid, undefined}]
                   end,
            ets:new(prepared_statements,
                    [named_table, public, {read_concurrency, true} | Heir]),
            ok;
        _ ->
            ok
    end.

start(HostType, Tag, WpoolOpts, RdbmsOpts) ->
    try do_start(HostType, Tag, WpoolOpts, RdbmsOpts)
    catch
        Err -> {error, Err}
    end.

default_opts() ->
    [{call_timeout, 60000}].

stop(_, _) ->
    ok.

%% --------------------------------------------------------------
%% Helper functions
do_start(HostType, Tag, WpoolOpts0, RdbmsOpts) when is_list(WpoolOpts0) and is_list(RdbmsOpts) ->
    BackendName = backend_name(RdbmsOpts),
    BackendOpts = RdbmsOpts ++ [{backend, BackendName}],
    mongoose_backend:init(global, mongoose_rdbms, [query, execute], BackendOpts),

    mongoose_metrics:ensure_db_pool_metric({rdbms, HostType, Tag}),
    WpoolOpts = make_wpool_opts(WpoolOpts0, RdbmsOpts),
    ProcName = mongoose_wpool:make_pool_name(rdbms, HostType, Tag),
    mongoose_wpool:start_sup_pool(rdbms, ProcName, WpoolOpts).

make_wpool_opts(WpoolOpts0, RdbmsOpts) ->
    Worker = {mongoose_rdbms, RdbmsOpts},
    [{worker, Worker}, {pool_sup_shutdown, infinity} | WpoolOpts0].

-spec backend_name(proplist:proplists()) -> odbc | pgsql | mysql.
backend_name(RdbmsOpts) ->
    case lists:keyfind(server, 1, RdbmsOpts) of
        {_, ConnStr} when is_list(ConnStr) -> odbc;
        {_, Tuple} when is_tuple(Tuple) -> element(1, Tuple)
    end.
