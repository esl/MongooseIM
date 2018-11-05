-module(mongoose_wpool_rdbms).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4]).
-export([default_opts/0]).
-export([stop/2]).

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

start(Host, Tag, WpoolOpts, RdbmsOpts) ->
    try do_start(Host, Tag, WpoolOpts, RdbmsOpts)
    catch
        Err -> {error, Err}
    end.

default_opts() ->
    [{call_timeout, 60000}].

stop(_, _) ->
    ok.

do_start(Host, Tag, WpoolOpts0, RdbmsOpts) when is_list(WpoolOpts0) and is_list(RdbmsOpts) ->
    BackendName = backend_name(RdbmsOpts),
    try mongoose_rdbms_backend:backend_name() of
        BackendName -> ok;
        OtherBackend ->
            throw(#{reason => "Cannot start an RDBMS connection pool: only one RDBMS backend can be used",
                    opts => RdbmsOpts, new_backend => BackendName, existing_backend => OtherBackend})
    catch
        error:undef ->
            backend_module:create(mongoose_rdbms, BackendName, [query, execute])
    end,

    mongoose_metrics:ensure_db_pool_metric({rdbms, Host, Tag}),

    Worker = {mongoose_rdbms, RdbmsOpts},
    %% Without lists:map dialyzer doesn't understand that WpoolOpts is a list (?) and the
    %% do_start function has no return.
    WpoolOpts = lists:map(fun(X) -> X end, [{worker, Worker}, {pool_sup_shutdown, infinity} | WpoolOpts0]),
    Name = mongoose_wpool:make_pool_name(rdbms, Host, Tag),
    mongoose_wpool:start_sup_pool(rdbms, Name, WpoolOpts).

-spec backend_name(proplist:proplists()) -> odbc | pgsql | mysql.
backend_name(RdbmsOpts) ->
    case lists:keyfind(server, 1, RdbmsOpts) of
        {_, ConnStr} when is_list(ConnStr) -> odbc;
        {_, Tuple} when is_tuple(Tuple) -> element(1, Tuple)
    end.
