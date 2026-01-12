-module(mongoose_rdbms_cockroachdb).
-author('janusz.jakubiec@erlang-solutions.com').
-behaviour(mongoose_rdbms_backend).

-type options() :: #{host := string(),
                     port := inet:port_number(),
                     database := string(),
                     username := string(),
                     password := string(),
                     atom() => any()}.

-export([escape_binary/1, unescape_binary/1, escape_string/1, connect/2,
         disconnect/1, query/3, prepare/5, execute/4]).

-include("mongoose.hrl").

%% API

-spec escape_string(iolist()) -> iodata().
escape_string(Iolist) ->
    Bin = iolist_to_binary(Iolist),
    [$', binary:replace(Bin, <<"'">>, <<"''">>, [global]), $'].

-spec escape_binary(binary()) -> iodata().
escape_binary(Bin) ->
    mongoose_rdbms_pgsql:escape_binary(Bin).

-spec unescape_binary(binary()) -> binary().
unescape_binary(Bin) ->
    mongoose_rdbms_pgsql:unescape_binary(Bin).

-spec connect(options(), QueryTimeout :: non_neg_integer()) ->
                     {ok, Connection :: term()} | {error, Reason :: any()}.
connect(Options, QueryTimeout) ->
    mongoose_rdbms_pgsql:connect(Options, QueryTimeout).

-spec disconnect(Connection :: epgsql:connection()) -> ok.
disconnect(Connection) ->
    mongoose_rdbms_pgsql:disconnect(Connection).

-spec query(Connection :: term(), Query :: any(),
            Timeout :: infinity | non_neg_integer()) -> mongoose_rdbms:query_result().
query(Connection, Query, Timeout) ->
    mongoose_rdbms_pgsql:query(Connection, Query, Timeout).

-spec prepare(Connection :: term(), Name :: atom(), Table :: binary(),
              Fields :: [binary()], Statement :: iodata()) ->
                     {ok, term()} | {error, any()}.
prepare(Connection, Name, Table, Fields, Statement) ->
    mongoose_rdbms_pgsql:prepare(Connection, Name, Table, Fields, Statement).

-spec execute(Connection :: term(), StatementRef :: term(), Params :: [term()],
              Timeout :: infinity | non_neg_integer()) -> mongoose_rdbms:query_result().
execute(Connection, StatementRef, Params, Timeout) ->
    Result = execute_with_retry(Connection, StatementRef, Params, Timeout, 3, 3, #{}).

do_execute(Connection, StatementRef, Params, Timeout) ->
    mongoose_rdbms_pgsql:execute(Connection, StatementRef, Params, Timeout).

execute_with_retry(Connection, StatementRef, Params, Timeout, MaxRetries, Retries, Info) ->
    case do_execute(Connection, StatementRef, Params, Timeout) of
        {error, "restart transaction" ++ _ = Reason} when Retries > 0 ->
            ?LOG_WARNING(Info#{what => cockroachdb_execute_retrying,
                connection => Connection, statement_ref => StatementRef, statement_params => Params,
                error => error, reason => Reason, retries_left => Retries}),
            mongoose_rdbms_pgsql:query(Connection, "rollback;", 0),
            execute_with_retry(Connection, StatementRef, Params, Timeout, MaxRetries, Retries - 1, Info);
        {error, "restart transaction" ++ _ = Reason} = Result ->
            ?LOG_ERROR(Info#{what => cockroachdb_execute_retrying_failed,
                connection => Connection, statement_ref => StatementRef, statement_params => Params,
                error => error, reason => Reason, retries_left => Retries}),
            Result;
        Other when Retries < MaxRetries ->
            ?LOG_WARNING(Info#{what => cockroachdb_execute_retrying_ok,
                connection => Connection, statement_ref => StatementRef, statement_params => Params,
                retries_left => Retries}),
            Other;
        Other ->
            Other
    end.
