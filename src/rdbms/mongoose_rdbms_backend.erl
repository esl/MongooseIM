%%%-------------------------------------------------------------------
%%% @copyright 2021, Erlang Solutions Ltd.
%%% @doc Proxy module for rdbms backends.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_rdbms_backend).
-export([escape_binary/1,
         escape_string/1,
         unescape_binary/1,
         connect/2,
         disconnect/1,
         query/3,
         prepare/5,
         execute/4]).

-define(MAIN_MODULE, mongoose_rdbms).

-type options() :: mongoose_rdbms:options().

-callback escape_binary(binary()) -> mongoose_rdbms:sql_query_part().
-callback escape_string(binary()|list()) -> mongoose_rdbms:sql_query_part().

-callback unescape_binary(binary()) -> binary().
-callback connect(options(), QueryTimeout :: non_neg_integer()) ->
    {ok, Connection :: term()} | {error, Reason :: any()}.
-callback disconnect(Connection :: term()) -> any().
-callback query(Connection :: term(), Query :: any(), Timeout :: infinity | non_neg_integer()) ->
    mongoose_rdbms:query_result().
-callback prepare(Connection :: term(), Name :: atom(),
                  Table :: binary(), Fields :: [binary()], Statement :: iodata()) ->
                     {ok, Ref :: term()} | {error, Reason :: any()}.
-callback execute(Connection :: term(), Ref :: term(), Parameters :: [term()],
                  Timeout :: infinity | non_neg_integer()) -> mongoose_rdbms:query_result().

%% If not defined, generic escaping is used
-optional_callbacks([escape_string/1]).


-spec escape_binary(binary()) -> mongoose_rdbms:sql_query_part().
escape_binary(Binary) ->
    Args = [Binary],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec escape_string(binary() | list()) -> mongoose_rdbms:sql_query_part().
escape_string(String) ->
    Args = [String],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec unescape_binary(binary()) -> binary().
unescape_binary(Binary) ->
    Args = [Binary],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec connect(options(), QueryTimeout :: non_neg_integer()) ->
    {ok, Connection :: term()} | {error, Reason :: any()}.
connect(Options, QueryTimeout) ->
    Args = [Options, QueryTimeout],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec disconnect(Connection :: term()) -> any().
disconnect(Connection) ->
    Args = [Connection],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec query(Connection :: term(), Query :: any(), Timeout :: infinity | non_neg_integer()) ->
    mongoose_rdbms:query_result().
query(Connection, Query, Timeout) ->
    Args = [Connection, Query, Timeout],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec prepare(Connection :: term(), Name :: atom(),
              Table :: binary(), Fields :: [binary()], Statement :: iodata()) ->
                 {ok, Ref :: term()} | {error, Reason :: any()}.
prepare(Connection, Name, Table, Fields, Statement) ->
    Args = [Connection, Name, Table, Fields, Statement],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec execute(Connection :: term(), Ref :: term(), Parameters :: [term()],
              Timeout :: infinity | non_neg_integer()) -> mongoose_rdbms:query_result().
execute(Connection, Ref, Parameters, Timeout) ->
    Args = [Connection, Ref, Parameters, Timeout],
    mongoose_backend:call_tracked(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
