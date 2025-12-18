%%%----------------------------------------------------------------------
%%% File    : mongoose_rdbms.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Serve RDBMS connection
%%% Created :  8 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%% Copyright 2016 Erlang Solutions Ltd.
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(mongoose_rdbms).
-author('alexey@process-one.net').
-author('konrad.zemek@erlang-solutions.com').

-behaviour(gen_server).

%% Part of SQL query string, produced by use_escaped/1 function
-type sql_query_part() :: iodata().
-type sql_query() :: iodata().

%% Blob data type to be used inside SQL queries
-opaque escaped_binary() :: {escaped_binary, sql_query_part()}.
%% Unicode string to be used inside SQL queries
-opaque escaped_string() :: {escaped_string, sql_query_part()}.
%% Unicode string to be used inside LIKE conditions
-opaque escaped_like() :: {escaped_like, sql_query_part()}.
-opaque escaped_integer() :: {escaped_integer, sql_query_part()}.
-opaque escaped_boolean() :: {escaped_boolean, sql_query_part()}.
-opaque escaped_null() :: {escaped_null, sql_query_part()}.
-opaque escaped_value() :: escaped_string() | escaped_binary() | escaped_integer() |
                           escaped_boolean() | escaped_null().

-export_type([escaped_binary/0,
              escaped_string/0,
              escaped_like/0,
              escaped_integer/0,
              escaped_boolean/0,
              escaped_null/0,
              escaped_value/0,
              sql_query/0,
              sql_query_part/0]).

-export([process_options/1]).

%% External exports
-export([prepare/4,
         prepared/1,
         execute/3, execute/4,
         execute_cast/3, execute_cast/4,
         execute_request/3, execute_request/4,
         execute_wrapped_request/4, execute_wrapped_request/5,
         execute_successfully/3, execute_successfully/4,
         sql_query/2, sql_query/3,
         sql_query_cast/2, sql_query_cast/3,
         sql_query_request/2, sql_query_request/3,
         sql_transaction/2, sql_transaction/3,
         sql_transaction_request/2, sql_transaction_request/3,
         sql_dirty/2, sql_dirty/3,
         sql_query_t/1,
         transaction_with_delayed_retry/3,
         to_bool/1,
         db_engine/1,
         use_escaped/1]).

%% Unicode escaping
-export([escape_string/1, use_escaped_string/1]).

%% Integer escaping
-export([escape_integer/1, use_escaped_integer/1]).

%% Boolean escaping
-export([escape_boolean/1, use_escaped_boolean/1]).

%% LIKE escaping
-export([escape_like/1, escape_prepared_like/1, escape_like_prefix/1, use_escaped_like/1]).

%% BLOB escaping
-export([escape_binary/2, unescape_binary/2, use_escaped_binary/1]).

%% Null escaping
%% (to keep uniform pattern of passing values)
-export([escape_null/0, use_escaped_null/1]).

%% count / integra types decoding
-export([result_to_integer/1, selected_to_integer/1]).

-export([character_to_integer/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% External exports
-ignore_xref([
              execute/4, execute_cast/3, execute_cast/4,
              execute_request/3, execute_request/4,
              execute_wrapped_request/4, execute_wrapped_request/5,
              execute_successfully/4,
              sql_query/3, sql_query_cast/2, sql_query_cast/3,
              sql_query_request/2, sql_query_request/3,
              sql_transaction/3, sql_transaction_request/2, sql_transaction_request/3,
              sql_dirty/3, sql_query_t/1,
              use_escaped/1,
              escape_like/1, escape_like_prefix/1, use_escaped_like/1,
              escape_binary/2, use_escaped_binary/1,
              escape_integer/1, use_escaped_integer/1,
              escape_string/1, use_escaped_string/1,
              escape_boolean/1, use_escaped_boolean/1,
              escape_null/0, use_escaped_null/1
             ]).

%% internal usage
-export([get_db_info/1]).

-include("mongoose.hrl").

-record(state, {db_ref,
                prepared = #{} :: #{binary() => term()},
                keepalive_interval :: undefined | pos_integer(),
                query_timeout :: pos_integer()
               }).
-type state() :: #state{}.

-define(DEFAULT_POOL_TAG, default).
-define(STATE_KEY, mongoose_rdbms_state).
-define(MAX_TRANSACTION_RESTARTS, 10).
-define(TRANSACTION_TIMEOUT, 60000). % milliseconds
-define(KEEPALIVE_QUERY, <<"SELECT 1;">>).
%% The value is arbitrary; supervisor will restart the connection once
%% the retry counter runs out. We just attempt to reduce log pollution.
-define(CONNECT_RETRIES, 5).

-type query_name() :: atom().
-type query_params() :: [term()].
-type request_wrapper() :: fun((fun(() -> T)) -> T).
-type rdbms_msg() :: {sql_query, _}
                   | {sql_transaction, fun()}
                   | {sql_dirty, fun()}
                   | {sql_execute, atom(), [iodata() | boolean() | integer() | null]}
                   | {sql_execute_wrapped, atom(), [iodata() | boolean() | integer() | null], request_wrapper()}.
-type single_query_result() :: {selected, [tuple()]} |
                               {updated, non_neg_integer() | undefined} |
                               {updated, non_neg_integer(), [tuple()]} |
                               {aborted, Reason :: term()} |
                               {error, Reason :: string() | duplicate_key}.
-type query_result() :: single_query_result() | [single_query_result()].
-type transaction_result() :: {aborted, _} | {atomic, _} | {error, _}.
-type dirty_result() :: {ok, any()} | {error, any()}.
-export_type([query_name/0, query_result/0, transaction_result/0]).

-type backend() :: pgsql | mysql | cockroachdb.
-type options() :: #{driver := backend(),
                     max_start_interval := pos_integer(),
                     query_timeout := pos_integer(),
                     host := nonempty_string(),
                     database := nonempty_string(),
                     username := nonempty_string(),
                     password := nonempty_string(),
                     port := inet:port_number(),
                     atom() => any()}.

-export_type([options/0, backend/0]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec process_options(map()) -> options().
process_options(Opts = #{host := _Host, database := _DB, username := _User, password := _Pass}) ->
    ensure_db_port(process_tls_options(Opts));
process_options(Opts) ->
    error(#{what => invalid_rdbms_connection_options, options => Opts}).

process_tls_options(Opts = #{driver := mysql, tls := #{required := _}}) ->
    error(#{what => invalid_rdbms_tls_options, options => Opts,
            text => <<"The 'required' option is not supported for MySQL">>});
process_tls_options(Opts = #{driver := Driver, tls := TLSOpts}) when Driver =:= pgsql;
                                                                     Driver =:= cockroachdb ->
    Opts#{tls := maps:merge(#{required => false}, TLSOpts)};
process_tls_options(Opts) ->
    Opts.

ensure_db_port(Opts = #{port := _}) -> Opts;
ensure_db_port(Opts = #{driver := pgsql}) -> Opts#{port => 5432};
ensure_db_port(Opts = #{driver := cockroachdb}) -> Opts#{port => 26257};
ensure_db_port(Opts = #{driver := mysql}) -> Opts#{port => 3306}.

-spec prepare(
        query_name(), Table :: binary() | atom(), Fields :: [binary() | atom()], Statement :: iodata()) ->
    {ok, query_name()} | {error, already_exists}.
prepare(Name, Table, Fields, Statement) when is_atom(Table) ->
    prepare(Name, atom_to_binary(Table, utf8), Fields, Statement);
prepare(Name, Table, [Field | _] = Fields, Statement) when is_atom(Field) ->
    prepare(Name, Table, [atom_to_binary(F, utf8) || F <- Fields], Statement);
prepare(Name, Table, Fields, Statement) when is_atom(Name), is_binary(Table) ->
    true = lists:all(fun is_binary/1, Fields),
    Tuple = {Name, Table, Fields, iolist_to_binary(Statement)},
    case ets:insert_new(prepared_statements, Tuple) of
        true  -> {ok, Name};
        false -> {error, already_exists}
    end.

-spec prepared(atom()) -> boolean().
prepared(Name) ->
    ets:member(prepared_statements, Name).

-spec execute(mongooseim:host_type_or_global(), query_name(), query_params()) -> query_result().
execute(HostType, Name, Parameters) ->
    execute(HostType, ?DEFAULT_POOL_TAG, Name, Parameters).

-spec execute(mongooseim:host_type_or_global(), mongoose_wpool:tag(), query_name(), query_params()) ->
    query_result().
execute(HostType, PoolTag, Name, Parameters) when is_atom(PoolTag), is_atom(Name), is_list(Parameters) ->
    sql_call(HostType, PoolTag, {sql_execute, Name, Parameters}).

-spec execute_cast(mongooseim:host_type_or_global(), query_name(), query_params()) -> query_result().
execute_cast(HostType, Name, Parameters) ->
    execute_cast(HostType, ?DEFAULT_POOL_TAG, Name, Parameters).

-spec execute_cast(mongooseim:host_type_or_global(), mongoose_wpool:tag(), query_name(), query_params()) ->
    query_result().
execute_cast(HostType, PoolTag, Name, Parameters) when is_atom(PoolTag), is_atom(Name), is_list(Parameters) ->
    sql_cast(HostType, PoolTag, {sql_execute, Name, Parameters}).

-spec execute_request(mongooseim:host_type_or_global(), query_name(), query_params()) ->
    gen_server:request_id().
execute_request(HostType, Name, Parameters) when is_atom(Name), is_list(Parameters) ->
    execute_request(HostType, ?DEFAULT_POOL_TAG, Name, Parameters).

-spec execute_request(mongooseim:host_type_or_global(), mongoose_wpool:tag(), query_name(), query_params()) ->
    gen_server:request_id().
execute_request(HostType, PoolTag, Name, Parameters) when is_atom(PoolTag), is_atom(Name), is_list(Parameters) ->
    sql_request(HostType, PoolTag, {sql_execute, Name, Parameters}).

-spec execute_wrapped_request(mongooseim:host_type_or_global(), query_name(), query_params(), request_wrapper()) ->
    gen_server:request_id().
execute_wrapped_request(HostType, Name, Parameters, Wrapper) ->
    execute_wrapped_request(HostType, ?DEFAULT_POOL_TAG, Name, Parameters, Wrapper).

-spec execute_wrapped_request(
        mongooseim:host_type_or_global(), mongoose_wpool:tag(), query_name(), query_params(), request_wrapper()) ->
    gen_server:request_id().
execute_wrapped_request(HostType, PoolTag, Name, Parameters, Wrapper)
  when is_atom(PoolTag), is_atom(Name), is_list(Parameters), is_function(Wrapper) ->
    sql_request(HostType, PoolTag, {sql_execute_wrapped, Name, Parameters, Wrapper}).

%% Same as execute/3, but would fail loudly on any error.
-spec execute_successfully(mongooseim:host_type_or_global(), query_name(), query_params()) ->
    query_result().
execute_successfully(HostType, Name, Parameters) ->
    execute_successfully(HostType, ?DEFAULT_POOL_TAG, Name, Parameters).

-spec execute_successfully(mongooseim:host_type_or_global(), mongoose_wpool:tag(), query_name(), query_params()) ->
    query_result().
execute_successfully(HostType, PoolTag, Name, Parameters) ->
    try execute(HostType, PoolTag, Name, Parameters) of
        {selected, _} = Result ->
            Result;
        {updated, _} = Result ->
            Result;
        Other ->
            Log = #{what => sql_execute_failed, host => HostType, statement_name => Name,
                    statement_query => query_name_to_string(Name),
                    statement_params => Parameters, reason => Other},
            ?LOG_ERROR(Log),
            error(Log)
    catch error:Reason:Stacktrace ->
            Log = #{what => sql_execute_failed, host => HostType, statement_name => Name,
                    statement_query => query_name_to_string(Name),
                    statement_params => Parameters,
                    reason => Reason, stacktrace => Stacktrace},
            ?LOG_ERROR(Log),
            erlang:raise(error, Reason, Stacktrace)
    end.

query_name_to_string(Name) ->
    case ets:lookup(prepared_statements, Name) of
        [] ->
            not_found;
        [{_, _Table, _Fields, Statement}] ->
            Statement
    end.

-spec sql_query(mongooseim:host_type_or_global(), Query :: any()) -> query_result().
sql_query(HostType, Query) ->
    sql_query(HostType, ?DEFAULT_POOL_TAG, Query).

-spec sql_query(mongooseim:host_type_or_global(), mongoose_wpool:tag(), Query :: any()) ->
    query_result().
sql_query(HostType, PoolTag, Query) ->
    sql_call(HostType, PoolTag, {sql_query, Query}).

-spec sql_query_request(mongooseim:host_type_or_global(), Query :: any()) ->
    gen_server:request_id().
sql_query_request(HostType, Query) ->
    sql_query_request(HostType, ?DEFAULT_POOL_TAG, Query).

-spec sql_query_request(mongooseim:host_type_or_global(), mongoose_wpool:tag(), Query :: any()) ->
    gen_server:request_id().
sql_query_request(HostType, PoolTag, Query) ->
    sql_request(HostType, PoolTag, {sql_query, Query}).

-spec sql_query_cast(mongooseim:host_type_or_global(), Query :: any()) -> query_result().
sql_query_cast(HostType, Query) ->
    sql_query_cast(HostType, ?DEFAULT_POOL_TAG, Query).

-spec sql_query_cast(mongooseim:host_type_or_global(), mongoose_wpool:tag(), Query :: any()) ->
    query_result().
sql_query_cast(HostType, PoolTag, Query) ->
    sql_cast(HostType, PoolTag, {sql_query, Query}).

%% @doc SQL transaction based on a list of queries
-spec sql_transaction(mongooseim:host_type_or_global(), fun() | maybe_improper_list()) ->
    transaction_result().
sql_transaction(HostType, Msg) ->
    sql_transaction(HostType, ?DEFAULT_POOL_TAG, Msg).

-spec sql_transaction(mongooseim:host_type_or_global(), atom(), fun() | maybe_improper_list()) ->
    transaction_result().
sql_transaction(HostType, PoolTag, Queries) when is_atom(PoolTag), is_list(Queries) ->
    F = fun() -> lists:map(fun sql_query_t/1, Queries) end,
    sql_transaction(HostType, PoolTag, F);
%% SQL transaction, based on a erlang anonymous function (F = fun)
sql_transaction(HostType, PoolTag, F) when is_atom(PoolTag), is_function(F) ->
    sql_call(HostType, PoolTag, {sql_transaction, F}).

%% @doc SQL transaction based on a list of queries
-spec sql_transaction_request(mongooseim:host_type_or_global(), fun() | maybe_improper_list()) ->
    gen_server:request_id().
sql_transaction_request(HostType, Queries) ->
    sql_transaction_request(HostType, ?DEFAULT_POOL_TAG, Queries).

-spec sql_transaction_request(mongooseim:host_type_or_global(), atom(), fun() | maybe_improper_list()) ->
    gen_server:request_id().
sql_transaction_request(HostType, PoolTag, Queries) when is_atom(PoolTag), is_list(Queries) ->
    F = fun() -> lists:map(fun sql_query_t/1, Queries) end,
    sql_transaction_request(HostType, PoolTag, F);
%% SQL transaction, based on a erlang anonymous function (F = fun)
sql_transaction_request(HostType, PoolTag, F) when is_atom(PoolTag), is_function(F) ->
    sql_request(HostType, PoolTag, {sql_transaction, F}).

%% This function allows to specify delay between retries.
-spec transaction_with_delayed_retry(mongooseim:host_type_or_global(), fun() | maybe_improper_list(), map()) ->
    transaction_result().
transaction_with_delayed_retry(HostType, F, Info) ->
    Retries = maps:get(retries, Info),
    Delay = maps:get(delay, Info),
    do_transaction_with_delayed_retry(HostType, F, Retries, Delay, Info).

do_transaction_with_delayed_retry(HostType, F, Retries, Delay, Info) ->
    Result = mongoose_rdbms:sql_transaction(HostType, F),
    case Result of
        {atomic, _} ->
            Result;
        {aborted, Reason} when Retries > 0 ->
            ?LOG_WARNING(Info#{what => rdbms_transaction_aborted,
                               text => <<"Transaction aborted. Restart">>,
                               reason => Reason, retries_left => Retries}),
            timer:sleep(Delay),
            do_transaction_with_delayed_retry(HostType, F, Retries - 1, Delay, Info);
        _ ->
            Err = Info#{what => mam_transaction_failed,
                        text => <<"Transaction failed. Do not restart">>,
                        reason => Result},
            ?LOG_ERROR(Err),
            erlang:error(Err)
    end.

-spec sql_dirty(mongooseim:host_type_or_global(), fun()) -> any() | no_return().
sql_dirty(HostType, F) ->
    sql_dirty(HostType, ?DEFAULT_POOL_TAG, F).

-spec sql_dirty(mongooseim:host_type_or_global(), atom(), fun()) -> any() | no_return().
sql_dirty(HostType, PoolTag, F) when is_function(F) ->
    case sql_call(HostType, PoolTag, {sql_dirty, F}) of
        {ok, Result} -> Result;
        {error, Error} -> throw(Error)
    end.

%% TODO: Better spec for RPC calls
-spec sql_call(mongooseim:host_type_or_global(), mongoose_wpool:tag(), Msg :: rdbms_msg()) -> any().
sql_call(HostType, PoolTag, Msg) ->
    case get_state() of
        undefined -> sql_call0(HostType, PoolTag, Msg);
        State     ->
            {Res, NewState} = nested_op(Msg, State),
            put_state(NewState),
            Res
    end.

-spec sql_call0(mongooseim:host_type_or_global(), mongoose_wpool:tag(), Msg :: rdbms_msg()) -> any().
sql_call0(HostType, PoolTag, Msg) ->
    Timestamp = erlang:monotonic_time(millisecond),
    mongoose_wpool:call(rdbms, HostType, PoolTag, {sql_cmd, Msg, Timestamp}).

-spec sql_request(mongooseim:host_type_or_global(), mongoose_wpool:tag(), Msg :: rdbms_msg()) -> any().
sql_request(HostType, PoolTag, Msg) ->
    case get_state() of
        undefined -> sql_request0(HostType, PoolTag, Msg);
        State ->
            {Res, NewState} = nested_op(Msg, State),
            put_state(NewState),
            Res
    end.

-spec sql_request0(mongooseim:host_type_or_global(), mongoose_wpool:tag(), Msg :: rdbms_msg()) -> any().
sql_request0(HostType, PoolTag, Msg) ->
    Timestamp = erlang:monotonic_time(millisecond),
    mongoose_wpool:send_request(rdbms, HostType, PoolTag, {sql_cmd, Msg, Timestamp}).

-spec sql_cast(mongooseim:host_type_or_global(), mongoose_wpool:tag(), Msg :: rdbms_msg()) -> any().
sql_cast(HostType, PoolTag, Msg) ->
    case get_state() of
        undefined -> sql_cast0(HostType, PoolTag, Msg);
        State ->
            {Res, NewState} = nested_op(Msg, State),
            put_state(NewState),
            Res
    end.

-spec sql_cast0(mongooseim:host_type_or_global(), mongoose_wpool:tag(), Msg :: rdbms_msg()) -> any().
sql_cast0(HostType, PoolTag, Msg) ->
    Timestamp = erlang:monotonic_time(millisecond),
    mongoose_wpool:cast(rdbms, HostType, PoolTag, {sql_cmd, Msg, Timestamp}).

-spec get_db_info(Target :: mongooseim:host_type_or_global() | pid()) ->
                         {ok, DbType :: atom(), DbRef :: term()} | {error, any()}.
get_db_info(Pid) when is_pid(Pid) ->
    wpool_process:call(Pid, get_db_info, 5000);
get_db_info(HostType) ->
    mongoose_wpool:call(rdbms, HostType, get_db_info).

%% This function is intended to be used from inside an sql_transaction:
sql_query_t(Query) ->
    sql_query_t(Query, get_state()).

sql_query_t(Query, State) ->
    QRes = sql_query_internal(Query, State),
    case QRes of
        {error, Reason} ->
            throw({aborted, #{reason => Reason, sql_query => Query}});
        _ when is_list(QRes) ->
            case lists:keysearch(error, 1, QRes) of
                {value, {error, Reason}} ->
                    throw({aborted, #{reason => Reason, sql_query => Query}});
                _ ->
                    QRes
            end;
        _ ->
            QRes
    end.

%% Only for binaries, escapes only the content, '%' has to be added afterwards
%% The escape character is '$' as '\' causes issues in PostgreSQL
%% Returned value is NOT safe to use outside of a prepared statement
-spec escape_prepared_like(binary()) -> binary().
escape_prepared_like(S) ->
    << (escape_prepared_like_character(C)) || <<C>> <= S >>.

%% @doc Escape character that will confuse an SQL engine
%% Percent and underscore only need to be escaped for
%% pattern matching like statement
%% INFO: Used in mod_vcard_rdbms.
%% Searches in the middle of text, non-efficient
-spec escape_like(binary() | string()) -> escaped_like().
escape_like(S) ->
    {escaped_like, [$', $%, escape_like_internal(S), $%, $']}.

-spec escape_like_prefix(binary() | string()) -> escaped_like().
escape_like_prefix(S) ->
    {escaped_like, [$', escape_like_internal(S), $%, $']}.

-spec escape_binary(mongooseim:host_type_or_global(), binary()) -> escaped_binary().
escape_binary(_HostType, Bin) when is_binary(Bin) ->
    {escaped_binary, mongoose_rdbms_backend:escape_binary(Bin)}.

%% @doc The same as escape, but returns value including ''
-spec escape_string(binary() | string()) -> escaped_string().
escape_string(S) ->
    {escaped_string, escape_string_internal(S)}.

-spec escape_integer(integer()) -> escaped_integer().
escape_integer(I) when is_integer(I) ->
    {escaped_integer, integer_to_binary(I)}.

%% Be aware, that we can't just use escaped_integer here.
%% Because of the error in pgsql:
%% column \"match_all\" is of type boolean but expression is of type integer
-spec escape_boolean(boolean()) -> escaped_boolean().
escape_boolean(true) ->
    {escaped_boolean, "'1'"};
escape_boolean(false) ->
    {escaped_boolean, "'0'"}.

-spec escape_null() -> escaped_null().
escape_null() ->
    {escaped_null, "null"}.


%% @doc SQL-injection check.
%% Call this function just before using value from escape_string/1 inside a query.
-spec use_escaped_string(escaped_string()) -> sql_query_part().
use_escaped_string({escaped_string, S}) ->
    S;
use_escaped_string(X) ->
    %% We need to print an error, because in some places
    %% the error can be just ignored, because of too wide catches.
    ?LOG_ERROR(#{what => rdbms_use_escaped_failure, value => X,
        stacktrace => erlang:process_info(self(), current_stacktrace)}),
    erlang:error({use_escaped_string, X}).

-spec use_escaped_binary(escaped_binary()) -> sql_query_part().
use_escaped_binary({escaped_binary, S}) ->
    S;
use_escaped_binary(X) ->
    ?LOG_ERROR(#{what => rdbms_use_escaped_failure, value => X,
        stacktrace => erlang:process_info(self(), current_stacktrace)}),
    erlang:error({use_escaped_binary, X}).

-spec use_escaped_like(escaped_like()) -> sql_query_part().
use_escaped_like({escaped_like, S}) ->
    S;
use_escaped_like(X) ->
    ?LOG_ERROR(#{what => rdbms_use_escaped_failure, value => X,
        stacktrace => erlang:process_info(self(), current_stacktrace)}),
    erlang:error({use_escaped_like, X}).

-spec use_escaped_integer(escaped_integer()) -> sql_query_part().
use_escaped_integer({escaped_integer, S}) ->
    S;
use_escaped_integer(X) ->
    ?LOG_ERROR(#{what => rdbms_use_escaped_failure, value => X,
        stacktrace => erlang:process_info(self(), current_stacktrace)}),
    erlang:error({use_escaped_integer, X}).

-spec use_escaped_boolean(escaped_boolean()) -> sql_query_part().
use_escaped_boolean({escaped_boolean, S}) ->
    S;
use_escaped_boolean(X) ->
    ?LOG_ERROR(#{what => rdbms_use_escaped_failure, value => X,
        stacktrace => erlang:process_info(self(), current_stacktrace)}),
    erlang:error({use_escaped_boolean, X}).

-spec use_escaped_null(escaped_null()) -> sql_query_part().
use_escaped_null({escaped_null, S}) ->
    S;
use_escaped_null(X) ->
    ?LOG_ERROR(#{what => rdbms_use_escaped_failure, value => X,
        stacktrace => erlang:process_info(self(), current_stacktrace)}),
    erlang:error({use_escaped_null, X}).

%% Use this function, if type is unknown.
%% Be aware, you can't pass escaped_like() there.
-spec use_escaped(Value) -> sql_query_part() when
      Value :: escaped_value().
use_escaped({escaped_string, _}=X) ->
    use_escaped_string(X);
use_escaped({escaped_binary, _}=X) ->
    use_escaped_binary(X);
use_escaped({escaped_integer, _}=X) ->
    use_escaped_integer(X);
use_escaped({escaped_boolean, _}=X) ->
    use_escaped_boolean(X);
use_escaped({escaped_null, _}=X) ->
    use_escaped_null(X);
use_escaped(X) ->
    ?LOG_ERROR(#{what => rdbms_use_escaped_failure, value => X,
        stacktrace => erlang:process_info(self(), current_stacktrace)}),
    erlang:error({use_escaped, X}).

-spec escape_prepared_like_character(char()) -> binary().
escape_prepared_like_character($%) -> <<"$%">>;
escape_prepared_like_character($_) -> <<"$_">>;
escape_prepared_like_character($$) -> <<"$$">>;
escape_prepared_like_character(C) -> <<C>>.

-spec escape_like_internal(binary() | string()) -> binary() | string().
escape_like_internal(S) when is_binary(S) ->
    list_to_binary(escape_like_internal(binary_to_list(S)));
escape_like_internal(S) when is_list(S) ->
    [escape_like_character(C) || C <- S].

escape_string_internal(S) ->
    case mongoose_backend:is_exported(global, ?MODULE, escape_string, 1) of
        true ->
            mongoose_rdbms_backend:escape_string(S);
        false ->
            %% generic escaping
            [$', escape_characters(S), $']
    end.

escape_characters(S) when is_binary(S) ->
    list_to_binary(escape_characters(binary_to_list(S)));
escape_characters(S) when is_list(S) ->
    [escape_character(C) || C <- S].

escape_like_character($%) -> "\\%";
escape_like_character($_) -> "\\_";
escape_like_character(C)  -> escape_character(C).

%% Characters to escape
escape_character($\0) -> "\\0";
escape_character($\n) -> "\\n";
escape_character($\t) -> "\\t";
escape_character($\b) -> "\\b";
escape_character($\r) -> "\\r";
escape_character($')  -> "''";
escape_character($")  -> "\\\"";
escape_character($\\) -> "\\\\";
escape_character(C)   -> C.


-spec unescape_binary(mongooseim:host_type_or_global(), binary()) -> binary().
unescape_binary(_HostType, Bin) when is_binary(Bin) ->
    mongoose_rdbms_backend:unescape_binary(Bin).


-spec result_to_integer(binary() | integer()) -> integer().
result_to_integer(Int) when is_integer(Int) ->
    Int;
result_to_integer(Bin) when is_binary(Bin) ->
    binary_to_integer(Bin).

selected_to_integer({selected, [{BInt}]}) ->
    result_to_integer(BInt).

%% Converts a value from a CHAR(1) field to integer
character_to_integer(<<X>>) -> X;
character_to_integer(X) when is_integer(X) -> X.

%% pgsql returns booleans as "t" or "f"
-spec to_bool(binary() | string() | atom() | integer() | any()) -> boolean().
to_bool(B) when is_binary(B) ->
    to_bool(binary_to_list(B));
to_bool("t") -> true;
to_bool("true") -> true;
to_bool("1") -> true;
to_bool(true) -> true;
to_bool(1) -> true;
to_bool(_) -> false.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------
-spec init(options()) -> {ok, state()}.
init(Opts = #{query_timeout := QueryTimeout, max_start_interval := MaxStartInterval}) ->
    process_flag(trap_exit, true),
    KeepaliveInterval = maps:get(keepalive_interval, Opts, undefined),
    % retries are delayed exponentially, max_start_interval limits the delay
    % e.g. if the limit is 30, the delays are: 2, 4, 8, 16, 30, 30, ...
    case connect(Opts, ?CONNECT_RETRIES, 2, MaxStartInterval) of
        {ok, DbRef} ->
            schedule_keepalive(KeepaliveInterval),
            {ok, #state{db_ref = DbRef,
                        keepalive_interval = KeepaliveInterval,
                        query_timeout = QueryTimeout}};
        Error ->
            {stop, Error}
    end.


handle_call({sql_cmd, Command, Timestamp}, From, State) ->
    {Result, NewState} = run_sql_cmd(Command, From, State, Timestamp),
    case abort_on_driver_error(Result) of
        {stop, Reason} -> {stop, Reason, Result, NewState};
        continue -> {reply, Result, NewState}
    end;
handle_call(get_db_info, _, #state{db_ref = DbRef} = State) ->
    {reply, {ok, db_engine(global), DbRef}, State};
handle_call(Request, From, State) ->
    ?UNEXPECTED_CALL(Request, From),
    {reply, {error, badarg}, State}.

handle_cast({sql_cmd, Command, Timestamp}, State) ->
    {Result, NewState} = run_sql_cmd(Command, undefined, State, Timestamp),
    case abort_on_driver_error(Result) of
        {stop, Reason} -> {stop, Reason, NewState};
        continue -> {noreply, NewState}
    end;
handle_cast(Request, State) ->
    ?UNEXPECTED_CAST(Request),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(keepalive, #state{keepalive_interval = KeepaliveInterval} = State) ->
    case sql_query_internal([?KEEPALIVE_QUERY], State) of
        {selected, _} ->
            schedule_keepalive(KeepaliveInterval),
            {noreply, State};
        {error, _} = Error ->
            {stop, {keepalive_failed, Error}, State}
    end;
handle_info({'EXIT', _Pid, _Reason} = Reason, State) ->
    {stop, Reason, State};
handle_info(Info, State) ->
    ?UNEXPECTED_INFO(Info),
    {noreply, State}.

-spec terminate(Reason :: term(), state()) -> any().
terminate(_Reason, #state{db_ref = DbRef}) ->
    catch mongoose_rdbms_backend:disconnect(DbRef).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

-spec run_sql_cmd(Command :: any(), From :: any(), State :: state(), Timestamp :: integer()) ->
    {Result :: term(), state()}.
run_sql_cmd(Command, _From, State, Timestamp) ->
    Now = erlang:monotonic_time(millisecond),
    case Now - Timestamp of
        Age when Age  < ?TRANSACTION_TIMEOUT ->
            outer_op(Command, State);
        Age ->
            ?LOG_ERROR(#{what => rdbms_db_not_available_or_too_slow,
                         text => <<"Discarding request">>, age => Age, command => Command}),
            {reply, {error, timeout}, State}
    end.

%% @doc Only called by handle_call, only handles top level operations.
-spec outer_op(rdbms_msg(), state()) -> {query_result()
                                         | transaction_result()
                                         | dirty_result(), state()}.
outer_op({sql_query, Query}, State) ->
    {sql_query_internal(Query, State), State};
outer_op({sql_transaction, F}, State) ->
    outer_transaction(F, ?MAX_TRANSACTION_RESTARTS, "", State);
outer_op({sql_dirty, F}, State) ->
    sql_dirty_internal(F, State);
outer_op({sql_execute, Name, Params}, State) ->
    sql_execute(outer_op, Name, Params, State);
outer_op({sql_execute_wrapped, Name, Params, Wrapper}, State) ->
    try
        Wrapper(fun() -> sql_execute(outer_op, Name, Params, State) end)
    catch
        _Class:Error ->
            ?LOG_ERROR(#{what => sql_execute_wrapped_failed, reason => Error,
                         statement_name => Name, wrapper_fun => Wrapper}),
            {{error, Error}, State}
    end.

%% @doc Called via sql_query/transaction/bloc from client code when inside a
%% nested operation
-spec nested_op(rdbms_msg(), state()) -> any().
nested_op({sql_query, Query}, State) ->
    %% XXX - use sql_query_t here insted? Most likely would break
    %% callers who expect {error, _} tuples (sql_query_t turns
    %% these into throws)
    {sql_query_internal(Query, State), State};
nested_op({sql_transaction, F}, State) ->
    %% Transaction inside a transaction
    inner_transaction(F, State);
nested_op({sql_execute, Name, Params}, State) ->
    sql_execute(nested_op, Name, Params, State);
nested_op({sql_execute_wrapped, Name, Params, Wrapper}, State) ->
    Wrapper(fun() -> sql_execute(nested_op, Name, Params, State) end).

%% @doc Never retry nested transactions - only outer transactions
-spec inner_transaction(fun(), state()) -> transaction_result() | {'EXIT', any()}.
inner_transaction(F, _State) ->
    case catch F() of
        {aborted, Reason} ->
            {aborted, Reason};
        {'EXIT', Reason} ->
            {'EXIT', Reason};
        {atomic, Res} ->
            {atomic, Res};
        Res ->
            {atomic, Res}
    end.

-spec outer_transaction(F :: fun(),
                        NRestarts :: 0..10,
                        Reason :: any(), state()) -> {transaction_result(), state()}.
outer_transaction(F, NRestarts, _Reason, State) ->
    sql_query_internal(rdbms_queries:begin_trans(), State),
    put_state(State),
    {Result, StackTrace} = apply_transaction_function(F),
    NewState = erase_state(),
    case Result of
        {aborted, Reason} when NRestarts > 0 ->
            %% Retry outer transaction upto NRestarts times.
            sql_query_internal([<<"rollback;">>], NewState),
            outer_transaction(F, NRestarts - 1, Reason, NewState);
        {aborted, #{reason := Reason, sql_query := SqlQuery}}
            when NRestarts =:= 0 ->
            %% Too many retries of outer transaction.
            ?LOG_ERROR(#{what => rdbms_sql_transaction_restarts_exceeded,
                restarts => ?MAX_TRANSACTION_RESTARTS, last_abort_reason => Reason,
                last_sql_query => iolist_to_binary(SqlQuery),
                stacktrace => StackTrace, state => NewState}),
            sql_query_internal([<<"rollback;">>], NewState),
            {{aborted, Reason}, NewState};
        {aborted, Reason} when NRestarts =:= 0 -> %% old format for abort
            %% Too many retries of outer transaction.
            ?LOG_ERROR(#{what => rdbms_sql_transaction_restarts_exceeded,
                restarts => ?MAX_TRANSACTION_RESTARTS,
                last_abort_reason => Reason, stacktrace => StackTrace,
                state => NewState}),
            sql_query_internal([<<"rollback;">>], NewState),
            {{aborted, Reason}, NewState};
        {'EXIT', Reason} ->
            %% Abort sql transaction on EXIT from outer txn only.
            sql_query_internal([<<"rollback;">>], NewState),
            {{aborted, Reason}, NewState};
        Res ->
            %% Commit successful outer txn
            sql_query_internal([<<"commit;">>], NewState),
            {{atomic, Res}, NewState}
    end.

-spec apply_transaction_function(F :: fun()) -> {any(), list()}.
apply_transaction_function(F) ->
    try
        {F(), []}
    catch
        throw:ThrowResult:StackTrace ->
            {ThrowResult, StackTrace};
        Class:Reason:StackTrace ->
            ?LOG_ERROR(#{what => rdbms_outer_transaction_failed, class => Class,
                reason => Reason, stacktrace => StackTrace}),
            {{'EXIT', Reason}, StackTrace}
    end.

sql_query_internal(Query, #state{db_ref = DBRef, query_timeout = QueryTimeout}) ->
    mongoose_rdbms_backend:query(DBRef, Query, QueryTimeout).

sql_dirty_internal(F, State) ->
    put_state(State),
    Result =
    try F() of
        Result0 ->
            {ok, Result0}
    catch
        _C:R ->
            {error, R}
    end,
    {Result, erase_state()}.

-spec sql_execute(Type :: atom(), query_name(), query_params(), state()) ->
    {query_result(), state()}.
sql_execute(Type, Name, Params, State = #state{db_ref = DBRef, query_timeout = QueryTimeout}) ->
    %% Postgres allows to prepare statement only once, so we should take care that NewState is updated
    {StatementRef, NewState} = prepare_statement(Name, State),
    put_state(NewState),
    Res = try mongoose_rdbms_backend:execute(DBRef, StatementRef, Params, QueryTimeout)
          catch Class:Reason:StackTrace ->
            ?LOG_ERROR(#{what => rdbms_sql_execute_failed, statement_name => Name,
                class => Class, reason => Reason, params => Params,
                stacktrace => StackTrace}),
            erlang:raise(Class, Reason, StackTrace)
          end,
    check_execute_result(Type, Res, Name, Params),
    {Res, NewState}.

%% Similar check as in sql_query_t
check_execute_result(outer_op, _Res, _Name, _Params) ->
    ok;
check_execute_result(nested_op, Res, Name, Params) ->
    %% Res is not a list (i.e. executes are one query only and one result set only)
    case Res of
        {error, Reason} ->
            throw({aborted, #{reason => Reason, statement_name => Name, params => Params}});
        _ when is_tuple(Res) ->
            ok
    end.

-spec prepare_statement(query_name(), state()) -> {Ref :: term(), state()}.
prepare_statement(Name, State = #state{db_ref = DBRef, prepared = Prepared}) ->
    case maps:get(Name, Prepared, undefined) of
        undefined ->
            {_, Table, Fields, Statement} = lookup_statement(Name),
            {ok, Ref} = mongoose_rdbms_backend:prepare(DBRef, Name, Table, Fields, Statement),
            {Ref, State#state{prepared = maps:put(Name, Ref, Prepared)}};

        Ref ->
            {Ref, State}
    end.

lookup_statement(Name) ->
    case ets:lookup(prepared_statements, Name) of
        [Rec] -> Rec;
        [] -> error({lookup_statement_failed, Name})
    end.

-spec abort_on_driver_error(_) -> continue | {stop, timeout | closed}.
abort_on_driver_error({error, "query timed out"}) -> %% mysql driver error
    {stop, timeout};
abort_on_driver_error({error, "Failed sending data on socket" ++ _}) -> %% mysql driver error
    {stop, closed};
abort_on_driver_error(_) ->
    continue.

-spec db_engine(mongooseim:host_type_or_global()) -> backend() | undefined.
db_engine(_HostType) ->
    try mongoose_backend:get_backend_name(global, ?MODULE)
    catch error:badarg -> undefined end.

-spec connect(options(), Retry :: non_neg_integer(), RetryAfter :: non_neg_integer(),
              MaxRetryDelay :: non_neg_integer()) -> {ok, term()} | {error, any()}.
connect(#{query_timeout := QueryTimeout} = Options, Retry, RetryAfter, MaxRetryDelay) ->
    case mongoose_rdbms_backend:connect(Options, QueryTimeout) of
        {ok, _} = Ok ->
            Ok;
        Error when Retry =:= 0 ->
            Error;
        Error ->
            SleepFor = rand:uniform(RetryAfter),
            Backend = mongoose_backend:get_backend_name(global, ?MODULE),
            ?LOG_ERROR(#{what => rdbms_connection_attempt_error, backend => Backend,
                error => Error, sleep_for => SleepFor}),
            timer:sleep(timer:seconds(SleepFor)),
            NextRetryDelay = RetryAfter * RetryAfter,
            connect(Options, Retry - 1, min(MaxRetryDelay, NextRetryDelay), MaxRetryDelay)
    end.


-spec schedule_keepalive(KeepaliveInterval :: undefined | pos_integer()) -> any().
schedule_keepalive(KeepaliveInterval) ->
    case KeepaliveInterval of
        _ when is_integer(KeepaliveInterval) ->
            erlang:send_after(timer:seconds(KeepaliveInterval), self(), keepalive);
        undefined ->
            ok;
        Other ->
            ?LOG_ERROR(#{what => rdbms_wrong_keepalive_interval, reason => Other}),
            ok
    end.

%% ----- process state access, for convenient tracing

put_state(State) ->
    put(?STATE_KEY, State).

erase_state() ->
    erase(?STATE_KEY).

get_state() ->
    get(?STATE_KEY).
