%%%----------------------------------------------------------------------
%%% File    : mongoose_rdbms.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Serve ODBC connection
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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mongoose_rdbms).
-author('alexey@process-one.net').
-author('konrad.zemek@erlang-solutions.com').

-behaviour(gen_server).

-callback escape_format(Host :: ejabberd:server()) -> atom().
-callback connect(Args :: any(), QueryTimeout :: non_neg_integer()) ->
    {ok, Connection :: term()} | {error, Reason :: any()}.
-callback disconnect(Connection :: term()) -> any().
-callback query(Connection :: term(), Query :: any(), Timeout :: infinity | non_neg_integer()) ->
    query_result().
-callback prepare(Connection :: term(), Name :: atom(), Statement :: iodata()) ->
    {ok, Ref :: term()} | {error, Reason :: any()}.
-callback execute(Connection :: term(), Ref :: term(), Parameters :: [term()],
                  Timeout :: infinity | non_neg_integer()) -> query_result().
-callback is_error_duplicate(Reason :: string()) -> boolean().

%% External exports
-export([prepare/2,
         execute/3,
         sql_query/2,
         sql_query_t/1,
         sql_transaction/2,
         escape/1,
         escape_like/1,
         to_bool/1,
         db_engine/1,
         print_state/1,
         is_error_duplicate/1]).

%% BLOB escaping
-export([escape_format/1,
         escape_binary/2,
         unescape_binary/2,
         unescape_odbc_binary/2]).

%% count / integra types decoding
-export([result_to_integer/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% internal usage
-export([get_db_info/1]).

-include("ejabberd.hrl").

-record(state, {db_ref,
                db_type :: atom(),
                host :: ejabberd:server(),
                prepared = #{} :: #{binary() => {term(), non_neg_integer()}}
               }).
-type state() :: #state{}.

-define(STATE_KEY, mongoose_rdbms_state).
-define(MAX_TRANSACTION_RESTARTS, 10).
-define(TRANSACTION_TIMEOUT, 60000). % milliseconds
-define(KEEPALIVE_TIMEOUT, 60000).
-define(KEEPALIVE_QUERY, <<"SELECT 1;">>).
-define(QUERY_TIMEOUT, 5000).
%% The value is arbitrary; supervisor will restart the connection once
%% the retry counter runs out. We just attempt to reduce log pollution.
-define(CONNECT_RETRIES, 3).

%% Points to ODBC server process
-type odbc_server() :: binary() | atom().
-type odbc_msg() :: {sql_query, _} | {sql_transaction, fun()} | {sql_execute, atom(), iodata()}.
-type single_query_result() :: {selected, [tuple()]} |
                               {updated, non_neg_integer() | undefined} |
                               {error, Reason :: any()}.
-type query_result() :: single_query_result() | [single_query_result()].
-type transaction_result() :: {aborted, _} | {atomic, _} | {error, _}.
-export_type([query_result/0]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec prepare(Name, Statement :: iodata()) -> {ok, Name} when Name :: atom().
prepare(Name, Statement) when is_atom(Name) ->
    Version =
        case ets:lookup(prepared_statements, Name) of
            [{_Name, _Statement, V}] -> V + 1;
            [] -> 0
        end,
    true = ets:insert(prepared_statements, {Name, Statement, Version}),
    {ok, Name}.

-spec execute(HostOrPool :: odbc_server(), Name :: atom(), Parameters :: [term()]) ->
                     query_result().
execute(HostOrPool, Name, Parameters) when is_atom(Name), is_list(Parameters) ->
    sql_call(HostOrPool, {sql_execute, Name, Parameters}).

-spec sql_query(HostOrPool :: odbc_server(), Query :: any()) -> query_result().
sql_query(HostOrPool, Query) ->
    sql_call(HostOrPool, {sql_query, Query}).

%% @doc SQL transaction based on a list of queries
-spec sql_transaction(odbc_server(), fun() | maybe_improper_list()) -> transaction_result().
sql_transaction(HostOrPool, Queries) when is_list(Queries) ->
    F = fun() -> lists:map(fun sql_query_t/1, Queries) end,
    sql_transaction(HostOrPool, F);
%% SQL transaction, based on a erlang anonymous function (F = fun)
sql_transaction(HostOrPool, F) when is_function(F) ->
    sql_call(HostOrPool, {sql_transaction, F}).

%% TODO: Better spec for RPC calls
-spec sql_call(HostOrPool :: odbc_server(), Msg :: odbc_msg()) -> any().
sql_call(HostOrPool, Msg) ->
    case get(?STATE_KEY) of
        undefined -> sql_call0(HostOrPool, Msg);
        State     ->
            {Res, NewState} = nested_op(Msg, State),
            put(?STATE_KEY, NewState),
            Res
    end.


-spec sql_call0(HostOrPool :: odbc_server(), Msg :: odbc_msg()) -> any().
sql_call0(Host, Msg) when is_binary(Host) ->
    sql_call0(mongoose_rdbms_sup:default_pool(Host), Msg);
sql_call0(Pool, Msg) when is_atom(Pool) ->
    case whereis(Pool) of
        undefined -> {error, {no_odbc_pool, Pool}};
        _ ->
            Timestamp = p1_time_compat:monotonic_time(milli_seconds),
            wpool:call(Pool, {sql_cmd, Msg, Timestamp}, best_worker, ?TRANSACTION_TIMEOUT)
    end.


-spec get_db_info(Target :: odbc_server() | pid()) ->
                         {ok, DbType :: atom(), DbRef :: term()} | {error, any()}.
get_db_info(Host) when is_binary(Host) ->
    get_db_info(mongoose_rdbms_sup:default_pool(Host));
get_db_info(Pool) when is_atom(Pool) ->
    case whereis(Pool) of
        undefined -> {error, {no_odbc_pool, Pool}};
        _ -> wpool:call(Pool, get_db_info)
    end;
get_db_info(Pid) when is_pid(Pid) ->
    wpool_process:call(Pid, get_db_info, 5000).



%% This function is intended to be used from inside an sql_transaction:
sql_query_t(Query) ->
    sql_query_t(Query, get(?STATE_KEY)).

sql_query_t(Query, State) ->
    {QRes, _} = sql_query_internal(Query, State),
    case QRes of
        {error, Reason} ->
            throw({aborted, Reason});
        _ when is_list(QRes) ->
            case lists:keysearch(error, 1, QRes) of
                {value, {error, Reason}} ->
                    throw({aborted, Reason});
                _ ->
                    QRes
            end;
        _ ->
            QRes
    end.


%% @doc Escape character that will confuse an SQL engine
-spec escape(binary() | string()) -> binary() | string().
escape(S) ->
    rdbms_queries:escape_string(S).


%% @doc Escape character that will confuse an SQL engine
%% Percent and underscore only need to be escaped for
%% pattern matching like statement
%% INFO: Used in mod_vcard_odbc.
-spec escape_like(binary() | string()) -> binary() | string().
escape_like(S) ->
    rdbms_queries:escape_like_string(S).


-spec escape_format(odbc_server()) -> hex | simple_escape.
escape_format(Host) ->
    mongoose_rdbms_backend:escape_format(Host).


-spec escape_binary('hex' | 'simple_escape', binary()) -> binary() | string().
escape_binary(hex, Bin) when is_binary(Bin) ->
    <<"\\\\x", (bin_to_hex:bin_to_hex(Bin))/binary>>;
escape_binary(mssql_hex, Bin) when is_binary(Bin) ->
    bin_to_hex:bin_to_hex(Bin);
escape_binary(simple_escape, Bin) when is_binary(Bin) ->
    escape(Bin).

-spec unescape_binary('hex' | 'simple_escape', binary()) -> binary().
unescape_binary(hex, <<"\\x", Bin/binary>>) when is_binary(Bin) ->
    hex_to_bin(Bin);
unescape_binary(_, Bin) ->
    Bin.

-spec unescape_odbc_binary(atom(), binary()) -> binary().
unescape_odbc_binary(odbc, Bin) when is_binary(Bin)->
    hex_to_bin(Bin);
unescape_odbc_binary(_, Bin) ->
    Bin.

-spec result_to_integer(binary() | integer()) -> integer().
result_to_integer(Int) when is_integer(Int) ->
    Int;
result_to_integer(Bin) when is_binary(Bin) ->
    binary_to_integer(Bin).

-spec hex_to_bin(binary()) -> <<_:_*1>>.
hex_to_bin(Bin) when is_binary(Bin) ->
    << <<(hex_to_int(X, Y))>> || <<X, Y>> <= Bin>>.

-spec hex_to_int(byte(), byte()) -> integer().
hex_to_int(X, Y) when is_integer(X), is_integer(Y) ->
    list_to_integer([X, Y], 16).

-spec to_bool(binary() | string() | atom() | integer() | any()) -> boolean().
to_bool(B) when is_binary(B) ->
    to_bool(binary_to_list(B));
to_bool("t") -> true;
to_bool("true") -> true;
to_bool("1") -> true;
to_bool(true) -> true;
to_bool(1) -> true;
to_bool(_) -> false.

-spec is_error_duplicate(Reason :: string()) -> boolean().
is_error_duplicate(Reason) ->
    mongoose_rdbms_backend:is_error_duplicate(Reason).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------
-spec init(ejabberd:server()) -> {ok, state()}.
init(Host) ->
    process_flag(trap_exit, true),
    backend_module:create(?MODULE, db_engine(Host), [query]),
    Settings = ejabberd_config:get_local_option({odbc_server, Host}),
    RetryAfterSeconds = get_start_interval(Host),
    case connect(Settings, ?CONNECT_RETRIES, RetryAfterSeconds) of
        {ok, DbRef} ->
            schedule_keepalive(Host),
            {ok, #state{db_type = db_engine(Host), host = Host, db_ref = DbRef}};
        Error ->
            {stop, Error}
    end.


handle_call({sql_cmd, Command, Timestamp}, From, State) ->
    run_sql_cmd(Command, From, State, Timestamp);
handle_call(get_db_info, _, #state{db_ref = DbRef, db_type = DbType} = State) ->
    {reply, {ok, DbType, DbRef}, State};
handle_call(_Event, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast(Request, State) ->
    ?WARNING_MSG("unexpected cast: ~p", [Request]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(keepalive, State) ->
    case sql_query_internal([?KEEPALIVE_QUERY], State) of
        {selected, _} ->
            schedule_keepalive(State#state.host),
            {noreply, State};
        {error, _} = Error ->
            {stop, {keepalive_failed, Error}, State}
    end;
handle_info({'EXIT', _Pid, _Reason} = Reason, State) ->
    {stop, Reason, State};
handle_info(Info, State) ->
    ?WARNING_MSG("unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(Reason :: term(), state()) -> any().
terminate(_Reason, #state{db_ref = DbRef}) ->
    catch mongoose_rdbms_backend:disconnect(DbRef).

%%----------------------------------------------------------------------
%% Func: print_state/1
%% Purpose: Prepare the state to be printed on error log
%% Returns: State to print
%%----------------------------------------------------------------------
print_state(State) ->
    State.
%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

-spec run_sql_cmd(Command :: any(), From :: any(), State :: state(), Timestamp :: integer()) ->
                         {reply, Reply :: any(), state()} | {stop, Reason :: term(), state()} |
                         {noreply, state()}.
run_sql_cmd(Command, _From, State, Timestamp) ->
    Now = p1_time_compat:monotonic_time(milli_seconds),
    case Now - Timestamp of
        Age when Age  < ?TRANSACTION_TIMEOUT ->
            abort_on_driver_error(outer_op(Command, State));
        Age ->
            ?ERROR_MSG("Database was not available or too slow,"
                       " discarding ~p milliseconds old request~n~p~n",
                       [Age, Command]),
            {reply, {error, timeout}, State}
    end.

%% @doc Only called by handle_call, only handles top level operations.
-spec outer_op(odbc_msg(), state()) -> query_result() | transaction_result().
outer_op({sql_query, Query}, State) ->
    sql_query_internal(Query, State);
outer_op({sql_transaction, F}, State) ->
    outer_transaction(F, ?MAX_TRANSACTION_RESTARTS, "", State);
outer_op({sql_execute, Name, Params}, State) ->
    sql_execute(Name, Params, State).

%% @doc Called via sql_query/transaction/bloc from client code when inside a
%% nested operation
-spec nested_op(odbc_msg(), state()) -> any().
nested_op({sql_query, Query}, State) ->
    %% XXX - use sql_query_t here insted? Most likely would break
    %% callers who expect {error, _} tuples (sql_query_t turns
    %% these into throws)
    sql_query_internal(Query, State);
nested_op({sql_transaction, F}, State) ->
    %% Transaction inside a transaction
    inner_transaction(F, State);
nested_op({sql_execute, Name, Params}, State) ->
    sql_execute(Name, Params, State).

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
    put(?STATE_KEY, State),
    Result = (catch F()),
    erase(?STATE_KEY), % Explicitly ignore state changed inside transaction
    case Result of
        {aborted, Reason} when NRestarts > 0 ->
            %% Retry outer transaction upto NRestarts times.
            sql_query_internal([<<"rollback;">>], State),
            outer_transaction(F, NRestarts - 1, Reason, State);
        {aborted, Reason} when NRestarts =:= 0 ->
            %% Too many retries of outer transaction.
            ?ERROR_MSG("SQL transaction restarts exceeded~n"
                       "** Restarts: ~p~n"
                       "** Last abort reason: ~p~n"
                       "** Stacktrace: ~p~n"
                       "** When State == ~p",
                       [?MAX_TRANSACTION_RESTARTS, Reason,
                        erlang:get_stacktrace(), State]),
            sql_query_internal([<<"rollback;">>], State),
            {{aborted, Reason}, State};
        {'EXIT', Reason} ->
            %% Abort sql transaction on EXIT from outer txn only.
            sql_query_internal([<<"rollback;">>], State),
            {{aborted, Reason}, State};
        Res ->
            %% Commit successful outer txn
            sql_query_internal([<<"commit;">>], State),
            {{atomic, Res}, State}
    end.

sql_query_internal(Query, State = #state{db_ref = DBRef}) ->
    case mongoose_rdbms_backend:query(DBRef, Query, ?QUERY_TIMEOUT) of
        {error, "No SQL-driver information available."} ->
            {{updated, 0}, State}; %% workaround for odbc bug
        Result ->
            {Result, State}
    end.

-spec sql_execute(Name :: atom(), Params :: [term()], state()) -> {query_result(), state()}.
sql_execute(Name, Params, State = #state{db_ref = DBRef, prepared = Prepared}) ->
    [{_, Statement, Version}] = ets:lookup(prepared_statements, Name),
    {NewPrepared, StatementRef} =
        case maps:get(Name, Prepared, undefined) of
            {Ref, Version} -> {Prepared, Ref};
            _ ->
                {ok, Ref} = mongoose_rdbms_backend:prepare(DBRef, Name, Statement),
                NewP = maps:put(Name, {Ref, Version}, Prepared),
                {NewP, Ref}
        end,
    Res = mongoose_rdbms_backend:execute(DBRef, StatementRef, Params, ?QUERY_TIMEOUT),
    {Res, State#state{prepared = NewPrepared}}.


%% @doc Generate the OTP callback return tuple depending on the driver result.
-spec abort_on_driver_error({_, state()}) ->
                                   {reply, Reply :: term(), state()} |
                                   {stop, timeout | closed, state()}.
abort_on_driver_error({{error, "query timed out"} = Reply, State}) ->
    %% mysql driver error
    {stop, timeout, Reply, State};
abort_on_driver_error({{error, "Failed sending data on socket" ++ _} = Reply, State}) ->
    %% mysql driver error
    {stop, closed, Reply, State};
abort_on_driver_error({Reply, State}) ->
    {reply, Reply, State}.


-spec db_engine(Host :: odbc_server()) -> ejabberd_config:value().
db_engine(Pool) when is_atom(Pool) ->
    {ok, DbType, _} = wpool:call(Pool, get_db_info),
    DbType;
db_engine(Host) when is_binary(Host) ->
    case ejabberd_config:get_local_option({odbc_server, Host}) of
        SQLServer when is_list(SQLServer) ->
            odbc;
        Other when is_tuple(Other) ->
            element(1, Other)
    end.


-spec connect(Settings :: term(), Retry :: non_neg_integer(),
              RetryAfterSeconds :: non_neg_integer()) -> {ok, term()} | {error, any()}.
connect(Settings, Retry, RetryAfterSeconds) ->
    case mongoose_rdbms_backend:connect(Settings, ?QUERY_TIMEOUT) of
        {ok, _} = Ok ->
            Ok;
        Error when Retry =:= 0 ->
            Error;
        Error ->
            ?ERROR_MSG("Database connection attempt with ~p resulted in ~p."
                       " Retrying in ~p seconds.", [Settings, Error, RetryAfterSeconds]),
            timer:sleep(timer:seconds(RetryAfterSeconds)),
            connect(Settings, Retry - 1, RetryAfterSeconds)
    end.


-spec schedule_keepalive(ejabberd:server()) -> any().
schedule_keepalive(Host) ->
    case ejabberd_config:get_local_option({odbc_keepalive_interval, Host}) of
        KeepaliveInterval when is_integer(KeepaliveInterval) ->
            erlang:send_after(timer:seconds(KeepaliveInterval), self(), keepalive);
        undefined ->
            ok;
        _Other ->
            ?ERROR_MSG("Wrong odbc_keepalive_interval definition '~p'"
                       " for host ~p.~n", [_Other, Host]),
            ok
    end.


-spec get_start_interval(ejabberd:server()) -> any().
get_start_interval(Host) ->
    DefaultInterval = 30,
    case ejabberd_config:get_local_option({odbc_start_interval, Host}) of
        StartInterval when is_integer(StartInterval) ->
            StartInterval;
        undefined ->
            DefaultInterval;
        _Other ->
            ?ERROR_MSG("Wrong odbc_start_interval definition '~p'"
                       " for host ~p, defaulting to ~p seconds.~n",
                       [_Other, Host, DefaultInterval]),
            DefaultInterval
    end.
