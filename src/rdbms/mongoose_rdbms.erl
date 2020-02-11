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
-type escaped_value() :: escaped_string() | escaped_binary() | escaped_integer() |
                         escaped_boolean() | escaped_null().

-export_types([escaped_binary/0,
               escaped_string/0,
               escaped_like/0,
               escaped_integer/0,
               escaped_boolean/0,
               escaped_null/0,
               escaped_value/0,
               sql_query/0,
               sql_query_part/0]).

-callback escape_binary(binary()) -> sql_query_part().
-callback escape_string(binary()|list()) -> sql_query_part().

-callback unescape_binary(binary()) -> binary().
-callback connect(Args :: any(), QueryTimeout :: non_neg_integer()) ->
    {ok, Connection :: term()} | {error, Reason :: any()}.
-callback disconnect(Connection :: term()) -> any().
-callback query(Connection :: term(), Query :: any(), Timeout :: infinity | non_neg_integer()) ->
    query_result().
-callback prepare(Connection :: term(), Name :: atom(),
                  Table :: binary(), Fields :: [binary()], Statement :: iodata()) ->
    {ok, Ref :: term()} | {error, Reason :: any()}.
-callback execute(Connection :: term(), Ref :: term(), Parameters :: [term()],
                  Timeout :: infinity | non_neg_integer()) -> query_result().

%% If not defined, generic escaping is used
-optional_callbacks([escape_string/1]).

%% External exports
-export([prepare/4,
         execute/3,
         sql_query/2,
         sql_query_t/1,
         sql_transaction/2,
         sql_dirty/2,
         to_bool/1,
         db_engine/1,
         print_state/1,
         use_escaped/1]).

%% Unicode escaping
-export([escape_string/1,
         use_escaped_string/1]).

%% Integer escaping
-export([escape_integer/1,
         use_escaped_integer/1]).

%% Boolean escaping
-export([escape_boolean/1,
         use_escaped_boolean/1]).

%% LIKE escaping
-export([escape_like/1,
         escape_like_prefix/1,
         use_escaped_like/1]).

%% BLOB escaping
-export([escape_binary/2,
         unescape_binary/2,
         use_escaped_binary/1]).

%% Null escaping
%% (to keep uniform pattern of passing values)
-export([escape_null/0,
         use_escaped_null/1]).

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

-include("mongoose.hrl").

-record(state, {db_ref,
                prepared = #{} :: #{binary() => term()},
                keepalive_interval :: undefined | pos_integer()
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
-define(CONNECT_RETRIES, 5).

-type server() :: binary() | global.
-type rdbms_msg() :: {sql_query, _}
                   | {sql_transaction, fun()}
                   | {sql_dirty, fun()}
                   | {sql_execute, atom(), iodata()}.
-type single_query_result() :: {selected, [tuple()]} |
                               {updated, non_neg_integer() | undefined} |
                               {updated, non_neg_integer(), [tuple()]} |
                               {aborted, Reason :: term()} |
                               {error, Reason :: string() | duplicate_key}.
-type query_result() :: single_query_result() | [single_query_result()].
-type transaction_result() :: {aborted, _} | {atomic, _} | {error, _}.
-type dirty_result() :: {ok, any()} | {error, any()}.
-export_type([query_result/0,
              server/0]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec prepare(Name, Table :: binary() | atom(), Fields :: [binary() | atom()],
              Statement :: iodata()) ->
                     {ok, Name} | {error, already_exists}
                         when Name :: atom().
prepare(Name, Table, Fields, Statement) when is_atom(Table) ->
    prepare(Name, atom_to_binary(Table, utf8), Fields, Statement);
prepare(Name, Table, [Field | _] = Fields, Statement) when is_atom(Field) ->
    prepare(Name, Table, [atom_to_binary(F, utf8) || F <- Fields], Statement);
prepare(Name, Table, Fields, Statement) when is_atom(Name), is_binary(Table) ->
    true = lists:all(fun is_binary/1, Fields),
    case ets:insert_new(prepared_statements, {Name, Table, Fields, Statement}) of
        true  -> {ok, Name};
        false -> {error, already_exists}
    end.

-spec execute(Host :: server(), Name :: atom(), Parameters :: [term()]) ->
                     query_result().
execute(Host, Name, Parameters) when is_atom(Name), is_list(Parameters) ->
    sql_call(Host, {sql_execute, Name, Parameters}).

-spec sql_query(Host :: server(), Query :: any()) -> query_result().
sql_query(Host, Query) ->
    sql_call(Host, {sql_query, Query}).

%% @doc SQL transaction based on a list of queries
-spec sql_transaction(server(), fun() | maybe_improper_list()) -> transaction_result().
sql_transaction(Host, Queries) when is_list(Queries) ->
    F = fun() -> lists:map(fun sql_query_t/1, Queries) end,
    sql_transaction(Host, F);
%% SQL transaction, based on a erlang anonymous function (F = fun)
sql_transaction(Host, F) when is_function(F) ->
    sql_call(Host, {sql_transaction, F}).

-spec sql_dirty(server(), fun()) -> any() | no_return().
sql_dirty(Host, F) when is_function(F) ->
    case sql_call(Host, {sql_dirty, F}) of
        {ok, Result} -> Result;
        {error, Error} -> throw(Error)
    end.

%% TODO: Better spec for RPC calls
-spec sql_call(Host :: server(), Msg :: rdbms_msg()) -> any().
sql_call(Host, Msg) ->
    case get_state() of
        undefined -> sql_call0(Host, Msg);
        State     ->
            {Res, NewState} = nested_op(Msg, State),
            put_state(NewState),
            Res
    end.

-spec sql_call0(Host :: server(), Msg :: rdbms_msg()) -> any().
sql_call0(Host, Msg) ->
    Timestamp = erlang:monotonic_time(millisecond),
    mongoose_wpool:call(rdbms, Host, {sql_cmd, Msg, Timestamp}).

-spec get_db_info(Target :: server() | pid()) ->
                         {ok, DbType :: atom(), DbRef :: term()} | {error, any()}.
get_db_info(Pid) when is_pid(Pid) ->
    wpool_process:call(Pid, get_db_info, 5000);
get_db_info(Host) ->
    mongoose_wpool:call(rdbms, Host, get_db_info).

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

-spec escape_binary(server(), binary()) -> escaped_binary().
escape_binary(_Host, Bin) when is_binary(Bin) ->
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
    ?ERROR_MSG("event=use_escaped_failure value=~p stacktrace=~p",
               [X, erlang:process_info(self(), current_stacktrace)]),
    erlang:error({use_escaped_string, X}).

-spec use_escaped_binary(escaped_binary()) -> sql_query_part().
use_escaped_binary({escaped_binary, S}) ->
    S;
use_escaped_binary(X) ->
    ?ERROR_MSG("event=use_escaped_failure value=~p stacktrace=~p",
               [X, erlang:process_info(self(), current_stacktrace)]),
    erlang:error({use_escaped_binary, X}).

-spec use_escaped_like(escaped_like()) -> sql_query_part().
use_escaped_like({escaped_like, S}) ->
    S;
use_escaped_like(X) ->
    ?ERROR_MSG("event=use_escaped_failure value=~p stacktrace=~p",
               [X, erlang:process_info(self(), current_stacktrace)]),
    erlang:error({use_escaped_like, X}).

-spec use_escaped_integer(escaped_integer()) -> sql_query_part().
use_escaped_integer({escaped_integer, S}) ->
    S;
use_escaped_integer(X) ->
    ?ERROR_MSG("event=use_escaped_failure value=~p stacktrace=~p",
               [X, erlang:process_info(self(), current_stacktrace)]),
    erlang:error({use_escaped_integer, X}).

-spec use_escaped_boolean(escaped_boolean()) -> sql_query_part().
use_escaped_boolean({escaped_boolean, S}) ->
    S;
use_escaped_boolean(X) ->
    ?ERROR_MSG("event=use_escaped_failure value=~p stacktrace=~p",
               [X, erlang:process_info(self(), current_stacktrace)]),
    erlang:error({use_escaped_boolean, X}).

-spec use_escaped_null(escaped_null()) -> sql_query_part().
use_escaped_null({escaped_null, S}) ->
    S;
use_escaped_null(X) ->
    ?ERROR_MSG("event=use_escaped_failure value=~p stacktrace=~p",
               [X, erlang:process_info(self(), current_stacktrace)]),
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
    ?ERROR_MSG("event=use_escaped_failure value=~p stacktrace=~p",
               [X, erlang:process_info(self(), current_stacktrace)]),
    erlang:error({use_escaped, X}).

-spec escape_like_internal(binary() | string()) -> binary() | string().
escape_like_internal(S) when is_binary(S) ->
    list_to_binary(escape_like_internal(binary_to_list(S)));
escape_like_internal(S) when is_list(S) ->
    [escape_like_character(C) || C <- S].

escape_string_internal(S) ->
    case erlang:function_exported(mongoose_rdbms_backend:backend(), escape_string, 1) of
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


-spec unescape_binary(server(), binary()) -> binary().
unescape_binary(_Host, Bin) when is_binary(Bin) ->
    mongoose_rdbms_backend:unescape_binary(Bin).


-spec result_to_integer(binary() | integer()) -> integer().
result_to_integer(Int) when is_integer(Int) ->
    Int;
result_to_integer(Bin) when is_binary(Bin) ->
    binary_to_integer(Bin).

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
-spec init(term()) -> {ok, state()}.
init(Opts) ->
    process_flag(trap_exit, true),
    {server, Settings} = lists:keyfind(server, 1, Opts),
    KeepaliveInterval = proplists:get_value(keepalive_interval, Opts),
    MaxStartInterval = proplists:get_value(start_interval, Opts, 30),
    case connect(Settings, ?CONNECT_RETRIES, 2, MaxStartInterval) of
        {ok, DbRef} ->
            schedule_keepalive(KeepaliveInterval),
            {ok, #state{db_ref = DbRef, keepalive_interval = KeepaliveInterval}};
        Error ->
            {stop, Error}
    end.


handle_call({sql_cmd, Command, Timestamp}, From, State) ->
    run_sql_cmd(Command, From, State, Timestamp);
handle_call(get_db_info, _, #state{db_ref = DbRef} = State) ->
    {reply, {ok, db_engine(?MYNAME), DbRef}, State};
handle_call(_Event, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast(Request, State) ->
    ?WARNING_MSG("unexpected cast: ~p", [Request]),
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
    Now = erlang:monotonic_time(millisecond),
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
    sql_execute(Name, Params, State).

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
            ?ERROR_MSG("event=sql_transaction_restarts_exceeded "
                       "restarts=~p "
                       "last_abort_reason=~1000p "
                       "last_sql_query=~1000p "
                       "stacktrace=~1000p "
                       "state=~1000p",
                       [?MAX_TRANSACTION_RESTARTS, Reason,
                        iolist_to_binary(SqlQuery),
                        StackTrace, NewState]),
            sql_query_internal([<<"rollback;">>], NewState),
            {{aborted, Reason}, NewState};
        {aborted, Reason} when NRestarts =:= 0 -> %% old format for abort
            %% Too many retries of outer transaction.
            ?ERROR_MSG("event=sql_transaction_restarts_exceeded "
                       "restarts=~p "
                       "last_abort_reason=~1000p "
                       "stacktrace=~1000p "
                       "state=~1000p",
                       [?MAX_TRANSACTION_RESTARTS, Reason,
                        StackTrace, NewState]),
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
        Class:Other:StackTrace ->
            ?ERROR_MSG("issue=outer_transaction_failed "
                       "reason=~p:~p stacktrace=~1000p",
                       [Class, Other, StackTrace]),
            {{'EXIT', Other}, StackTrace}
    end.

sql_query_internal(Query, #state{db_ref = DBRef}) ->
    case mongoose_rdbms_backend:query(DBRef, Query, ?QUERY_TIMEOUT) of
        {error, "No SQL-driver information available."} ->
            {updated, 0}; %% workaround for odbc bug
        Result ->
            Result
    end.

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

-spec sql_execute(Name :: atom(), Params :: [term()], state()) -> {query_result(), state()}.
sql_execute(Name, Params, State = #state{db_ref = DBRef}) ->
    {StatementRef, NewState} = prepare_statement(Name, State),
    Res = try mongoose_rdbms_backend:execute(DBRef, StatementRef, Params, ?QUERY_TIMEOUT)
          catch Class:Reason:StackTrace ->
            ?ERROR_MSG("event=sql_execute_failed "
                        "statement_name=~p reason=~p:~p "
                        "params=~1000p stacktrace=~1000p",
                       [Name, Class, Reason, Params, StackTrace]),
            erlang:raise(Class, Reason, StackTrace)
          end,
    {Res, NewState}.

-spec prepare_statement(Name :: atom(), state()) -> {Ref :: term(), state()}.
prepare_statement(Name, State = #state{db_ref = DBRef, prepared = Prepared}) ->
    case maps:get(Name, Prepared, undefined) of
        undefined ->
            [{_, Table, Fields, Statement}] = ets:lookup(prepared_statements, Name),
            {ok, Ref} = mongoose_rdbms_backend:prepare(DBRef, Name, Table, Fields, Statement),
            {Ref, State#state{prepared = maps:put(Name, Ref, Prepared)}};

        Ref ->
            {Ref, State}
    end.

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

-spec db_engine(Host :: server()) -> odbc | mysql | pgsql | undefined.
db_engine(_Host) ->
    try mongoose_rdbms_backend:backend_name()
    catch error:undef -> undefined end.


-spec connect(Settings :: term(), Retry :: non_neg_integer(), RetryAfter :: non_neg_integer(),
              MaxRetryDelay :: non_neg_integer()) -> {ok, term()} | {error, any()}.
connect(Settings, Retry, RetryAfter, MaxRetryDelay) ->
    case mongoose_rdbms_backend:connect(Settings, ?QUERY_TIMEOUT) of
        {ok, _} = Ok ->
            Ok;
        Error when Retry =:= 0 ->
            Error;
        Error ->
            SleepFor = rand:uniform(RetryAfter),
            Backend = mongoose_rdbms_backend:backend_name(),
            ?ERROR_MSG("Connection attempt to ~s resulted in ~p."
                       " Retrying in ~p seconds.", [Backend, Error, SleepFor]),
            timer:sleep(timer:seconds(SleepFor)),
            NextRetryDelay = RetryAfter * RetryAfter,
            connect(Settings, Retry - 1, min(MaxRetryDelay, NextRetryDelay), MaxRetryDelay)
    end.


-spec schedule_keepalive(KeepaliveInterval :: undefined | pos_integer()) -> any().
schedule_keepalive(KeepaliveInterval) ->
    case KeepaliveInterval of
        _ when is_integer(KeepaliveInterval) ->
            erlang:send_after(timer:seconds(KeepaliveInterval), self(), keepalive);
        undefined ->
            ok;
        Other ->
            ?ERROR_MSG("Wrong keepalive_interval definition '~p'~n", [Other]),
            ok
    end.

%% ----- process state access, for convenient tracing

put_state(State) ->
    put(?STATE_KEY, State).

erase_state() ->
    erase(?STATE_KEY).

get_state() ->
    get(?STATE_KEY).
