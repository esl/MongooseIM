%%%----------------------------------------------------------------------
%%% File    : ejabberd_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Serve ODBC connection
%%% Created :  8 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
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

-module(ejabberd_odbc).
-author('alexey@process-one.net').

-behaviour(p1_fsm).

%% External exports
-export([start_link/1,
         sql_query/2,
         sql_query_t/1,
         sql_transaction/2,
         sql_bloc/2,
         escape/1,
         escape_like/1,
         to_bool/1,
         keep_alive/1,
         db_engine/1]).

%% BLOB escaping
-export([escape_format/1,
         escape_binary/2,
         unescape_binary/2,
         unescape_odbc_binary/2]).

%% count / integra types decoding
-export([result_to_integer/1]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         print_state/1,
         code_change/4]).

%% gen_fsm states
-export([connecting/2,
         connecting/3,
         session_established/2,
         session_established/3]).

%% internal usage
-export([get_db_info/1]).

-include("ejabberd.hrl").

-record(state, {db_ref,
                db_type :: atom(),
                start_interval :: integer(),
                host :: odbc_server(),
                max_pending_requests_len :: integer(),
                pending_requests
               }).
-type state() :: #state{}.

-define(STATE_KEY, ejabberd_odbc_state).
-define(MAX_TRANSACTION_RESTARTS, 10).
-define(PGSQL_PORT, 5432).
-define(MYSQL_PORT, 3306).

-define(TRANSACTION_TIMEOUT, 60000). % milliseconds
-define(KEEPALIVE_TIMEOUT, 60000).
-define(KEEPALIVE_QUERY, <<"SELECT 1;">>).

-define(QUERY_TIMEOUT, 5000).

%%-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

%% Points to ODBC server process
-type odbc_server() :: ejabberd:server() | pid() | {atom(), pid()}.
-export_type([odbc_server/0]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-spec start_link([Host :: ejabberd:server() |
                 integer()]) ->
                        'ignore' | {'error',_} | {'ok',pid()}.
start_link([Host, StartInterval]) ->
    p1_fsm:start_link(ejabberd_odbc, [Host, StartInterval],
                        fsm_limit_opts() ++ ?FSMOPTS).

-spec sql_query(Host :: odbc_server(), Query :: any()) -> any().
sql_query(Host, Query) ->
    sql_call(Host, {sql_query, Query}).

%% @doc SQL transaction based on a list of queries
-spec sql_transaction(odbc_server(), fun() | maybe_improper_list()) -> any().
sql_transaction(Host, Queries) when is_list(Queries) ->
    F = fun() -> lists:foreach(fun sql_query_t/1, Queries) end,
    sql_transaction(Host, F);
%% SQL transaction, based on a erlang anonymous function (F = fun)
sql_transaction(Host, F) when is_function(F) ->
    sql_call(Host, {sql_transaction, F}).

%% @doc SQL bloc, based on a erlang anonymous function (F = fun)
-spec sql_bloc(Host :: odbc_server(), F :: any()) -> any().
sql_bloc(Host, F) ->
    sql_call(Host, {sql_bloc, F}).

%% TODO: Better spec for RPC calls
-spec sql_call(Host :: odbc_server(),
               Msg :: {'sql_bloc',_} | {'sql_query',_} | {'sql_transaction',fun()}) ->
                      any().
sql_call(Host, Msg) when is_binary(Host) ->
    case get(?STATE_KEY) of
        undefined ->
            poolboy:transaction(ejabberd_odbc_sup:default_pool(Host),
                                fun(Worker) -> sql_call(Worker, Msg) end,
                                ?TRANSACTION_TIMEOUT);
        State ->
            nested_op(Msg, State)
    end;
%% For dedicated connections.
sql_call({_Host, Pid}, Msg) when is_pid(Pid) ->
    sql_call(Pid, Msg);
sql_call(Pid, Msg) when is_pid(Pid) ->
    Timestamp = p1_time_compat:monotonic_time(milli_seconds),
    p1_fsm:sync_send_event(Pid, {sql_cmd, Msg, Timestamp}, ?TRANSACTION_TIMEOUT).


%% @doc perform a harmless query on all opened connexions to avoid connexion close.
keep_alive(PID) ->
    case sql_call(PID, {sql_query, [?KEEPALIVE_QUERY]}) of
        {selected, _, _} ->
            ok;
        {error, _} = Error ->
            ok = p1_fsm:sync_send_all_state_event(PID, {keepalive_failed, Error})
    end.


get_db_info(Pid) ->
    p1_fsm:sync_send_all_state_event(Pid, get_db_info).


%% This function is intended to be used from inside an sql_transaction:
sql_query_t(Query) ->
    sql_query_t(Query, get(?STATE_KEY)).

sql_query_t(Query, State) ->
    QRes = sql_query_internal(Query, State),
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
    odbc_queries:escape_string(S).


%% @doc Escape character that will confuse an SQL engine
%% Percent and underscore only need to be escaped for
%% pattern matching like statement
%% INFO: Used in mod_vcard_odbc.
-spec escape_like(binary() | string()) -> binary() | string().
escape_like(S) ->
    odbc_queries:escape_like_string(S).


-spec escape_format(odbc_server()) -> hex | simple_escape.
escape_format(Host) ->
    case db_engine(Host) of
        pgsql -> hex;
        odbc ->
            Key = {odbc_server_type, Host},
            case ejabberd_config:get_local_option_or_default(Key, odbc) of
                pgsql ->
                    hex;
                mssql ->
                    mssql_hex;
                _ ->
                    simple_escape
            end;
        _     -> simple_escape
    end.


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

-spec hex_to_int(byte(),byte()) -> integer().
hex_to_int(X, Y) when is_integer(X), is_integer(Y) ->
    list_to_integer([X,Y], 16).

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
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------
init([Host, StartInterval]) ->
    %% For debugging and introspection only.
    put(mim_host, Host),
    put(mim_process_type, odbc_worker),
    case ejabberd_config:get_local_option({odbc_keepalive_interval, Host}) of
        KeepaliveInterval when is_integer(KeepaliveInterval) ->
            timer:apply_interval(timer:seconds(KeepaliveInterval), ?MODULE,
                                 keep_alive, [self()]);
        undefined ->
            ok;
        _Other ->
            ?ERROR_MSG("Wrong odbc_keepalive_interval definition '~p'"
                       " for host ~p.~n", [_Other, Host])
    end,
    [DBType | _] = db_opts(Host),

    p1_fsm:send_event(self(), connect),

    erlang:process_flag(trap_exit, true),
    {ok, connecting, #state{db_type = DBType,
                            host = Host,
                            max_pending_requests_len = max_fsm_queue(),
                            pending_requests = {0, queue:new()},
                            start_interval = StartInterval}}.

-spec connecting(_, state())
                -> {'next_state','connecting' | 'session_established',_}.
connecting(connect, #state{host = Host} = State) ->
    ConnectRes = case db_opts(Host) of
                     [mysql | Args] ->
                         apply(fun mysql_connect/5, Args);
                     [pgsql | Args] ->
                         apply(fun pgsql_connect/5, Args);
                     [odbc | Args] ->
                         apply(fun odbc_connect/1, Args)
                 end,
    {_, PendingRequests} = State#state.pending_requests,
    case ConnectRes of
        {ok, Ref} ->
            erlang:monitor(process, Ref),
            lists:foreach(
              fun(Req) ->
                      p1_fsm:send_event(self(), Req)
              end, queue:to_list(PendingRequests)),
            {next_state, session_established,
             State#state{db_ref = Ref,
                         pending_requests = {0, queue:new()}}};
        {error, Reason} ->
            ?INFO_MSG("~p connection failed:~n"
                      "** Reason: ~p~n"
                      "** Retry after: ~p seconds",
                      [State#state.db_type, Reason,
                       State#state.start_interval div 1000]),
            p1_fsm:send_event_after(State#state.start_interval,
                                      connect),
            {next_state, connecting, State}
    end;
connecting(Event, State) ->
    ?WARNING_MSG("unexpected event in 'connecting': ~p", [Event]),
    {next_state, connecting, State}.

-spec connecting(_, From :: any(), state()) ->
                        {'next_state','connecting',_} | {'reply',{'error','badarg'},'connecting',_}.
connecting({sql_cmd, {sql_query, ?KEEPALIVE_QUERY}, _Timestamp}, From, State) ->
    p1_fsm:reply(From, {error, "SQL connection failed"}),
    {next_state, connecting, State};
connecting({sql_cmd, Command, Timestamp} = Req, From, State) ->
    ?DEBUG("queuing pending request while connecting:~n\t~p", [Req]),
    {Len, PendingRequests} = State#state.pending_requests,
    NewPendingRequests =
        if Len < State#state.max_pending_requests_len ->
                {Len + 1, queue:in({sql_cmd, Command, From, Timestamp}, PendingRequests)};
           true ->
                lists:foreach(
                  fun({sql_cmd, _, To, _Timestamp}) ->
                          p1_fsm:reply(
                             To, {error, "SQL connection failed"})
                  end, queue:to_list(PendingRequests)),
                {1, queue:from_list([{sql_cmd, Command, From, Timestamp}])}
        end,
    {next_state, connecting,
     State#state{pending_requests = NewPendingRequests}};
connecting(Request, {Who, _Ref}, State) ->
    ?WARNING_MSG("unexpected call ~p from ~p in 'connecting'",
                 [Request, Who]),
    {reply, {error, badarg}, connecting, State}.

-spec session_established(_, From :: _, state())
                         -> {'next_state','session_established',_}
                                | {'stop','closed' | 'timeout',_}
                                | {'reply',{'error','badarg'},'session_established',_}.
session_established({sql_cmd, Command, Timestamp}, From, State) ->
    run_sql_cmd(Command, From, State, Timestamp);
session_established(Request, {Who, _Ref}, State) ->
    ?WARNING_MSG("unexpected call ~p from ~p in 'session_established'",
                 [Request, Who]),
    {reply, {error, badarg}, session_established, State}.

-spec session_established(_, state()) -> {'next_state','session_established',_}
                                             | {'stop','closed' | 'timeout',_}.
session_established(Event, State) ->
    ?WARNING_MSG("unexpected event in 'session_established': ~p", [Event]),
    {next_state, session_established, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(get_db_info, _, StateName,
                  #state{db_ref = DbRef, db_type = DbType} = State) ->
    {reply, {ok, DbType, DbRef}, StateName, State};
handle_sync_event({keepalive_failed, Error}, _From, _StateName, State) ->
    {stop, {keepalive_failed, Error}, ok, State};
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, {error, badarg}, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% We receive the down signal when we loose the MySQL connection (we are
%% monitoring the connection)
handle_info({'DOWN', _MonitorRef, process, _Pid, _Info}, _StateName, State) ->
    p1_fsm:send_event(self(), connect),
    {next_state, connecting, State};
handle_info({'EXIT', _From, _Reason}, StateName, State) ->
    {next_state, StateName, State};
handle_info(Info, StateName, State) ->
    ?WARNING_MSG("unexpected info in ~p: ~p", [StateName, Info]),
    {next_state, StateName, State}.

-spec terminate(_,_,state()) -> 'ok'.
terminate(_Reason, _StateName, State) ->
    case State#state.db_type of
        mysql ->
            %% old versions of mysql driver don't have the stop function
            %% so the catch
            catch p1_mysql_conn:stop(State#state.db_ref);
        pgsql ->
            catch pgsql:terminate(State#state.db_ref);
        _ ->
            ok
    end,
    ok.

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

-type odbc_timestamp() :: {non_neg_integer(),non_neg_integer(),non_neg_integer()}.
-spec run_sql_cmd(Command :: any(),
                  From :: any(),
                  State :: state(),
                  Timestamp :: odbc_timestamp())
                 -> {'next_state','session_established',state()}
                        | {'stop','closed' | 'timeout',state()}.
run_sql_cmd(Command, From, State, Timestamp) ->
    Now = p1_time_compat:monotonic_time(milli_seconds),
    case Now - Timestamp of
        Age when Age  < ?TRANSACTION_TIMEOUT ->
            abort_on_driver_error(outer_op(Command, State), State);
        Age ->
            ?ERROR_MSG("Database was not available or too slow,"
                       " discarding ~p milliseconds old request~n~p~n",
                       [Age, Command]),
            {next_state, session_established, State}
    end.

%% @doc Only called by handle_call, only handles top level operations.
-spec outer_op({'sql_bloc',_} | {'sql_query',_} | {'sql_transaction',fun()}, state()) ->
    {error | aborted | atomic, _}.
outer_op({sql_query, Query}, State) ->
    sql_query_internal(Query, State);
outer_op({sql_transaction, F}, State) ->
    outer_transaction(F, ?MAX_TRANSACTION_RESTARTS, "", State);
outer_op({sql_bloc, F}, State) ->
    execute_bloc(F, State).

%% @doc Called via sql_query/transaction/bloc from client code when inside a
%% nested operation
-spec nested_op({'sql_bloc',_} | {'sql_query',_} | {'sql_transaction',fun()}, state()) -> any().
nested_op({sql_query, Query}, State) ->
    %% XXX - use sql_query_t here insted? Most likely would break
    %% callers who expect {error, _} tuples (sql_query_t turns
    %% these into throws)
    sql_query_internal(Query, State);
nested_op({sql_transaction, F}, State) ->
    case in_transaction() of
        false ->
            %% First transaction inside a (series of) sql_blocs
            outer_transaction(F, ?MAX_TRANSACTION_RESTARTS, "", State);
        true ->
            %% Transaction inside a transaction
            inner_transaction(F, State)
    end;
nested_op({sql_bloc, F}, State) ->
    execute_bloc(F, State).

%% @doc Never retry nested transactions - only outer transactions
-spec inner_transaction(fun(), state()) -> {'EXIT',_} | {'aborted',_} | {'atomic',_}.
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
                        Reason :: any(), state()) -> {'aborted',_} | {'atomic',_}.
outer_transaction(F, NRestarts, _Reason, State) ->
    sql_query_internal(odbc_queries:begin_trans(), State),
    put(?STATE_KEY, State),
    Result = (catch F()),
    erase(?STATE_KEY),
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
            {aborted, Reason};
        {'EXIT', Reason} ->
            %% Abort sql transaction on EXIT from outer txn only.
            sql_query_internal([<<"rollback;">>], State),
            {aborted, Reason};
        Res ->
            %% Commit successful outer txn
            sql_query_internal([<<"commit;">>], State),
            {atomic, Res}
    end.

-spec execute_bloc(fun(), state()) -> {'aborted',_} | {'atomic',_}.
execute_bloc(F, _State) ->
    case catch F() of
        {aborted, Reason} ->
            {aborted, Reason};
        {'EXIT', Reason} ->
            {aborted, Reason};
        Res ->
            {atomic, Res}
    end.

sql_query_internal(Query, State) ->
    Res = case State#state.db_type of
              odbc ->
                  binaryze_odbc(odbc:sql_query(State#state.db_ref, Query,
                                               ?QUERY_TIMEOUT));
              pgsql ->
                  ?DEBUG("Postres, Send query~n~p~n", [Query]),
                  pgsql_to_odbc(pgsql:squery(State#state.db_ref, Query,
                                             ?QUERY_TIMEOUT));
              mysql ->
                  ?DEBUG("MySQL, Send query~n~p~n", [Query]),
                  mysql_to_odbc(p1_mysql_conn:squery(State#state.db_ref, Query,
                                                     self(), [{timeout, ?QUERY_TIMEOUT}, {result_type, binary}]))
          end,
    case Res of
        {error, "No SQL-driver information available."} ->
                                                % workaround for odbc bug
            {updated, 0};
        _Else -> Res
    end.

%% @doc Generate the OTP callback return tuple depending on the driver result.
-spec abort_on_driver_error(_, state())
                           -> {'next_state','session_established',_} | {'stop','closed' | 'timeout',_}.
abort_on_driver_error({error, "query timed out"} = Reply, State) ->
    %% mysql driver error
    {stop, timeout, Reply, State};
abort_on_driver_error({error, "Failed sending data on socket" ++ _} = Reply, State) ->
    %% mysql driver error
    {stop, closed, Reply, State};
abort_on_driver_error(Reply, State) ->
    {reply, Reply, session_established, State}.


%% == pure ODBC code

%% part of init/1
%% @doc Open an ODBC database connection
-spec odbc_connect(ConnString :: string()) -> {ok | error, _}.
odbc_connect(SQLServer) ->
    application:start(odbc),
    Opts = [{scrollable_cursors, off},
            {binary_strings, on},
            {timeout, 5000}],
    odbc:connect(SQLServer, Opts).

binaryze_odbc(ODBCResults) when is_list(ODBCResults) ->
    lists:map(fun binaryze_odbc/1, ODBCResults);
binaryze_odbc({selected, ColNames, Rows}) ->
    ColNamesB = lists:map(fun ejabberd_binary:string_to_binary/1, ColNames),
    {selected, ColNamesB, Rows};
binaryze_odbc(ODBCResult) ->
    ODBCResult.

%% == Native PostgreSQL code

%% part of init/1
%% @doc Open a database connection to PostgreSQL
pgsql_connect(Server, Port, DB, Username, Password) ->
    Params = [
              {host, Server},
              {database, DB},
              {user, Username},
              {password, Password},
              {port, Port},
              {as_binary, true}],
    case pgsql:connect(Params) of
        {ok, Ref} ->
            {ok,[<<"SET">>]} =
                pgsql:squery(Ref, "SET standard_conforming_strings=off;", ?QUERY_TIMEOUT),
            {ok, Ref};
        Err -> Err
    end.


%% @doc Convert PostgreSQL query result to Erlang ODBC result formalism
-spec pgsql_to_odbc({'ok', PGSQLResult :: [any()]})
                   -> [{'error',_} | {'updated','undefined' | integer()} | {'selected',[any()],[any()]}]
                          | {'error',_}
                          | {'updated','undefined' | integer()}
                          | {'selected',[any()],[tuple()]}.
pgsql_to_odbc({ok, PGSQLResult}) ->
    case PGSQLResult of
        [Item] ->
            pgsql_item_to_odbc(Item);
        Items ->
            [pgsql_item_to_odbc(Item) || Item <- Items]
    end.

-spec pgsql_item_to_odbc(tuple() | binary())
                        -> {'error',_}
                               | {'updated','undefined' | integer()}
                               | {'selected',[any()],[tuple()]}.
pgsql_item_to_odbc({<<"SELECT", _/binary>>, Rows, Recs}) ->
    {selected,
     [element(1, Row) || Row <- Rows],
     [list_to_tuple(Rec) || Rec <- Recs]};
pgsql_item_to_odbc(<<"INSERT ", OIDN/binary>>) ->
    [_OID, N] = binary:split(OIDN, <<" ">>),
    {updated, list_to_integer(binary_to_list(N))};
pgsql_item_to_odbc(<<"DELETE ", N/binary>>) ->
    {updated, list_to_integer(binary_to_list(N))};
pgsql_item_to_odbc(<<"UPDATE ", N/binary>>) ->
    {updated, list_to_integer(binary_to_list(N))};
pgsql_item_to_odbc({error, Error}) ->
    {error, Error};
pgsql_item_to_odbc(_) ->
    {updated,undefined}.

%% == Native MySQL code

%% part of init/1
%% @doc Open a database connection to MySQL
mysql_connect(Server, Port, Database, Username, Password) ->
    case p1_mysql_conn:start(Server, Port, Username, Password, Database, fun log/3) of
        {ok, Ref} ->
            p1_mysql_conn:squery(Ref, [<<"set names 'utf8';">>],
                                 self(), [{timeout, ?QUERY_TIMEOUT}, {result_type, binary}]),
            p1_mysql_conn:squery(Ref, [<<"SET SESSION query_cache_type=1;">>],
                                 self(), [{timeout, ?QUERY_TIMEOUT}, {result_type, binary}]),
            {ok, Ref};
        Err ->
            Err
    end.

%% @doc Convert MySQL query result to Erlang ODBC result formalism
-spec mysql_to_odbc({'data', _} | {'error', _} | {'updated', _})
                   -> {'error', _} | {'updated', _} | {'selected', [any()], [tuple()]}.
mysql_to_odbc({updated, MySQLRes}) ->
    {updated, p1_mysql:get_result_affected_rows(MySQLRes)};
mysql_to_odbc({data, MySQLRes}) ->
    mysql_item_to_odbc(p1_mysql:get_result_field_info(MySQLRes),
                       p1_mysql:get_result_rows(MySQLRes));
mysql_to_odbc({error, MySQLRes}) when is_list(MySQLRes) ->
    {error, MySQLRes};
mysql_to_odbc({error, MySQLRes}) ->
    {error, p1_mysql:get_result_reason(MySQLRes)}.

%% @doc When tabular data is returned, convert it to the ODBC formalism
-spec mysql_item_to_odbc(Columns :: [tuple()],
                         Recs :: [[any()]]) -> {'selected',[any()],[tuple()]}.
mysql_item_to_odbc(Columns, Recs) ->
    %% For now, there is a bug and we do not get the correct value from MySQL
    %% module:
    {selected,
     [element(2, Column) || Column <- Columns],
     [list_to_tuple(Rec) || Rec <- Recs]}.


%% @doc log function used by MySQL driver
-spec log(Level :: 'debug' | 'error' | 'normal',
          Format :: string(),
          Args :: list()) -> any().
log(Level, Format, Args) ->
    case Level of
        debug ->
            ?DEBUG(Format, Args);
        normal ->
            ?INFO_MSG(Format, Args);
        error ->
            ?ERROR_MSG(Format, Args)
    end.


-spec db_opts(Host :: atom()) -> [odbc | mysql | pgsql | [char() | tuple()],...].
db_opts(Host) ->
    case ejabberd_config:get_local_option({odbc_server, Host}) of
        %% Default pgsql port
        {pgsql, Server, DB, User, Pass} ->
            [pgsql, Server, ?PGSQL_PORT, DB, User, Pass];
        {pgsql, Server, Port, DB, User, Pass} when is_integer(Port) ->
            [pgsql, Server, Port, DB, User, Pass];
        %% Default mysql port
        {mysql, Server, DB, User, Pass} ->
            [mysql, Server, ?MYSQL_PORT, DB, User, Pass];
        {mysql, Server, Port, DB, User, Pass} when is_integer(Port) ->
            [mysql, Server, Port, DB, User, Pass];
        SQLServer when is_list(SQLServer) ->
            [odbc, SQLServer]
    end.


-spec db_engine(Host :: odbc_server() | pid()) -> ejabberd_config:value().
db_engine(Pid) when is_pid(Pid) ->
    {ok, DbType, _} = p1_fsm:sync_send_all_state_event(Pid, get_db_info),
    DbType;
db_engine(Host) when is_binary(Host) ->
    case ejabberd_config:get_local_option({odbc_server, Host}) of
        SQLServer when is_list(SQLServer) ->
            odbc;
        Other when is_tuple(Other) ->
            element(1, Other)
    end;
db_engine({Host, _Pid}) ->
    db_engine(Host).


-spec max_fsm_queue() -> 'undefined' | pos_integer().
max_fsm_queue() ->
    case ejabberd_config:get_local_option(max_fsm_queue) of
        N when is_integer(N), N>0 ->
            N;
        _ ->
            undefined
    end.


-spec fsm_limit_opts() -> [{'max_queue',pos_integer()}].
fsm_limit_opts() ->
    case max_fsm_queue() of
        N when is_integer(N) ->
            [{max_queue, N}];
        _ ->
            []
    end.


-spec in_transaction() -> boolean().
in_transaction() ->
    get(?STATE_KEY) =/= undefined.
