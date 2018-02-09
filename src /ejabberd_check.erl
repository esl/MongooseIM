%%%----------------------------------------------------------------------
%%% File    : ejabberd_check.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : Check ejabberd configuration and
%%% Created : 27 Feb 2008 by Mickael Remond <mremond@process-one.net>
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

-module(ejabberd_check).

-export([libs/0, config/0]).

-include("mongoose.hrl").
-include("ejabberd_config.hrl").

-compile([export_all]).

%% @doc TODO: We want to implement library checking at launch time to issue
%% human readable user messages.
libs() ->
    ok.


%% @doc Consistency check on ejabberd configuration
-spec config() -> ['ok'].
config() ->
    check_database_modules().


-spec check_database_modules() -> ['ok'].
check_database_modules() ->
     [check_database_module(M)||M<-get_db_used()].


-spec check_database_module('mysql' | 'odbc' | 'pgsql') -> 'ok'.
check_database_module(odbc) ->
    check_modules(odbc, [odbc, odbc_app, odbc_sup, mongoose_rdbms, mongoose_rdbms_sup,
                         rdbms_queries]);
check_database_module(mysql) ->
    check_modules(mysql, [mysql, mysql_cache, mysql_encode, mysql_protocol]);
check_database_module(pgsql) ->
    check_modules(pgsql, [epgsql, epgsql_binary, epgsql_errcodes, epgsql_fdatetime,
                          epgsql_idatetime, epgsql_sock, epgsql_types, epgsql_wire, epgsqla,
                          epgsqli]).


%% @doc Issue a critical error and throw an exit if needing module is
%% missing.
-spec check_modules(atom(), [module()]) -> 'ok'.
check_modules(DB, Modules) ->
    case get_missing_modules(Modules) of
        [] ->
            ok;
        MissingModules ->
            ?CRITICAL_MSG("MongooseIM is configured to use '~p', but the following Erlang modules "
                          "are not installed: '~p'", [DB, MissingModules]),
            exit(database_module_missing)
    end.


%% @doc Return the list of undefined modules
-spec get_missing_modules([module()]) -> [module()].
get_missing_modules(Modules) ->
    ModulesLoadResult = [{Module, code:ensure_loaded(Module)} || Module <- Modules],
    [Module || {Module, {error, _}} <- ModulesLoadResult].


%% @doc Return the list of databases used
-spec get_db_used() -> [atom()].
get_db_used() ->
    %% Retrieve domains with a database configured:
    Domains =
        ets:match(local_config, #local_config{key={odbc_server, '$1'},
                                              value='$2'}),
    %% Check that odbc is the auth method used for those domains:
    %% and return the database name
    DBs = lists:foldr(
            fun([Domain, DB], Acc) ->
                    case check_odbc_option(
                           ejabberd_config:get_local_option(
                             {auth_method, Domain})) of
                        true -> [get_db_type(DB)|Acc];
                        _ -> Acc
                    end
            end,
            [], Domains),
    lists:usort(DBs).


%% @doc Depending in the DB definition, return which type of DB this is.
%% Note that MSSQL is detected as ODBC.
-spec get_db_type(list() | tuple()) -> mysql | pgsql | odbc.
get_db_type(DB) when is_tuple(DB) ->
    element(1, DB);
get_db_type(DB) when is_list(DB) ->
    odbc.


%% @doc Return true if odbc option is used
-spec check_odbc_option(_) -> boolean().
check_odbc_option(odbc) ->
    true;
check_odbc_option(AuthMethods) when is_list(AuthMethods) ->
    lists:member(odbc, AuthMethods);
check_odbc_option(_) ->
    false.
