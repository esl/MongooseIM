%%%-------------------------------------------------------------------
%%% @author michal.piotrowski
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2014 13:36
%%%-------------------------------------------------------------------
-module(odbc_queries_mssql).
-author("michal.piotrowski").

%% API
-export([get_db_type/0,
         begin_trans/0,
         count_offline_messages/4]).


get_db_type() ->
    mssql.

count_offline_messages(LServer, SUser, SServer, Limit) ->
    ejabberd_odbc:sql_query(
        LServer,
        [<<"select top ">>, integer_to_list(Limit),
         <<"count(*) from offline_message "
         "where server = '">>, SServer, <<"' and "
         "username = '">>, SUser, <<"'">>]).

begin_trans() ->
    [<<"BEGIN TRANSACTION;">>].
