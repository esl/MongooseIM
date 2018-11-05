%%%----------------------------------------------------------------------
%%% File    : rdbms_queries_mssql.erl
%%% Purpose : MSSQL specific queries
%%% Created :  17 Sep 2014
%%%
%%% ejabberd, Copyright (C) 2014   Erlang Solutions Ltd.
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
-module(rdbms_queries_mssql).
-author("michal.piotrowski").

-include("mongoose.hrl").

%% API
-export([begin_trans/0,
         query_archive_id/3,
         search_vcard/3,
         count_offline_messages/4]).


begin_trans() ->
    [<<"BEGIN TRANSACTION;">>].

search_vcard(LServer, RestrictionSQL, infinity) ->
    do_search_vcard(LServer, RestrictionSQL, <<"">>);
search_vcard(LServer, RestrictionSQL, Limit) when is_integer(Limit) ->
    LimitBin = integer_to_binary(Limit),
    do_search_vcard(LServer, RestrictionSQL, <<" TOP ", LimitBin/binary, " ">>).

do_search_vcard(LServer, RestrictionSQL, Limit) ->
    mongoose_rdbms:sql_query(
    LServer,
    [<<"select", Limit/binary, "username, server, fn, family, given, middle, "
     "nickname, bday, ctry, locality, "
     "email, orgname, orgunit from vcard_search ">>,
     RestrictionSQL, ";"]).

query_archive_id(Host, SServer, SUserName) ->
    mongoose_rdbms:sql_query(
        Host,
        ["SELECT TOP 1 id "
        "FROM mam_server_user "
        "WHERE server=", mongoose_rdbms:use_escaped_string(SServer),
        " AND user_name=", mongoose_rdbms:use_escaped_string(SUserName)]).

count_offline_messages(LServer, SUser, SServer, Limit) ->
    mongoose_rdbms:sql_query(
        LServer,
        [<<"SELECT TOP ">>, integer_to_list(Limit),
         <<"count(*) FROM offline_message "
         "WHERE server=">>, mongoose_rdbms:use_escaped_string(SServer),
         <<" AND username=">>, mongoose_rdbms:use_escaped_string(SUser)]).
