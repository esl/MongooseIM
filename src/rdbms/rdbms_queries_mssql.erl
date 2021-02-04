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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------
-module(rdbms_queries_mssql).
-author("michal.piotrowski").

-include("mongoose.hrl").

%% API
-export([begin_trans/0]).


begin_trans() ->
    [<<"BEGIN TRANSACTION;">>].
