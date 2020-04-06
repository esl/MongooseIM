%%%----------------------------------------------------------------------
%%% File    : ejabberd_rdbms.erl
%%% Author  : Mickael Remond <mickael.remond@process-one.net>
%%% Purpose : Manage the start of the database modules when needed
%%% Created : 31 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_rdbms).
-author('alexey@process-one.net').

-export([start/0]).
-include("mongoose.hrl").

-spec start() -> ok.
start() ->
    compile_rdbms_type_helper(),
    ok.

compile_rdbms_type_helper() ->
    %% TODO This parameter should not be global, but pool-name parameterized
    Type = ejabberd_config:get_local_option(rdbms_server_type),
    CodeStr = rdbms_type_helper(Type),
    {Mod, Code} = dynamic_compile:from_string(CodeStr),
    code:load_binary(Mod, "mongoose_rdbms_type.erl", Code).

rdbms_type_helper(Type) ->
    lists:flatten(
        ["-module(mongoose_rdbms_type).
         -export([get/0]).
         -spec get() -> atom().
         get() -> ", normalize_type(Type), ".\n"]
    ).

normalize_type(mssql) ->
    "mssql";
normalize_type(_) ->
    "generic".
