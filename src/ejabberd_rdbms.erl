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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_rdbms).
-author('alexey@process-one.net').

-export([start/0, start_pool/1, stop_pool/1, pools/0]).
-include("mongoose.hrl").

-spec start() -> 'ok' | {'error', 'lager_not_running'}.
start() ->
    compile_odbc_type_helper(),
    %% Check if ejabberd has been compiled with ODBC
    case catch mongoose_rdbms_sup:module_info() of
        {'EXIT', {undef, _}} ->
            ?INFO_MSG("MongooseIM has not been compiled with relational database support. "
                      "Skipping database startup.", []);
        _ ->
            {ok, _Pid} = start_pool_sup(),
            [start_pool(Pool) || Pool <- pools()],
            ok
    end.


start_pool_sup() ->
    ChildSpec =
        {mongoose_rdbms_sup,
         {mongoose_rdbms_sup, start_link, []},
         transient,
         infinity,
         supervisor,
         [mongoose_rdbms_sup]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

pools() ->
    ejabberd_config:get_local_option_or_default(odbc_pools, []).

start_pool(Pool) ->
    mongoose_rdbms_sup:add_pool(Pool).

stop_pool(Pool) ->
    mongoose_rdbms_sup:remove_pool(Pool).

compile_odbc_type_helper() ->
    Key = {odbc_server_type, ?MYNAME},
    Type = ejabberd_config:get_local_option(Key),
    CodeStr = odbc_type_helper(Type),
    {Mod, Code} = dynamic_compile:from_string(CodeStr),
    code:load_binary(Mod, "mongoose_rdbms_type.erl", Code).

odbc_type_helper(Type) ->
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
